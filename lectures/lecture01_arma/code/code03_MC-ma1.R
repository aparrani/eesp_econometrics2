###############################################################################
# Lecture: Stationary ARMA (p,q) Models
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Understanding MLE estimator's consistency and asymptotic distribution
# based on an MA(1) process.
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment
rm(list = ls())

# Load the required packages
library("ggplot2")
library("quantmod")
library("doParallel")
library("gganimate")
library("transformr")

# Set seed
set.seed(211216)

# Setup parallel backend to use all but one of the cores.
n.cores <- 6
cl <- makeCluster(n.cores)
registerDoParallel(cl)

#######################################
# Parameters
#######################################
# The shock term is Gaussian if g == 1 and exponential if g == 0
g <- 0

# MA coefficient
theta <- 0.5

# Length of the time series
capT_vec <- c(4:100, seq(110, 200, 10), seq(250, 500, 50))

# Number of MC repetitions
M <- 1000

# Allowed distance for convergence in probability
delta <- c(0.25, 0.2, 0.15, 0.1)

#######################################
##############################################################################
# Run the Monte Carlo Experiment
##############################################################################
# Run a parallel loop over sample sizes
results <- foreach(
  capT = capT_vec, .inorder = TRUE, .errorhandling = "remove", .verbose = FALSE
) %dopar% {
  # Create a dataframe to store the results for each MC repetition
  resultsT <- data.frame(
    "capT" = rep(capT, M),
    "bias" = rep(NA, M),
    "normalized_coef" = rep(NA, M),
    "reject" = rep(NA, M)
  )

  # Loop over MC repetitions
  for (m in 1:M) {
    # Simulate an MA(1) process. If g == 1, it is a Gaussian process.
    if (g == 1) {
      Y <- arima.sim(model = list(ma = theta), n = capT, rand.gen = rnorm)

      # If g == 0, it is a exponential process
    } else {
      Y <- arima.sim(model = list(ma = theta), n = capT, rand.gen = rexp)

    }

    # Estimate an MA(1) model
    ma1 <- arima(
      x = Y,
      order = c(0, 0, 1),
      include.mean = TRUE
    )

    ###################################
    # Store the results
    ###################################
    # Store the estimated bias
    resultsT$bias[m] <- ma1$coef[1] - theta

    # Store the normalized coefficient
    resultsT$normalized_coef[m] <- (ma1$coef[1] - theta) / sqrt(ma1$var.coef[1,1])

    # Store the test decision
    resultsT$reject[m] <- as.numeric(
      abs((ma1$coef[1] - theta) / sqrt(ma1$var.coef[1,1])) >= qnorm(0.975)
    )

  }

  # Return the results
  return(resultsT)
}

# Stop parallel backend
stopCluster(cl)

##############################################################################
# Illustrate convergence in probability
##############################################################################
# Create a matrix to store the results
probs <- data.frame(
  "capT" = capT_vec,
  "delta1" = rep(NA, length(capT_vec)),
  "delta2" = rep(NA, length(capT_vec)),
  "delta3" = rep(NA, length(capT_vec)),
  "delta4" = rep(NA, length(capT_vec))
)

# Loop over sample size
for (t in 1:length(capT_vec)) {
  # Loop over values of delta: Once more, I chose a slow code so that I would
  # save my own time.
  for (d in delta) {
    # Compare all MC estimates against delta
    temp <- abs(results[[t]]$bias) > d

    # Compute the probability of the bias being small
    probs[t, which(d == delta) + 1] <- mean(temp)
  }

}

# Create a plot with the results
gg <- ggplot(data = probs, aes(x = capT)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Sample Size") + ylab("Probability") +
  geom_line(
    aes(y = delta1, color = "d = 0.25"),
    size = 1.5
  ) +
  geom_line(
    aes(y = delta2, color = "d = 0.20"),
    size = 1.5
  ) +
  geom_line(
    aes(y = delta3, color = "d = 0.15"),
    size = 1.5
  ) +
  geom_line(
    aes(y = delta4, color = "d = 0.10"),
    size = 1.5
  ) +
  scale_colour_manual(values = c(
    "#85C0F9", "#0F2080", "#F5793A", "#A95AA1"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
print(gg)

# Save figure: The figure's file name depends on the value of g
if (g == 1) {
  ggsave("figures/figure_convergence-prob-normal.pdf", width = 11, height = 8.5)
} else {
  ggsave("figures/figure_convergence-prob-exp.pdf", width = 11, height = 8.5)
}

##############################################################################
# Illustrate convergence in distribution
##############################################################################
# Collect the CDF of our normalized coefficient for each sample size
#######################################
# Create a data frame to store the results
temp <- data.frame(
  "capT" = rep(NA, 6 * 101),
  "Fy" = rep(NA, 6 * 101),
  "Qy" = rep(NA, 6 * 101)
)

# Find the relevant sample size indexes
i_vec <- which(capT_vec %in% c(5, 10, 15, 20, 50, 100))

# Loop over the sample sizes
for (i in i_vec) {
  # Index within i_vec
  j <- which(i == i_vec)

  # Write the sample size
  temp$capT[(1 + (j - 1) * 101):(j * 101)] <- results[[i]]$capT[1]

  # Write down the probabilities
  temp$Fy[(1 + (j - 1) * 101):(j * 101)] <- seq(0, 1, 0.01)

  # Write down the quantiles
  temp$Qy[(1 + (j - 1) * 101):(j * 101)] <- quantile(
    results[[i]]$normalized_coef, probs = seq(0, 1, 0.01), na.rm = TRUE
  )
}

# Write capT as a factor to enforce the ordering
temp$capT <- factor(temp$capT)

# Create a ggplot
gg <- ggplot(temp, aes(x = Qy, y = Fy)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Normalized Coefficient") + ylab("CDF") +
  geom_line(aes(colour = capT), size = 2) +
  stat_function(fun = pnorm, size = 1, linetype = "dashed")  +
  theme(
    legend.position = "bottom"
  ) + guides(
    colour = guide_legend(nrow = 2, byrow = TRUE, title = "Sample Size")
  )

# Show the plot
print(gg)

# Create a gganimate
anim <- gg +
  transition_states(capT, transition_length = 2, state_length = 1) +
  ggtitle('T = {closest_state}')

# Show the animation
anim

# Save the animation: The gif's file name depends on the value of g
if (g == 1) {
  anim_save("figures/gif_convergence-dist-normal.gif")
} else {
  anim_save("figures/gif_convergence-dist-exp.gif")
}

##############################################################################
# Illustrate proper size control
##############################################################################
# Create a matrix to store the results
rej_rate <- data.frame(
  "capT" = capT_vec,
  "rej" = rep(NA, length(capT_vec))
)

# Loop over sample size
for (t in 1:length(capT_vec)) {
  # Compute the test size
  rej_rate[t, 2] <- mean(results[[t]]$reject, na.rm = TRUE)

}

# Create a plot with the results
gg <- ggplot(data = rej_rate, aes(x = capT)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Sample Size") + ylab("Rejection Rate") +
  scale_y_continuous(
    limits = c(0, 0.5)
  ) +
  geom_line(
    aes(y = rej),
    size = 1.5, colour = "#0F2080"
  ) +
  geom_hline(
    yintercept = 0.05, size = 1.5, linetype = "dashed", color = "#F5793A"
  )
print(gg)

# Save figure: The figure's file name depends on the value of g
if (g == 1) {
  ggsave("figures/figure_rejection-rate-normal.pdf", width = 11, height = 8.5)
} else {
  ggsave("figures/figure_rejection-rate-exp.pdf", width = 11, height = 8.5)
}


















