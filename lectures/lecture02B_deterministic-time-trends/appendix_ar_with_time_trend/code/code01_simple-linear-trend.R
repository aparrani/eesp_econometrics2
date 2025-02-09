###############################################################################
# Lecture: Simple Time Trend Model
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Multiplying the trend estimator by sqrt{T} is not enough to avoid
#       convergence in in probability to a constant. To ensure that our
#       trend estimator converges to a nondegenerate distribution, we must
#       multiply by T^{3/2}
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment
rm(list = ls())

# Load the required packages
library("ggplot2")
library("doParallel")

# Set seed
set.seed(240514)

# Setup parallel backend to use all but one of the cores.
n.cores <- 6
cl <- makeCluster(n.cores)
registerDoParallel(cl)

#######################################
# Parameters
#######################################
# Intercept
alpha <- 1

# Trend
delta <- 0.2

# Variance of the white noise process
sigma <- 0.1

# Length of the time series
capT_vec <- c(10:100)

# Number of MC repetitions
M <- 1000

# Allowed distance for convergence in probability
delta_prob <- c(0.025, 0.02, 0.015, 0.01)

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
    "bias" = rep(NA, M)
  )
  
  # Loop over MC repetitions
  for (m in 1:M) {
    # Simulate a white noise process
    epsilon <- rnorm(n = capT, sd = sigma)
    
    # Simulate a simple deterministic time trend model
    Y <- alpha + delta * seq(from = 1, to = capT, by = 1) + epsilon
    
    # Estimate an time trend model
    t_mod <- summary(lm(Y ~ seq(from = 1, to = capT, by = 1)))
    
    ###################################
    # Store the results
    ###################################
    # Store the estimated bias
    resultsT$bias[m] <- t_mod$coefficients[2,1] - delta
    
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
  "delta_prob1" = rep(NA, length(capT_vec)),
  "delta_prob2" = rep(NA, length(capT_vec)),
  "delta_prob3" = rep(NA, length(capT_vec)),
  "delta_prob4" = rep(NA, length(capT_vec))
)

# Loop over sample size
for (t in 1:length(capT_vec)) {
  # Loop over values of delta_prob: Once more, I chose a slow code so that I would
  # save my own time.
  for (d in delta_prob) {
    # Compare all MC estimates against delta_prob
    temp <- sqrt(t) * abs(results[[t]]$bias) > d
    
    # Compute the probability of the bias being small
    probs[t, which(d == delta_prob) + 1] <- mean(temp)
  }
  
}

# Create a plot with the results
gg <- ggplot(data = probs, aes(x = capT)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Sample Size") + ylab("Probability") +
  geom_line(
    aes(y = delta_prob1, color = "d = 0.025"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = delta_prob2, color = "d = 0.020"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = delta_prob3, color = "d = 0.015"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = delta_prob4, color = "d = 0.010"),
    linewidth = 1.5
  ) +
  scale_colour_manual(values = c(
    "#85C0F9", "#0F2080", "#F5793A", "#A95AA1"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
print(gg)

ggsave("figures/figure_convergence_in_probability_sqrtT.pdf", width = 11, height = 8.5)

# We find that, even after multiply our estimator by sqrt{T}, we still have
# convergence in probability. To avoid this convergence in probability and
# reach a nondegenerated distribution, we must multiply by T^{3/2}. Here, we
# illustrate that multiplying our estimator by T^{3/2} destroys convergence in
# probability.

# Create a matrix to store the results
probs <- data.frame(
  "capT" = capT_vec,
  "delta_prob1" = rep(NA, length(capT_vec)),
  "delta_prob2" = rep(NA, length(capT_vec)),
  "delta_prob3" = rep(NA, length(capT_vec)),
  "delta_prob4" = rep(NA, length(capT_vec))
)

# Loop over sample size
for (t in 1:length(capT_vec)) {
  # Loop over values of delta_prob: Once more, I chose a slow code so that I would
  # save my own time.
  for (d in delta_prob) {
    # Compare all MC estimates against delta_prob
    temp <- (t^(3/2)) * abs(results[[t]]$bias) > d
    
    # Compute the probability of the bias being small
    probs[t, which(d == delta_prob) + 1] <- mean(temp)
  }
  
}

# Create a plot with the results
gg <- ggplot(data = probs, aes(x = capT)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Sample Size") + ylab("Probability") +
  geom_line(
    aes(y = delta_prob1, color = "d = 0.025"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = delta_prob2, color = "d = 0.020"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = delta_prob3, color = "d = 0.015"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = delta_prob4, color = "d = 0.010"),
    linewidth = 1.5
  ) +
  scale_colour_manual(values = c(
    "#85C0F9", "#0F2080", "#F5793A", "#A95AA1"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
print(gg)

ggsave("figures/figure_convergence_in_probability_T-3-2.pdf", width = 11, height = 8.5)

# When we multiply our estimator by T^{3/2}, it does not converge in probability.

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
i_vec <- which(capT_vec %in% c(10, 15, 20, 30, 50, 100))

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
    sqrt(results[[i]]$capT[1]) * results[[i]]$bias,
    probs = seq(0, 1, 0.01), na.rm = TRUE
  )
}

# Write capT as a factor to enforce the ordering
temp$capT <- factor(temp$capT)


temp1 <- data.frame(
  "capT" = rep("Normal", 101),
  "Fy" = pnorm(q = temp$Qy[1:101], sd = sigma * sqrt(12)),
  "Qy" = temp$Qy[1:101]
)
temp2 <- rbind(temp, temp1)

# Create a ggplot
gg <- ggplot(temp2, aes(x = Qy, y = Fy)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Centered Coefficient") + ylab("CDF") +
  geom_line(aes(colour = capT), size = 2) +
  theme(
    legend.position = "bottom"
  ) + guides(
    colour = guide_legend(nrow = 2, byrow = TRUE, title = "Sample Size")
  )

# Show the plot
print(gg)

ggsave("figures/figure_convergence_in_distribution_sqrtT.pdf", width = 11, height = 8.5)

# We compare the distribution of "sqrt(T) * (estimator - true coefficient)"
# against the normal distribution of Theorem 1 in our slides. Note that the
# distribution of "sqrt(T) * (estimator - true coefficient)" is very
# concentrated around 0. This behavior is due to the fact that
# "sqrt(T) * (estimator - true coefficient)" converges in probability as we saw
# in the first plot.

# To avoid this convergence in probability and reach a non-degenerated
# distribution, we must multiply by T^{3/2}. Here, we illustrate that multiplying
# our estimator by T^{3/2} is sufficient for it to converge in distribution
# to the distribution derived in Theorem 1.

#######################################
# Collect the CDF of our normalized coefficient for each sample size
#######################################
# Create a data frame to store the results
temp <- data.frame(
  "capT" = rep(NA, 6 * 101),
  "Fy" = rep(NA, 6 * 101),
  "Qy" = rep(NA, 6 * 101)
)

# Find the relevant sample size indexes
i_vec <- which(capT_vec %in% c(10, 15, 20, 30, 50, 100))

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
    (results[[i]]$capT[1]^(3/2)) * results[[i]]$bias,
    probs = seq(0, 1, 0.01), na.rm = TRUE
  )
}

# Write capT as a factor to enforce the ordering
temp$capT <- factor(temp$capT)


temp1 <- data.frame(
  "capT" = rep("Normal", 101),
  "Fy" = pnorm(q = temp$Qy[1:101], sd = sigma * sqrt(12)),
  "Qy" = temp$Qy[1:101]
)
temp2 <- rbind(temp, temp1)

# Create a ggplot
gg <- ggplot(temp2, aes(x = Qy, y = Fy)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Centered Coefficient") + ylab("CDF") +
  geom_line(aes(colour = capT), size = 2) +
  theme(
    legend.position = "bottom"
  ) + guides(
    colour = guide_legend(nrow = 2, byrow = TRUE, title = "Sample Size")
  )

# Show the plot
print(gg)

ggsave("figures/figure_convergence_in_distribution_T-3-2.pdf", width = 11, height = 8.5)