###############################################################################
# Lecture: Stationary ARMA (p,q) Models
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Illustrating the AR(2) model's stationarity
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment
rm(list = ls())

# Load the required packages
library("ggplot2")

# Set seed: To ensure our simulations are reproducible, we must set the seed
# for the random number generator algorithm.
set.seed(230509)

#######################################
# Parameters
#######################################
# The process is weakly stationary if stationary == 1.
stationary <- 0

# Choose the AR coefficients
if (stationary == 1) {
  # If the process is stationary, the roots of "-phi2 * z^2 - phi1 * z + 1 = 0"
  # lie outside the unit circle.
  phi1 <- 0.25
  phi2 <- 0.25

} else {
  # If the process is not stationary, the roots of "-phi2 * z^2 - phi1 * z + 1 = 0"
  # do not lie outside the unit circle.
  phi1 <- 1
  phi2 <- 1

}

# Length of the time series
capT <- 100

# Number of MC repetitions
M <- 10000

#######################################
###############################################################################
# Monte Carlo Exercise
###############################################################################
# Create a matrix to store all simulated time series
timeseries <- matrix(NA, nrow = capT, ncol = M)

# Loop over MC repetitions
for (m in 1:M) {
  #######################################
  # Simulate our AR(2) process
  #######################################
  # If the model is stationary, we can use the function arima.sim
  if (stationary == 1) {
    timeseries[, m] <- arima.sim(model = list(ar = c(phi1, phi2)), n = 100)

    # If the model is not stationary, we have to manually simulate our process.
  } else {
    # Create a white noise series. Good practice would require me to have a
    # burn-in period. I ignore this step, because I am looking at many
    # many realization of my stochastic process. On average, it does not depend
    # on the initial conditions.
    wnoise <- rnorm(n = capT)

    # Define y0
    timeseries[1, m] <- wnoise[1]

    # Define y1
    timeseries[2, m] <- wnoise[2]

    # Recursively define the remaining timeseries
    for (t in 3:capT) {
      timeseries[t, m] <- phi1 * timeseries[t - 1, m] +
        phi2 * timeseries[t - 2, m] + wnoise[t]
    }

  }
}

# Compute the expected value of each Yt
expected <- rowMeans(timeseries)

# Compute the variance of each Yt
variance <- apply(X = timeseries, MARGIN = 1, FUN = var)

# Compute the variance of Yt based on our formula. (Its expected value is zero.)
varY <- (1 - phi2)/((1 + phi2) * ((1 - phi2)^2 - phi1^2))

#######################################
# Plot the expected values of each Yt
#######################################
# Create the ggplot's dataframe
temp <- data.frame("t" = 1:capT, "expected" = expected)

# Use ggplot
gg <- ggplot(data = temp, aes(x = t)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Time") + ylab("Expected Value") +
  geom_line(
    aes(y = expected),
    color = "#0F2080",
    size = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.5, color = "#F5793A")
print(gg)

###################
# Save the last plot
###################
# Name the figure
figname <- paste0("figures/figure_ar2_phi1-", phi1, "_phi2-", phi2, "_mean.pdf")

# Save the figure
ggsave(figname, width = 11, height = 8.5)

###################
#######################################
# Plot the variance of each Yt
#######################################
# Create the ggplot's dataframe
temp <- data.frame("t" = 1:capT, "variance" = variance)

# Use ggplot
gg <- ggplot(data = temp, aes(x = t)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Time") + ylab("Variance") +
  geom_line(
    aes(y = variance),
    color = "#0F2080",
    size = 1.5
  ) +
  geom_hline(yintercept = varY, linetype = "dashed", size = 1.5, color = "#F5793A")
print(gg)

###################
# Save the last plot
###################
# Name the figure
figname <- paste0("figures/figure_ar2_phi1-", phi1, "_phi2-", phi2, "_var.pdf")

# Save the figure
ggsave(figname, width = 11, height = 8.5)

###################
#######################################
###############################################################################

