###############################################################################
# Lecture Notes 5B
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Analyzing cointegrated variables in R
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment
rm(list = ls())

# Load packages
library("urca")
library("vars")
library("zoo")
library("ggplot2")
library("stargazer")

# Load the dataset: Our dataset was used for estimating a money demand function
# in Finland by Johansen and Juselius (1990). This dataset consists of
# logarithm of real money M2 (lrm1), logarithm of real income (lny), marginal
# interest rate (lnmr) and inflation (difp). It covers the period
# 1958:Q1 – 1984:Q2.
data(finland)

# We re-order the variables so that they match a similar order to the variables
# in our VAR example in Lecture 4B.
finland <- finland[, c("lny", "difp", "lnmr", "lrm1")]

##############################################################################
# Plotting the data
##############################################################################
# Loop over our variables
for (v in 1:4) {
  # Plot the variable
  gg <- ggplot(finland, aes(x = 1:nrow(finland))) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Quarter") + ylab(colnames(finland)[v]) +
    geom_line(
      aes(y = finland[, v]),
      size = 1.5, color = "#F5793A"
    )
  print(gg)

  # Create a name for the file
  namefig <- paste0("figures/raw_", colnames(finland)[v], ".pdf")

  # Save the plot
  ggsave(namefig, width = 11, height = 8.5)
}

##############################################################################
# lrm1: Testing for a unit root
##############################################################################
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is a
# trend, we use the largest version of the test. Since we have quarterly data,
# we use 4 lags.
df_lrm1 <- ur.df(
  y = finland$lrm1,
  type = 'trend',
  lags = 4
)

# Report the test
print(summary(df_lrm1))

# The phi2 test statistic is equal to 3.29 while its 5% critical value is 4.75.
# Hence, we do not reject the phi2 null. This result suggests that this variable
# follows a unit root process without a drift nor a trend.

##############################################################################
# lny: Testing for a unit root
##############################################################################
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is a
# trend, we use the largest version of the test. Since we have quarterly data,
# we use 4 lags.
df_lny <- ur.df(
  y = finland$lny,
  type = 'trend',
  lags = 4
)

# Report the test
print(summary(df_lny))

# The phi2 test statistic is equal to 4.93 while its 5% critical value is 4.75.
# Hence, we reject the phi2 null. The phi3 test statistic is equal to 2.56
# while its 5% critical value is 6.49. Hence, we do not reject the phi3 null.
# This result suggests that this variable follows a unit root process with
# drift.

##############################################################################
# lnmr: Testing for a unit root
##############################################################################
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is no
# trend, we use the simplest version of the test. Since we have quarterly data,
# we use 4 lags.
df_lnmr <- ur.df(
  y = finland$lnmr,
  type = "none",
  lags = 4
)

# Report the test
print(summary(df_lnmr))

# The tau1 test statistic is equal to -0.69 while its 5% critical value is -1.95.
# Hence, we do not reject the tau1 null. This result suggests that this variable
# follows a unit root process.

##############################################################################
# difp: Testing for a unit root
##############################################################################
# Run the Augmented Dickey-Fuller Test: Since our plot suggest that there is no
# trend, we use the simplest version of the test. Since we have quarterly data,
# we use 4 lags.
df_difp <- ur.df(
  y = finland$difp,
  type = "none",
  lags = 4
)

# Report the test
print(summary(df_difp))

# The tau1 test statistic is equal to -0.88 while its 5% critical value is -1.95.
# Hence, we do not reject the tau1 null. This result suggests that this variable
# follows a unit root process.

# Since all variables are I(1) processes individually, we can proceed to testing
# for cointegration.

##############################################################################
# Testing for cointegration
##############################################################################
# Johansen's test: Instead of using the specification used by Johansen and
# Juselius (1990), we control for more lags (K = 4 instead of 2) instead of
# controlling directly for dummy variables.
vecm1 <- ca.jo(
  x = finland,
  ecdet = "none",
  type  = "eigen",
  K = 4,
  spec = "transitory"
)

# Explanation:
  # x: Variables that we care about.
  # ecdet: Since most macroeconomic model suggest that those variables have a
    # stable long-run equilibrium, we set this option to be "none".
  # type: There are two tests for cointegration: "eigen" and "trace". In class,
    # we only saw the test "eigen".
  # K: Since we have quarterly data, we set our model to be a VAR(4)
  # spec: There are two possible specification for the VECM. In class, we only
    # discussed the transitory normalization.

# Print the results
summary(vecm1)

# We start by testing the null hypothesis of no cointegrating relation (r = 0)
# against the alternative that there is at least one cointegrating relation.
# Our test statistic is 56.69 and our 5% critical value is 27.14. Consequently,
# we reject the null of no cointegration.

# Now, we test that we have only one cointegrating relation (r = 1) against the
# alternative that there are at least two cointegrating relations.
# Our test statistic is 15.38 and our 5% critical value is 21.07. Consequently,
# we do not reject the null that there is only one cointegrating relation.

##############################################################################
# Finding the cointegrating vector
##############################################################################
# R reports all estimated cointegrating vectors in the matrix V. Each column
# represents a cointegrating vector.
print(vecm1@V)

# Since our test concluded that there is only one cointegrating vector, we can
# focus on the first column of matrix V
print(vecm1@V[, 1])

##############################################################################
# Understanding the VAR model associated with our VECM
##############################################################################
# Estimate the VAR model in levels based on our VEC model in differences. We
# have to specify how many cointegration relation we want to consider.
var1 <- vec2var(vecm1, r = 1)

# Print the results for our VAR model
print(var1)

#######################################
# Forecasting
#######################################
# Forecast the next four years
var_forecast <- predict(var1, n.ahead = 16)

# Print the forecasted results
print(var_forecast)

#######################################
# Reduced-form IRF
#######################################
# Compute the reduced-form IRF with bootstrapped standard errors
rf_irf <- irf(
  var1, impulse = "lnmr", response = "lny",
  n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95,
  ortho = FALSE
)

# Construct a dataframe to plot the reduced-form IRF
temp <- data.frame(
  "period" = 0:16,
  "irf" = rf_irf$irf$lnmr,
  "CI_U" = rf_irf$Upper$lnmr,
  "CI_L" = rf_irf$Lower$lnmr
)
colnames(temp) <- c("period", "irf", "CI_U", "CI_L")

# Plot the IRF
gg <- ggplot(data = temp, aes(x = period)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Quarter") + ylab("IRF") +
  geom_ribbon(
    aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
  ) + geom_hline(yintercept = 0, color = "black") +
  geom_line(
    aes(y = irf), size = 1.5, color = "red"
  )
print(gg)

# Save the last plot
ggsave("figures/rf-irf-of-lny-to-lnmr.pdf", width = 11, height = 8.5)

# Note that an important difference to stationary VAR models is that the IRF of
# a cointegrated VAR model does not necessarily approach zero, because the
# variables are not stationary.

#######################################
# Structural IRF
#######################################
# To estimate a structural VAR IRF, we proceed in the same way we did for the
# stationary case. In particular, we would need to justify the ordering of our
# variables. We use a similar order to the order used in Lecture 4B.
# Compute the structural-form IRF with bootstrapped standard errors
s_irf <- irf(
  var1, impulse = "lnmr", response = "lny",
  n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95,
  ortho = TRUE
)

# Construct a dataframe to plot the reduced-form IRF
temp <- data.frame(
  "period" = 0:16,
  "irf" = s_irf$irf$lnmr,
  "CI_U" = s_irf$Upper$lnmr,
  "CI_L" = s_irf$Lower$lnmr
)
colnames(temp) <- c("period", "irf", "CI_U", "CI_L")

# Plot the IRF
gg <- ggplot(data = temp, aes(x = period)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Quarter") + ylab("IRF") +
  geom_ribbon(
    aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
  ) + geom_hline(yintercept = 0, color = "black") +
  geom_line(
    aes(y = irf), size = 1.5, color = "red"
  )
print(gg)

# Save the last plot
ggsave("figures/s-irf-of-lny-to-lnmr.pdf", width = 11, height = 8.5)
