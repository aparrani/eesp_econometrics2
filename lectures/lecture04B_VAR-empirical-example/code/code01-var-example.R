###############################################################################
# Lecture: VAR - Empirical Example
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Analyze whether monetary policy has real effects.
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment
rm(list = ls())

# Load the required packages
library("ggplot2")
library("tidyr")
library("dplyr")
library("zoo")
library("tidyverse")
library("vars")
library("stargazer")
library("mFilter")

# Load the data: The data are transformed using 100*log(x), except for the
# FED funds rate and commodity prices.
ds <- read.csv("data/data_CEE.csv")

###############################################################################
# Cleaning the data
###############################################################################
# Dates should be a quarterly variable instead of character
ds$DATE <- as.yearqtr(ds$DATE, format = "%Y:%q")

#######################################
# Plot all the variables
#######################################
# Loop over variables
for (v in 2:10) {
  # Find the name of the variable
  varname <- colnames(ds)[v]

  # Plot variable v
  gg <- ggplot(data = ds, aes(x = DATE)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Date") + ylab(varname) +
    scale_x_yearqtr(format = "%Y Q%q", n = 16) +
    geom_line(
      aes(y = ds[, v]), size = 1.5, color = "#0F2080"
    ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(gg)

  # Define the name of file
  figvarname <- paste0("figures/figure-", varname, ".pdf")

  # Save the plot
  ggsave(figvarname, width = 11, height = 8.5)
}

#######################################
# HP Filtering: Our variables have trend components. To use only stationary
# variables, we have to detrend our original variables. One way to do so is to
# use a tool known as HP filter. The smoothing parameter \lambda = 1600 is
# appropriate for quarterly data. (https://en.wikipedia.org/wiki/Hodrick%E2%80%93Prescott_filter)
#######################################
# Loop over variables
for (v in 2:10) {
  # Find the name of the variable
  varname <- colnames(ds)[v]

  # Run the HP filer for variable v
  hp <- hpfilter(ds[, v], type = "lambda", freq = 1600, drift = FALSE)

  # Detrend variable v
  ds[, v] <- ds[, v] - hp$trend

  # Plot variable v
  gg <- ggplot(data = ds, aes(x = DATE)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Date") + ylab(paste0("Detrended ", varname)) +
    scale_x_yearqtr(format = "%Y Q%q", n = 16) +
    geom_line(
      aes(y = ds[, v]), size = 1.5, color = "#0F2080"
    ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(gg)

  # Define the name of file
  figvarname <- paste0("figures/figure-detrended-", varname, ".pdf")

  # Save the plot
  ggsave(figvarname, width = 11, height = 8.5)
}

#######################################
# Choose only one money aggregate: M1
ds <- ds[, 1:8]

# To use function VAR, the data must be a tsobject: Option frequency is equal
# to 4 because we have quarterly data.
ds_ts <- ts(data = ds[, 2:8], start = c(1959, 01), frequency = 4)

###############################################################################
# Reduced-form VAR(2)
###############################################################################
# Estimate our model
rfvar <- VAR(ds_ts, p = 2, type = "const")

# Collect the results
fitvar <- rfvar$varresult

# Build a table
stargazer(fitvar, type = "text" )

###############################################################################
# Forecast based on our reduced-form VAR(2) model
###############################################################################
# Forecast the next four years
var_forecast <- predict(rfvar, n.ahead = 16)

#######################################
# Plot the GDP forecast
#######################################
# Create a matrix with dates to attach to the previous data
dates_temp <- data.frame(
  "DATE" = seq(as.Date("1995-07-01"), by = "quarter", length.out = 16),
  "GDP87" = rep(NA, 16)
)

# Use the quarterly dates
dates_temp$DATE <- as.yearqtr(dates_temp$DATE, format = "%Y-%m-%d")

# Create extra rows for the future dates
gdp <- rbind(ds[, c("DATE", "GDP87")], dates_temp)

# Create a matrix for the forecasts
temp <- rbind(
  matrix(NA, nrow = 146, ncol = 3),
  var_forecast$fcst$GDP87[, 1:3]
)

# Combine the forecast with the original data
gdp$forecast <- temp[, 1]
gdp$CI_L <- temp[, 2]
gdp$CI_U <- temp[, 3]

# Create the plot
gg <- ggplot(data = gdp, aes(x = DATE)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Quarter") + ylab("Quarterly GDP") +
  scale_x_yearqtr(format = "%Y Q%q", n = 16) +
  geom_line(
    aes(y = GDP87, color = "Realized"),
    size = 1.5
  ) +
  geom_line(
    aes(y = forecast, color = "Forecast: VAR(1)"),
    size = 1.5
  ) +
  geom_line(
    aes(y = CI_U),
    color = "#85C0F9",
    linetype = "dashed",
    size = 1.5
  ) +
  geom_line(
    aes(y = CI_L),
    color = "#85C0F9",
    linetype = "dashed",
    size = 1.5
  ) +
  scale_colour_manual(values = c(
    "#85C0F9", "#0F2080"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(gg)

# Save the last plot
ggsave("figures/figure-forecast-GDP87.pdf", width = 11, height = 8.5)

#######################################
###############################################################################
# Reduced-form Impulse Response Function: We will compute only the response of
# GDP to a monetary shock (FF). As an exercise, you can compute all the other
# reduced form impulse response functions. To force R to compute the
# hard-to-interpret reduced-form impulse response functions, we must impose
# "ortho = FALSE". The bootstrap that is used to compute confidence intervals
# is the parametric bootstrap (see Hamilton's pages 337 and 338).
###############################################################################
# Compute the reduced-form IRF with bootstrapped standard errors
rf_irf <- irf(
  rfvar, impulse = "FF", response = "GDP87",
  n.ahead = 16, boot = TRUE, runs = 1000, ci=0.95,
  ortho = FALSE
)

# Construct a dataframe to plot the reduced-form IRF
temp <- data.frame(
  "period" = 0:16,
  "irf" = rf_irf$irf$FF,
  "CI_U" = rf_irf$Upper$FF,
  "CI_L" = rf_irf$Lower$FF
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
  ) + ylim(-0.75, 0.30)
print(gg)

# Save the last plot
ggsave("figures/figure-irf-of-GDP87-to-FF.pdf", width = 11, height = 8.5)

###############################################################################
# Structural IRF:

# Justifying our exclusion restriction, i.e., the ordering of our variables.
# To estimate our Structural VAR, the variables in the dataset must be ordered
# from the most exogenous variable (first column) to the least exogenous
# variable (last column).

# Since firms have to make investments decisions in advance due to managerial
# constraints and there are fixed investments costs, investments take time to
# react to changes in other macroeconomic variables. A similar argument can be
# applied to consumption of durable goods and to some government expenditures.
# For this reason, GDP takes some time to respond to shocks in the other
# variables.

# According to microeconomics evidence, prices are sticky and they take time to
# respond to demand shocks. Since they react to supply shocks, they should be
# the second instead of the first variable.

# Commodity prices are sticky but more flexible than other prices according
# to microeconomic evidence.

# The FOMC meets each six weeks to change the FED Funds rate. For this reason,
# relevant changes in this variable only happen each six weeks.

# NBR, TOTR and our M1 react naturally and quickly to changes in the interest
# rate. For this reason, they are the last variables in our ordering.

# We will compute only the response of GDP to a monetary shock (FF). As an
# exercise, you can compute all the other structural impulse response functions.
# To force R to compute the structural impulse response functions, we must
# impose "ortho = TRUE". The bootstrap that is used to compute confidence
# intervals is the parametric bootstrap (see Hamilton's pages 337 and 338).
#######################################
# Compute the structural IRF with bootstrapped standard errors
s_irf <- irf(
  rfvar, impulse = "FF", response = "GDP87",
  n.ahead = 16, boot = TRUE, runs = 1000, ci=0.95,
  ortho = TRUE
)

# Construct a dataframe to plot the structural IRF
temp <- data.frame(
  "period" = 0:16,
  "irf" = s_irf$irf$FF,
  "CI_U" = s_irf$Upper$FF,
  "CI_L" = s_irf$Lower$FF
)
colnames(temp) <- c("period", "irf", "CI_U", "CI_L")

# Plot the IRF
gg <- ggplot(data = temp, aes(x = period)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Quarter") + ylab("SIRF") +
  geom_ribbon(
    aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
  ) + geom_hline(yintercept = 0, color = "black") +
  geom_line(
    aes(y = irf), size = 1.5, color = "red"
  ) + ylim(-0.75, 0.30)
print(gg)

# Save the last plot
ggsave("figures/figure-sirf-of-GDP87-to-FF.pdf", width = 11, height = 8.5)

# What are the main differences between the estimated reduced-form IRF and the
# estimated Structural IRF?

###############################################################################
# As an extra coding exercise, we will compute the structural impulse response
# function of all variables to a monetary shock.
###############################################################################
# Compute the Structural IRF with bootstrapped standard errors
s_irf <- irf(
  rfvar, impulse = "FF",
  n.ahead = 16, boot = TRUE, runs = 1000, ci=0.95,
  ortho = TRUE
)

# Loop over variables
for (v in 1:7) {
  # Construct a dataframe to plot the structural IRF
  temp <- data.frame(
    "period" = 0:16,
    "irf" = s_irf$irf$FF[, v],
    "CI_U" = s_irf$Upper$FF[, v],
    "CI_L" = s_irf$Lower$FF[, v]
  )
  colnames(temp) <- c("period", "irf", "CI_U", "CI_L")

  # Define the label of the Y-axis
  ylabel <- paste0("SIRF - Response: ", colnames(s_irf$irf$FF)[v])

  # Plot the IRF
  gg <- ggplot(data = temp, aes(x = period)) +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
    xlab("Quarter") + ylab(ylabel) +
    geom_ribbon(
      aes(ymin = CI_L, ymax = CI_U), fill = "grey80"
    ) + geom_hline(yintercept = 0, color = "black") +
    geom_line(
      aes(y = irf), size = 1.5, color = "red"
    )
  print(gg)

  # Define the name of the figure
  figvarname <- paste0(
    "figures/figure-sirf-of-", colnames(s_irf$irf$FF)[v], "-to-FF.pdf"
  )

  # Save the last plot
  ggsave(figvarname, width = 11, height = 8.5)
}

# What do we learn about the real effects of a monetary shock?

###############################################################################
