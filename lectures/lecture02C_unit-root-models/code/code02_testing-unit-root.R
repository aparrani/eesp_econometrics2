###############################################################################
# Lecture: Stationary ARMA (p,q) Models
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Testing for unit roots
# Another example: https://www.r-bloggers.com/2021/12/augmented-dickey-fuller-adf-test-in-r/
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment
rm(list = ls())

# Load the required packages
library("urca")
library("zoo")
library("stringr")
library("ggplot2")

# Load the data
data(denmark)

###############################################################################
# Cleaning the data
###############################################################################
# Keep only the dates and the bond rates (measured in decimals per year)
ds <- denmark[, c("ENTRY", "IBO")]

#######################################
# ENTRY is a factor. We want it to be a date.
#######################################
# First, write it as a character
ds$quarter <- as.character(ds$ENTRY)

# Second, we need to replace :0 by -
ds$quarter <- str_replace(
  string = ds$quarter, pattern = ":0", replacement = "-"
)

# Finally, we use the function as.yearqtr to tell R we have quarterly data
ds$quarter <- as.yearqtr(ds$quarter)

# Keep only quarter and IBO
ds <- ds[, c("quarter", "IBO")]

###############################################################################
# Defining which Augmented Dickey-Fuller Test to Run
###############################################################################
# Plot the data
gg <- ggplot(ds, aes(x = quarter)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("") + ylab("IBO") +
  scale_x_yearqtr(format = "%Y Q%q", n = 20) +
  geom_line(aes(y = IBO), size = 1.5, color = "#0F2080") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(gg)

# There is no clear trend in the Danish bond rate. So We will use a Augmented
# Dickey-Fuller test with drift but no time trend.

##############################################################################
# Preferred Augmented Dickey-Fuller Test
##############################################################################
# Run the test using BIC to choose the number of lags
df_drift <- ur.df(
  y = ds$IBO,
  type = 'drift',
  selectlags = c("BIC")
)

# Report the results: BIC ended up choosing a AR(2) model. The first test
# statistic is tau2 in the slides, while the second one is phi1. Their critical
# values are reported at the bottom.
print(summary(df_drift))

# We start by testing the most restrictive model. So, we focus on phi1 which
# imposes rho = 1 and alpha = 0. Since we do not reject the null of this test
# (because observed test statistic is smaller than the critical values),
# we stop here and conclude that the Danish bond rate is an AR(2) with a unit
# root, but without a drift. If we had rejected the null of phi1, we would
# proceed to use tau2.

##############################################################################
# Other Augmented Dickey-Fuller Test: Drift and Time Trend
##############################################################################
# Run the test using BIC to choose the number of lags
df_drift <- ur.df(
  y = ds$IBO,
  type = 'trend',
  selectlags = c("BIC")
)

# Report the results: BIC ended up choosing a AR(2) model. The first test
# statistic is tau3 in the slides, while the second one is phi2 and the third
# one is phi3. Their critical values are reported at the bottom.
print(summary(df_drift))

##############################################################################
# Other Augmented Dickey-Fuller Test: No Drift and No Time Trend
##############################################################################
# Run the test using BIC to choose the number of lags
df_drift <- ur.df(
  y = ds$IBO,
  type = 'none',
  selectlags = c("BIC")
)

# Report the results: BIC ended up choosing a AR(2) model. The test statistic
# is tau1 in the slides. Its critical values are reported at the bottom.
print(summary(df_drift))

