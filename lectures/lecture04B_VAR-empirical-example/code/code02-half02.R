###############################################################################
# Lecture: VAR - Empirical Example
# Instructor: Vitor Possebom
# TA: Carolina Nour
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
# HP Filtering: Our variables have trend components. To use only stationary
# variables, we have to detrend our original variables. One one to do so is to
# use a tool known as HP filter. The smothing parameter \lambda = 1600 is
# appropriate for quarterly data.
#######################################
# Loop over variables
for (v in 2:10) {
  # Find the name of the variable
  varname <- colnames(ds)[v]

  # Run the HP filer for variable v
  hp <- hpfilter(ds[, v], type = "lambda", freq = 1600, drift = FALSE)

  # Detrend variable v
  ds[, v] <- ds[, v] - hp$trend
}

#######################################
# Choose only one money aggregate: M1
ds <- ds[, 1:8]

# Use only the first half of our data
ds <- ds[74:146, ]

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
    "figures/figure-sirf-of-", colnames(s_irf$irf$FF)[v], "-to-FF-half02.pdf"
  )

  # Save the last plot
  ggsave(figvarname, width = 11, height = 8.5)
}

###############################################################################
