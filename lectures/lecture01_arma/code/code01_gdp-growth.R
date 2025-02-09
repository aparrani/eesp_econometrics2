###############################################################################
# Lecture: Stationary ARMA (p,q) Models
# Instructor: Vitor Possebom
# Course: Econometrics 2
# Goal: Forecasting Brazilian GDP growth with an AR(p) model
###############################################################################
# Organize the working environment
###############################################################################
# Clean the working environment: My R settings automatically clean everything
# whenever I close it and I strongly recommend you do the same. Any piece of
# coding should be able to run from scratch. Keeping temporarily stored old
# objects create unecessary challenges to reproducible code.
rm(list = ls())

# Load the required packages
library("quantmod")
library("ggplot2")

# Parameters
P <- 10 # Maximum number of lags in my models
alpha <- 0.05 # Significance level

###############################################################################
# Clean the data
###############################################################################
# Read the dataset: Note that I do not need to set my working directory
# because I have created a R project. It also saves space because I do not need
# to type the full file path. I used the function read.csv because the file
# formats uses "," as a column separator. This format is also my notebook's
# default setting because I frequently use datasets created in the US.
ds <- read.csv("data/data_gdp_brazil.csv")


# The original dataset comes from Ipeadata, that has weird naming conventions.

# Rename the variables: gdp growth is measured as % per year.
colnames(ds) <- c("year", "gdp_growth")

# Time series data can be annoying because the order of our observations matter
# a lot. For this reason, it is good practice to use the "xts" object type.
# To do so, we first must tell R that our variable "year" is a date.

# Create a xts object
ds_xts <- xts(
  x = ds$gdp_growth,
  order.by = as.Date(as.character(ds$year), format = "%Y")
)

###############################################################################
# Looking at the data
###############################################################################
# Plot the data: I use color-blind friendly palettes. My favorite can be found
# in this website (https://venngage.com/blog/color-blind-friendly-palette/#4).
gg <- ggplot(data = ds, aes(x = year)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Year") + ylab("Yearly GDP Growth (%)") +
  scale_x_continuous(
    limits = c(1900, 2021),
    expand = c(0,0),
    breaks = seq(from = 1900, to = 2020, by = 20)
  ) +
  geom_line(
    aes(y = gdp_growth),
    color = "#0F2080",
    size = 1.5
  )
print(gg)

# Save the last plot
ggsave("figures/figure_gdp_growth.pdf", width = 11, height = 8.5)

# Plot the autocorrelation of GDP growth: An AR(2) model seems reasonable.
pdf(file = "figures/figure_autocorrelation.pdf", width = 11, height = 8.5)
acf(ds_xts, ylab = "Autocorrelation", main = "Brazilian Yearly GDP Growth")
dev.off()

###############################################################################
# Estimate AR(1) and AR(2) models
###############################################################################
# Estimate an AR(1) model
ar1 <- arima(
  x = ds_xts,
  order = c(1, 0, 0),
  include.mean = TRUE
)

# Estimate an AR(2) model
ar2 <- arima(
  x = ds_xts,
  order = c(2, 0, 0),
  include.mean = TRUE
)

# Report the results of the AR(1) model
print(ar1)

# Report the results of the AR(2) model
print(ar2)

###############################################################################
# Estimate many AR(p) models using loops
###############################################################################
# Create a list to store the results of all my models: List are extremely
# flexible objects. They can store pretty much everything.
ar <- vector(mode = "list", length = P)

# Loop over AR(p) models
for (p in 1:P){
  # Estimate an AR(p) model
  ar[[p]] <- arima(
    x = ds_xts,
    order = c(p, 0, 0),
    include.mean = TRUE
  )
}

#######################################
# Report the estimated coefficients, BIC and AIC of each model
#######################################
# Create a matrix to store the results
results <- matrix(NA, nrow = P + 3, ncol = P)

# Rename the columns
colnames(results) <- paste0("AR", 1:P)

# Rename the rows
rownames(results) <- c("BIC", "AIC", "Intercept", paste0("Lag ", 1:P))

# Loop over AR(p) models
for (p in 1:P){
  # Store BIC: We want to minimize it.
  results[1, p] <- BIC(ar[[p]])

  # Store AIC: We want to minimize it.
  results[2, p] <- AIC(ar[[p]])

  # Store the intercept
  results[3, p] <- ar[[p]]$coef[p + 1]

  # Store the AR coefficients
  results[4:(4 + p - 1), p] <- ar[[p]]$coef[1:p]
}

#######################################
###############################################################################
# Forecast 10-years ahead using our AR(1) model
###############################################################################
# Estimate our forecasted values using our AR(1) model
forecast <- predict(ar1, n.ahead = 10)

#######################################
# Plot our forecast
#######################################
# Create a matrix with all the necessary objects for ggplot
temp <- data.frame(
  "year" = 2000:2030,
  "gdp_growth" = c(ds$gdp_growth[ds$year %in% 2000:2030], rep(NA, 10)),
  "forecast" = c(rep(NA, 21), forecast$pred),
  "CI_U" = c(rep(NA, 21), forecast$pred + qnorm(1 - alpha/2) * forecast$se),
  "CI_L" = c(rep(NA, 21), forecast$pred + qnorm(alpha/2) * forecast$se)
)

# Create the plot
gg <- ggplot(data = temp, aes(x = year)) +
  theme_bw(base_size = 25) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Year") + ylab("Yearly GDP Growth (%)") +
  scale_x_continuous(
    limits = c(2000, 2031),
    expand = c(0,0),
    breaks = seq(from = 2000, to = 2030, by = 5)
  ) +
  geom_line(
    aes(y = gdp_growth, color = "Realized"),
    size = 1.5
  ) +
  geom_line(
    aes(y = forecast, color = "Forecast: AR(1)"),
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
  )
print(gg)

# Save the last plot
ggsave("figures/figure_gdp_growth_forecast.pdf", width = 11, height = 8.5)

#######################################
##############################################################################
