# This code creates regression

rm(list = ls())
gc()

library(arrow)
library(data.table)
library(fixest)
library(ggplot2)

# Import Data -------------------------------------------------------------
cars = read_parquet('../data/final/clean_data.parquet')

# Regression --------------------------------------------------------------

reg_1 = feols(log(cars_sales) ~ i(exposure, post_crash), fixef = c("year", "state"), data = cars)
summary(reg_1)


# having fun --------------------------------------------------------------

cars[, low_exposition := ifelse(exposure <= 0.05, 1,0)]
cars[, mid_exposition := ifelse(exposure < 0.20 & exposure > 0.05, 1,0)]
cars[, high_exposition := ifelse(exposure >= 0.20, 1,0)]
cars[, distance_treat := (year - 1929) * 12 + (month - 10)]
cars_et= cars[distance_treat  %in% -9:24,]
reg_low = feols(log(cars_sales) ~ i(distance_treat, low_exposition), fixef = c("month", "state"), data = cars_et)
reg_mid = feols(log(cars_sales) ~ i(distance_treat, mid_exposition), fixef = c("year", "state"), data = cars_et)
reg_high = feols(log(cars_sales) ~ i(distance_treat, high_exposition), fixef = c("year", "state"), data = cars_et)

summary_low = summary(reg_low)
estimates_low <- data.frame(
  term = names(summary_low$coefficients),  # Names of the coefficients
  estimate = summary_low$coefficients, 
  sd = summary_low$se # Coefficient estimates (vector)
)

estimates_low$distance_treat <- as.numeric(sub("distance_treat::(-?\\d+):low_exposition", "\\1", estimates_low$term))

# Create the plot using estimates_low for both x and y
ggplot(estimates_low, aes(x = distance_treat, y = estimate)) +
  geom_point() +  # Add points for the estimated coefficients
  geom_line() +   # Add a line to connect the points
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 1929 October
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Vertical line at 1929 October
  geom_errorbar(aes(ymin = estimate - 1.96 * sd, ymax = estimate + 1.96 * sd), width = 0.2) +  # Add error bars for 95% CI
  labs(title = "Coefficient Estimates Over Time (Low Exposition)", 
       x = "Distance to Treatment (Months)", 
       y = "Coefficient Estimate (log of car sales)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

