# This code creates regression

rm(list = ls())
gc()

library(arrow)
library(data.table)
library(fixest)
library(ggplot2)
library(dplyr)
library(stargazer)
library(sandwich)

# Import Data -------------------------------------------------------------
cars = read_parquet('../data/final/clean_data.parquet')
list_states = fread('../data/final/exposed_state.csv')

# Creating a new time fix effect -----------------------------------------

cars = cars %>% 
  mutate(timefe = as.numeric(paste0(
    as.character(year), 
    ifelse(nchar(as.character(month)) == 1, paste0("0", as.character(month)), as.character(month))
  ), .after = state)) %>%
  arrange(state, timefe)

value_exposure_total = list_states[state == "Total", exposure]

# Regression --------------------------------------------------------------
#Adjust reference levels - Average as reference level
cars$state <- factor(cars$state, 
                     levels = c("Total", grep("Total", unique(list_states$state), value = TRUE, invert = TRUE)))

reg_2 = lm(log(cars_sales) ~ exposure * post_crash + factor(timefe) + state -1, data = cars)

lm_1_se <- sqrt(diag(vcovHC(reg_2, type = "HC3")))

stargazer(reg_2,
          type = "text", 
          se = list(lm_1_se),
          title = "Regression Results for Panel Data baseline model",
          omit = c("state", "timefe"),
          align = TRUE)

summary(reg_2)

# Effect by State
reg_1 = feols(log(cars_sales) ~ i(exposure, post_crash), fixef = c("timefe","state"),data = cars)

retrieve_result = function(reg){
  summary = summary(reg)
  tables = data.frame(
    exposure = as.numeric(gsub("[^0-9.-]", "", x = names(summary$coefficients))),
    estimate = summary$coefficients,
    sd = summary$se
  )
}

result = retrieve_result(reg_1)
result = result %>% 
  mutate(estimate_low = estimate - 1.96*sd,
         estimate_high = estimate + 1.96*sd)
result = merge(result, list_states, by = "exposure", all.x = T)
setorderv(result, cols = c("exposure"))

ggplot(result, aes(x = estimate, y = reorder(state, exposure))) +  
  geom_pointrange(aes(xmin = estimate_low, xmax = estimate_high), color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(
    title = "Effect of the 1929 Krach on (log) Car Sales by U.S. State",
    x = "Log(CarSales)",
    y = "State",
    caption = "This figure shows the effect of the 1929 financial krach on car sales in the U.S.\nThe states are presented in increasing order of exposure to the loss of dividend revenue."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")  # Left-align and italicize caption
  )
ggsave(filename = "../krach29carsales.pdf", height = 10, width = 7)

# Bonus -------------------------------------------------------------------

cars[, distance_treat := (year - 1929) * 12 + (month - 10)]
cars_et= cars[distance_treat  %in% -9:24,]
cars_et[, exposure_perc := exposure * 100]

bonus_reg = feols(log(cars_sales) ~ i(distance_treat, ref = 0, exposure_perc), fixef = c("timefe", "state"), 
                  data = cars_et, se = "hetero")
summary(bonus_reg)
result_bonus = retrieve_result(bonus_reg)
result_bonus = result_bonus %>% 
  mutate(estimate_low = estimate - 1.96 * sd,
         estimate_high = estimate + 1.96 * sd)

# Create the plot using estimates_low for both x and y
ggplot(result_bonus, aes(x = exposure, y = estimate)) +
  geom_point(color = "blue") +  # Add points for the estimated coefficients
  # geom_line(color = "blue")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at the treatment point (e.g., October 1929)
  geom_errorbar(aes(ymin = estimate_low , ymax = estimate_high), width = 0.2, color = "blue") +  # 95% CI
  labs(title = "Event Study: Impact of Treatment on Car Sales",
       x = "Distance to Treatment (Months)",
       y = "Coefficient Estimate Exposure*MD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "../et_car_sales.pdf", height = 8, width = 10)

# having fun --------------------------------------------------------------
# 
# cars[, low_exposition := ifelse(exposure <= 0.05, 1,0)]
# cars[, mid_exposition := ifelse(exposure < 0.20 & exposure > 0.05, 1,0)]
# cars[, high_exposition := ifelse(exposure >= 0.20, 1,0)]
# cars[, distance_treat := (year - 1929) * 12 + (month - 10)]
# cars_et= cars[distance_treat  %in% -9:24,]
# reg_low = feols(log(cars_sales) ~ i(distance_treat, low_exposition), fixef = c("month", "state"), data = cars_et)
# reg_mid = feols(log(cars_sales) ~ i(distance_treat, mid_exposition), fixef = c("year", "state"), data = cars_et)
# reg_high = feols(log(cars_sales) ~ i(distance_treat, high_exposition), fixef = c("year", "state"), data = cars_et)
# 
# summary_low = summary(reg_low)
# estimates_low <- data.frame(
#   term = names(summary_low$coefficients),  # Names of the coefficients
#   estimate = summary_low$coefficients, 
#   sd = summary_low$se # Coefficient estimates (vector)
# )
# 
# estimates_low$distance_treat <- as.numeric(sub("distance_treat::(-?\\d+):low_exposition", "\\1", estimates_low$term))
# 
# # Create the plot using estimates_low for both x and y
# ggplot(estimates_low, aes(x = distance_treat, y = estimate)) +
#   geom_point() +  # Add points for the estimated coefficients
#   geom_line() +   # Add a line to connect the points
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 1929 October
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Vertical line at 1929 October
#   geom_errorbar(aes(ymin = estimate - 1.96 * sd, ymax = estimate + 1.96 * sd), width = 0.2) +  # Add error bars for 95% CI
#   labs(title = "Coefficient Estimates Over Time (Low Exposition)", 
#        x = "Distance to Treatment (Months)", 
#        y = "Coefficient Estimate (log of car sales)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# 
