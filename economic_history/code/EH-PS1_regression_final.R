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

#### BASELINE MODEL ####
#Create Time FE variable (year + month)
cars = cars %>% 
  mutate(timefe = as.numeric(paste0(
    as.character(year), 
    ifelse(nchar(as.character(month)) == 1, paste0("0", as.character(month)), as.character(month))
  ), .after = state)) %>%
  arrange(state, timefe)

cars = cars %>% 
  mutate(exposure_perc = exposure * 100)

#Estimate regression model
lm_1 = lm(log(cars_sales) ~ exposure_perc +
            post_crash + 
            exposure_perc*post_crash,
          data = cars)

lm_2 = lm(log(cars_sales) ~ exposure_perc +
            post_crash + 
            exposure_perc*post_crash + 
            state,
          data = cars)

lm_3 = lm(log(cars_sales) ~ exposure_perc +
            post_crash + 
            exposure_perc*post_crash + 
            factor(timefe) +
            state,
          data = cars)

#Compute robust standard errors
lm_1_se <- sqrt(diag(vcovHC(lm_1, type = "HC3")))
lm_2_se <- sqrt(diag(vcovHC(lm_2, type = "HC3")))
lm_3_se <- sqrt(diag(vcovHC(lm_3, type = "HC3")))

#Output results in html and Latex format
for (extension in c(".html", ".tex")){
  stargazer(lm_3,
            type = "text", 
            se = list(lm_1_se),
            dep.var.labels = "Log(Car sales)",
            covariate.labels = c("Exposure (%)", "Post-Crash", "Exposure (%) X Post-Crash"),
            title = "Regression results for baseline model",
            omit = c("state", "timefe", "Constant"),
            add.lines = list(c("State FE", "Yes"),
                             c("Time FE", "Yes")),
            align = TRUE, 
            out = paste0("../output/baseline_model", extension))
}

# Bonus -------------------------------------------------------------------
#####BONUS QUESTION #####

cars[, distance_treat := (year - 1929) * 12 + (month - 10)]
cars_et= cars[distance_treat  %in% -9:24,]
cars_et[, exposure_perc := exposure * 100]

bonus_reg = feols(log(cars_sales) ~ i(distance_treat, ref = 0, exposure_perc), fixef = c("timefe", "state"), 
                  data = cars_et, se = "hetero")
summary(bonus_reg)

retrieve_result = function(reg){
  summary = summary(reg)
  tables = data.frame(
    exposure = as.numeric(gsub("[^0-9.-]", "", x = names(summary$coefficients))),
    estimate = summary$coefficients,
    sd = summary$se
  )
}
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
