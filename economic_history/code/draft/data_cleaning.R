# This code clean the two datasets and save in parquet file

rm(list=ls())
gc()

library(arrow)
library(data.table)
library(readxl)

# Import Data -------------------------------------------------------------

cars = as.data.table(read_excel("../data/raw/StateNewCarRegistrations.xlsx"))
income = as.data.table(read_excel("../data/raw/stock_income.xlsx"))

# Data Cleaning -----------------------------------------------------------
# convert data to a long format
cars_melt = melt(cars, id.vars = c("month", "year"), 
                  variable.name = "state", value.name = "cars_sales")
# keep month that are only numeric later
cars_melt = cars_melt[!month %in% c("6-year total","total check (sum)"), .SD, .SDcols = c("state", "month", "year", "cars_sales")]

# merge with income data
cars_melt = merge(cars_melt, income, by = "state", all.x = TRUE)

# Creating Dummies & Variables ---------------------------------------------------------
cars_melt$month = as.numeric(cars_melt$month) #convert to numeric values the month data
cars_melt[, post_crash := ifelse(year > 1929 | (year == 1929 & month >= 10), 1, 0)] #set the post_crash dummy
cars_melt[, exposure := dividend_income/total_income, by = c("state")] # we build an exposure measure to the 1929 krach

exposed_state = unique(cars_melt[, .(state, exposure)]) # we create a subdataset with the list of state with their exposition

# Save Data in parquet and csv format -------------------------------------

write_parquet(cars_melt, "../data/final/clean_data.parquet")
fwrite(cars_melt, "../data/final/clean_data.csv")
fwrite(exposed_state, "../data/final/exposed_state.csv")
