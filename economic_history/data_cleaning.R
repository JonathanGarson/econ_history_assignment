# This code clean the two datasets and save in parquet file

rm(list=ls())
gc()

library(arrow)
library(data.table)
library(readxl)

# Import Data -------------------------------------------------------------

cars = read_excel("../StateNewCarRegistrations.xlsx")
income = read_excel("../stock_income.xlsx")

# Data Cleaning -----------------------------------------------------------



