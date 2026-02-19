

## ------------------------------------------------------
# Master File for Product Data Analysis Case Study

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)


# load and set working directory to where data and code is located
rm(list = ls())
setwd("C:/Users/Zahra/Desktop/NaturalCycles") # change this to your working directory


## ------------------------------------------------------
# load data and variable construction
source("./codes/A1_0_data_cleaning.R")

## ------------------------------------------------------
# data validation and exploration
source("./codes/A1_1_data_exploration.R")

## ------------------------------------------------------
# linear regression  model for analyzing A/B experiment results and dynamic graphs
source("./codes/A1_2_conversion_analysis.R")

## ------------------------------------------------------
# dynamic effect on activity conditional of subscription
source("./codes/A1_3_dynamic_activity.R")

