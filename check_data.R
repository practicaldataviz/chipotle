# Comparing calorie distribution in meals.csv to original Upshot data

setwd("~/Dropbox/pdv/data/chipotle") # set working directory if necessary
meals <- read.csv("meals.csv") # load meals data

# What is the calorie total of a typical meal?

mean(meals$total_calories) # mean = 1,095
median(meals$total_calories) # median = 1,060

# How many calories are in a meal at the 75th percentile?

quantile(meals$total_calories, 0.75) # 1,340 calories

# How many calories are in a meal at the 95th percentile?

quantile(meals$total_calories, 0.95) # 1,758 calories

# How many meals were found in the original order data?

length(meals$total_calories) # 3,009 meals
