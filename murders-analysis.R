#---
#title: "Murders-analysis"
#---
  
# load in the dataset
library(tidyverse)
murders <- read_csv("./data/murders.csv")
names(murders)

# Look at dataset content
str(murders)

# Look at first 6 records
head(murders)

#Find the state with the lowest total number of murders
murders$state[which.min(murders$total)]

# Find the state with the highest total number of murders
murders$state[which.max(murders$total)]

# Create a vector for the state names 
# Order the states by population
states <- murders$state
ranks <- rank(murders$population)
index <- order(murders$population)
states_in_order <- data.frame( state_name = states[index],
                               state_rank = ranks[index])
states_in_order

# Calculate the murder rate per capita 
# Order the states by descending murder rate
murder_rate <- murders$total / murders$population * 100000
murders$state[ order(murder_rate, decreasing = TRUE)]

# We want to find the states where the murder rate is less than 0.71
sum(murder_rate < 0.71)
murders$state[murder_rate < 0.71]

# Find states that meet two conditions: Region = West and murder rate is less than 1
west <- murders$region == "West"
safe <- murder_rate <=1
index <- safe & west
murders$state[index]

# Find the murder rate for Massachusetts
murder_rate[which(murders$state == "Massachusetts")]

## dplyr package
#Install the dplyr package
library(dplyr)

# Add a new column to the table, for the murder rate per capita
murders <- mutate(murders, rate = total / population * 100000)
head(murders)

# Display the 5 states with a murder rate less than 0.71
filter(murders, rate < 0.71)

# Display only 3 columns for the 5 states with the lowest rate
murders %>% select(state, region, rate) %>% filter( rate < 0.71 )

#Create a scatterplot of population in millions & total murders
population_in_millions <- murders$population / 1000000
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

# The graph shows that there is a relationship between population and number of murders 

# Create a histogram for the murder rate
hist(murders$rate)

# There is one very high anomaly
murders$state[which.max(murders$rate)]

# Create a boxplot
boxplot(rate~region, data = murders)

# The region South has the highest murder rate 
  
  
