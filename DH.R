# Script for Digital Health on-hands project-------------------------------------------------------------
# Purpose: Data Cleaning for analysis
# Project focus: Data visualization For Family Planning Service delivery waiting time in Ghana
# # Student: John Amissah
# Student ID: 22205995
# Course: Digital Health
# Programme of Study: MSc Global Public Health


#settting my working directory
  setwd("C:/Users/pinno/Desktop/Digital Health/GPH Assignment")
  


#Installing needed packages for data analysis------------------------------------------------------
  
install.packages("pacman")
pacman::p_load(readr,
               rio,
               here,
               tidyverse,
               dplyr,
               plotly,
               janitor,
               ggplot2,
               ggpubr)


#Importing data using the rio------------------------------------------------------
library(readr)

FP_data <- read_csv("FP_data.csv", 
col_types = cols(level3 = col_factor(levels = c("urban", "rural")), 
age = col_integer(), 
sex = col_factor(levels = c("male", "female")), 
amt_paid_contracep = col_number(),
travel_duration = col_number(),
))


###converting character and string variables to factor and integers individually
as.factor(temp_data$mstatus)
as.factor (temp_data$education)
as.factor (temp_data$level3)
 

str(temp_data)

View(FP_data)


class("amt_paid_contracep")

table(temp_data$mstatus)

#regrouping marrital status

temp_data[temp_data == "currently_married"] <- "married"
temp_data[temp_data == "living_together"] <- "married"
temp_data[temp_data == "separated"] <- "divorced"
temp_data[temp_data == "widowed"] <- "single"

#regrouping age

temp_data["age_group"] = cut(temp_data$age, c(0, 14, 44, 49, Inf), c("0-19", "20-29", "30-39", ">40"), include.lowest=TRUE)


table(temp_data$mstatus)

#Viewing the variables to have an idea of the variables names i may be working with------------------------------------------------------
View(FP_data)

print(FP_data, n = 10)

names(FP_data)


summary(FP_data$age)

summary(FP_data$amt_paid_contracep)


##creating a subset and saving it------------------------------------------------------
temp_data <- subset(FP_data, select=c(age, mstatus, education, level3, travel_duration, amt_paid_contracep ))
View(temp_data)

glimpse(temp_data)


###checking the distribution of the continous variables (age,travel_duration, amt_paid_contracep)

plot(temp_data$age, temp_data$amt_paid_contracep)


# plot experience vs. salary 
# (color represents rank, shape represents sex)

library(ggplot2)
ggplot(fp_new, aes(x =amt_paid_contracep, 
                     y =age,
                     color=education,
                   shape = level3,
                     size=     travel_duration)) +
  geom_point(size =12, alpha = .6) +

  labs(title = "Academic salary by rank, sex, and years since degree")




###Stacked bar chart
ggplot(temp_data, aes(x = mstatus, fill =education)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


barplot(levels3)


