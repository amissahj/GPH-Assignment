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
library(haven)
FP_data <- read_csv("FP_data.csv")

#Viewing the variables to have an idea of the variables names i may be working with------------------------------------------------------
View(FP_data)


names(FP_data)

summary(FP_data$age)

summary(FP_data$amt_paid_contracep)


##creating a subset and saving it------------------------------------------------------
temp_data <- subset(FP_data, select=c(age, mstatus, education, level3, travel_duration, amt_paid_contracep ))
View(temp_data)

glimpse(temp_data)

View(temp_data)



###converting character and string variables to factor and integers individually
as.factor(temp_data$mstatus)
as.factor (temp_data$education)
as.factor (temp_data$level3)



str(temp_data)



class("amt_paid_contracep")

table(temp_data$mstatus)

#regrouping marrital status

temp_data[temp_data == "currently_married"] <- "married"
temp_data[temp_data == "living_together"] <- "married"
temp_data[temp_data == "separated"] <- "divorced"
temp_data[temp_data == "widowed"] <- "single"

#regrouping age

temp_data["age_group"] = cut(temp_data$age, c(0, 14, 44, 49, Inf), c("0-19", "20-29", "30-39", ">40"), include.lowest=TRUE)


table(temp_data$age_group)

as.numeric(temp_data$age_group)

###checking the distribution of the continous variables (age,travel_duration, amt_paid_contracep)

plot(temp_data$age, temp_data$amt_paid_contracep)

plot(temp_data$age_group)

# plot experience vs. salary 
# (color represents rank, shape represents sex)

library(ggplot2)
ggplot(temp_data, aes(x =amt_paid_contracep, 
                     y =age,
                     color=education,
                   shape = level3,
                     size=     travel_duration)) +
  geom_point(size =12, alpha = .6) +

  labs(title = "Contaceptive cost by location, age, and level of education")




###Stacked bar chart
ggplot(temp_data, aes(x = age_group, fill =education)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")+
  labs(title = "Educational level of Particiapntsby Age")




# Create the 2x2 table
my_table <- table(temp_data$level3, temp_data$education)

# Add row and column margins
my_margins <- addmargins(my_table)

# Print the table with margins
print(my_margins)




pacman::p_load(gridExtra)
age_table <- temp_data |>
  tably(age_group, level3) |>
  adorn_totals(where="both") |>
  adorn_percentages(denominator = "col") |>
  adorn_pct_formatting() |>
  adorn_ns(position = "front") |>
  adorn_title(row_name = "Age group",
              col_name = "place of residence")
  
  

plot(tableGrob(level3))
library(tableGrob)