library(tidyverse)
library(Hmisc)

#read data
data <- read.csv("C:/Users/dell/Desktop/R/Covid Project/COVID19_line_list_data.csv")
#showing some info
describe(data)

#cleaning death column
data$death_dummy <- as.integer(data$death != 0)
unique(data$death_dummy)  #now it is 0 or 1 only

#death rate 
sum(data$death_dummy) / nrow(data)
#5.8% dies

#Age
# claim: people who die are older 
dead <- subset(data,death_dummy==1)  
alive <- subset(data,death_dummy==0)
mean(dead$age,na.rm=T)  #69yrs
mean(alive$age,na.rm=T) #48yrs

t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
#p-vale ~ 0, reject null hypothesis


#Gender
# claim: gender has no effect
men <- subset(data,gender=="male")  
women <- subset(data,gender=="female")
mean(men$death_dummy,na.rm=T)  #8.5%
mean(women$death_dummy,na.rm=T) #3.7%

t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#men have 0.8% to 8.8% higher chance of dying
#p-vale =0.002 < 0.05,  reject null hypothesis



