###################################
## Course   : Visualization
## Lab      : 1
## Group    : 8
## 
## 
####################################
rm(list = ls())

## Assignment 1

install.packages("MASS")
library(MASS)

data(Cars93, package = "MASS")
mydata_1 <- Cars93


df1=aggregate(Price~Type, data=mydata_1, FUN=mean) 
barplot(df1$Price, names.arg=df1$Type)
