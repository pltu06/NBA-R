#R Preparation
#Patrick Tu
#Started on 11/28/21
#Purpose: Prepare the R workspace by loading packages and variables that are 
#needed across all scripts

#load packages----
library(nbastatR) #This is for loading NBA data
library(dplyr) #This is for manipulating data sets
library(ggplot2) #This is for graphing data
library(tidyr) # functions for tidying data
library(Hmisc) # loads the %nin% filter

#necessary for loading in NBA data----
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

