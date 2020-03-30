
library(tidyverse)
library(reshape2)
library(plyr)
library(dplyr)

# CIS data
cisData <- subset(read.csv("working_data/CIS_1110023901_databaseLoadingData.csv", head=TRUE, sep=","), 
                  select=c(REF_DATE, GEO, Age.group, Sex, Income.source, Statistics, VALUE))

cisData$GEO <- revalue(cisData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                   "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                   "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                   "British Columbia" = "BC"))

cisData$Age.group <- revalue(cisData$Age.group, c("16 years and over" = "Total age group", "16 to 24 years" = "0-24 yrs", 
                                                  "25 to 34 years" = "25-34 yrs", "35 to 44 years" = "35-44 yrs", 
                                                  "45 to 54 years" = "45-54 yrs", "55 to 64 years" ="55-64 yrs", 
                                                  "65 years and over" = "65+ yrs"))

cisData$Statistics <- revalue(cisData$Statistics, c("Average income (excluding zeros)" = "Average income",
                                                    "Median income (excluding zeros)" = "Median income"))

cisData <- within(cisData, c(VALUE[Statistics == "Number of persons" | 
                                     Statistics == "Number with income"] <- VALUE[Statistics == "Number of persons" | 
                                                                                    Statistics == "Number with income"]*1000,
                             VALUE[Statistics == "Aggregate income"] <- VALUE[Statistics == "Aggregate income"]*1000))

cisData$Data.source <- "CIS"

cisData <- rename(cisData, Year = REF_DATE)
 

# T1FF data A
t1ffaData <- subset(read.csv("working_data/T1FFA_1110000801_databaseLoadingData.csv", head=TRUE, sep=","), 
                  select=c(REF_DATE, GEO, Age.group, Sex, Persons.with.income, VALUE))

t1ffaData$GEO <- revalue(t1ffaData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                      "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                      "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                      "British Columbia" = "BC", "Yukon" = "YT", "Northwest Territories" = "NT",
                                      "Nunavut" = "NU"))

t1ffaData$Age.group <- revalue(t1ffaData$Age.group, c("All age groups" = "Total age group", "0 to 24 years" = "0-24 yrs", 
                                                  "25 to 34 years" = "25-34 yrs", "35 to 44 years" = "35-44 yrs", 
                                                  "45 to 54 years" = "45-54 yrs", "55 to 64 years" ="55-64 yrs", 
                                                  "65 years and over" = "65+ yrs", "65 to 74 years" = "65-74 yrs",
                                                  "75 years and over" = "75+ yrs"))

t1ffaData$Persons.with.income <- revalue(t1ffaData$Persons.with.income, c("Total persons with income" = "Number with income",
                                                                          "Median total income" = "Median income"))

t1ffaData$Income.source <- factor("Total income")

t1ffaData$Data.source <- "T1FF"

t1ffaData <- rename(t1ffaData, Year = REF_DATE, Statistics = Persons.with.income)

#take out of t1ffaData where total age group, total income source, number with income

# T1FF data B
t1ffbData <- subset(read.csv("working_data/T1FFB_1110000701_databaseLoadingData.csv", head=TRUE, sep=","), 
                    select=c(REF_DATE, GEO, Individuals.and.income, Sex, Source.of.income, VALUE))

t1ffbData$GEO <- revalue(t1ffbData$GEO, c("Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                          "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                          "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                          "British Columbia" = "BC", "Yukon" = "YT", "Northwest Territories" = "NT",
                                          "Nunavut" = "NU"))

t1ffbData$Individuals.and.income <- revalue(t1ffbData$Individuals.and.income, 
                                            c("Number of tax filers and dependants" = "Number with income",
                                              "Amount of income (Dollars)" = "Aggregate income"))

t1ffbData$Source.of.income <- revalue(t1ffbData$Source.of.income, c("Net self-employment income" = "Self-employment income",
                                   "Total employment income" = "Employment income",
                                   "Total government transfers" = "Government transfers"))

t1ffbData$Age.group <- "Total age group"

t1ffbData$Data.source <- "T1FF"

t1ffbData <- rename(t1ffbData, Income.source = Source.of.income, Statistics = Individuals.and.income, Year = REF_DATE)

#use t1ffbData to calculate average income by income source and other vars
t1ffbData_t <- dcast(t1ffbData, Data.source+Year+GEO+Sex+Income.source+Age.group~Statistics, value.var="VALUE")
t1ffbData_t$`Aggregate income` <- t1ffbData_t$`Aggregate income`*1000
t1ffbData_t$VALUE <- round_any(t1ffbData_t$`Aggregate income`/t1ffbData_t$`Number with income`, 100)
t1ffbData_t$Statistics <- "Average income"
t1ffbData_t$`Aggregate income` <- NULL
t1ffbData_t$`Number with income` <- NULL

# Combine sources

finalData <- rbind(cisData, t1ffaData, t1ffbData, t1ffbData_t)
finalData$GEO <- factor(finalData$GEO, levels=c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"))

finalData <- distinct(finalData)

write.csv(finalData, file="data/finalData.csv", na="",row.names = F)

