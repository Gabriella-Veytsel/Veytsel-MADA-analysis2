###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#Dataset from https://data.cdc.gov/NCHS/AH-Cumulative-Provisional-COVID-19-Deaths-by-Sex-R/mwk9-wnfr
#Cumulative provisional counts of deaths sex, race/Hispanic origin, age group, and by select underlying causes of death. 
#The dataset also includes provisional counts of death for COVID-19, coded to ICD-10 code U07.1 as an underlying or multiple cause of death. 
#Includes deaths that occurred between January 1, 2020 to July 28, 2020.

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(tidyverse) #for data processing
library(here) #to set paths
library(lubridate) #dates

#path to data (note the use of the here() package and not absolute paths)
data_location <- here::here("data","raw_data","AH_Cumulative_Provisional_COVID-19_Deaths_by_Sex__Race__and_Age_from_1_1_2020_to_7_28_2020.csv")

#load data. 
rawdata <- read.csv(data_location) #don't edit raw data

#Examine several variables: Sex, Age.Group, and Race.Ethnicity
##############################################################
table(rawdata$Sex) #Female (F) and Male (M)
class(rawdata$Sex) #Character

table(rawdata$Age.group) 
class(rawdata$Age.group) #Character

class(rawdata$Year) #Integer

#Change Sex and Age.group from character -> factor 
processeddata <- rawdata %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Age.group = as.factor(Age.group)) 

#Edit Race.Ethnity variable
table(processeddata$Race.Ethnicity)
processeddata$Ethnicity = word(processeddata$Race.Ethnicity, 2, -1) #extract all but the first word
processeddata$Race = word(processeddata$Race.Ethnicity, 1) #extract the first word
table(processeddata$Ethnicity)
table(processeddata$Race)

processeddata %>%
  select(Race.Ethnicity, Race, Ethnicity) #double check
processeddata <- processeddata %>%
  select(-c(Race.Ethnicity)) #good to remove race.ethnicity now

# save data as RDS

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)
