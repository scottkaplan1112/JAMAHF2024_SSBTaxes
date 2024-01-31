library(data.table)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(lfe)
library(fixest)
library(stargazer)
library(modelr)

set.seed(05051992)
memory.limit(size = 25000000)

################################################
###############Reading in Data##################
################################################

##Setting the working directory for all data

# ##Mazu
# setwd("D:/FacultyData/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files")

##Ceto
setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files")

####################################################################
###############DID and Event Study Data Prep Function###############
####################################################################

SyntheticControl_DataPrep_Fun <- function(data){
  
  #data <- sample_n(MonthLevel_2012, 1000000)

  ##Subsetting to data needed for synthetic control
  data_name <- select(data, -Post, -time_to_treat, -time_to_treat_months)
  
  print("Filtering out any NA or 0 observations")
  
  ##Removing any row with an NA
  data_name <- data_name[complete.cases(data_name),]
  
  ##Removing any row with a "0" or negative value for "TotalOz"
  data_name <- filter(data_name, !(TotalOz <= 0))
  
  ##Removing any row with a missing entry for "store_zip3" 
  data_name <- filter(data_name, !(store_zip3 == ""))
  
  ################################################
  #########CREATING NEW VARIABLES FOR SC##########
  ################################################
  
  print("Now creating the SC variables")
  
  ##Creating a time-to-treat variable FOR ALL UNITS
  data_name <- mutate(data_name,
                      time_to_treat_SanFrancisco = month_year - as.Date("2018-01-01"),
                      time_to_treat_DC = month_year - as.Date("2019-10-01"),
                      time_to_treat_Seattle = month_year - as.Date("2018-01-01"),
                      time_to_treat_Philadelphia = month_year - as.Date("2017-01-01"),
                      time_to_treat_Boulder= month_year - as.Date("2017-07-01"),
                      time_to_treat_Berkeley = month_year - as.Date("2015-03-01"),
                      time_to_treat_Oakland = month_year - as.Date("2017-07-01")) %>%
    mutate(time_to_treat_months_SanFrancisco = round(time_to_treat_SanFrancisco/30.417, digit = 0),
           time_to_treat_months_DC = round(time_to_treat_DC/30.417, digit = 0),
           time_to_treat_months_Seattle = round(time_to_treat_Seattle/30.417, digit = 0),
           time_to_treat_months_Philadelphia = round(time_to_treat_Philadelphia/30.417, digit = 0),
           time_to_treat_months_Boulder = round(time_to_treat_Boulder/30.417, digit = 0),
           time_to_treat_months_Berkeley = round(time_to_treat_Berkeley/30.417, digit = 0),
           time_to_treat_months_Oakland = round(time_to_treat_Oakland/30.417, digit = 0))
  
  data_name <- select(data_name, -c(time_to_treat_SanFrancisco, time_to_treat_DC,
                                    time_to_treat_Seattle, time_to_treat_Philadelphia,
                                    time_to_treat_Boulder, time_to_treat_Berkeley,
                                    time_to_treat_Oakland))
  
  data_name$time_to_treat_months_SanFrancisco <- as.numeric(data_name$time_to_treat_months_SanFrancisco)
  data_name$time_to_treat_months_DC <- as.numeric(data_name$time_to_treat_months_DC)
  data_name$time_to_treat_months_Seattle <- as.numeric(data_name$time_to_treat_months_Seattle)
  data_name$time_to_treat_months_Philadelphia<- as.numeric(data_name$time_to_treat_months_Philadelphia)
  data_name$time_to_treat_months_Boulder <- as.numeric(data_name$time_to_treat_months_Boulder)
  data_name$time_to_treat_months_Berkeley <- as.numeric(data_name$time_to_treat_months_Berkeley)
  data_name$time_to_treat_months_Oakland <- as.numeric(data_name$time_to_treat_months_Oakland)
  
  return(data_name)
  
}

#########################
########2012#############
#########################

##Reading in Nielsen Movement Data
MonthLevel_2012 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2012_SSBs_MonthLevel.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "Date",
                                             upc10 = "character",
                                             store_zip3 = "character"))

MonthLevel_2012_SCVariables <- SyntheticControl_DataPrep_Fun(MonthLevel_2012)
fwrite(MonthLevel_2012_SCVariables, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2012_SSBs_MonthLevel_SCEstimation.csv" )

rm(MonthLevel_2012)
rm(MonthLevel_2012_SCVariables)


#########################
########2013-2014########
#########################

##Reading in Nielsen Movement Data
MonthLevel_2013_2014 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2013_2014_SSBs_MonthLevel.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "Date",
                                             upc10 = "character",
                                             store_zip3 = "character"))

MonthLevel_2013_2014_SCVariables <- SyntheticControl_DataPrep_Fun(MonthLevel_2013_2014)
fwrite(MonthLevel_2013_2014_SCVariables, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2013_2014_SSBs_MonthLevel_SCEstimation.csv" )

rm(MonthLevel_2013_2014)
rm(MonthLevel_2013_2014_SCVariables)



#########################
########2015-2016########
#########################

##Reading in Nielsen Movement Data
MonthLevel_2015_2016 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2015_2016_SSBs_MonthLevel.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "Date",
                                             upc10 = "character",
                                             store_zip3 = "character"))

MonthLevel_2015_2016_SCVariables <- SyntheticControl_DataPrep_Fun(MonthLevel_2015_2016)
fwrite(MonthLevel_2015_2016_SCVariables, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2015_2016_SSBs_MonthLevel_SCEstimation.csv" )

rm(MonthLevel_2015_2016)
rm(MonthLevel_2015_2016_SCVariables)

#########################
########2017-2018########
#########################

MonthLevel_2017_2018 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2017_2018_SSBs_MonthLevel.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "Date",
                                             upc10 = "character",
                                             store_zip3 = "character"))

MonthLevel_2017_2018_SCVariables <- SyntheticControl_DataPrep_Fun(MonthLevel_2017_2018)
fwrite(MonthLevel_2017_2018_SCVariables, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2017_2018_SSBs_MonthLevel_SCEstimation.csv" )

rm(MonthLevel_2017_2018)
rm(MonthLevel_2017_2018_SCVariables)

#########################
########2019-2020########
#########################

MonthLevel_2019_2020 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2019_2020_SSBs_MonthLevel.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "Date",
                                             upc10 = "character",
                                             store_zip3 = "character"))

MonthLevel_2019_2020_SCVariables <- SyntheticControl_DataPrep_Fun(MonthLevel_2019_2020)
fwrite(MonthLevel_2019_2020_SCVariables, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2019_2020_SSBs_MonthLevel_SCEstimation.csv" )

rm(MonthLevel_2019_2020)
rm(MonthLevel_2019_2020_SCVariables)

gc()

