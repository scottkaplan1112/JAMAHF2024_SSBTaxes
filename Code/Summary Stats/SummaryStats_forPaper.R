library(tidysynth)
library(data.table)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(lfe)
library(fixest)
library(stargazer)
library(modelr)
library(xtable)
library(purrr)
library(Synth)
library(SCtools)
library(MASS)
library(ggrepel)
library(reshape)
library(reshape2)
library(devtools)
#devtools::install_github("ebenmichael/augsynth")
library(augsynth)

set.seed(05051992)
memory.limit(size = 25000000)

##Setting the working directory for SSB Project

# #FacultyData
# setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis")

#Ceto
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

##ALL SYNTHETIC CONTROL FUNCTIONS IN THIS SOURCE FILE
source("Code/SyntheticControl_Functions.R")
source("Code/augsynth_inference.R")

# ##Toggle to drop immediately bordering zip codes in synthetic control estimation
# DropBorderingZips <- 1
# 
# ##Toggle to drop post-COVID data
# DropCovid <- 1
# 
# ##Toggle to just do treated zip codes + those starting with 9 (=0 if just 9, =1 if all)
# Zips_All <- 1
# 
# ##Toggle to 1 if running synthetic control analyses for immediate bordering zip codes
# Border_Analysis <- 0

##Drop Berkeley
Drop_Berkeley <- 1

####################################################
###############QUANTITY DATA########################
####################################################

################################################
###############Reading in Data##################
################################################

NielsenMovement_AllMonths_withCensusData_ZipCode <- fread("Data/NielsenMovement_AllMonths_withCensusData_ZipCode.csv",
                                                          colClasses = c(store_zip3 = "character"))

##Calculating Mean of dep. var.
Mean_OzSoldbyMonth_ZipLevel <- mean(NielsenMovement_AllMonths_withCensusData_ZipCode$TotalOz_zipcode)

#creating a calendar time variable
NielsenMovement_AllMonths_withCensusData_ZipCode <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           calendar_time = ifelse(time_to_treat_months_Berkeley < 25,
                                                                                  time_to_treat_months_Berkeley - min(time_to_treat_months_Berkeley) + 1,
                                                                                  time_to_treat_months_SanFrancisco+73))

#List of taxed and border zip codes (including DC and its border zips)
taxed_zips <- c("946", "981", "803", "191", "941")
border_zips <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                 "081", "080", "948", "945")

#creating the treated variable for taxed zip codes
NielsenMovement_AllMonths_withCensusData_ZipCode <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           treated = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0 |
                                                                              store_zip3 == "981" & time_to_treat_months_Seattle >= 0 |
                                                                              store_zip3 == "803" & time_to_treat_months_Boulder >= 0 |
                                                                              store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0 |
                                                                              store_zip3 == "946" & time_to_treat_months_Oakland >= 0 |
                                                                              store_zip3 == "200" & time_to_treat_months_DC >= 0 |
                                                                              store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))
# treated_SF = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
# treated_Seattle = ifelse(store_zip3 == "981" & time_to_treat_months_Seattle >= 0, 1, 0),
# treated_Boulder = ifelse(store_zip3 == "803" & time_to_treat_months_Boulder >= 0, 1, 0),
# treated_Philadelphia = ifelse(store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0, 1, 0),
# treated_Oakland = ifelse(store_zip3 == "946" & time_to_treat_months_Oakland >= 0, 1, 0),
# treated_DC = ifelse(store_zip3 == "200" & time_to_treat_months_DC >= 0, 1, 0),
# treated_Berkeley = ifelse(store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))


#creating the overall border, border groupings, and individual taxed city treated variables
NielsenMovement_AllMonths_withCensusData_ZipCode <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           treated_unit_border = ifelse(store_zip3 == "949" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                          store_zip3 == "940" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                          store_zip3 == "980" & time_to_treat_months_Seattle >= 0 | 
                                                                                          store_zip3 == "982" & time_to_treat_months_Seattle >= 0 | 
                                                                                          store_zip3 == "983" & time_to_treat_months_Seattle >= 0 | 
                                                                                          store_zip3 == "984" & time_to_treat_months_Seattle >= 0 |
                                                                                          store_zip3 == "800" & time_to_treat_months_Boulder >= 0 | 
                                                                                          store_zip3 == "804" & time_to_treat_months_Boulder >= 0 | 
                                                                                          store_zip3 == "805" & time_to_treat_months_Boulder >= 0 | 
                                                                                          store_zip3 == "081" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                          store_zip3 == "080" & time_to_treat_months_Philadelphia >= 0 |
                                                                                          store_zip3 == "945" & time_to_treat_months_Oakland >= 0 |
                                                                                          store_zip3 == "948" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                           treated_unit_border_SF = ifelse(store_zip3 == "949" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                             store_zip3 == "940" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
                                                           treated_unit_border_Seattle = ifelse(store_zip3 == "980" & time_to_treat_months_Seattle >= 0 | 
                                                                                                  store_zip3 == "982" & time_to_treat_months_Seattle >= 0 | 
                                                                                                  store_zip3 == "983" & time_to_treat_months_Seattle >= 0 | 
                                                                                                  store_zip3 == "984" & time_to_treat_months_Seattle >= 0, 1, 0),
                                                           treated_unit_border_Boulder = ifelse(store_zip3 == "800" & time_to_treat_months_Boulder >= 0 | 
                                                                                                  store_zip3 == "804" & time_to_treat_months_Boulder >= 0 | 
                                                                                                  store_zip3 == "805" & time_to_treat_months_Boulder >= 0, 1, 0),
                                                           treated_unit_border_Philadelphia = ifelse(store_zip3 == "081" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                                       store_zip3 == "080" & time_to_treat_months_Philadelphia >= 0, 1, 0),
                                                           treated_unit_border_Oakland = ifelse(store_zip3 == "945" & time_to_treat_months_Oakland >= 0 |
                                                                                                  store_zip3 == "948" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                           treated_unit_border_Berkeley = ifelse(store_zip3 == "945" & time_to_treat_months_Berkeley >= 0 |
                                                                                                   store_zip3 == "948" & time_to_treat_months_Berkeley >= 0, 1, 0),
                                                           treated_unit_SF = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
                                                           treated_unit_Seattle = ifelse(store_zip3 == "981" & time_to_treat_months_Seattle >= 0, 1, 0),
                                                           treated_unit_Boulder = ifelse(store_zip3 == "803" & time_to_treat_months_Boulder >= 0, 1, 0),
                                                           treated_unit_Philadelphia = ifelse(store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0, 1, 0),
                                                           treated_unit_Oakland = ifelse(store_zip3 == "946" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                           #treated_unit_DC = ifelse(store_zip3 == "200", 1, 0),
                                                           treated_unit_Berkeley = ifelse(store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))


#Filtering out DC for now
NielsenMovement_AllMonths_withCensusData_ZipCode <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           !(store_zip3 == "200"))

if(Drop_Berkeley == 1){
  NielsenMovement_AllMonths_withCensusData_ZipCode <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                             !(store_zip3 == "947"))
}

#Including urbanicity variable
CensusData_Combined_Final_3DigitZip <- fread("Data/CensusData_Combined_Final_3DigitZip.csv")
CensusData_Combined_Final_3DigitZip <- dplyr::select(CensusData_Combined_Final_3DigitZip, GEOID_3, perc_urban)

NielsenMovement_AllMonths_withCensusData_ZipCode$store_zip3_numeric <- as.numeric(NielsenMovement_AllMonths_withCensusData_ZipCode$store_zip3)
NielsenMovement_AllMonths_withCensusData_ZipCode <- left_join(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                              CensusData_Combined_Final_3DigitZip,
                                                              by = c("store_zip3_numeric" = "GEOID_3"))

##Number of stores by zip code
stores_census <- fread("Data/Store_Census_Combined.csv")

stores_treated <- filter(stores_census, store_zip3 %in% taxed_zips) %>%
  group_by(store_zip3) %>%
  summarise(count = n())

stores_borders <- filter(stores_census, store_zip3 %in% border_zips) %>%
  summarise(count = n())


##Total Ounces in first year of tax by treated zip code
TaxedData <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% taxed_zips)
TaxedData <- filter(TaxedData, (store_zip3 == "803" & 
                      time_to_treat_months_Boulder >= 0 &
                      time_to_treat_months_Boulder < 12) |
                      (store_zip3 == "191" & 
                         time_to_treat_months_Philadelphia >= 0 &
                         time_to_treat_months_Philadelphia < 12) |
                      (store_zip3 == "946" & 
                         time_to_treat_months_Oakland >= 0 &
                         time_to_treat_months_Oakland < 12) |
                      (store_zip3 == "941" & 
                         time_to_treat_months_SanFrancisco >= 0 &
                         time_to_treat_months_SanFrancisco < 12) |
                      (store_zip3 == "981" & 
                         time_to_treat_months_Seattle >= 0 &
                         time_to_treat_months_Seattle < 12))

TotalOunces_byCity <- group_by(TaxedData, store_zip3) %>%
  summarise(TotalOunces = sum(TotalOz_zipcode))

#Filtering out non-urban donor zip codes
MeanUrbanicity_Treated <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% taxed_zips) %>%
  summarise(MeanUrbanicity = mean(perc_urban)) %>% as.numeric()

MeanUrbanicity_Borders <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% border_zips) %>%
  summarise(MeanUrbanicity = mean(perc_urban)) %>% as.numeric()

SDUrbanicity <- summarise(NielsenMovement_AllMonths_withCensusData_ZipCode, 
                          SDUrbanicity = sd(perc_urban)) %>% as.numeric()

UrbanZipCodes <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, perc_urban > MeanUrbanicity_Treated-SDUrbanicity &
                          !(store_zip3 %in% taxed_zips) & 
                          !(store_zip3 %in% border_zips))

stores_UrbanDonors <- filter(stores_census, store_zip3 %in% UrbanZipCodes$store_zip3) %>%
  summarise(count = n())
