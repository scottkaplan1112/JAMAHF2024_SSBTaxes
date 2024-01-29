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
library(here)
library(did)
library(plm)
library(ggrepel)
library(reshape)
library(reshape2)
library(devtools)
#devtools::install_github("ebenmichael/augsynth")
library(augsynth)

# Turn off warning-error-conversion, because the tiniest warning stops installation
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")


set.seed(05051992)
memory.limit(size = 25000000)

##Setting the working directory for SSB Project

# #FacultyData
# setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis")

#Ceto
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

##ALL SYNTHETIC CONTROL FUNCTIONS IN THIS SOURCE FILE
source("Code/SyntheticControl_Functions.R")

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
taxed_zips <- c("946", "981", "803", "191", "200", "947", "941")
border_zips_withDC <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                        "081", "080", "222", "223", "201", "220", "216", "218", "948", "945")
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

#################################################
###############PRICE DATA########################
#################################################

################################################
###############Reading in Data##################
################################################

NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- fread("Data/NielsenMovement_AllMonths_withCensusData_ZipCode_Prices.csv",
                                                                 colClasses = c(store_zip3 = "character"))

##Calculating Mean of dep. var.
Mean_AvgPriceperOzbyMonth_ZipLevel <- mean(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices$AvgPrice_PerOz_zipcode)

#creating a calendar time variable
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                  calendar_time = ifelse(time_to_treat_months_Berkeley < 25,
                                                                                         time_to_treat_months_Berkeley - min(time_to_treat_months_Berkeley) + 1,
                                                                                         time_to_treat_months_SanFrancisco+73))

#List of taxed and border zip codes (including DC and its border zips)
taxed_zips <- c("946", "981", "803", "191", "200", "947", "941")
border_zips_withDC <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                        "081", "080", "222", "223", "201", "220", "216", "218", "948", "945")
border_zips <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                 "081", "080", "948", "945")

#creating the treated variable for taxed zip codes
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
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
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
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
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                  !(store_zip3 == "200"))



if(Drop_Berkeley == 1){
  NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                    !(store_zip3 == "947"))
}

#Including urbanicity variable
CensusData_Combined_Final_3DigitZip <- fread("Data/CensusData_Combined_Final_3DigitZip.csv")
CensusData_Combined_Final_3DigitZip <- dplyr::select(CensusData_Combined_Final_3DigitZip, GEOID_3, perc_urban)

NielsenMovement_AllMonths_withCensusData_ZipCode_Prices$store_zip3_numeric <- as.numeric(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices$store_zip3)
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- left_join(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                     CensusData_Combined_Final_3DigitZip,
                                                                     by = c("store_zip3_numeric" = "GEOID_3"))



#########################################################
###############URBANICITY CUTOFFS########################
#########################################################

MeanUrbanicity_Treated <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% taxed_zips) %>%
  summarise(MeanUrbanicity = mean(perc_urban)) %>% as.numeric()

MeanUrbanicity_Borders <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% border_zips) %>%
  summarise(MeanUrbanicity = mean(perc_urban)) %>% as.numeric()

SDUrbanicity <- summarise(NielsenMovement_AllMonths_withCensusData_ZipCode, 
                          SDUrbanicity = sd(perc_urban)) %>% as.numeric()


######################################################
###############TWFE ESTIMATION########################
######################################################


#####################################################################
###########################CONSUMPTION###############################
#####################################################################

TWFEEstimationData_Composite <- NielsenMovement_AllMonths_withCensusData_ZipCode

#Filtering out non-urban donor zip codes
TWFEEstimationData_Composite <- filter(TWFEEstimationData_Composite, perc_urban > MeanUrbanicity_Treated-SDUrbanicity | 
                                       store_zip3 %in% taxed_zips |
                                       store_zip3 %in% border_zips)

#Filtering out all border zip codes
TWFEEstimationData_Composite <- filter(TWFEEstimationData_Composite,
                                     !(store_zip3 %in% border_zips))


#Adding logged total ounces as a variable
TWFEEstimationData_Composite <- mutate(TWFEEstimationData_Composite, log_TotalOz_zipcode = log(TotalOz_zipcode))

#Running a balanced estimation (for the composite)
TWFEEstimationData_Composite <- mutate(TWFEEstimationData_Composite, treat = ifelse(max(treated) > 0, 1, 0),
         calendar_time_new = treated*calendar_time*-1,
         calendar_time_new = ifelse(treat == 1 & calendar_time_new == 0, -999, calendar_time_new),
         time_to_treat = ifelse(calendar_time_new == -999, 0, -1*(calendar_time_new - max(calendar_time_new))),
         time_to_treat = ifelse(calendar_time_new == -999 & treat == 1, calendar_time, time_to_treat),
         time_to_treat_final = ifelse(calendar_time_new == -999 & treat == 1, time_to_treat-max(time_to_treat)-1, time_to_treat))

#Calculating pretreatment means for all taxed zip codes
PreTreatmentMeans_Taxed <- filter(TWFEEstimationData_Composite, store_zip3 %in% taxed_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PreTreatmentMeans = mean(TotalOz_zipcode))


##Running DID regressions (LEVELS) - composite and individual cities

  #Composite
  DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated | calendar_time + store_zip3 | 0 | store_zip3, 
                                  data = filter(TWFEEstimationData_Composite, time_to_treat_final >= -60 & time_to_treat_final <= 25))
  summary(DIDReg_FE_clustered_zip)
  TWFE_1 <- DIDReg_FE_clustered_zip
  
  #SF
  SFData <-TWFEEstimationData_Composite
  names(SFData)[names(SFData) == "treated"] <- "treated_SF"
  
  DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_SF | calendar_time + store_zip3 | 0 | store_zip3, 
                                  data = filter(SFData, 
                                                !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
  summary(DIDReg_FE_clustered_zip)
  TWFE_1_SF <- DIDReg_FE_clustered_zip
  
  #Oakland
  OakData <-TWFEEstimationData_Composite
  names(OakData)[names(OakData) == "treated"] <- "treated_Oakland"
  
  DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Oakland | calendar_time + store_zip3 | 0 | store_zip3, 
                                  data = filter(OakData, 
                                                !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
  summary(DIDReg_FE_clustered_zip)
  TWFE_1_Oakland <- DIDReg_FE_clustered_zip
  
  #Philadelphia
  PhillyData <-TWFEEstimationData_Composite
  names(PhillyData)[names(PhillyData) == "treated"] <- "treated_Philadelphia"
  
  DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Philadelphia | calendar_time + store_zip3 | 0 | store_zip3, 
                                  data = filter(PhillyData, 
                                                !(store_zip3 == "941" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
  summary(DIDReg_FE_clustered_zip)
  TWFE_1_Philadelphia <- DIDReg_FE_clustered_zip
  
  #Boulder
  BoulderData <-TWFEEstimationData_Composite
  names(BoulderData)[names(BoulderData) == "treated"] <- "treated_Boulder"
  
  DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Boulder | calendar_time + store_zip3 | 0 | store_zip3, 
                                  data = filter(BoulderData, 
                                                !(store_zip3 == "191" | store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "981")))
  summary(DIDReg_FE_clustered_zip)
  TWFE_1_Boulder <- DIDReg_FE_clustered_zip
  
  #Seattle
  SeattleData <-TWFEEstimationData_Composite
  names(SeattleData)[names(SeattleData) == "treated"] <- "treated_Seattle"
  
  DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Seattle | calendar_time + store_zip3 | 0 | store_zip3, 
                                  data = filter(SeattleData, 
                                                !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "941")))
  summary(DIDReg_FE_clustered_zip)
  TWFE_1_Seattle <- DIDReg_FE_clustered_zip




#########################################
#########EVENT STUDY ESTIMATION##########
#########################################

##Info from: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
EventStudy_EstimationData_Composite <- group_by(TWFEEstimationData_Composite, store_zip3) %>%
  mutate(treat = ifelse(max(treated) > 0, 1, 0),
         calendar_time_new = treated*calendar_time*-1,
         calendar_time_new = ifelse(treat == 1 & calendar_time_new == 0, -999, calendar_time_new),
         time_to_treat = ifelse(calendar_time_new == -999, 0, -1*(calendar_time_new - max(calendar_time_new))),
         time_to_treat = ifelse(calendar_time_new == -999 & treat == 1, calendar_time, time_to_treat),
         time_to_treat_final = ifelse(calendar_time_new == -999 & treat == 1, time_to_treat-max(time_to_treat)-1, time_to_treat))

##Event-Study (LEVELS)
  
  #Composite
  mod_twfe = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                   |                    ## Other controls
                     store_zip3 + calendar_time,        ## FEs
                   cluster = "store_zip3",                       ## Clustered SEs
                   data = filter(EventStudy_EstimationData_Composite, time_to_treat_final >= -60 & time_to_treat_final <= 25))
  summary(mod_twfe)
  
  png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Consumption.png",
      width = 800, height = 460)
  iplot(mod_twfe, 
        xlab = 'Time to treatment (Months)',
        ylab = 'Change in Total Ounces Sold',
        main = 'Event study: TWFE Model (Levels)')
  dev.off()
  
  #SF
  mod_twfe_sf = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                   |                    ## Other controls
                     store_zip3 + calendar_time,        ## FEs
                   cluster = "store_zip3",                       ## Clustered SEs
                   data = filter(EventStudy_EstimationData_Composite,
                                 !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
  summary(mod_twfe_sf)
  
  png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Consumption_SF.png",
      width = 800, height = 460)
  iplot(mod_twfe_sf, 
        xlab = 'Time to treatment (Months)',
        ylab = 'Change in Total Ounces Sold',
        main = 'Event study: TWFE Model (Levels)')
  dev.off()
  
  #Oakland
  mod_twfe_oakland = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                      |                    ## Other controls
                        store_zip3 + calendar_time,        ## FEs
                      cluster = "store_zip3",                       ## Clustered SEs
                      data = filter(EventStudy_EstimationData_Composite,
                                    !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
  summary(mod_twfe_oakland)
  
  png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Consumption_Oakland.png",
      width = 800, height = 460)
  iplot(mod_twfe_oakland, 
        xlab = 'Time to treatment (Months)',
        ylab = 'Change in Total Ounces Sold',
        main = 'Event study: TWFE Model (Levels)')
  dev.off()
  
  #Philadelphia
  mod_twfe_philadelphia = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                           |                    ## Other controls
                             store_zip3 + calendar_time,        ## FEs
                           cluster = "store_zip3",                       ## Clustered SEs
                           data = filter(EventStudy_EstimationData_Composite,
                                         !(store_zip3 == "946" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
  summary(mod_twfe_philadelphia)
  
  png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Consumption_Philadelphia.png",
      width = 800, height = 460)
  iplot(mod_twfe_philadelphia, 
        xlab = 'Time to treatment (Months)',
        ylab = 'Change in Total Ounces Sold',
        main = 'Event study: TWFE Model (Levels)')
  dev.off()
  
  #Boulder
  mod_twfe_boulder = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                                |                    ## Other controls
                                  store_zip3 + calendar_time,        ## FEs
                                cluster = "store_zip3",                       ## Clustered SEs
                                data = filter(EventStudy_EstimationData_Composite,
                                              !(store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "191" | store_zip3 == "981")))
  summary(mod_twfe_boulder)
  
  png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Consumption_Boulder.png",
      width = 800, height = 460)
  iplot(mod_twfe_boulder, 
        xlab = 'Time to treatment (Months)',
        ylab = 'Change in Total Ounces Sold',
        main = 'Event study: TWFE Model (Levels)')
  dev.off()
  
  #Seattle
  mod_twfe_seattle = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                           |                    ## Other controls
                             store_zip3 + calendar_time,        ## FEs
                           cluster = "store_zip3",                       ## Clustered SEs
                           data = filter(EventStudy_EstimationData_Composite,
                                         !(store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "191" | store_zip3 == "803")))
  summary(mod_twfe_seattle)
  
  png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Consumption_Seattle.png",
      width = 800, height = 460)
  iplot(mod_twfe_seattle, 
        xlab = 'Time to treatment (Months)',
        ylab = 'Change in Total Ounces Sold',
        main = 'Event study: TWFE Model (Levels)')
  dev.off()


#######GGPlot with added best-fit line through pre-period points#######
plot_coefficients <- function(coefficients, standard_errors, time_sequence, plot_name) {
  
  # # Example usage
  # coefficients <- summary(mod_twfe)$coefficients #save the coefficients
  # standard_errors <- summary(mod_twfe)$se #save the standard errors
  # # Generate a sequence from -72 to 37
  # time_sequence <- seq(-72, 37)
  # # Remove -1 from the sequence
  # time_sequence <- sequence[sequence != -1]
  
  # Create a data frame with the desired x and y values
  custom_point <- data.frame(x = -1, y = 0)
  
  # Generate 95% confidence intervals
  lower_bound <- coefficients - 1.96 * standard_errors
  upper_bound <- coefficients + 1.96 * standard_errors
  
  # Create a data frame for plotting
  data <- data.frame(
    time_period = time_sequence,
    coefficients = coefficients,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
  
  # Fit a best-fit line using coefficients from negative time periods
  # Here, we assume the first half of the coefficients are from negative time periods
  neg_time_data <- filter(data, time_period < 0)
  best_fit_line <- lm(coefficients ~ time_period, data = neg_time_data)
  best_fit_data <- data.frame(
    time_period = data$time_period,
    best_fit = predict(best_fit_line, newdata = data)
  )
  
  # Plot using ggplot
  specialplot <- ggplot(data, aes(x = time_period, y = coefficients)) +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.2) +
    geom_point(color = "blue") +
    geom_point(data = custom_point, aes(x=x, y=y), color = "blue") +
    #geom_line(data = best_fit_data, aes(y = best_fit), color = "purple", linetype = "solid", size = 1.5) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 1) +
    #ggtitle(paste0("TWFE Event Study Plot: ", plot_name)) +
    xlab("Event Time") +
    ylab("Change in Ounces Sold (1000s) by Zip Code-Month") +
    ggplot2::theme_minimal() +
    scale_x_continuous(breaks = seq(min(data$time_period, na.rm = TRUE), 
                                    max(data$time_period, na.rm = TRUE), by = 12)) +
    scale_y_continuous(labels = comma) +
    theme_bw() + 
    theme_classic() +
    theme(legend.title=element_blank(), 
          plot.title = element_text(hjust = 0.5), 
          text = element_text(size=38), 
          legend.key.height=unit(2,"line"),
          axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 
  specialplot
  
  ggsave(paste0("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Event Study Plots/TWFE_EventStudy_Modified_", plot_name, ".png"),
         width = 40, height = 23, units = "cm")
  
  return(specialplot)
}

##Composite Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-60, 25)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Composite Volume Purchases")

##SF Plot

  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_sf)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_sf)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-72, 25)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "San Francisco Volume Purchases")
  
##Oakland Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_oakland)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_oakland)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-66, 31)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Oakland Volume Purchases")

##Philadelphia Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_philadelphia)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_philadelphia)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-60, 37)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Philadelphia Volume Purchases")
  
##Boulder Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_boulder)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_boulder)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-66, 31)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Boulder Volume Purchases")
  
##Seattle Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_seattle)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_seattle)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-72, 25)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Seattle Volume Purchases")
  
  



#################################################################
###########################BORDERS###############################
#################################################################

TWFEEstimationData_Composite_Borders <- NielsenMovement_AllMonths_withCensusData_ZipCode

#Filtering out non-urban donor zip codes
TWFEEstimationData_Composite_Borders <- filter(TWFEEstimationData_Composite_Borders, perc_urban > MeanUrbanicity_Borders-SDUrbanicity | 
                                               store_zip3 %in% taxed_zips |
                                               store_zip3 %in% border_zips)

#Filtering out all taxed zip codes
TWFEEstimationData_Composite_Borders <- filter(TWFEEstimationData_Composite_Borders,
                                             !(store_zip3 %in% taxed_zips))

#Removing "treated" variable and renaming "treated_unit_border" to treated
TWFEEstimationData_Composite_Borders <- dplyr::select(TWFEEstimationData_Composite_Borders, -treated)
names(TWFEEstimationData_Composite_Borders)[names(TWFEEstimationData_Composite_Borders) == 'treated_unit_border'] <- 'treated'

#Adding logged total ounces as a variable
TWFEEstimationData_Composite_Borders <- mutate(TWFEEstimationData_Composite_Borders, log_TotalOz_zipcode = log(TotalOz_zipcode))

#Running a balanced estimation (for the composite)
TWFEEstimationData_Composite_Borders <- mutate(TWFEEstimationData_Composite_Borders, treat = ifelse(max(treated) > 0, 1, 0),
                                       calendar_time_new = treated*calendar_time*-1,
                                       calendar_time_new = ifelse(treat == 1 & calendar_time_new == 0, -999, calendar_time_new),
                                       time_to_treat = ifelse(calendar_time_new == -999, 0, -1*(calendar_time_new - max(calendar_time_new))),
                                       time_to_treat = ifelse(calendar_time_new == -999 & treat == 1, calendar_time, time_to_treat),
                                       time_to_treat_final = ifelse(calendar_time_new == -999 & treat == 1, time_to_treat-max(time_to_treat)-1, time_to_treat))

#Calculating pretreatment means for all border zip codes
PreTreatmentMeans_Borders <- filter(TWFEEstimationData_Composite_Borders, store_zip3 %in% border_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PreTreatmentMeans = mean(TotalOz_zipcode))


##Running DID regressions (LEVELS)

#Composite
DIDReg_FE_clustered_zip_Borders <- felm(TotalOz_zipcode ~ treated | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(TWFEEstimationData_Composite_Borders, time_to_treat_final >= -60 & time_to_treat_final <= 25))
summary(DIDReg_FE_clustered_zip_Borders)
TWFE_3 <- DIDReg_FE_clustered_zip_Borders

#SF
SFData <- TWFEEstimationData_Composite_Borders
SFData <- dplyr::select(SFData, -treated)
names(SFData)[names(SFData) == "treated_unit_border_SF"] <- "treated_SF"

DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_SF | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(SFData, 
                                              !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip)
TWFE_3_SF <- DIDReg_FE_clustered_zip

#Oakland
OakData <- TWFEEstimationData_Composite_Borders
OakData <- dplyr::select(OakData, -treated)
names(OakData)[names(OakData) == "treated_unit_border_Oakland"] <- "treated_Oakland"

DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Oakland | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(OakData, 
                                              !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip)
TWFE_3_Oakland <- DIDReg_FE_clustered_zip

#Philadelphia
PhillyData <- TWFEEstimationData_Composite_Borders
PhillyData <- dplyr::select(PhillyData, -treated)
names(PhillyData)[names(PhillyData) == "treated_unit_border_Philadelphia"] <- "treated_Philadelphia"

DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Philadelphia | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(PhillyData, 
                                              !(store_zip3 == "941" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip)
TWFE_3_Philadelphia <- DIDReg_FE_clustered_zip

#Boulder
BoulderData <- TWFEEstimationData_Composite_Borders
BoulderData <- dplyr::select(BoulderData, -treated)
names(BoulderData)[names(BoulderData) == "treated_unit_border_Boulder"] <- "treated_Boulder"

DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Boulder | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(BoulderData, 
                                              !(store_zip3 == "191" | store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip)
TWFE_3_Boulder <- DIDReg_FE_clustered_zip

#Seattle
SeattleData <- TWFEEstimationData_Composite_Borders
SeattleData <- dplyr::select(SeattleData, -treated)
names(SeattleData)[names(SeattleData) == "treated_unit_border_Seattle"] <- "treated_Seattle"

DIDReg_FE_clustered_zip <- felm(TotalOz_zipcode ~ treated_Seattle | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(SeattleData, 
                                              !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "941")))
summary(DIDReg_FE_clustered_zip)
TWFE_3_Seattle <- DIDReg_FE_clustered_zip



#########################################
#########EVENT STUDY ESTIMATION##########
#########################################

##Info from: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
EventStudy_EstimationData_Composite_Borders <- group_by(TWFEEstimationData_Composite_Borders, store_zip3) %>%
  mutate(treat = ifelse(max(treated) > 0, 1, 0),
         treat_SF_border = ifelse(max(treated_unit_border_SF) > 0, 1, 0),
         treat_Oakland_border = ifelse(max(treated_unit_border_Oakland) > 0, 1, 0),
         treat_Philadelphia_border = ifelse(max(treated_unit_border_Philadelphia) > 0, 1, 0),
         treat_Boulder_border = ifelse(max(treated_unit_border_Boulder) > 0, 1, 0),
         treat_Seattle_border = ifelse(max(treated_unit_border_Seattle) > 0, 1, 0),
         calendar_time_new = treated*calendar_time*-1,
         calendar_time_new = ifelse(treat == 1 & calendar_time_new == 0, -999, calendar_time_new),
         time_to_treat = ifelse(calendar_time_new == -999, 0, -1*(calendar_time_new - max(calendar_time_new))),
         time_to_treat = ifelse(calendar_time_new == -999 & treat == 1, calendar_time, time_to_treat),
         time_to_treat_final = ifelse(calendar_time_new == -999 & treat == 1, time_to_treat-max(time_to_treat)-1, time_to_treat))


##Event-Study (LEVELS)

#Composite
mod_twfe_borders = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                 |                    ## Other controls
                   store_zip3 + calendar_time,        ## FEs
                 cluster = "store_zip3",                       ## Clustered SEs
                 data = filter(EventStudy_EstimationData_Composite_Borders, time_to_treat_final >= -60 & time_to_treat_final <= 25))
summary(mod_twfe_borders)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Borders.png",
    width = 800, height = 460)
iplot(mod_twfe_borders, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Total Ounces Sold',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#SF
mod_twfe_sf = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat_SF_border, ref = -1) ## Our key interaction: time ? treatment status
                    |                    ## Other controls
                      store_zip3 + calendar_time,        ## FEs
                    cluster = "store_zip3",                       ## Clustered SEs
                    data = filter(EventStudy_EstimationData_Composite_Borders,
                                  !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
summary(mod_twfe_sf)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Borders_SF.png",
    width = 800, height = 460)
iplot(mod_twfe_sf, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Total Ounces Sold',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Oakland
mod_twfe_oakland = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat_Oakland_border, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Borders,
                                       !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
summary(mod_twfe_oakland)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Borders_Oakland.png",
    width = 800, height = 460)
iplot(mod_twfe_oakland, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Total Ounces Sold',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Philadelphia
mod_twfe_philadelphia = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat_Philadelphia_border, ref = -1) ## Our key interaction: time ? treatment status
                              |                    ## Other controls
                                store_zip3 + calendar_time,        ## FEs
                              cluster = "store_zip3",                       ## Clustered SEs
                              data = filter(EventStudy_EstimationData_Composite_Borders,
                                            !(store_zip3 == "946" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
summary(mod_twfe_philadelphia)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Borders_Philadelphia.png",
    width = 800, height = 460)
iplot(mod_twfe_philadelphia, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Total Ounces Sold',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Boulder
mod_twfe_boulder = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat_Boulder_border, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Borders,
                                       !(store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "191" | store_zip3 == "981")))
summary(mod_twfe_boulder)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Borders_Boulder.png",
    width = 800, height = 460)
iplot(mod_twfe_boulder, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Total Ounces Sold',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Seattle
mod_twfe_seattle = feols(TotalOz_zipcode ~ i(time_to_treat_final, treat_Seattle_border, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Borders,
                                       !(store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "191" | store_zip3 == "803")))
summary(mod_twfe_seattle)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Borders_Seattle.png",
    width = 800, height = 460)
iplot(mod_twfe_seattle, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Total Ounces Sold',
      main = 'Event study: TWFE Model (Levels)')
dev.off()


##Composite Plot

  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_borders)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_borders)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-60, 25)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Composite Border Volume Purchases")
  
##SF Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_sf)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_sf)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-72, 25)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "San Francisco Border Volume Purchases")

##Oakland Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_oakland)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_oakland)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-66, 31)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Oakland Border Volume Purchases")
  
##Philadelphia Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_philadelphia)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_philadelphia)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-60, 37)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Philadelphia Border Volume Purchases")

##Boulder Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_boulder)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_boulder)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-66, 31)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Boulder Border Volume Purchases")

##Seattle Plot
  
  # Getting the Coefficients and standard errors
  coefficients <- summary(mod_twfe_seattle)$coefficients/1000 #save the coefficients
  standard_errors <- summary(mod_twfe_seattle)$se/1000 #save the standard errors
  # Generate a sequence based on observed time for this estimation
  time_sequence <- seq(-72, 25)
  # Remove -1 from the sequence
  time_sequence <- time_sequence[time_sequence != -1]
  
  # Generating the plot
  plot_coefficients(coefficients, standard_errors, time_sequence, "Seattle Border Volume Purchases")  
  



################################################################
###########################PRICES###############################
################################################################

TWFEEstimationData_Composite_Prices <- NielsenMovement_AllMonths_withCensusData_ZipCode_Prices

#Filtering out non-urban donor zip codes
TWFEEstimationData_Composite_Prices <- filter(TWFEEstimationData_Composite_Prices, perc_urban > MeanUrbanicity_Treated-SDUrbanicity | 
                                              store_zip3 %in% taxed_zips |
                                              store_zip3 %in% border_zips)

#Filtering out all border zip codes
TWFEEstimationData_Composite_Prices <- filter(TWFEEstimationData_Composite_Prices,
                                            !(store_zip3 %in% border_zips))


#Adding logged total ounces as a variable
TWFEEstimationData_Composite_Prices <- mutate(TWFEEstimationData_Composite_Prices, log_AvgPrice_PerOz_zipcode = log(AvgPrice_PerOz_zipcode))

#Running a balanced estimation (for the composite)
TWFEEstimationData_Composite_Prices <- mutate(TWFEEstimationData_Composite_Prices, treat = ifelse(max(treated) > 0, 1, 0),
                                       calendar_time_new = treated*calendar_time*-1,
                                       calendar_time_new = ifelse(treat == 1 & calendar_time_new == 0, -999, calendar_time_new),
                                       time_to_treat = ifelse(calendar_time_new == -999, 0, -1*(calendar_time_new - max(calendar_time_new))),
                                       time_to_treat = ifelse(calendar_time_new == -999 & treat == 1, calendar_time, time_to_treat),
                                       time_to_treat_final = ifelse(calendar_time_new == -999 & treat == 1, time_to_treat-max(time_to_treat)-1, time_to_treat))


#Calculating pretreatment means for all taxed zip codes
PreTreatmentMeans_Taxed_Prices <- filter(TWFEEstimationData_Composite_Prices, store_zip3 %in% taxed_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PreTreatmentMeans = mean(AvgPrice_PerOz_zipcode))


##Running DID regressions (LEVELS)

#Composite
DIDReg_FE_clustered_zip_Prices <- felm(AvgPrice_PerOz_zipcode ~ treated | calendar_time + store_zip3 | 0 | store_zip3, 
                                        data = filter(TWFEEstimationData_Composite_Prices, time_to_treat_final >= -60 & time_to_treat_final <= 25))
summary(DIDReg_FE_clustered_zip_Prices)
TWFE_5 <- DIDReg_FE_clustered_zip_Prices

#SF
SFData <-TWFEEstimationData_Composite_Prices
names(SFData)[names(SFData) == "treated"] <- "treated_SF"

DIDReg_FE_clustered_zip_Prices <- felm(AvgPrice_PerOz_zipcode ~ treated_SF | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(SFData, 
                                              !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip_Prices)
TWFE_5_SF <- DIDReg_FE_clustered_zip_Prices

#Oakland
OakData <-TWFEEstimationData_Composite_Prices
names(OakData)[names(OakData) == "treated"] <- "treated_Oakland"

DIDReg_FE_clustered_zip_Prices <- felm(AvgPrice_PerOz_zipcode ~ treated_Oakland | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(OakData, 
                                              !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip_Prices)
TWFE_5_Oakland <- DIDReg_FE_clustered_zip_Prices

#Philadelphia
PhillyData <-TWFEEstimationData_Composite_Prices
names(PhillyData)[names(PhillyData) == "treated"] <- "treated_Philadelphia"

DIDReg_FE_clustered_zip_Prices <- felm(AvgPrice_PerOz_zipcode ~ treated_Philadelphia | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(PhillyData, 
                                              !(store_zip3 == "941" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip_Prices)
TWFE_5_Philadelphia <- DIDReg_FE_clustered_zip_Prices

#Boulder
BoulderData <-TWFEEstimationData_Composite_Prices
names(BoulderData)[names(BoulderData) == "treated"] <- "treated_Boulder"

DIDReg_FE_clustered_zip_Prices <- felm(AvgPrice_PerOz_zipcode ~ treated_Boulder | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(BoulderData, 
                                              !(store_zip3 == "191" | store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "981")))
summary(DIDReg_FE_clustered_zip_Prices)
TWFE_5_Boulder <- DIDReg_FE_clustered_zip_Prices

#Seattle
SeattleData <-TWFEEstimationData_Composite_Prices
names(SeattleData)[names(SeattleData) == "treated"] <- "treated_Seattle"

DIDReg_FE_clustered_zip_Prices <- felm(AvgPrice_PerOz_zipcode ~ treated_Seattle | calendar_time + store_zip3 | 0 | store_zip3, 
                                data = filter(SeattleData, 
                                              !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "941")))
summary(DIDReg_FE_clustered_zip_Prices)
TWFE_5_Seattle <- DIDReg_FE_clustered_zip_Prices



#########################################
#########EVENT STUDY ESTIMATION##########
#########################################

##Info from: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
EventStudy_EstimationData_Composite_Prices <- group_by(TWFEEstimationData_Composite_Prices, store_zip3) %>%
  mutate(treat = ifelse(max(treated) > 0, 1, 0),
         calendar_time_new = treated*calendar_time*-1,
         calendar_time_new = ifelse(treat == 1 & calendar_time_new == 0, -999, calendar_time_new),
         time_to_treat = ifelse(calendar_time_new == -999, 0, -1*(calendar_time_new - max(calendar_time_new))),
         time_to_treat = ifelse(calendar_time_new == -999 & treat == 1, calendar_time, time_to_treat),
         time_to_treat_final = ifelse(calendar_time_new == -999 & treat == 1, time_to_treat-max(time_to_treat)-1, time_to_treat))


##Event-Study (LEVELS)

#Composite
mod_twfe_prices = feols(AvgPrice_PerOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Prices, time_to_treat_final >= -60 & time_to_treat_final <= 25))
summary(mod_twfe_prices)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Prices.png",
    width = 800, height = 460)
iplot(mod_twfe_prices, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Avg. Price per Oz.',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#SF
mod_twfe_sf_prices = feols(AvgPrice_PerOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                    |                    ## Other controls
                      store_zip3 + calendar_time,        ## FEs
                    cluster = "store_zip3",                       ## Clustered SEs
                    data = filter(EventStudy_EstimationData_Composite_Prices,
                                  !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "946" | store_zip3 == "981")))
summary(mod_twfe_sf_prices)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Prices_SF.png",
    width = 800, height = 460)
iplot(mod_twfe_sf_prices, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Avg. Price per Oz.',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Oakland
mod_twfe_oakland_prices = feols(AvgPrice_PerOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Prices,
                                       !(store_zip3 == "191" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
summary(mod_twfe_oakland_prices)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Prices_Oakland.png",
    width = 800, height = 460)
iplot(mod_twfe_oakland_prices, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Avg. Price per Oz.',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Philadelphia
mod_twfe_philadelphia_prices = feols(AvgPrice_PerOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                              |                    ## Other controls
                                store_zip3 + calendar_time,        ## FEs
                              cluster = "store_zip3",                       ## Clustered SEs
                              data = filter(EventStudy_EstimationData_Composite_Prices,
                                            !(store_zip3 == "946" | store_zip3 == "803" | store_zip3 == "941" | store_zip3 == "981")))
summary(mod_twfe_philadelphia_prices)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Prices_Philadelphia.png",
    width = 800, height = 460)
iplot(mod_twfe_philadelphia_prices, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Avg. Price per Oz.',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Boulder
mod_twfe_boulder_prices = feols(AvgPrice_PerOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Prices,
                                       !(store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "191" | store_zip3 == "981")))
summary(mod_twfe_boulder_prices)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Prices_Boulder.png",
    width = 800, height = 460)
iplot(mod_twfe_boulder_prices, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Avg. Price per Oz.',
      main = 'Event study: TWFE Model (Levels)')
dev.off()

#Seattle
mod_twfe_seattle_prices = feols(AvgPrice_PerOz_zipcode ~ i(time_to_treat_final, treat, ref = -1) ## Our key interaction: time ? treatment status
                         |                    ## Other controls
                           store_zip3 + calendar_time,        ## FEs
                         cluster = "store_zip3",                       ## Clustered SEs
                         data = filter(EventStudy_EstimationData_Composite_Prices,
                                       !(store_zip3 == "941" | store_zip3 == "946" | store_zip3 == "191" | store_zip3 == "803")))
summary(mod_twfe_seattle_prices)

png("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/TWFE_EventStudy_Prices_Seattle.png",
    width = 800, height = 460)
iplot(mod_twfe_seattle_prices, 
      xlab = 'Time to treatment (Months)',
      ylab = 'Change in Avg. Price per Oz.',
      main = 'Event study: TWFE Model (Levels)')
dev.off()


#######GGPlot with added best-fit line through pre-period points#######
plot_coefficients_prices <- function(coefficients, standard_errors, time_sequence, plot_name) {
  
  # # Example usage
  # coefficients <- summary(mod_twfe)$coefficients #save the coefficients
  # standard_errors <- summary(mod_twfe)$se #save the standard errors
  # # Generate a sequence from -72 to 37
  # time_sequence <- seq(-72, 37)
  # # Remove -1 from the sequence
  # time_sequence <- sequence[sequence != -1]
  
  # Create a data frame with the desired x and y values
  custom_point <- data.frame(x = -1, y = 0)
  
  # Generate 95% confidence intervals
  lower_bound <- coefficients - 1.96 * standard_errors
  upper_bound <- coefficients + 1.96 * standard_errors
  
  # Create a data frame for plotting
  data <- data.frame(
    time_period = time_sequence,
    coefficients = coefficients,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
  
  # Fit a best-fit line using coefficients from negative time periods
  # Here, we assume the first half of the coefficients are from negative time periods
  neg_time_data <- filter(data, time_period < 0)
  best_fit_line <- lm(coefficients ~ time_period, data = neg_time_data)
  best_fit_data <- data.frame(
    time_period = data$time_period,
    best_fit = predict(best_fit_line, newdata = data)
  )
  
  # Plot using ggplot
  specialplot <- ggplot(data, aes(x = time_period, y = coefficients)) +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.2) +
    geom_point(color = "blue") +
    geom_point(data = custom_point, aes(x=x, y=y), color = "blue") +
    #geom_line(data = best_fit_data, aes(y = best_fit), color = "purple", linetype = "solid", size = 1.5) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 1) +
    #ggtitle(paste0("TWFE Event Study Plot: ", plot_name)) +
    xlab("Event Time") +
    ylab("Change in Avg. Price per Oz. ($) by Zip Code-Month") +
    ggplot2::theme_minimal() +
    scale_x_continuous(breaks = seq(min(data$time_period, na.rm = TRUE), 
                                    max(data$time_period, na.rm = TRUE), by = 12)) +
    scale_y_continuous(labels = comma) +
    theme_bw() + 
    theme_classic() +
    theme(legend.title=element_blank(), 
          plot.title = element_text(hjust = 0.5), 
          text = element_text(size=38), 
          legend.key.height=unit(2,"line"),
          axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 
  specialplot
  
  ggsave(paste0("C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Event Study Plots/TWFE_EventStudy_Modified_", plot_name, ".png"),
         width = 40, height = 23, units = "cm")
  
  return(specialplot)
}

##Composite Plot

# Getting the Coefficients and standard errors
coefficients <- summary(mod_twfe_prices)$coefficients #save the coefficients
standard_errors <- summary(mod_twfe_prices)$se #save the standard errors
# Generate a sequence based on observed time for this estimation
time_sequence <- seq(-60, 25)
# Remove -1 from the sequence
time_sequence <- time_sequence[time_sequence != -1]

# Generating the plot
plot_coefficients_prices(coefficients, standard_errors, time_sequence, "Composite Shelf Prices")

##SF Plot

# Getting the Coefficients and standard errors
coefficients <- summary(mod_twfe_sf_prices)$coefficients #save the coefficients
standard_errors <- summary(mod_twfe_sf_prices)$se #save the standard errors
# Generate a sequence based on observed time for this estimation
time_sequence <- seq(-72, 25)
# Remove -1 from the sequence
time_sequence <- time_sequence[time_sequence != -1]

# Generating the plot
plot_coefficients_prices(coefficients, standard_errors, time_sequence, "San Francisco Shelf Prices")

##Oakland Plot

# Getting the Coefficients and standard errors
coefficients <- summary(mod_twfe_oakland_prices)$coefficients #save the coefficients
standard_errors <- summary(mod_twfe_oakland_prices)$se #save the standard errors
# Generate a sequence based on observed time for this estimation
time_sequence <- seq(-66, 31)
# Remove -1 from the sequence
time_sequence <- time_sequence[time_sequence != -1]

# Generating the plot
plot_coefficients_prices(coefficients, standard_errors, time_sequence, "Oakland Shelf Prices")

##Philadelphia Plot

# Getting the Coefficients and standard errors
coefficients <- summary(mod_twfe_philadelphia_prices)$coefficients #save the coefficients
standard_errors <- summary(mod_twfe_philadelphia_prices)$se #save the standard errors
# Generate a sequence based on observed time for this estimation
time_sequence <- seq(-60, 37)
# Remove -1 from the sequence
time_sequence <- time_sequence[time_sequence != -1]

# Generating the plot
plot_coefficients_prices(coefficients, standard_errors, time_sequence, "Philadelphia Shelf Prices")

##Boulder Plot

# Getting the Coefficients and standard errors
coefficients <- summary(mod_twfe_boulder_prices)$coefficients #save the coefficients
standard_errors <- summary(mod_twfe_boulder_prices)$se #save the standard errors
# Generate a sequence based on observed time for this estimation
time_sequence <- seq(-66, 31)
# Remove -1 from the sequence
time_sequence <- time_sequence[time_sequence != -1]

# Generating the plot
plot_coefficients_prices(coefficients, standard_errors, time_sequence, "Boulder Shelf Prices")

##Seattle Plot

# Getting the Coefficients and standard errors
coefficients <- summary(mod_twfe_seattle_prices)$coefficients #save the coefficients
standard_errors <- summary(mod_twfe_seattle_prices)$se #save the standard errors
# Generate a sequence based on observed time for this estimation
time_sequence <- seq(-72, 25)
# Remove -1 from the sequence
time_sequence <- time_sequence[time_sequence != -1]

# Generating the plot
plot_coefficients_prices(coefficients, standard_errors, time_sequence, "Seattle Shelf Prices")




#####################################################################
###########################TWFE TABLES###############################
#####################################################################


##Formatting a stargazer table

#Composite
stargazer(TWFE_1, TWFE_5, TWFE_3, 
          title="Composite Impact of SSB Taxes (Levels)",
          #dep.var.caption = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          #align = TRUE,
          omit.stat=c("f", "ser", "adj.rsq"), dep.var.labels.include = FALSE,
          add.lines = list(c("Dep. Var. Pretreatment Mean",  
                             round(mean(PreTreatmentMeans_Taxed$PreTreatmentMeans), 0), 
                             round(mean(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans), 3),
                             round(mean(PreTreatmentMeans_Borders$PreTreatmentMeans), 0)),
                           c("Percent Change",  
                             round(100*TWFE_1$coefficients[1]/mean(PreTreatmentMeans_Taxed$PreTreatmentMeans), 2), 
                             round(100*TWFE_5$coefficients[1]/mean(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans), 2),
                             round(100*TWFE_3$coefficients[1]/mean(PreTreatmentMeans_Borders$PreTreatmentMeans), 2)),
                           c("Month-Year FE", "X", "X", "X"),
                           c("Zip Code FE", "X", "X", "X"),
                           c("Clustered Robust SEs (Zip Code)", "X", "X", "X")),
          column.labels = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          covariate.labels = c("Treatment * Post"),
          font.size="small",
          star.cutoffs = c(.05, .01, .001),
          digits = 4,
          table.placement = "!h",
          #column.sep.width = "-5pt",
          model.numbers=FALSE,
          label="TWFE_Levels",
          out="C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Tables/TWFETable_Levels.html")

#San Francisco
stargazer(TWFE_1_SF, TWFE_5_SF, TWFE_3_SF, 
          title="San Francisco Impact of SSB Taxes (Levels)",
          #dep.var.caption = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          #align = TRUE,
          omit.stat=c("f", "ser", "adj.rsq"), dep.var.labels.include = FALSE,
          add.lines = list(c("Dep. Var. Pretreatment Mean",  
                             round(PreTreatmentMeans_Taxed$PreTreatmentMeans[3], 0), 
                             round(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[3], 3),
                             round(PreTreatmentMeans_Borders$PreTreatmentMeans[3], 0)),
                           c("Percent Change",  
                             round(100*TWFE_1_SF$coefficients[1]/PreTreatmentMeans_Taxed$PreTreatmentMeans[3], 2), 
                             round(100*TWFE_5_SF$coefficients[1]/PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[3], 2),
                             round(100*TWFE_3_SF$coefficients[1]/PreTreatmentMeans_Borders$PreTreatmentMeans[3], 2)),
                           c("Month-Year FE", "X", "X", "X"),
                           c("Zip Code FE", "X", "X", "X"),
                           c("Clustered Robust SEs (Zip Code)", "X", "X", "X")),
          column.labels = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          covariate.labels = c("Treatment * Post"),
          font.size="small",
          star.cutoffs = c(.05, .01, .001),
          digits = 4,
          table.placement = "!h",
          #column.sep.width = "-5pt",
          model.numbers=FALSE,
          label="TWFE_Levels_SF",
          out="C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Tables/TWFETable_Levels_SF.html")

#Oakland
stargazer(TWFE_1_Oakland, TWFE_5_Oakland, TWFE_3_Oakland, 
          title="Oakland Impact of SSB Taxes (Levels)",
          #dep.var.caption = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          #align = TRUE,
          omit.stat=c("f", "ser", "adj.rsq"), dep.var.labels.include = FALSE,
          add.lines = list(c("Dep. Var. Pretreatment Mean",  
                             round(PreTreatmentMeans_Taxed$PreTreatmentMeans[4], 0), 
                             round(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[4], 3),
                             round(PreTreatmentMeans_Borders$PreTreatmentMeans[4], 0)),
                           c("Percent Change",  
                             round(100*TWFE_1_Oakland$coefficients[1]/PreTreatmentMeans_Taxed$PreTreatmentMeans[4], 2), 
                             round(100*TWFE_5_Oakland$coefficients[1]/PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[4], 2),
                             round(100*TWFE_3_Oakland$coefficients[1]/PreTreatmentMeans_Borders$PreTreatmentMeans[4], 2)),
                           c("Month-Year FE", "X", "X", "X"),
                           c("Zip Code FE", "X", "X", "X"),
                           c("Clustered Robust SEs (Zip Code)", "X", "X", "X")),
          column.labels = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          covariate.labels = c("Treatment * Post"),
          font.size="small",
          star.cutoffs = c(.05, .01, .001),
          digits = 4,
          table.placement = "!h",
          #column.sep.width = "-5pt",
          model.numbers=FALSE,
          label="TWFE_Levels_Oakland",
          out="C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Tables/TWFETable_Levels_Oakland.html")

#Philadelphia
stargazer(TWFE_1_Philadelphia, TWFE_5_Philadelphia, TWFE_3_Philadelphia, 
          title="Philadelphia Impact of SSB Taxes (Levels)",
          #dep.var.caption = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          #align = TRUE,
          omit.stat=c("f", "ser", "adj.rsq"), dep.var.labels.include = FALSE,
          add.lines = list(c("Dep. Var. Pretreatment Mean",  
                             round(PreTreatmentMeans_Taxed$PreTreatmentMeans[1], 0), 
                             round(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[1], 3),
                             round(PreTreatmentMeans_Borders$PreTreatmentMeans[1], 0)),
                           c("Percent Change",  
                             round(100*TWFE_1_Philadelphia$coefficients[1]/PreTreatmentMeans_Taxed$PreTreatmentMeans[1], 2), 
                             round(100*TWFE_5_Philadelphia$coefficients[1]/PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[1], 2),
                             round(100*TWFE_3_Philadelphia$coefficients[1]/PreTreatmentMeans_Borders$PreTreatmentMeans[1], 2)),
                           c("Month-Year FE", "X", "X", "X"),
                           c("Zip Code FE", "X", "X", "X"),
                           c("Clustered Robust SEs (Zip Code)", "X", "X", "X")),
          column.labels = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          covariate.labels = c("Treatment * Post"),
          font.size="small",
          star.cutoffs = c(.05, .01, .001),
          digits = 4,
          table.placement = "!h",
          #column.sep.width = "-5pt",
          model.numbers=FALSE,
          label="TWFE_Levels_Philadelphia",
          out="C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Tables/TWFETable_Levels_Philadelphia.html")

#Boulder
stargazer(TWFE_1_Boulder, TWFE_5_Boulder, TWFE_3_Boulder, 
          title="Boulder Impact of SSB Taxes (Levels)",
          #dep.var.caption = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          #align = TRUE,
          omit.stat=c("f", "ser", "adj.rsq"), dep.var.labels.include = FALSE,
          add.lines = list(c("Dep. Var. Pretreatment Mean",  
                             round(PreTreatmentMeans_Taxed$PreTreatmentMeans[2], 0), 
                             round(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[2], 3),
                             round(PreTreatmentMeans_Borders$PreTreatmentMeans[2], 0)),
                           c("Percent Change",  
                             round(100*TWFE_1_Boulder$coefficients[1]/PreTreatmentMeans_Taxed$PreTreatmentMeans[2], 2), 
                             round(100*TWFE_5_Boulder$coefficients[1]/PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[2], 2),
                             round(100*TWFE_3_Boulder$coefficients[1]/PreTreatmentMeans_Borders$PreTreatmentMeans[2], 2)),
                           c("Month-Year FE", "X", "X", "X"),
                           c("Zip Code FE", "X", "X", "X"),
                           c("Clustered Robust SEs (Zip Code)", "X", "X", "X")),
          column.labels = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          covariate.labels = c("Treatment * Post"),
          font.size="small",
          star.cutoffs = c(.05, .01, .001),
          digits = 4,
          table.placement = "!h",
          #column.sep.width = "-5pt",
          model.numbers=FALSE,
          label="TWFE_Levels_Boulder",
          out="C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Tables/TWFETable_Levels_Boulder.html")

#Seattle
stargazer(TWFE_1_Seattle, TWFE_5_Seattle, TWFE_3_Seattle, 
          title="Seattle Impact of SSB Taxes (Levels)",
          #dep.var.caption = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          #align = TRUE,
          omit.stat=c("f", "ser", "adj.rsq"), dep.var.labels.include = FALSE,
          add.lines = list(c("Dep. Var. Pretreatment Mean",  
                             round(PreTreatmentMeans_Taxed$PreTreatmentMeans[5], 0), 
                             round(PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[5], 3),
                             round(PreTreatmentMeans_Borders$PreTreatmentMeans[5], 0)),
                           c("Percent Change",  
                             round(100*TWFE_1_Seattle$coefficients[1]/PreTreatmentMeans_Taxed$PreTreatmentMeans[5], 2), 
                             round(100*TWFE_5_Seattle$coefficients[1]/PreTreatmentMeans_Taxed_Prices$PreTreatmentMeans[5], 2),
                             round(100*TWFE_3_Seattle$coefficients[1]/PreTreatmentMeans_Borders$PreTreatmentMeans[5], 2)),
                           c("Month-Year FE", "X", "X", "X"),
                           c("Zip Code FE", "X", "X", "X"),
                           c("Clustered Robust SEs (Zip Code)", "X", "X", "X")),
          column.labels = c("Total Oz.", "Avg. Price per Oz.", "Total Oz."),
          covariate.labels = c("Treatment * Post"),
          font.size="small",
          star.cutoffs = c(.05, .01, .001),
          digits = 4,
          table.placement = "!h",
          #column.sep.width = "-5pt",
          model.numbers=FALSE,
          label="TWFE_Levels_Seattle",
          out="C:/Users/skaplan/Backed Up Data/SSB Taxes/Figures/Results/TWFE Results/Final Tables/TWFETable_Levels_Seattle.html")



