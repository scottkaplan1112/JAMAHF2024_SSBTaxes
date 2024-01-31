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
library(haven)
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
setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis")

##Toggle to drop immediately bordering zip codes in synthetic control estimation
DropBorderingZips <- 1

##Toggle to drop post-COVID data
DropCovid <- 1

##Toggle to just do treated zip codes + those starting with 9 (=0 if just 9, =1 if all)
Zips_All <- 1

##Toggle to 1 if running synthetic control analyses for immediate bordering zip codes
Border_Analysis <- 0


#########################################################################
##############################QUANTITIES#################################
#########################################################################


################################################
###############Reading in Data##################
################################################

##Full Nielsen Movement Data
MonthLevel_2012 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2012_SSBs_MonthLevel_SCEstimation.csv",
                         colClasses = c(store_code_uc = "character",
                                        month_year = "character",
                                        upc10 = "character",
                                        store_zip3 = "character"))
MonthLevel_2013_2014 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2013_2014_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))
MonthLevel_2015_2016 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2015_2016_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))
MonthLevel_2017_2018 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2017_2018_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))
MonthLevel_2019_2020 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2019_2020_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))

NielsenMovement_All_months <- rbind(MonthLevel_2012, MonthLevel_2013_2014, 
                                    MonthLevel_2015_2016, MonthLevel_2017_2018, MonthLevel_2019_2020)
NielsenMovement_All_months <- dplyr::select(NielsenMovement_All_months, month_year, store_code_uc,
                                            store_zip3, upc10, SF, TotalOz, time_to_treat_months_SanFrancisco,
                                            time_to_treat_months_DC, time_to_treat_months_Seattle,
                                            time_to_treat_months_Philadelphia, time_to_treat_months_Boulder,
                                            time_to_treat_months_Berkeley, time_to_treat_months_Oakland)
rm(MonthLevel_2012)
rm(MonthLevel_2013_2014)
rm(MonthLevel_2015_2016)
rm(MonthLevel_2017_2018)
rm(MonthLevel_2019_2020)

gc()

##Removing any row with an NA
NielsenMovement_All_months <- NielsenMovement_All_months[complete.cases(NielsenMovement_All_months),]

##Removing any row with a "0" or negative value for "TotalOz"
NielsenMovement_All_months <- filter(NielsenMovement_All_months, !(TotalOz <= 0))


##Reading in the store and census data
Store_Census_Data <- fread("Processed Data/Final Nielsen Files/Store_Census_Combined.csv",
                           colClasses = c(store_code_uc = "character",
                                          parent_code = "character",
                                          retailer_code = "character",
                                          store_zip3 = "character",
                                          fips_state_code = "character",
                                          fips_county_code = "character",
                                          dma_code = "character"))

##Creating an SF indicator variable (was done just for descriptives)
Store_Census_Data <- mutate(Store_Census_Data,
                            SF = ifelse(store_zip3 == "941", "SF", "Not SF"))

##Subsetting to store data needed for synthetic control
Store_Census_Data_forMerge <- dplyr::select(Store_Census_Data, store_code_uc, medHHincome, population, medage,
                                            num_housingunits, pwhite, pblack, pamindalask, pasian, phawaii, pother,
                                            phisp, poverty_10K, age_18to64)


###############################################
###############Combining Data##################
###############################################

NielsenMovement_AllMonths_withCensusData <- left_join(NielsenMovement_All_months, Store_Census_Data_forMerge,
                                                      by = c("store_code_uc" = "store_code_uc"))

##Removing large Nielsen dataset
rm(NielsenMovement_All_months)

if(Zips_All == 0){
  
  if(Border_Analysis == 0){
    ##Filtering to just zip codes starting with 9 to start + zip codes for other treated areas
    NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData,
                                                       store_zip1 = as.integer(substr(store_zip3, 1, 1))) %>%
      filter(store_zip3 == "941" | store_zip3 == "945" | store_zip3 == "946" | 
               store_zip3 == "947" | store_zip3 == "200" | store_zip3 == "202" | 
               store_zip3 == "203" | store_zip3 == "204" | store_zip3 == "205" | store_zip3 == "803" | 
               store_zip3 == "190" | store_zip3 == "191" | store_zip3 == "192" | store_zip3 == "981" | 
               store_zip1 == 9 | store_zip1 == 8 | store_zip1 == 1 | store_zip1 == 0)
  }
  
  if(Border_Analysis == 1){
    ##Filtering to just zip codes starting with 9 to start + zip codes for other treated areas
    NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData,
                                                       store_zip1 = as.integer(substr(store_zip3, 1, 1))) %>%
      filter(store_zip3 == "941" | store_zip3 == "945" | store_zip3 == "946" | 
               store_zip3 == "947" | store_zip3 == "200" | store_zip3 == "202" | 
               store_zip3 == "203" | store_zip3 == "204" | store_zip3 == "205" | store_zip3 == "803" | 
               store_zip3 == "190" | store_zip3 == "191" | store_zip3 == "192" | store_zip3 == "981" | 
               store_zip3 == "800" | store_zip3 == "804" | store_zip3 == "805" | store_zip3 == "080" |
               store_zip3 == "081" | store_zip3 == "222" | store_zip3 == "223" | store_zip3 == "201" |
               store_zip3 == "220" | store_zip3 == "216" | store_zip3 == "218" | store_zip1 == 9)
  }
}

if(Zips_All == 1){
  ##Filtering to all zip codes
  NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData,
                                                     store_zip1 = as.integer(substr(store_zip3, 1, 1))) %>%
    filter(store_zip3 == "941" | store_zip3 == "945" | store_zip3 == "946" | 
             store_zip3 == "947" | store_zip3 == "200" | store_zip3 == "202" | 
             store_zip3 == "203" | store_zip3 == "204" | store_zip3 == "205" | store_zip3 == "803" | 
             store_zip3 == "190" | store_zip3 == "191" | store_zip3 == "192" | store_zip3 == "981" | 
             store_zip1 == 9 | store_zip1 == 8 | store_zip1 == 7 | store_zip1 == 6 | store_zip1 == 5 |
             store_zip1 == 4 | store_zip1 == 3 | store_zip1 == 2 | store_zip1 == 1 | store_zip1 == 0)
}


##Seeing how many time periods each zip code has and dropping incomplete zips
NumMonths_perZip <- group_by(NielsenMovement_AllMonths_withCensusData, store_zip3) %>%
  summarise(NumMonths = length(unique(time_to_treat_months_SanFrancisco))) %>%
  filter(NumMonths >= 108) #Zips removed: 202, 203, 204 (not there in first place), 205, 968, 996, 999

##Filtering to zip codes with all months
NielsenMovement_AllMonths_withCensusData <- filter(NielsenMovement_AllMonths_withCensusData, 
                                                   store_zip3 %in% NumMonths_perZip$store_zip3)

##Dropping COVID months
if(DropCovid == 1){
  NielsenMovement_AllMonths_withCensusData <- filter(NielsenMovement_AllMonths_withCensusData,
                                                     month_year != "2020-03-01" &
                                                       month_year != "2020-04-01" & 
                                                       month_year != "2020-05-01" & 
                                                       month_year != "2020-06-01" & 
                                                       month_year != "2020-07-01" & 
                                                       month_year != "2020-08-01" & 
                                                       month_year != "2020-09-01" & 
                                                       month_year != "2020-10-01" & 
                                                       month_year != "2020-11-01" & 
                                                       month_year != "2020-12-01")
}


##Taking means at the zip code - month level
NielsenMovement_AllMonths_withCensusData_ZipCode <- group_by(NielsenMovement_AllMonths_withCensusData,
                                                             store_zip3, time_to_treat_months_SanFrancisco,
                                                             time_to_treat_months_DC, time_to_treat_months_Seattle,
                                                             time_to_treat_months_Philadelphia, time_to_treat_months_Boulder,
                                                             time_to_treat_months_Berkeley, time_to_treat_months_Oakland,
                                                             medHHincome, population, medage,
                                                             num_housingunits, pwhite, pblack, pamindalask,
                                                             pasian, phawaii, pother,
                                                             phisp, poverty_10K, age_18to64) %>%
  summarise(TotalOz_zipcode = sum(TotalOz)) %>%
  ungroup()

#Removing large Nielsen dataset (that includes census data)
rm(NielsenMovement_AllMonths_withCensusData)

##Grouping all zip codes for a given treatment area into a single zip code (FIGURE OUT WHICH ZIPS ARE FULLY TREATED)

#Just use 191 for Philly (drop 190 and 192)
NielsenMovement_AllMonths_withCensusData_ZipCode <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           store_zip3 != "190" & store_zip3 != "192")

##Removing any row with an NA
NielsenMovement_AllMonths_withCensusData_ZipCode <- NielsenMovement_AllMonths_withCensusData_ZipCode[complete.cases(NielsenMovement_AllMonths_withCensusData_ZipCode),]

##Removing any zip code with an "Inf" for one of the covariates
NielsenMovement_AllMonths_withCensusData_ZipCode <- filter_all(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                               all_vars(!is.infinite(.)))

#Testing for duplicate rows - if 0, then good to go!
ex <- group_by(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3, time_to_treat_months_SanFrancisco) %>%
  filter(n()>1)


#################################################################
######Outputting Zip code level data for quicker analyses########
#################################################################

fwrite(NielsenMovement_AllMonths_withCensusData_ZipCode, 
       "Processed Data/Final Nielsen Files/NielsenMovement_AllMonths_withCensusData_ZipCode.csv")





##########################################################################
################################PRICES####################################
##########################################################################


################################################
###############Reading in Data##################
################################################

##Full Nielsen Movement Data
MonthLevel_2012 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2012_SSBs_MonthLevel_SCEstimation.csv",
                         colClasses = c(store_code_uc = "character",
                                        month_year = "character",
                                        upc10 = "character",
                                        store_zip3 = "character"))
MonthLevel_2013_2014 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2013_2014_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))
MonthLevel_2015_2016 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2015_2016_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))
MonthLevel_2017_2018 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2017_2018_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))
MonthLevel_2019_2020 <- fread("Processed Data/Final Nielsen Files/NielsenMovement_2019_2020_SSBs_MonthLevel_SCEstimation.csv",
                              colClasses = c(store_code_uc = "character",
                                             month_year = "character",
                                             upc10 = "character",
                                             store_zip3 = "character"))

NielsenMovement_All_months <- rbind(MonthLevel_2012, MonthLevel_2013_2014, 
                                    MonthLevel_2015_2016, MonthLevel_2017_2018, MonthLevel_2019_2020)
NielsenMovement_All_months <- dplyr::select(NielsenMovement_All_months, month_year, store_code_uc,
                                            store_zip3, upc10, TotalUnits, TotalOz, AvgPrice_perUnit, 
                                            size_per_unit, time_to_treat_months_SanFrancisco,
                                            time_to_treat_months_DC, time_to_treat_months_Seattle,
                                            time_to_treat_months_Philadelphia, time_to_treat_months_Boulder,
                                            time_to_treat_months_Berkeley, time_to_treat_months_Oakland)
rm(MonthLevel_2012)
rm(MonthLevel_2013_2014)
rm(MonthLevel_2015_2016)
rm(MonthLevel_2017_2018)
rm(MonthLevel_2019_2020)

gc()


##Removing any row with an NA
NielsenMovement_All_months <- NielsenMovement_All_months[complete.cases(NielsenMovement_All_months),]

##Removing any row with a "0" or negative value for "TotalOz", "AvgPrice_perUnit", "TotalUnits", and "size_per_unit"
NielsenMovement_All_months <- filter(NielsenMovement_All_months, !(TotalOz <= 0 & AvgPrice_perUnit <= 0 & 
                                                                     TotalUnits <= 0 & size_per_unit <= 0))


##Reading in the store and census data
Store_Census_Data <- fread("Processed Data/Final Nielsen Files/Store_Census_Combined.csv",
                           colClasses = c(store_code_uc = "character",
                                          parent_code = "character",
                                          retailer_code = "character",
                                          store_zip3 = "character",
                                          fips_state_code = "character",
                                          fips_county_code = "character",
                                          dma_code = "character"))


##Subsetting to store data needed for synthetic control
Store_Census_Data_forMerge <- dplyr::select(Store_Census_Data, store_code_uc, medHHincome, population, medage,
                                            num_housingunits, pwhite, pblack, pamindalask, pasian, phawaii, pother,
                                            phisp, poverty_10K, age_18to64)


###############################################
###############Combining Data##################
###############################################

NielsenMovement_AllMonths_withCensusData <- left_join(NielsenMovement_All_months, Store_Census_Data_forMerge,
                                                      by = c("store_code_uc" = "store_code_uc"))

##Removing large Nielsen dataset
rm(NielsenMovement_All_months)

if(Zips_All == 0){
  
  if(Border_Analysis == 0){
    ##Filtering to just zip codes starting with 9 to start + zip codes for other treated areas
    NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData,
                                                       store_zip1 = as.integer(substr(store_zip3, 1, 1))) %>%
      filter(store_zip3 == "941" | store_zip3 == "945" | store_zip3 == "946" | 
               store_zip3 == "947" | store_zip3 == "200" | store_zip3 == "202" | 
               store_zip3 == "203" | store_zip3 == "204" | store_zip3 == "205" | store_zip3 == "803" | 
               store_zip3 == "190" | store_zip3 == "191" | store_zip3 == "192" | store_zip3 == "981" | 
               store_zip1 == 9 | store_zip1 == 8 | store_zip1 == 1 | store_zip1 == 0)
  }
  
  if(Border_Analysis == 1){
    ##Filtering to just zip codes starting with 9 to start + zip codes for other treated areas
    NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData,
                                                       store_zip1 = as.integer(substr(store_zip3, 1, 1))) %>%
      filter(store_zip3 == "941" | store_zip3 == "945" | store_zip3 == "946" | 
               store_zip3 == "947" | store_zip3 == "200" | store_zip3 == "202" | 
               store_zip3 == "203" | store_zip3 == "204" | store_zip3 == "205" | store_zip3 == "803" | 
               store_zip3 == "190" | store_zip3 == "191" | store_zip3 == "192" | store_zip3 == "981" | 
               store_zip3 == "800" | store_zip3 == "804" | store_zip3 == "805" | store_zip3 == "080" |
               store_zip3 == "081" | store_zip3 == "222" | store_zip3 == "223" | store_zip3 == "201" |
               store_zip3 == "220" | store_zip3 == "216" | store_zip3 == "218" | store_zip1 == 9)
  }
}

if(Zips_All == 1){
  ##Filtering to all zip codes
  NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData,
                                                     store_zip1 = as.integer(substr(store_zip3, 1, 1))) %>%
    filter(store_zip3 == "941" | store_zip3 == "945" | store_zip3 == "946" | 
             store_zip3 == "947" | store_zip3 == "200" | store_zip3 == "202" | 
             store_zip3 == "203" | store_zip3 == "204" | store_zip3 == "205" | store_zip3 == "803" | 
             store_zip3 == "190" | store_zip3 == "191" | store_zip3 == "192" | store_zip3 == "981" | 
             store_zip1 == 9 | store_zip1 == 8 | store_zip1 == 7 | store_zip1 == 6 | store_zip1 == 5 |
             store_zip1 == 4 | store_zip1 == 3 | store_zip1 == 2 | store_zip1 == 1 | store_zip1 == 0)
}


##Seeing how many time periods each zip code has and dropping incomplete zips
NumMonths_perZip <- group_by(NielsenMovement_AllMonths_withCensusData, store_zip3) %>%
  summarise(NumMonths = length(unique(time_to_treat_months_SanFrancisco))) %>%
  filter(NumMonths >= 108) #Zips removed: 202, 203, 204 (not there in first place), 205, 968, 996, 999

##Filtering to zip codes with all months
NielsenMovement_AllMonths_withCensusData <- filter(NielsenMovement_AllMonths_withCensusData, 
                                                   store_zip3 %in% NumMonths_perZip$store_zip3)

##Dropping COVID months
if(DropCovid == 1){
  NielsenMovement_AllMonths_withCensusData <- filter(NielsenMovement_AllMonths_withCensusData,
                                                     month_year != "2020-03-01" &
                                                       month_year != "2020-04-01" & 
                                                       month_year != "2020-05-01" & 
                                                       month_year != "2020-06-01" & 
                                                       month_year != "2020-07-01" & 
                                                       month_year != "2020-08-01" & 
                                                       month_year != "2020-09-01" & 
                                                       month_year != "2020-10-01" & 
                                                       month_year != "2020-11-01" & 
                                                       month_year != "2020-12-01")
}

#Creating a total number of ounces variable & a total revenue variable
NielsenMovement_AllMonths_withCensusData <- mutate(NielsenMovement_AllMonths_withCensusData, 
                                                   TotalOz = TotalUnits*size_per_unit,
                                                   TotalRevenue = TotalOz*(AvgPrice_perUnit/size_per_unit))

##Taking means at the zip code - month level
NielsenMovement_AllMonths_withCensusData_ZipCode <- group_by(NielsenMovement_AllMonths_withCensusData,
                                                             store_zip3, time_to_treat_months_SanFrancisco,
                                                             time_to_treat_months_DC, time_to_treat_months_Seattle,
                                                             time_to_treat_months_Philadelphia, time_to_treat_months_Boulder,
                                                             time_to_treat_months_Berkeley, time_to_treat_months_Oakland,
                                                             medHHincome, population, medage,
                                                             num_housingunits, pwhite, pblack, pamindalask,
                                                             pasian, phawaii, pother,
                                                             phisp, poverty_10K, age_18to64) %>%
  summarise(TotalRevenue_Sum = sum(TotalRevenue),
            TotalOz_Sum = sum(TotalOz),
            AvgPrice_PerOz_zipcode = sum(TotalRevenue)/sum(TotalOz)) %>%
  ungroup()

#Removing large Nielsen dataset (that includes census data)
rm(NielsenMovement_AllMonths_withCensusData)

##Grouping all zip codes for a given treatment area into a single zip code (FIGURE OUT WHICH ZIPS ARE FULLY TREATED)

#Just use 191 for Philly (drop 190 and 192)
NielsenMovement_AllMonths_withCensusData_ZipCode <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           store_zip3 != "190" & store_zip3 != "192")

##Removing any row with an NA
NielsenMovement_AllMonths_withCensusData_ZipCode <- NielsenMovement_AllMonths_withCensusData_ZipCode[complete.cases(NielsenMovement_AllMonths_withCensusData_ZipCode),]

##Removing any zip code with an "Inf" for one of the covariates
NielsenMovement_AllMonths_withCensusData_ZipCode <- filter_all(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                               all_vars(!is.infinite(.)))


#Testing for duplicate rows - if 0, then good to go!
ex <- group_by(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3, time_to_treat_months_SanFrancisco) %>%
  filter(n()>1)


#################################################################
######Outputting Zip code level data for quicker analyses########
#################################################################

fwrite(NielsenMovement_AllMonths_withCensusData_ZipCode, 
       "Processed Data/Final Nielsen Files/NielsenMovement_AllMonths_withCensusData_ZipCode_Prices.csv")


