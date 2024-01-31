library(data.table)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)

################################################
###############Reading in Data##################
################################################

##Setting the working directory for all data
setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files")

##Reading in the UPC Characteristics data
UPC_Characteristics <- fread("Matched UPC Characteristics_ALL.csv",
                             colClasses = c(upc = "character",
                                            upc10 = "character",
                                            flavor_code = "character",
                                            form_code = "character",
                                            formula_code = "character",
                                            container_code = "character",
                                            salt_content_code = "character",
                                            style_code = "character",
                                            type_code = "character",
                                            product_code = "character",
                                            variety_code = "character",
                                            organic_claim_code = "character",
                                            usda_organic_seal_code = "character",
                                            common_consumer_name_code = "character",
                                            strength_code = "character",
                                            scent_code = "character",
                                            dosage_code = "character",
                                            gender_code = "character",
                                            target_skin_condition_code = "character",
                                            use_code = "character",
                                            size2_code = "character"))

##Taking distinct characteristics across all potential panel years (2012-20)
UPC_Characteristics <- distinct(UPC_Characteristics, upc10, .keep_all = TRUE)
UPC_Characteristics <- filter(UPC_Characteristics, !is.na(size))
UPC_Characteristics_SSBs <- filter(UPC_Characteristics, ssb == 1)

##Reading in the Store/Census data
Store_Census_Data <- fread("Store_Census_Combined.csv",
                           colClasses = c(store_code_uc = "character",
                                          parent_code = "character",
                                          retailer_code = "character",
                                          store_zip3 = "character",
                                          fips_state_code = "character",
                                          fips_county_code = "character",
                                          dma_code = "character"))

##Taking distinct store observations
Store_Census_Data <- distinct(Store_Census_Data, store_code_uc, .keep_all = TRUE)

##Creating an SF indicator variable
Store_Census_Data <- mutate(Store_Census_Data,
                            SF = ifelse(store_zip3 == "941", "SF", "Not SF"))

##How many SF vs. non-SF stores? (just a descriptive piece of information)
SFStores <- filter(Store_Census_Data, store_zip3 == "941") %>% nrow
NonSFStores <- filter(Store_Census_Data, store_zip3 != "941") %>% nrow


####################################################################
###############DID and Event Study Data Prep Function###############
####################################################################

DID_EventStudy_DataPrep_Fun <- function(data){
  
  #data <- NielsenMovement_2012_SSBs[1:100000,]
  
  ##Joining size and zip code data to movement data
  UPC_Characteristics_SSBs_forJoin <- select(UPC_Characteristics_SSBs, upc10, size)
  Store_Census_Data_forJoin <- select(Store_Census_Data, store_code_uc, store_zip3, SF)
  data_name <- select(data,
                      upc10, store_code_uc, week_end, units, price)
  
  ##Split dataframe in half because it is too big for a vector merge
  data_tophalf <- data_name[1:floor(nrow(data_name)/2),]
  data_bottomhalf <- data_name[floor(nrow(data_name)/2+1):nrow(data_name),]
  
  print("Data split into two")
  
  data_tophalf <- left_join(data_tophalf,
                            UPC_Characteristics_SSBs_forJoin,
                            by = c("upc10" = "upc10"))
  
  data_bottomhalf <- left_join(data_bottomhalf,
                               UPC_Characteristics_SSBs_forJoin,
                               by = c("upc10" = "upc10"))
  
  data_tophalf <- left_join(data_tophalf,
                            Store_Census_Data_forJoin,
                            by = c("store_code_uc" = "store_code_uc"))
  
  data_bottomhalf <- left_join(data_bottomhalf,
                               Store_Census_Data_forJoin,
                               by = c("store_code_uc" = "store_code_uc"))
  
  data_name <- rbind(data_tophalf, data_bottomhalf)
  
  ##Creating week date variable
  data_name$week_end <- ymd(data_name$week_end)
  
  ##Creating a month-year date variable
  data_name$month_year <- format(data_name$week_end, "%Y-%m")
  data_name$month_year <- ym(data_name$month_year)
  
  print("Data joined and row-bound")
  
  ##Creating a total ounces sold variable
  data_name <- mutate(data_name, size_times_units = size*units) 
  
  ##Grouping data by month, store, and upc and obtaining total ounces sold
  data_name_MonthLevel <- group_by(data_name, month_year, store_code_uc, store_zip3, upc10, SF) %>%
    summarise(TotalUnits = sum(units),
              TotalOz = sum(size_times_units),
              AvgPrice_perUnit = weighted.mean(price, units))
  
  data_name_MonthLevel <- mutate(data_name_MonthLevel,
                                 size_per_unit = TotalOz/TotalUnits)
  
  
  print("Now creating the DID and ES variables")
  
  ####################################################
  #########CREATING NEW VARIABLES FOR DID/ES##########
  ####################################################
  
  ##Making SF Variable a 0/1 variable & Creating DID post variable
  data_name_MonthLevel <- mutate(data_name_MonthLevel, 
                                       Post = ifelse(month_year < "2018-01-01", 0, 1),
                                       SF = ifelse(SF == "SF", 1, 0))
  
  ##Creating a time-to-treat variable
  data_name_MonthLevel <- mutate(data_name_MonthLevel,
                                       time_to_treat = ifelse(SF == 1,
                                                              month_year - as.Date("2018-01-01"), 
                                                              0)) %>%
    mutate(time_to_treat_months = round(time_to_treat/30.417, digit = 0))
  
  ##Removing any row with an NA
  data_name_MonthLevel <- data_name_MonthLevel[complete.cases(data_name_MonthLevel),]
  
  ##Filtering out any row with "0" or negative value for the "TotalOz" variable
  data_name_MonthLevel <- filter(data_name_MonthLevel, !(TotalOz <= 0))
  
  return(data_name_MonthLevel)
  
}

#########################
########2012#############
#########################

##Reading in Nielsen Movement Data
NielsenMovement_2012_SSBs <- fread("NielsenMovement_2012_SSBs.csv",
                                        colClasses = c(store_code_uc = "character", 
                                                       upc = "character", 
                                                       week_end = "character",
                                                       upc10 = "character"))

MonthLevel_2012 <- DID_EventStudy_DataPrep_Fun(NielsenMovement_2012_SSBs)
fwrite(MonthLevel_2012, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2012_SSBs_MonthLevel.csv" )

rm(NielsenMovement_2012_SSBs)
rm(MonthLevel_2012)


#########################
########2013-2014########
#########################

##Reading in Nielsen Movement Data
NielsenMovement_2013_2014_SSBs <- fread("NielsenMovement_2013_2014_SSBs.csv",
                                        colClasses = c(store_code_uc = "character", 
                                                       upc = "character", 
                                                       week_end = "character",
                                                       upc10 = "character"))

MonthLevel_2013_2014 <- DID_EventStudy_DataPrep_Fun(NielsenMovement_2013_2014_SSBs)
fwrite(MonthLevel_2013_2014, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2013_2014_SSBs_MonthLevel.csv" )

rm(NielsenMovement_2013_2014_SSBs)
rm(MonthLevel_2013_2014)


#########################
########2015-2016########
#########################

##Reading in Nielsen Movement Data
NielsenMovement_2015_2016_SSBs <- fread("NielsenMovement_2015_2016_SSBs.csv",
                                        colClasses = c(store_code_uc = "character", 
                                                       upc = "character", 
                                                       week_end = "character",
                                                       upc10 = "character"))

MonthLevel_2015_2016 <- DID_EventStudy_DataPrep_Fun(NielsenMovement_2015_2016_SSBs)
fwrite(MonthLevel_2015_2016, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2015_2016_SSBs_MonthLevel.csv" )

rm(NielsenMovement_2015_2016_SSBs)
rm(MonthLevel_2015_2016)

#########################
########2017-2018########
#########################

##Reading in Nielsen Movement Data
NielsenMovement_2017_2018_SSBs <- fread("NielsenMovement_2017_2018_SSBs.csv",
                                        colClasses = c(store_code_uc = "character", 
                                                       upc = "character", 
                                                       week_end = "character",
                                                       upc10 = "character"))

MonthLevel_2017_2018 <- DID_EventStudy_DataPrep_Fun(NielsenMovement_2017_2018_SSBs)
fwrite(MonthLevel_2017_2018, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2017_2018_SSBs_MonthLevel.csv" )

rm(NielsenMovement_2017_2018_SSBs)
rm(MonthLevel_2017_2018)


#########################
########2019-2020########
#########################

##Reading in Nielsen Movement Data
NielsenMovement_2019_2020_SSBs <- fread("NielsenMovement_2019_2020_SSBs.csv",
                                        colClasses = c(store_code_uc = "character", 
                                                       upc = "character", 
                                                       week_end = "character",
                                                       upc10 = "character"))

MonthLevel_2019_2020 <- DID_EventStudy_DataPrep_Fun(NielsenMovement_2019_2020_SSBs)
fwrite(MonthLevel_2019_2020, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2019_2020_SSBs_MonthLevel.csv" )

rm(NielsenMovement_2019_2020_SSBs)
rm(MonthLevel_2019_2020)

