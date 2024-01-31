library(data.table)
library(dplyr)

################################################
###############Reading in Data##################
################################################

##Setting the working directory for all data
setwd("C:/Users/skaplan/Projects/SSB Taxes")

##Reading in the UPC Characteristics data
UPC_Characteristics <- fread("Data/Matched UPC Characteristics_ALL.csv",
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

##Filter UPCs with an "NA" for size
UPC_Characteristics <- filter(UPC_Characteristics, !is.na(size))

##Filtering to just SSBs
UPC_Characteristics_SSBs <- filter(UPC_Characteristics, ssb == 1)

########################################
####Reading in Nielsen Movement Data####
########################################

##2012
NielsenMovement_2012 <- fread("Nielsen_2012_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2012_SSBs <- filter(NielsenMovement_2012, upc10 %in% UPC_Characteristics_SSBs$upc10)
rm(NielsenMovement_2012)

##Output this file for future use
fwrite(NielsenMovement_2012_SSBs, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2012_SSBs.csv")
rm(NielsenMovement_2012_SSBs)


##2013 and 2014
NielsenMovement_2013 <- fread("Nielsen_2013_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2014 <- fread("Nielsen_2014_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2013_2014 <- rbind(NielsenMovement_2013, NielsenMovement_2014)
rm(NielsenMovement_2013)
rm(NielsenMovement_2014)

NielsenMovement_2013_2014_SSBs <- filter(NielsenMovement_2013_2014, upc10 %in% UPC_Characteristics_SSBs$upc10)
rm(NielsenMovement_2013_2014)

##Output this file for future use
fwrite(NielsenMovement_2013_2014_SSBs, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2013_2014_SSBs.csv")
rm(NielsenMovement_2013_2014_SSBs)


##2015 and 2016
NielsenMovement_2015 <- fread("Nielsen_2015_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2016 <- fread("Nielsen_2016_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2015_2016 <- rbind(NielsenMovement_2015, NielsenMovement_2016)
rm(NielsenMovement_2015)
rm(NielsenMovement_2016)

NielsenMovement_2015_2016_SSBs <- filter(NielsenMovement_2015_2016, upc10 %in% UPC_Characteristics_SSBs$upc10)
rm(NielsenMovement_2015_2016)

##Output this file for future use
fwrite(NielsenMovement_2015_2016_SSBs, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2015_2016_SSBs.csv")
rm(NielsenMovement_2015_2016_SSBs)



##2017 and 2018
NielsenMovement_2017 <- fread("Nielsen_2017_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2018 <- fread("Nielsen_2018_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2017_2018 <- rbind(NielsenMovement_2017, NielsenMovement_2018)
rm(NielsenMovement_2017)
rm(NielsenMovement_2018)

NielsenMovement_2017_2018_SSBs <- filter(NielsenMovement_2017_2018, upc10 %in% UPC_Characteristics_SSBs$upc10)
rm(NielsenMovement_2017_2018)

##Output this file for future use
fwrite(NielsenMovement_2017_2018_SSBs, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2017_2018_SSBs.csv")
rm(NielsenMovement_2017_2018_SSBs)



##2019 and 2020
NielsenMovement_2019 <- fread("Nielsen_2019_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2020 <- fread("Nielsen_2020_MatchedData_NoCharacteristics_NoStores.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character",
                                             upc10 = "character"))

NielsenMovement_2019_2020 <- rbind(NielsenMovement_2019, NielsenMovement_2020)
rm(NielsenMovement_2019)
rm(NielsenMovement_2020)

NielsenMovement_2019_2020_SSBs <- filter(NielsenMovement_2019_2020, upc10 %in% UPC_Characteristics_SSBs$upc10)
rm(NielsenMovement_2019_2020)


##Output this file for future use
fwrite(NielsenMovement_2019_2020_SSBs, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/NielsenMovement_2019_2020_SSBs.csv")
rm(NielsenMovement_2019_2020_SSBs)

