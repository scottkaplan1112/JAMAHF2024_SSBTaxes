library(data.table)
library(dplyr)

######################################################################
###############Store and Product Characteristic Data##################
######################################################################

##Setting the working directory for store and product characteristic data
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Annual Files")

#Product data
Products_2012 <- fread("products_extra_2012.tsv",
                       colClasses = c(upc = "character",
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
Products_2013 <- fread("products_extra_2013.tsv",
                       colClasses = c(upc = "character",
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
Products_2014 <- fread("products_extra_2014.tsv",
                       colClasses = c(upc = "character",
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
Products_2015 <- fread("products_extra_2015.tsv",
                       colClasses = c(upc = "character",
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
Products_2016 <- fread("products_extra_2016.tsv",
                       colClasses = c(upc = "character",
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
Products_2017 <- fread("products_extra_2017.tsv",
                       colClasses = c(upc = "character",
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
Products_2018 <- fread("products_extra_2018.tsv",
                       colClasses = c(upc = "character",
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
Products_2019 <- fread("products_extra_2019.tsv",
                       colClasses = c(upc = "character",
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
Products_2020 <- fread("products_extra_2020.tsv",
                       colClasses = c(upc = "character",
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

# #rms data (DON'T BELIEVE THIS IS NECESSARY SINCE I HAVE EACH YEARLY PRODUCT FILE)
# RMS_2015 <- fread("rms_versions_2015.tsv", 
#                   colClasses = c(upc = "character", upc_ver_uc = "numeric", panel_year = "numeric"))
# RMS_2016 <- fread("rms_versions_2016.tsv", 
#                   colClasses = c(upc = "character", upc_ver_uc = "numeric", panel_year = "numeric"))
# RMS_2017 <- fread("rms_versions_2017.tsv", 
#                   colClasses = c(upc = "character", upc_ver_uc = "numeric", panel_year = "numeric"))
# RMS_2018 <- fread("rms_versions_2018.tsv", 
#                   colClasses = c(upc = "character", upc_ver_uc = "numeric", panel_year = "numeric"))
# RMS_2019 <- fread("rms_versions_2019.tsv", 
#                   colClasses = c(upc = "character", upc_ver_uc = "numeric", panel_year = "numeric"))
# RMS_2020 <- fread("rms_versions_2020.tsv", 
#                   colClasses = c(upc = "character", upc_ver_uc = "numeric", panel_year = "numeric"))


###################################
#####Aggregating Product Files#####
###################################

Products_Aggregated <- rbind(Products_2012, Products_2013, Products_2014, Products_2015, 
                             Products_2016, Products_2017, Products_2018, Products_2019, Products_2020)

##(DON'T BELIEVE THIS IS NECESSARY SINCE I HAVE EACH YEARLY PRODUCT FILE)
# RMS_Aggregated <- rbind(RMS_2015, RMS_2016, RMS_2017, RMS_2018, RMS_2019, RMS_2020)
# 
# ##Removing the "panel_year" column from the products data
# Products_Aggregated_NoYear <- dplyr::select(Products_Aggregated, -panel_year)
# 
# ##Taking only unique rows based on upc code and upc version
# Products_Aggregated_NoYear_Unique <- distinct(Products_Aggregated_NoYear, upc, upc_ver_uc, .keep_all = TRUE)
# 
# ##Combining the RMS data with the Products data
# Products_RMS_Aggregated <- left_join(RMS_Aggregated, Products_Aggregated_NoYear_Unique, 
#                                      by = c("upc" = "upc",
#                                             "upc_ver_uc" = "upc_ver_uc"))



######################################################
##Combining with MATCHED Product Characteristic Data##
######################################################

MatchedProductCharacteristics <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Matched UPC Characteristics_LI_and_Handcoded.csv",
                                       colClasses = c(upc10 = "character"))

Products_Aggregated_Matched <- mutate(Products_Aggregated, 
                                                    upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedProductCharacteristics$upc10)

####Writing out matched UPC characteristic data as a .csv
fwrite(Products_Aggregated_Matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Matched UPC Characteristics_NielsenProductFiles.csv")

##Joining nutrition characteristics and product characteristics data for a data fidelity check
Products_Aggregated_Matched_LI_Handcoded_Joined <- left_join(Products_Aggregated_Matched, MatchedProductCharacteristics,
                                                             by = c("upc10" = "upc10"))

##Outputting combined characteristics data
fwrite(Products_Aggregated_Matched_LI_Handcoded_Joined, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Matched UPC Characteristics_ALL.csv")



##########################################################################
#############################Store Data###################################
##########################################################################

#Reading in Store data
Stores_2012 <- fread("stores_2012.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2013 <- fread("stores_2013.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2014 <- fread("stores_2014.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2015 <- fread("stores_2015.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2016 <- fread("stores_2016.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2017 <- fread("stores_2017.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2018 <- fread("stores_2018.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2019 <- fread("stores_2019.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))
Stores_2020 <- fread("stores_2020.tsv",
                     colClasses = c(store_code_uc = "character",
                                    parent_code = "character",
                                    retailer_code = "character",
                                    store_zip3 = "character",
                                    fips_state_code = "character",
                                    fips_county_code = "character",
                                    dma_code = "character"))

########################################
##Merging Census Data with Store Files##
########################################

##Reading in Census Data
CensusData <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Census Data/CensusData_Combined_Final_3DigitZip.csv",
                    colClasses = c(GEOID_3 = "character"))

##Joining Census data to each Store data file
Stores_2012 <- left_join(Stores_2012, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2013 <- left_join(Stores_2013, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2014 <- left_join(Stores_2014, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2015 <- left_join(Stores_2015, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2016 <- left_join(Stores_2016, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2017 <- left_join(Stores_2017, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2018 <- left_join(Stores_2018, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2019 <- left_join(Stores_2019, CensusData,
                         by = c("store_zip3" = "GEOID_3"))
Stores_2020 <- left_join(Stores_2020, CensusData,
                         by = c("store_zip3" = "GEOID_3"))

Store_Census_Combined <- rbind(Stores_2012, Stores_2013, Stores_2014, Stores_2015, Stores_2016, Stores_2017, Stores_2018, Stores_2019, Stores_2020)

##Identifying stores that switch zip codes and drop them
Stores_switchedZips <- distinct(Store_Census_Combined, store_code_uc, store_zip3) %>%
  group_by(store_code_uc) %>%
  filter(n()>1)

Store_Census_Combined_droppedSwitches <- filter(Store_Census_Combined, !(store_code_uc %in% Stores_switchedZips$store_code_uc)) 

##Taking distinct store observations
Store_Census_Combined_droppedSwitches <- group_by(Store_Census_Combined_droppedSwitches, store_code_uc) %>%
  filter(row_number(desc(year)) == 1)

##Outputting combined store and census data
fwrite(Store_Census_Combined_droppedSwitches, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Store_Census_Combined.csv")




