library(data.table)
library(haven)
library(dplyr)

############################################################
###############Product Characteristic Data##################
############################################################

##Setting the working directory for store and product characteristic data
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes/Data")

#Main product data
Products_MAIN <- fread("products.tsv",
                       colClasses = c(upc = "character",
                                      product_module_code = "character",
                                      product_group_code = "character",
                                      department_code = "character",
                                      brand_code_uc = "character",
                                      salt_content_code = "character",
                                      size1_code_uc = "character"))

Products_Aggregated <- distinct(Products_MAIN, upc, .keep_all = TRUE)

##Generate UPC10 from these 12-digit upcs
Products_Aggregated$upc10 <- substr(Products_Aggregated$upc, 3, 12)

###########################################################
#####Reading in Matched and Unmatched UPC # Units Data#####
###########################################################

TotalUnits_byUPC_All <- fread("TotalUnits_byUPC_All_Unmatched_NoCharacteristics.csv",
                              colClasses = c(upc = "character",
                                             upc10 = "character"))
TotalUnits_byUPC_MATCHED_All <- fread("TotalUnits_byUPC_All_Matched_NoCharacteristics.csv",
                                      colClasses = c(upc = "character",
                                                     upc10 = "character"))

#####################################################
#####Total Units (matched + unmatched) summation#####
#####################################################

Products_Aggregated_AllRelevant <- filter(Products_Aggregated, upc10 %in% TotalUnits_byUPC_All$upc10) %>%
  distinct(upc10, .keep_all = TRUE)

TotalUnits_byUPC_All <- left_join(TotalUnits_byUPC_All, Products_Aggregated_AllRelevant,
                                  by = c("upc10" = "upc10"))

##Filter out all products not in liquid units (OZ, QT, LI, CF, and ML)
TotalUnits_byUPC_All_Liquids <- filter(TotalUnits_byUPC_All, 
                                       !(size1_units == "CT" | 
                                           size1_units == "YD" |
                                           size1_units == "SQ FT" |
                                           size1_units == "PO" |
                                           size1_units == "EXP" |
                                           size1_units == "FT"))

##Convert everything to ounces
TotalUnits_byUPC_All_Liquids <- mutate(TotalUnits_byUPC_All_Liquids,
                                       size1_amount_ounces = ifelse(size1_units == "OZ", size1_amount*1,
                                                                    ifelse(size1_units == "QT", size1_amount*32,
                                                                           ifelse(size1_units == "ML", size1_amount*0.033814,
                                                                                  ifelse(size1_units == "LI", size1_amount*33.814, size1_amount)))))

##Multiply by the "multi" category
TotalUnits_byUPC_All_Liquids <- mutate(TotalUnits_byUPC_All_Liquids,
                                       size1_amount_ounces_multi = size1_amount_ounces*multi)

##Multiply by the total number of units
TotalUnits_byUPC_All_Liquids <- mutate(TotalUnits_byUPC_All_Liquids,
                                       size1_amount_ounces_multi_totalunits = size1_amount_ounces_multi*TotalUnits)

sum(TotalUnits_byUPC_All_Liquids$size1_amount_ounces_multi_totalunits)

#################################
#####MATCHED Units summation#####
#################################

Products_Aggregated_Matched <- filter(Products_Aggregated, upc10 %in% TotalUnits_byUPC_MATCHED_All$upc10) %>%
  distinct(upc10, .keep_all = TRUE)

TotalUnits_byUPC_MATCHED_All <- left_join(TotalUnits_byUPC_MATCHED_All, Products_Aggregated_Matched,
                                  by = c("upc10" = "upc10"))

##Filter out all products not in liquid units (OZ, QT, LI, CF, and ML)
TotalUnits_byUPC_MATCHED_All_Liquids <- filter(TotalUnits_byUPC_MATCHED_All, 
                                       !(size1_units == "CT" | 
                                           size1_units == "YD" |
                                           size1_units == "SQ FT" |
                                           size1_units == "PO" |
                                           size1_units == "EXP" |
                                           size1_units == "FT"))

##Convert everything to ounces
TotalUnits_byUPC_MATCHED_All_Liquids <- mutate(TotalUnits_byUPC_MATCHED_All_Liquids,
                                       size1_amount_ounces = ifelse(size1_units == "OZ", size1_amount*1,
                                                                    ifelse(size1_units == "QT", size1_amount*32,
                                                                           ifelse(size1_units == "ML", size1_amount*0.033814,
                                                                                  ifelse(size1_units == "LI", size1_amount*33.814, size1_amount)))))

##Multiply by the "multi" category
TotalUnits_byUPC_MATCHED_All_Liquids <- mutate(TotalUnits_byUPC_MATCHED_All_Liquids,
                                       size1_amount_ounces_multi = size1_amount_ounces*multi)

##Multiply by the total number of units
TotalUnits_byUPC_MATCHED_All_Liquids <- mutate(TotalUnits_byUPC_MATCHED_All_Liquids,
                                       size1_amount_ounces_multi_totalunits = size1_amount_ounces_multi*TotalUnits)

sum(TotalUnits_byUPC_MATCHED_All_Liquids$size1_amount_ounces_multi_totalunits)


##########################
#####% VOLUME MATCHED#####
##########################
Coverage <- sum(TotalUnits_byUPC_MATCHED_All_Liquids$size1_amount_ounces_multi_totalunits)/sum(TotalUnits_byUPC_All_Liquids$size1_amount_ounces_multi_totalunits)
Coverage

##########################################
#####% OF MATCHED VOLUME THAT IS SSBS#####
##########################################

#Reading in matched UPCs with LI/handcoded data
MatchedUPCs <- read_dta("Unique Nielsen UPCs/nielsen_upcs_with_nutrition_short_SK.dta")

##Obtaining just the UPCs that had a match to either the Label Insights OR Handcoded data
MatchedUPCs <- filter(MatchedUPCs, merge_li == 3 | merge_handcoded == 3)

##Joining to compute % of volume that is SSBs
TotalUnits_byUPC_MATCHED_All_Liquids <- left_join(TotalUnits_byUPC_MATCHED_All_Liquids,
                                                  MatchedUPCs,
                                                  by = c("upc10" = "upc10"))

##Computing unique number of UPC10 SSB products
UniqueSSBUPC10s <- filter(TotalUnits_byUPC_MATCHED_All_Liquids, ssb == 1) %>%
  summarise(TotalSSBVolume = length(unique(upc10)))
UniqueSSBUPC10s

##Computing total SSB volume
TotalVolume_SSBs <- filter(TotalUnits_byUPC_MATCHED_All_Liquids, ssb == 1) %>%
  summarise(TotalSSBVolume = sum(size1_amount_ounces_multi_totalunits))

##Computing total volume (SSBs + non-SSBs)
TotalVolume_All <- filter(TotalUnits_byUPC_MATCHED_All_Liquids, ssb == 1 | ssb == 0) %>%
  summarise(TotalSSBVolume = sum(size1_amount_ounces_multi_totalunits))

##Computing % of volume that is SSB
SSBVolumePercent <- TotalVolume_SSBs[1]/TotalVolume_All
SSBVolumePercent
