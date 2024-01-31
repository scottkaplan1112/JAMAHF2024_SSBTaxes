library(data.table)
library(dplyr)
library(haven)

##Setting the working directory for matched Nielsen UPCs
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes/Data/Unique Nielsen UPCs")


###############################################################
###############Nielsen UPCs that were matched##################
###############################################################

MatchedUPCs <- read_dta("nielsen_upcs_with_nutrition_short_SK.dta")

##Obtaining just the UPCs that had a match to either the Label Insights OR Handcoded data
MatchedUPCs <- filter(MatchedUPCs, merge_li == 3 | merge_handcoded == 3)

##Drop UPCs that had a match ONLY between the handcoded and Label Insight data but NOT the Nielsen data
MatchedUPCs <- filter(MatchedUPCs, !(merge_li == 1 & merge_handcoded == 3))

#############THIS INFORMATION FOR TOTAL UNIQUE SSB UPCS MATCHED ON FOR SUMMARY STAT IN JAMA HF 2024 PAPER#############
SSBUPCs <- filter(MatchedUPCs, ssb == 1) 
SSBUPCs_Number <- length(unique(SSBUPCs$upc10))

##From STATA file, there were 52,584 unique 10-digit upcs from Nielsen. Conversion rate is 18471/52584


##Matching summary statistics 
handcoded <- filter(MatchedUPCs, merge_handcoded == 3 & merge_li != 3) %>% nrow()
labelinsights <- filter(MatchedUPCs, merge_li == 3 & merge_handcoded != 3) %>% nrow()
both <- filter(MatchedUPCs, merge_li == 3 & merge_handcoded == 3) %>% nrow

####Writing out matched UPC characteristic data as a .csv
fwrite(MatchedUPCs, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Matched UPC Characteristics_LI_and_Handcoded.csv")


##################################################################################
###############Obtaining only matched upcs from raw Nielsen data##################
##################################################################################

##Setting the working directory for processed Nielsen data
setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics")

###############2012##################

####Reading in 2012 processed Nielsen data
Nielsen_RawData_2012 <- fread("Nielsen_2012_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2012_matched <- mutate(Nielsen_RawData_2012, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2012_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2012_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2012)
rm(Nielsen_RawData_2012_matched)


###############2013##################

####Reading in 2013 processed Nielsen data
Nielsen_RawData_2013 <- fread("Nielsen_2013_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2013_matched <- mutate(Nielsen_RawData_2013, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2013_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2013_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2013)
rm(Nielsen_RawData_2013_matched)


###############2014##################

####Reading in 2014 processed Nielsen data
Nielsen_RawData_2014 <- fread("Nielsen_2014_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2014_matched <- mutate(Nielsen_RawData_2014, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2014_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2014_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2014)
rm(Nielsen_RawData_2014_matched)


###############2015##################

####Reading in 2015 processed Nielsen data
Nielsen_RawData_2015 <- fread("Nielsen_2015_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2015_matched <- mutate(Nielsen_RawData_2015, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2015_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2015_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2015)
rm(Nielsen_RawData_2015_matched)


###############2016##################

####Reading in 2016 processed Nielsen data
Nielsen_RawData_2016 <- fread("Nielsen_2016_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2016_matched <- mutate(Nielsen_RawData_2016, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2016_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2016_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2016)
rm(Nielsen_RawData_2016_matched)


###############2017##################

####Reading in 2017 processed Nielsen data
Nielsen_RawData_2017 <- fread("Nielsen_2017_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2017_matched <- mutate(Nielsen_RawData_2017, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2017_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2017_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2017)
rm(Nielsen_RawData_2017_matched)


###############2018##################

####Reading in 2018 processed Nielsen data
Nielsen_RawData_2018 <- fread("Nielsen_2018_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2018_matched <- mutate(Nielsen_RawData_2018, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2018_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2018_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2018)
rm(Nielsen_RawData_2018_matched)


###############2019##################

####Reading in 2019 processed Nielsen data
Nielsen_RawData_2019 <- fread("Nielsen_2019_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2019_matched <- mutate(Nielsen_RawData_2019, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2019_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2019_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2019)
rm(Nielsen_RawData_2019_matched)


###############2020##################

####Reading in 2020 processed Nielsen data
Nielsen_RawData_2020 <- fread("Nielsen_2020_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

Nielsen_RawData_2020_matched <- mutate(Nielsen_RawData_2020, upc10 = substr(upc, 3, length(upc))) %>%
  filter(upc10 %in% MatchedUPCs$upc10)


####Writing out matched Nielsen movement data as a .csv
fwrite(Nielsen_RawData_2020_matched, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Final Nielsen Files/Nielsen_2020_MatchedData_NoCharacteristics_NoStores.csv")

rm(Nielsen_RawData_2020)
rm(Nielsen_RawData_2020_matched)