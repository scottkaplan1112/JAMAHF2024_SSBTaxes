library(data.table)
library(haven)
library(dplyr)

setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

#Reading in all Nielsen UPCs
NielsenUPCs_All <- read_dta("Data/Unique Nielsen UPCs/temp/upc_list_nielsen.dta")

#Reading in matched UPCs with LI/handcoded data
MatchedUPCs <- read_dta("Data/Unique Nielsen UPCs/nielsen_upcs_with_nutrition_short_SK.dta")

##Obtaining just the UPCs that had a match to either the Label Insights OR Handcoded data
MatchedUPCs <- filter(MatchedUPCs, merge_li == 3 | merge_handcoded == 3)

##Drop UPCs that had a match ONLY between the handcoded and Label Insight data but NOT the Nielsen data
MatchedUPCs <- filter(MatchedUPCs, !(merge_li == 1 & merge_handcoded == 3))



###############2012##################

####Reading in 2012 processed Nielsen data with all UPCs
Nielsen_RawData_2012 <- fread("Data/Nielsen_2012_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2012 <- group_by(Nielsen_RawData_2012, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2012)

####Reading in 2012 processed Nielsen data with only matched UPCs
Nielsen_RawData_2012_matched <- fread("Data/Nielsen_2012_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2012 <- group_by(Nielsen_RawData_2012_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2012 <- mutate(TotalUnits_byUPC_MATCHED_2012, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2012_matched)


###############2013##################

####Reading in 2013 processed Nielsen data with all UPCs
Nielsen_RawData_2013 <- fread("Data/Nielsen_2013_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2013 <- group_by(Nielsen_RawData_2013, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2013)

####Reading in 2013 processed Nielsen data with only matched UPCs
Nielsen_RawData_2013_matched <- fread("Data/Nielsen_2013_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2013 <- group_by(Nielsen_RawData_2013_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2013 <- mutate(TotalUnits_byUPC_MATCHED_2013, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2013_matched)


###############2014##################

####Reading in 2014 processed Nielsen data with all UPCs
Nielsen_RawData_2014 <- fread("Data/Nielsen_2014_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2014 <- group_by(Nielsen_RawData_2014, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2014)

####Reading in 2014 processed Nielsen data with only matched UPCs
Nielsen_RawData_2014_matched <- fread("Data/Nielsen_2014_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2014 <- group_by(Nielsen_RawData_2014_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2014 <- mutate(TotalUnits_byUPC_MATCHED_2014, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2014_matched)


###############2015##################

####Reading in 2015 processed Nielsen data with all UPCs
Nielsen_RawData_2015 <- fread("Data/Nielsen_2015_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2015 <- group_by(Nielsen_RawData_2015, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2015)

####Reading in 2015 processed Nielsen data with only matched UPCs
Nielsen_RawData_2015_matched <- fread("Data/Nielsen_2015_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2015 <- group_by(Nielsen_RawData_2015_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2015 <- mutate(TotalUnits_byUPC_MATCHED_2015, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2015_matched)


###############2016##################

####Reading in 2016 processed Nielsen data with all UPCs
Nielsen_RawData_2016 <- fread("Data/Nielsen_2016_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2016 <- group_by(Nielsen_RawData_2016, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2016)

####Reading in 2016 processed Nielsen data with only matched UPCs
Nielsen_RawData_2016_matched <- fread("Data/Nielsen_2016_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2016 <- group_by(Nielsen_RawData_2016_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2016 <- mutate(TotalUnits_byUPC_MATCHED_2016, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2016_matched)


###############2017##################

####Reading in 2017 processed Nielsen data with all UPCs
Nielsen_RawData_2017 <- fread("Data/Nielsen_2017_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2017 <- group_by(Nielsen_RawData_2017, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2017)

####Reading in 2017 processed Nielsen data with only matched UPCs
Nielsen_RawData_2017_matched <- fread("Data/Nielsen_2017_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2017 <- group_by(Nielsen_RawData_2017_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2017 <- mutate(TotalUnits_byUPC_MATCHED_2017, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2017_matched)


###############2018##################

####Reading in 2018 processed Nielsen data with all UPCs
Nielsen_RawData_2018 <- fread("Data/Nielsen_2018_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2018 <- group_by(Nielsen_RawData_2018, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2018)

####Reading in 2018 processed Nielsen data with only matched UPCs
Nielsen_RawData_2018_matched <- fread("Data/Nielsen_2018_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2018 <- group_by(Nielsen_RawData_2018_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2018 <- mutate(TotalUnits_byUPC_MATCHED_2018, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2018_matched)


###############2019##################

####Reading in 2019 processed Nielsen data with all UPCs
Nielsen_RawData_2019 <- fread("Data/Nielsen_2019_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2019 <- group_by(Nielsen_RawData_2019, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2019)

####Reading in 2019 processed Nielsen data with only matched UPCs
Nielsen_RawData_2019_matched <- fread("Data/Nielsen_2019_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2019 <- group_by(Nielsen_RawData_2019_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2019 <- mutate(TotalUnits_byUPC_MATCHED_2019, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2019_matched)


###############2020##################

####Reading in 2020 processed Nielsen data with all UPCs
Nielsen_RawData_2020 <- fread("Data/Nielsen_2020_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", 
                                             upc = "character", 
                                             week_end = "character"))

##Sum up total units of all products in this year
TotalUnits_byUPC_2020 <- group_by(Nielsen_RawData_2020, upc) %>%
  summarise(TotalUnits = sum(units))

##Remove data
rm(Nielsen_RawData_2020)

####Reading in 2020 processed Nielsen data with only matched UPCs
Nielsen_RawData_2020_matched <- fread("Data/Nielsen_2020_MatchedData_NoCharacteristics_NoStores.csv",
                                      colClasses = c(store_code_uc = "character", 
                                                     upc = "character", 
                                                     week_end = "character"))


##Sum up total ounces of all products in this year
TotalUnits_byUPC_MATCHED_2020 <- group_by(Nielsen_RawData_2020_matched, upc) %>%
  summarise(TotalUnits = sum(units))

#test if any upcs have less than 12 digits
test_12digits_2020 <- mutate(TotalUnits_byUPC_MATCHED_2020, upc_nchar = nchar(upc)) %>%
  filter(upc_nchar != 12)

##Remove data
rm(Nielsen_RawData_2020_matched)


###############Unmatched UPCs # Units##################
TotalUnits_byUPC_All <- rbind(TotalUnits_byUPC_2012, TotalUnits_byUPC_2013, TotalUnits_byUPC_2014,
                              TotalUnits_byUPC_2015, TotalUnits_byUPC_2016, TotalUnits_byUPC_2017,
                              TotalUnits_byUPC_2018, TotalUnits_byUPC_2019, TotalUnits_byUPC_2020)

TotalUnits_byUPC_All <- group_by(TotalUnits_byUPC_All, upc) %>%
  summarise(TotalUnits = sum(TotalUnits))

##take off first two digits
TotalUnits_byUPC_All$upc10 <- substr(TotalUnits_byUPC_All$upc, 3, 12)

fwrite(TotalUnits_byUPC_All, "Data/TotalUnits_byUPC_All_Unmatched_NoCharacteristics.csv")

###############Matched UPCs # Units##################
TotalUnits_byUPC_MATCHED_All <- rbind(TotalUnits_byUPC_MATCHED_2012, TotalUnits_byUPC_MATCHED_2013, TotalUnits_byUPC_MATCHED_2014,
                              TotalUnits_byUPC_MATCHED_2015, TotalUnits_byUPC_MATCHED_2016, TotalUnits_byUPC_MATCHED_2017,
                              TotalUnits_byUPC_MATCHED_2018, TotalUnits_byUPC_MATCHED_2019, TotalUnits_byUPC_MATCHED_2020)

TotalUnits_byUPC_MATCHED_All <- group_by(TotalUnits_byUPC_MATCHED_All, upc) %>%
  summarise(TotalUnits = sum(TotalUnits))

##take off first two digits
TotalUnits_byUPC_MATCHED_All$upc10 <- substr(TotalUnits_byUPC_MATCHED_All$upc, 3, 12)

##joining by matched data
test <- left_join(MatchedUPCs, TotalUnits_byUPC_MATCHED_All,
                  by = c("upc10" = "upc10"))

fwrite(TotalUnits_byUPC_MATCHED_All, "Data/TotalUnits_byUPC_All_Matched_NoCharacteristics.csv")
