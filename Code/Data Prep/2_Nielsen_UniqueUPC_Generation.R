library(data.table)
library(dplyr)

##############################################################
###############Nielsen UPC Matching Analysis##################
##############################################################

##Setting the working directory for processed Nielsen data
setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics")


#####################################
###############2012##################
#####################################

####Reading in 2012 processed Nielsen data
Nielsen_RawData_2012 <- fread("Nielsen_2012_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2012 <- distinct(Nielsen_RawData_2012, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2012, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2012_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2012)


#####################################
###############2013##################
#####################################

####Reading in 2013 processed Nielsen data
Nielsen_RawData_2013 <- fread("Nielsen_2013_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2013 <- distinct(Nielsen_RawData_2013, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2013, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2013_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2013)


#####################################
###############2014##################
#####################################

####Reading in 2014 processed Nielsen data
Nielsen_RawData_2014 <- fread("Nielsen_2014_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2014 <- distinct(Nielsen_RawData_2014, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2014, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2014_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2014)


#####################################
###############2015##################
#####################################

####Reading in 2015 processed Nielsen data
Nielsen_RawData_2015 <- fread("Nielsen_2015_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2015 <- distinct(Nielsen_RawData_2015, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2015, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2015_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2015)


#####################################
###############2016##################
#####################################

####Reading in 2016 processed Nielsen data
Nielsen_RawData_2016 <- fread("Nielsen_2016_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2016 <- distinct(Nielsen_RawData_2016, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2016, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2016_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2016)


#####################################
###############2017##################
#####################################

####Reading in 2017 processed Nielsen data
Nielsen_RawData_2017 <- fread("Nielsen_2017_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2017 <- distinct(Nielsen_RawData_2017, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2017, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2017_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2017)


#####################################
###############2018##################
#####################################

####Reading in 2018 processed Nielsen data
Nielsen_RawData_2018 <- fread("Nielsen_2018_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2018 <- distinct(Nielsen_RawData_2018, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2018, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2018_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2018)



#####################################
###############2019##################
#####################################

####Reading in 2019 processed Nielsen data
Nielsen_RawData_2019 <- fread("Nielsen_2019_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2019 <- distinct(Nielsen_RawData_2019, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2019, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2019_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2019)



#####################################
###############2020##################
#####################################

####Reading in 2020 processed Nielsen data
Nielsen_RawData_2020 <- fread("Nielsen_2020_Aggregated_FirstPass.csv",
                              colClasses = c(store_code_uc = "character", upc = "character", week_end = "character"))

####Outputting unique upcs
UniqueUPCs_2020 <- distinct(Nielsen_RawData_2020, upc) 

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_2020, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2020_UniqueUPCs.csv")

####Removing files from environment
rm(Nielsen_RawData_2020)



#####################################
######Combining unique UPC Files#####
#####################################

# UniqueUPCs_2015 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2015_UniqueUPCs.csv",
#                          colClasses = c(upc = "character"))
# UniqueUPCs_2016 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2016_UniqueUPCs.csv",
#                          colClasses = c(upc = "character"))
# UniqueUPCs_2017 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2017_UniqueUPCs.csv",
#                          colClasses = c(upc = "character"))
# UniqueUPCs_2018 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2018_UniqueUPCs.csv",
#                          colClasses = c(upc = "character"))
# UniqueUPCs_2019 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2019_UniqueUPCs.csv",
#                          colClasses = c(upc = "character"))
# UniqueUPCs_2020 <- fread("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_2020_UniqueUPCs.csv",
#                          colClasses = c(upc = "character"))

##Aggregating the unique UPC files
UniqueUPCs_Aggregate <- rbind(UniqueUPCs_2012, UniqueUPCs_2013, UniqueUPCs_2014, UniqueUPCs_2015, 
                              UniqueUPCs_2016, UniqueUPCs_2017, UniqueUPCs_2018, UniqueUPCs_2019, UniqueUPCs_2020)

##Taking just the unique UPCs
UniqueUPCs_Aggregate <- distinct(UniqueUPCs_Aggregate, upc)

####Writing out unique upc's as a .csv
fwrite(UniqueUPCs_Aggregate, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Unique Nielsen UPCs/Nielsen_Aggregate_UniqueUPCs.csv")



##Determining length of each upc
NielsenUPCs <- mutate(UniqueUPCs_Aggregate, upc_nchar = nchar(upc))

##Determining how many upcs are =12 digits
NielsenUPCs_12 <- filter(NielsenUPCs, upc_nchar == 12) 

