library(data.table)
library(dplyr)

#####################################
###############2012##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2012/1503_2012")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2012/1503_2012")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2012_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], 
                     stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2012_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2012_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2012/1508_2012")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2012/1508_2012")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2012_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2012_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2012_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2012/0507_2012")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2012/0507_2012")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2012_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2012_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2012_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2012_Aggregated <- rbind(Nielsen_2012_1503_CarbonatedBeverages,
                                 Nielsen_2012_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2012_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2012_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2012_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2012_1503_CarbonatedBeverages,
   Nielsen_2012_1508_SoftDrinksNonCarbonated,
   Nielsen_2012_0507_JuiceDrinksCannedBottled,
   Nielsen_2012_Aggregated)




#####################################
###############2013##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2013/1503_2013")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2013/1503_2013")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2013_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], 
                     stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2013_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2013_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2013/1508_2013")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2013/1508_2013")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2013_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2013_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2013_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2013/0507_2013")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2013/0507_2013")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2013_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2013_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2013_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2013_Aggregated <- rbind(Nielsen_2013_1503_CarbonatedBeverages,
                                 Nielsen_2013_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2013_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2013_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2013_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2013_1503_CarbonatedBeverages,
   Nielsen_2013_1508_SoftDrinksNonCarbonated,
   Nielsen_2013_0507_JuiceDrinksCannedBottled,
   Nielsen_2013_Aggregated)




#####################################
###############2014##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2014/1503_2014")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2014/1503_2014")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2014_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], 
                     stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2014_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2014_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2014/1508_2014")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2014/1508_2014")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2014_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2014_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2014_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2014/0507_2014")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep_2012_14/2014/0507_2014")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2014_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2014_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2014_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2014_Aggregated <- rbind(Nielsen_2014_1503_CarbonatedBeverages,
                                 Nielsen_2014_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2014_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2014_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2014_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2014_1503_CarbonatedBeverages,
   Nielsen_2014_1508_SoftDrinksNonCarbonated,
   Nielsen_2014_0507_JuiceDrinksCannedBottled,
   Nielsen_2014_Aggregated)


#####################################
###############2015##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2015/1503_2015")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2015/1503_2015")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2015_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], 
                     stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2015_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2015_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2015/1508_2015")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2015/1508_2015")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2015_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2015_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2015_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2015/0507_2015")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2015/0507_2015")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2015_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2015_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2015_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2015_Aggregated <- rbind(Nielsen_2015_1503_CarbonatedBeverages,
                                 Nielsen_2015_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2015_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2015_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2015_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2015_1503_CarbonatedBeverages,
   Nielsen_2015_1508_SoftDrinksNonCarbonated,
   Nielsen_2015_0507_JuiceDrinksCannedBottled,
   Nielsen_2015_Aggregated)



#####################################
###############2016##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2016/1503_2016")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2016/1503_2016")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2016_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2016_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2016_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2016/1508_2016")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2016/1508_2016")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2016_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2016_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2016_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2016/0507_2016")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2016/0507_2016")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2016_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2016_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2016_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2016_Aggregated <- rbind(Nielsen_2016_1503_CarbonatedBeverages,
                                 Nielsen_2016_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2016_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2016_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2016_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2016_1503_CarbonatedBeverages,
   Nielsen_2016_1508_SoftDrinksNonCarbonated,
   Nielsen_2016_0507_JuiceDrinksCannedBottled,
   Nielsen_2016_Aggregated)



#####################################
###############2017##################
#####################################


####Reading in the ORIGINAL data first####


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory (for the ORIGINAL data)
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Original/1503_2017")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Original/1503_2017")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2017_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2017_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2017_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Original/1508_2017")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Original/1508_2017")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2017_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2017_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2017_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Original/0507_2017")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Original/0507_2017")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2017_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2017_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2017_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2017_Aggregated <- rbind(Nielsen_2017_1503_CarbonatedBeverages,
                                 Nielsen_2017_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2017_0507_JuiceDrinksCannedBottled)

##Taking out the incorrect week
Nielsen_2017_Aggregated <- dplyr::filter(Nielsen_2017_Aggregated, week_end != 20170715)


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2017_1503_CarbonatedBeverages,
   Nielsen_2017_1508_SoftDrinksNonCarbonated,
   Nielsen_2017_0507_JuiceDrinksCannedBottled)



##############################################
####Reading in the CORRECTED WEEK data now####
##############################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory (for the ORIGINAL data)
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Corrected_Week/1503_2017")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Corrected_Week/1503_2017")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2017_1503_CarbonatedBeverages_CorrectedWeek <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2017_1503_CarbonatedBeverages_CorrectedWeek <- rbindlist(list(Nielsen_2017_1503_CarbonatedBeverages_CorrectedWeek, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Corrected_Week/1508_2017")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Corrected_Week/1508_2017")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2017_1508_SoftDrinksNonCarbonated_CorrectedWeek <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2017_1508_SoftDrinksNonCarbonated_CorrectedWeek <- rbindlist(list(Nielsen_2017_1508_SoftDrinksNonCarbonated_CorrectedWeek, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Corrected_Week/0507_2017")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2017/Corrected_Week/0507_2017")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2017_0507_JuiceDrinksCannedBottled_CorrectedWeek <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2017_0507_JuiceDrinksCannedBottled_CorrectedWeek <- rbindlist(list(Nielsen_2017_0507_JuiceDrinksCannedBottled_CorrectedWeek, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2017_Aggregated_CorrectedWeek <- rbind(Nielsen_2017_1503_CarbonatedBeverages_CorrectedWeek,
                                 Nielsen_2017_1508_SoftDrinksNonCarbonated_CorrectedWeek,
                                 Nielsen_2017_0507_JuiceDrinksCannedBottled_CorrectedWeek)

##Merging in corrected week data
Nielsen_2017_Aggregated <- rbind(Nielsen_2017_Aggregated, Nielsen_2017_Aggregated_CorrectedWeek)

fwrite(Nielsen_2017_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2017_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

##CORRECTED WEEK##

rm(temp_data, 
   Nielsen_2017_1503_CarbonatedBeverages_CorrectedWeek,
   Nielsen_2017_1508_SoftDrinksNonCarbonated_CorrectedWeek,
   Nielsen_2017_0507_JuiceDrinksCannedBottled_CorrectedWeek,
   Nielsen_2017_Aggregated_CorrectedWeek,
   Nielsen_2017_Aggregated)



#####################################
###############2018##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2018/1503_2018")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2018/1503_2018")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2018_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2018_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2018_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2018/1508_2018")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2018/1508_2018")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2018_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2018_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2018_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2018/0507_2018")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2018/0507_2018")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2018_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2018_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2018_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2018_Aggregated <- rbind(Nielsen_2018_1503_CarbonatedBeverages,
                                 Nielsen_2018_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2018_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2018_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2018_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2018_1503_CarbonatedBeverages,
   Nielsen_2018_1508_SoftDrinksNonCarbonated,
   Nielsen_2018_0507_JuiceDrinksCannedBottled,
   Nielsen_2018_Aggregated)



#####################################
###############2019##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2019/1503_2019")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2019/1503_2019")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2019_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2019_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2019_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2019/1508_2019")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2019/1508_2019")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2019_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2019_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2019_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2019/0507_2019")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2019/0507_2019")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2019_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2019_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2019_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2019_Aggregated <- rbind(Nielsen_2019_1503_CarbonatedBeverages,
                                 Nielsen_2019_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2019_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2019_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2019_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2019_1503_CarbonatedBeverages,
   Nielsen_2019_1508_SoftDrinksNonCarbonated,
   Nielsen_2019_0507_JuiceDrinksCannedBottled,
   Nielsen_2019_Aggregated)




#####################################
###############2020##################
#####################################


#################################
###1503 - Carbonated Beverages###
#################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2020/1503_2020")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2020/1503_2020")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2020_1503_CarbonatedBeverages <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2020_1503_CarbonatedBeverages <- rbindlist(list(Nielsen_2020_1503_CarbonatedBeverages, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#########################################
###1508 - Soft Drinks - Non-Carbonated###
#########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2020/1508_2020")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2020/1508_2020")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2020_1508_SoftDrinksNonCarbonated <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2020_1508_SoftDrinksNonCarbonated <- rbindlist(list(Nielsen_2020_1508_SoftDrinksNonCarbonated, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##########################################
###0507 - Juice Drinks - Canned/Bottled###
##########################################

##Setting the working directory
setwd("S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2020/0507_2020")

##Creating a file list of all files to read in
file_list <- list.files(path="S:/Kaplan/SSB Taxes/Nielsen Raw Data/Files_to_Keep/2020/0507_2020")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
Nielsen_2020_0507_JuiceDrinksCannedBottled <- data.frame()

##Reading in Data
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F,
                     colClasses = c(store_code_uc = "character", upc = "character", week_end = "character")) #read in files using the fread function from the data.table package
  Nielsen_2020_0507_JuiceDrinksCannedBottled <- rbindlist(list(Nielsen_2020_0507_JuiceDrinksCannedBottled, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


#####################
##Aggregating Files##
#####################

Nielsen_2020_Aggregated <- rbind(Nielsen_2020_1503_CarbonatedBeverages,
                                 Nielsen_2020_1508_SoftDrinksNonCarbonated,
                                 Nielsen_2020_0507_JuiceDrinksCannedBottled)

fwrite(Nielsen_2020_Aggregated, "S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Aggregated Nielsen Files_No Characteristics/Nielsen_2020_Aggregated_FirstPass.csv")


#########################
##Removing Stored Files##
#########################

rm(temp_data, 
   Nielsen_2020_1503_CarbonatedBeverages,
   Nielsen_2020_1508_SoftDrinksNonCarbonated,
   Nielsen_2020_0507_JuiceDrinksCannedBottled,
   Nielsen_2020_Aggregated)