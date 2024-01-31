##libraries and API key
library(tidycensus)
library(tidyverse)
library(haven)
library(data.table)
library(viridis)
census_api_key("7255af21a1b094e0df2452a8e065ecec00435754", install = TRUE, overwrite = TRUE)

##Ceto
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

##############################
###########EXAMPLES###########
##############################

#Basic usage of tidycensus instructions found here: https://walker-data.com/tidycensus/articles/basic-usage.html#searching-for-variables
#get_acs() example
tarr <- get_acs(geography = "zcta", variables = "B19013_001", year = 2020)

#get_decennial() example
vars10 <- c("P005003", "P005004", "P005006", "P004003")
il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "IL", geometry = TRUE)

###############################
###########VARIABLES###########
###############################

# ####Bringing in demographic data from IRI analysis to retrieve variable codes
# IRIDemographicData <- read_dta("D:/FacultyData/Kaplan/SSB Taxes/SF Synthetic Control Analysis/Processed Data/Census Data/demographics.dta")
# IRIDemographicData_KeyVariables <- select(IRIDemographicData, 
#                                           c(hc01_est_vc13, 
#                                             #income,
#                                             hc01_est_vc02, 
#                                             #poverty,
#                                             hd02_s03,
#                                             hd02_s04,
#                                             hd02_s05,
#                                             hd02_s10,
#                                             hd02_s18,
#                                             hd02_s23,
#                                             hd02_s38,
#                                             subhd0201_s23, 
#                                             #p18to64,
#                                             subhd0101_s41)) 
#                                             #age))


#list of variables examples: The function takes two required arguments: 
#the year of the Census or endyear of the ACS sample, and the dataset name, which varies in availability by year.
v17_acs5 <- load_variables(2016, "acs5", cache = TRUE)
v17_acs1 <- load_variables(2016, "acs1", cache = TRUE)
v17_dec_sf1 <- load_variables(2010, "sf1", cache = TRUE)
v17_dec_sf2 <- load_variables(2010, "sf2", cache = TRUE)


###list of variables I need:

#population size (2010) - P001001
#median household income (2016) - B19013_001
#racial composition (proportion non-Hispanic White, non-Hispanic Black, Hispanic, and Asian, 2010) - P008003 to P008008 (need to divide by total population of zcta),
#proportion hispanic or latino (2010) - P005010 (need to divide by total population of zcta)
#proportion in poverty (household income <$10,000K, 2016) - B19001_002 (needs to be divided by total population of zcta),
#proportion aged 18 to 64 (2010) - P043013 (male) and P043044 (female) (needs to be divided by total population of zcta), 
#median age (2010) - P013001, 
#and number of housing units (2010) - H001001."


#####################################
###########DATA EXTRACTION###########
#####################################

##ACS data extraction
vars16 <- c("B19013_001", "B19001_002")
ACS_data <- get_acs(geography = "zcta", 
                                variables = vars16, 
                                year = 2016)
colnames(ACS_data)[4] <- "value"

##Decennial census data extraction
vars10 <- c("P001001", "P008003", "P008004", "P008005", "P008006", "P008007", "P008008", "P005010",
            "P013001", "P043013", "P043044", "H001001", "P002002")
Decennial_data <- get_decennial(geography = "zcta", 
                                variables = vars10, 
                                year = 2010)

Decennial_data <- mutate(Decennial_data, moe = NA)



################################################
########Combining Decennial and ACS data########
################################################

CensusData_Combined <- rbind(ACS_data, Decennial_data)
CensusData_Combined <- pivot_wider(CensusData_Combined, names_from = variable, values_from = c(value, moe))
CensusData_Combined <- dplyr::select(CensusData_Combined, -NAME)

##Creating proportions for certain variables
CensusData_Combined <- mutate(CensusData_Combined,
                              pwhite = value_P008003/value_P001001,
                              pblack = value_P008004/value_P001001,
                              pamindalask = value_P008005/value_P001001,
                              pasian= value_P008006/value_P001001,
                              phawaii = value_P008007/value_P001001,
                              pother = value_P008008/value_P001001,
                              phisp = value_P005010/value_P001001,
                              poverty_10K = value_B19001_002/value_P001001,
                              age_18to64 = (value_P043013 + value_P043044)/value_P001001,
                              perc_urban = value_P002002/value_P001001)

##Renaming other important variables
names(CensusData_Combined)[names(CensusData_Combined) == 'value_B19013_001'] <- 'medHHincome'
names(CensusData_Combined)[names(CensusData_Combined) == 'value_P001001'] <- 'population'
names(CensusData_Combined)[names(CensusData_Combined) == 'value_P013001'] <- 'medage'
names(CensusData_Combined)[names(CensusData_Combined) == 'value_H001001'] <- 'num_housingunits'

##Selecting important variables for merge to store data
CensusData_Combined_Final <- dplyr::select(CensusData_Combined, 
                                    c(GEOID, medHHincome, population, medage, num_housingunits,
                                      pwhite, pblack, pamindalask, pasian, phawaii, pother,
                                      phisp, poverty_10K, age_18to64, perc_urban))


##Aggregate by 3-digit zip code
CensusData_Combined_Final <- mutate(CensusData_Combined_Final,
                              GEOID_3 = substr(GEOID, 1, 3))
CensusData_Combined_Final_3DigitZip <- group_by(CensusData_Combined_Final, GEOID_3) %>%
  dplyr::select(-GEOID) %>%
  summarise_all(mean, na.rm = TRUE)


##Output census data
fwrite(CensusData_Combined_Final_3DigitZip, "Data/CensusData_Combined_Final_3DigitZip.csv")

##Output total population
CensusData_Combined_Final_3DigitZip_Population <- group_by(CensusData_Combined_Final, GEOID_3) %>%
  dplyr::select(population, num_housingunits) %>%
  summarise_all(sum, na.rm = TRUE)

##Output census data for population weighted estimates
fwrite(CensusData_Combined_Final_3DigitZip_Population, "Data/CensusData_Combined_Final_3DigitZip_Population.csv")
