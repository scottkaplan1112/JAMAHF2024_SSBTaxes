# devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
library(reshape)
library(data.table)
library(tm)
library(dplyr)
library(ggpubr)

#Ceto
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

########################################
#################ACTUAL#################
########################################

################################################
###############Reading in Data##################
################################################

##Weights - Quantity
Weights_Quantity <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_weights_AllTaxed.csv")

##Weights - Price
Weights_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_weights_AllTaxed_prices.csv")

##Long DF - Quantity
LongDF_Quantity <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed.csv")

##Long DF - Price
LongDF_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed_prices.csv")

##Census Data
CensusData_Combined_Final_3DigitZip <- fread("Data/CensusData_Combined_Final_3DigitZip.csv")
CensusData <- filter(CensusData_Combined_Final_3DigitZip, GEOID_3 %in% LongDF_Quantity$store_zip3)
names(CensusData)[names(CensusData) == "GEOID_3"] <- "store_zip3"

#List of taxed and border zip codes 
taxed_zips <- c("946", "981", "803", "191", "941")
border_zips <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                 "81", "80", "948", "945")


###################################################################
###############Creating Treated + Synthetic Data###################
###################################################################


##OUTCOMES##

Outcomes_Quantity_Treated <- filter(LongDF_Quantity, store_zip3 %in% taxed_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PretreatmentMean_Quantity_Treated = mean(TotalOz_zipcode))

Outcomes_Prices_Treated <- filter(LongDF_Prices, store_zip3 %in% taxed_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PretreatmentMean_Prices_Treated = mean(AvgPrice_PerOz_zipcode))

Outcomes_Quantity_Synthetic <- filter(LongDF_Quantity, !(store_zip3 %in% taxed_zips)) %>%
  group_by(store_zip3) %>%
  summarise(PretreatmentMean_Quantity = mean(TotalOz_zipcode)) %>%
  left_join(Weights_Quantity, by = c("store_zip3" = "store_zip3")) %>%
  summarise(PretreatmentMean_Quantity_Synthetic_Philly = weighted.mean(PretreatmentMean_Quantity, Philadelphia),
            PretreatmentMean_Quantity_Synthetic_Boulder = weighted.mean(PretreatmentMean_Quantity, Boulder),
            PretreatmentMean_Quantity_Synthetic_Oakland = weighted.mean(PretreatmentMean_Quantity, Oakland),
            PretreatmentMean_Quantity_Synthetic_SF = weighted.mean(PretreatmentMean_Quantity, SF),
            PretreatmentMean_Quantity_Synthetic_Seattle = weighted.mean(PretreatmentMean_Quantity, Seattle))

Outcomes_Prices_Synthetic <- filter(LongDF_Prices, !(store_zip3 %in% taxed_zips)) %>%
  group_by(store_zip3) %>%
  summarise(PretreatmentMean_Prices = mean(AvgPrice_PerOz_zipcode)) %>%
  left_join(Weights_Prices, by = c("store_zip3" = "store_zip3")) %>%
  summarise(PretreatmentMean_Prices_Synthetic_Philly = weighted.mean(PretreatmentMean_Prices, Philadelphia),
            PretreatmentMean_Prices_Synthetic_Boulder = weighted.mean(PretreatmentMean_Prices, Boulder),
            PretreatmentMean_Prices_Synthetic_SF = weighted.mean(PretreatmentMean_Prices, SF),
            PretreatmentMean_Prices_Synthetic_Oakland = weighted.mean(PretreatmentMean_Prices, Oakland),
            PretreatmentMean_Prices_Synthetic_Seattle = weighted.mean(PretreatmentMean_Prices, Seattle))

Outcomes_Quantity <- mutate(Outcomes_Quantity_Treated, PretreatmentMean_Quantity_Synthetic = NA)
Outcomes_Quantity$PretreatmentMean_Quantity_Synthetic[1] <- Outcomes_Quantity_Synthetic$PretreatmentMean_Quantity_Synthetic_Philly
Outcomes_Quantity$PretreatmentMean_Quantity_Synthetic[2] <- Outcomes_Quantity_Synthetic$PretreatmentMean_Quantity_Synthetic_Boulder
Outcomes_Quantity$PretreatmentMean_Quantity_Synthetic[3] <- Outcomes_Quantity_Synthetic$PretreatmentMean_Quantity_Synthetic_SF
Outcomes_Quantity$PretreatmentMean_Quantity_Synthetic[4] <- Outcomes_Quantity_Synthetic$PretreatmentMean_Quantity_Synthetic_Oakland
Outcomes_Quantity$PretreatmentMean_Quantity_Synthetic[5] <- Outcomes_Quantity_Synthetic$PretreatmentMean_Quantity_Synthetic_Seattle

Outcomes_Prices <- mutate(Outcomes_Prices_Treated, PretreatmentMean_Prices_Synthetic = NA)
Outcomes_Prices$PretreatmentMean_Prices_Synthetic[1] <- Outcomes_Prices_Synthetic$PretreatmentMean_Prices_Synthetic_Philly
Outcomes_Prices$PretreatmentMean_Prices_Synthetic[2] <- Outcomes_Prices_Synthetic$PretreatmentMean_Prices_Synthetic_Boulder
Outcomes_Prices$PretreatmentMean_Prices_Synthetic[3] <- Outcomes_Prices_Synthetic$PretreatmentMean_Prices_Synthetic_SF
Outcomes_Prices$PretreatmentMean_Prices_Synthetic[4] <- Outcomes_Prices_Synthetic$PretreatmentMean_Prices_Synthetic_Oakland
Outcomes_Prices$PretreatmentMean_Prices_Synthetic[5] <- Outcomes_Prices_Synthetic$PretreatmentMean_Prices_Synthetic_Seattle


##COVARIATES##
Covariates_Treated <- filter(CensusData, store_zip3 %in% taxed_zips) %>%
  group_by(store_zip3) %>%
  summarise(population_Treated = mean(population),
            medage_Treated = mean(medage),
            num_housingunits_Treated = mean(num_housingunits),
            pwhite_Treated = mean(pwhite),
            pblack_Treated = mean(pblack),
            pamindalask_Treated = mean(pamindalask),
            pasian_Treated = mean(pasian),
            phawaii_Treated = mean(phawaii),
            pother_Treated = mean(pother),
            phisp_Treated = mean(phisp),
            poverty_10K_Treated = mean(poverty_10K),
            age_18to64_Treated = mean(age_18to64),
            perc_urban_Treated = mean(perc_urban))

Covariates_Synthetic_Quantity <- filter(CensusData, store_zip3 %in% LongDF_Quantity$store_zip3 & !(store_zip3 %in% taxed_zips)) %>%
  group_by(store_zip3) %>%
  summarise(population_Synthetic = mean(population),
            medage_Synthetic = mean(medage),
            num_housingunits_Synthetic = mean(num_housingunits),
            pwhite_Synthetic = mean(pwhite),
            pblack_Synthetic = mean(pblack),
            pamindalask_Synthetic = mean(pamindalask),
            pasian_Synthetic = mean(pasian),
            phawaii_Synthetic = mean(phawaii),
            pother_Synthetic = mean(pother),
            phisp_Synthetic = mean(phisp),
            poverty_10K_Synthetic = mean(poverty_10K),
            age_18to64_Synthetic = mean(age_18to64),
            perc_urban_Synthetic = mean(perc_urban)) %>%
  left_join(Weights_Quantity, by = c("store_zip3" = "store_zip3")) %>%
  summarise(population_Synthetic_Philly = weighted.mean(population_Synthetic, Philadelphia),
            population_Synthetic_Boulder = weighted.mean(population_Synthetic, Boulder),
            population_Synthetic_SF = weighted.mean(population_Synthetic, SF),
            population_Synthetic_Oakland = weighted.mean(population_Synthetic, Oakland),
            population_Synthetic_Seattle= weighted.mean(population_Synthetic, Seattle),
            medage_Synthetic_Philly = weighted.mean(medage_Synthetic, Philadelphia),
            medage_Synthetic_Boulder = weighted.mean(medage_Synthetic, Boulder),
            medage_Synthetic_SF = weighted.mean(medage_Synthetic, SF),
            medage_Synthetic_Oakland = weighted.mean(medage_Synthetic, Oakland),
            medage_Synthetic_Seattle= weighted.mean(medage_Synthetic, Seattle),
            num_housingunits_Synthetic_Philly = weighted.mean(num_housingunits_Synthetic, Philadelphia),
            num_housingunits_Synthetic_Boulder = weighted.mean(num_housingunits_Synthetic, Boulder),
            num_housingunits_Synthetic_SF = weighted.mean(num_housingunits_Synthetic, SF),
            num_housingunits_Synthetic_Oakland = weighted.mean(num_housingunits_Synthetic, Oakland),
            num_housingunits_Synthetic_Seattle= weighted.mean(num_housingunits_Synthetic, Seattle),
            pwhite_Synthetic_Philly = weighted.mean(pwhite_Synthetic, Philadelphia),
            pwhite_Synthetic_Boulder = weighted.mean(pwhite_Synthetic, Boulder),
            pwhite_Synthetic_SF = weighted.mean(pwhite_Synthetic, SF),
            pwhite_Synthetic_Oakland = weighted.mean(pwhite_Synthetic, Oakland),
            pwhite_Synthetic_Seattle= weighted.mean(pwhite_Synthetic, Seattle),
            pblack_Synthetic_Philly = weighted.mean(pblack_Synthetic, Philadelphia),
            pblack_Synthetic_Boulder = weighted.mean(pblack_Synthetic, Boulder),
            pblack_Synthetic_SF = weighted.mean(pblack_Synthetic, SF),
            pblack_Synthetic_Oakland = weighted.mean(pblack_Synthetic, Oakland),
            pblack_Synthetic_Seattle= weighted.mean(pblack_Synthetic, Seattle),
            pamindalask_Synthetic_Philly = weighted.mean(pamindalask_Synthetic, Philadelphia),
            pamindalask_Synthetic_Boulder = weighted.mean(pamindalask_Synthetic, Boulder),
            pamindalask_Synthetic_SF = weighted.mean(pamindalask_Synthetic, SF),
            pamindalask_Synthetic_Oakland = weighted.mean(pamindalask_Synthetic, Oakland),
            pamindalask_Synthetic_Seattle= weighted.mean(pamindalask_Synthetic, Seattle),
            pasian_Synthetic_Philly = weighted.mean(pasian_Synthetic, Philadelphia),
            pasian_Synthetic_Boulder = weighted.mean(pasian_Synthetic, Boulder),
            pasian_Synthetic_SF = weighted.mean(pasian_Synthetic, SF),
            pasian_Synthetic_Oakland = weighted.mean(pasian_Synthetic, Oakland),
            pasian_Synthetic_Seattle= weighted.mean(pasian_Synthetic, Seattle),
            phawaii_Synthetic_Philly = weighted.mean(phawaii_Synthetic, Philadelphia),
            phawaii_Synthetic_Boulder = weighted.mean(phawaii_Synthetic, Boulder),
            phawaii_Synthetic_SF = weighted.mean(phawaii_Synthetic, SF),
            phawaii_Synthetic_Oakland = weighted.mean(phawaii_Synthetic, Oakland),
            phawaii_Synthetic_Seattle= weighted.mean(phawaii_Synthetic, Seattle),
            pother_Synthetic_Philly = weighted.mean(pother_Synthetic, Philadelphia),
            pother_Synthetic_Boulder = weighted.mean(pother_Synthetic, Boulder),
            pother_Synthetic_SF = weighted.mean(pother_Synthetic, SF),
            pother_Synthetic_Oakland = weighted.mean(pother_Synthetic, Oakland),
            pother_Synthetic_Seattle= weighted.mean(pother_Synthetic, Seattle),
            phisp_Synthetic_Philly = weighted.mean(phisp_Synthetic, Philadelphia),
            phisp_Synthetic_Boulder = weighted.mean(phisp_Synthetic, Boulder),
            phisp_Synthetic_SF = weighted.mean(phisp_Synthetic, SF),
            phisp_Synthetic_Oakland = weighted.mean(phisp_Synthetic, Oakland),
            phisp_Synthetic_Seattle= weighted.mean(phisp_Synthetic, Seattle),
            poverty_10K_Synthetic_Philly = weighted.mean(poverty_10K_Synthetic, Philadelphia),
            poverty_10K_Synthetic_Boulder = weighted.mean(poverty_10K_Synthetic, Boulder),
            poverty_10K_Synthetic_SF = weighted.mean(poverty_10K_Synthetic, SF),
            poverty_10K_Synthetic_Oakland = weighted.mean(poverty_10K_Synthetic, Oakland),
            poverty_10K_Synthetic_Seattle= weighted.mean(poverty_10K_Synthetic, Seattle),
            age_18to64_Synthetic_Philly = weighted.mean(age_18to64_Synthetic, Philadelphia),
            age_18to64_Synthetic_Boulder = weighted.mean(age_18to64_Synthetic, Boulder),
            age_18to64_Synthetic_SF = weighted.mean(age_18to64_Synthetic, SF),
            age_18to64_Synthetic_Oakland = weighted.mean(age_18to64_Synthetic, Oakland),
            age_18to64_Synthetic_Seattle= weighted.mean(age_18to64_Synthetic, Seattle),
            perc_urban_Synthetic_Philly = weighted.mean(perc_urban_Synthetic, Philadelphia),
            perc_urban_Synthetic_Boulder = weighted.mean(perc_urban_Synthetic, Boulder),
            perc_urban_Synthetic_SF = weighted.mean(perc_urban_Synthetic, SF),
            perc_urban_Synthetic_Oakland = weighted.mean(perc_urban_Synthetic, Oakland),
            perc_urban_Synthetic_Seattle= weighted.mean(perc_urban_Synthetic, Seattle))

#Merging into treated covariate summary stats
Covariates_Quantity <- Covariates_Treated
Covariates_Quantity$population_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$population_Synthetic_Philly
Covariates_Quantity$population_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$population_Synthetic_Boulder
Covariates_Quantity$population_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$population_Synthetic_SF
Covariates_Quantity$population_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$population_Synthetic_Oakland
Covariates_Quantity$population_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$population_Synthetic_Seattle
Covariates_Quantity$medHHincome_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$medHHincome_Synthetic_Philly
Covariates_Quantity$medHHincome_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$medHHincome_Synthetic_Boulder
Covariates_Quantity$medHHincome_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$medHHincome_Synthetic_SF
Covariates_Quantity$medHHincome_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$medHHincome_Synthetic_Oakland
Covariates_Quantity$medHHincome_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$medHHincome_Synthetic_Seattle
Covariates_Quantity$medage_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$medage_Synthetic_Philly
Covariates_Quantity$medage_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$medage_Synthetic_Boulder
Covariates_Quantity$medage_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$medage_Synthetic_SF
Covariates_Quantity$medage_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$medage_Synthetic_Oakland
Covariates_Quantity$medage_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$medage_Synthetic_Seattle
Covariates_Quantity$num_housingunits_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$num_housingunits_Synthetic_Philly
Covariates_Quantity$num_housingunits_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$num_housingunits_Synthetic_Boulder
Covariates_Quantity$num_housingunits_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$num_housingunits_Synthetic_SF
Covariates_Quantity$num_housingunits_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$num_housingunits_Synthetic_Oakland
Covariates_Quantity$num_housingunits_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$num_housingunits_Synthetic_Seattle
Covariates_Quantity$pwhite_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$pwhite_Synthetic_Philly
Covariates_Quantity$pwhite_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$pwhite_Synthetic_Boulder
Covariates_Quantity$pwhite_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$pwhite_Synthetic_SF
Covariates_Quantity$pwhite_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$pwhite_Synthetic_Oakland
Covariates_Quantity$pwhite_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$pwhite_Synthetic_Seattle
Covariates_Quantity$pblack_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$pblack_Synthetic_Philly
Covariates_Quantity$pblack_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$pblack_Synthetic_Boulder
Covariates_Quantity$pblack_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$pblack_Synthetic_SF
Covariates_Quantity$pblack_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$pblack_Synthetic_Oakland
Covariates_Quantity$pblack_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$pblack_Synthetic_Seattle
Covariates_Quantity$pamindalask_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$pamindalask_Synthetic_Philly
Covariates_Quantity$pamindalask_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$pamindalask_Synthetic_Boulder
Covariates_Quantity$pamindalask_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$pamindalask_Synthetic_SF
Covariates_Quantity$pamindalask_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$pamindalask_Synthetic_Oakland
Covariates_Quantity$pamindalask_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$pamindalask_Synthetic_Seattle
Covariates_Quantity$pasian_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$pasian_Synthetic_Philly
Covariates_Quantity$pasian_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$pasian_Synthetic_Boulder
Covariates_Quantity$pasian_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$pasian_Synthetic_SF
Covariates_Quantity$pasian_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$pasian_Synthetic_Oakland
Covariates_Quantity$pasian_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$pasian_Synthetic_Seattle
Covariates_Quantity$phawaii_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$phawaii_Synthetic_Philly
Covariates_Quantity$phawaii_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$phawaii_Synthetic_Boulder
Covariates_Quantity$phawaii_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$phawaii_Synthetic_SF
Covariates_Quantity$phawaii_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$phawaii_Synthetic_Oakland
Covariates_Quantity$phawaii_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$phawaii_Synthetic_Seattle
Covariates_Quantity$pother_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$pother_Synthetic_Philly
Covariates_Quantity$pother_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$pother_Synthetic_Boulder
Covariates_Quantity$pother_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$pother_Synthetic_SF
Covariates_Quantity$pother_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$pother_Synthetic_Oakland
Covariates_Quantity$pother_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$pother_Synthetic_Seattle
Covariates_Quantity$phisp_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$phisp_Synthetic_Philly
Covariates_Quantity$phisp_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$phisp_Synthetic_Boulder
Covariates_Quantity$phisp_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$phisp_Synthetic_SF
Covariates_Quantity$phisp_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$phisp_Synthetic_Oakland
Covariates_Quantity$phisp_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$phisp_Synthetic_Seattle
Covariates_Quantity$poverty_10K_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$poverty_10K_Synthetic_Philly
Covariates_Quantity$poverty_10K_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$poverty_10K_Synthetic_Boulder
Covariates_Quantity$poverty_10K_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$poverty_10K_Synthetic_SF
Covariates_Quantity$poverty_10K_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$poverty_10K_Synthetic_Oakland
Covariates_Quantity$poverty_10K_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$poverty_10K_Synthetic_Seattle
Covariates_Quantity$age_18to64_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$age_18to64_Synthetic_Philly
Covariates_Quantity$age_18to64_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$age_18to64_Synthetic_Boulder
Covariates_Quantity$age_18to64_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$age_18to64_Synthetic_SF
Covariates_Quantity$age_18to64_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$age_18to64_Synthetic_Oakland
Covariates_Quantity$age_18to64_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$age_18to64_Synthetic_Seattle
Covariates_Quantity$perc_urban_Quantity_Synthetic[1] <- Covariates_Synthetic_Quantity$perc_urban_Synthetic_Philly
Covariates_Quantity$perc_urban_Quantity_Synthetic[2] <- Covariates_Synthetic_Quantity$perc_urban_Synthetic_Boulder
Covariates_Quantity$perc_urban_Quantity_Synthetic[3] <- Covariates_Synthetic_Quantity$perc_urban_Synthetic_SF
Covariates_Quantity$perc_urban_Quantity_Synthetic[4] <- Covariates_Synthetic_Quantity$perc_urban_Synthetic_Oakland
Covariates_Quantity$perc_urban_Quantity_Synthetic[5] <- Covariates_Synthetic_Quantity$perc_urban_Synthetic_Seattle


Covariates_Synthetic_Prices <- filter(CensusData, store_zip3 %in% LongDF_Prices$store_zip3 & !(store_zip3 %in% taxed_zips)) %>%
  group_by(store_zip3) %>%
  summarise(population_Synthetic = mean(population),
            medage_Synthetic = mean(medage),
            num_housingunits_Synthetic = mean(num_housingunits),
            pwhite_Synthetic = mean(pwhite),
            pblack_Synthetic = mean(pblack),
            pamindalask_Synthetic = mean(pamindalask),
            pasian_Synthetic = mean(pasian),
            phawaii_Synthetic = mean(phawaii),
            pother_Synthetic = mean(pother),
            phisp_Synthetic = mean(phisp),
            poverty_10K_Synthetic = mean(poverty_10K),
            age_18to64_Synthetic = mean(age_18to64),
            perc_urban_Synthetic = mean(perc_urban)) %>%
  left_join(Weights_Prices, by = c("store_zip3" = "store_zip3")) %>%
  summarise(population_Synthetic_Philly = weighted.mean(population_Synthetic, Philadelphia),
            population_Synthetic_Boulder = weighted.mean(population_Synthetic, Boulder),
            population_Synthetic_SF = weighted.mean(population_Synthetic, SF),
            population_Synthetic_Oakland = weighted.mean(population_Synthetic, Oakland),
            population_Synthetic_Seattle= weighted.mean(population_Synthetic, Seattle),
            medage_Synthetic_Philly = weighted.mean(medage_Synthetic, Philadelphia),
            medage_Synthetic_Boulder = weighted.mean(medage_Synthetic, Boulder),
            medage_Synthetic_SF = weighted.mean(medage_Synthetic, SF),
            medage_Synthetic_Oakland = weighted.mean(medage_Synthetic, Oakland),
            medage_Synthetic_Seattle= weighted.mean(medage_Synthetic, Seattle),
            num_housingunits_Synthetic_Philly = weighted.mean(num_housingunits_Synthetic, Philadelphia),
            num_housingunits_Synthetic_Boulder = weighted.mean(num_housingunits_Synthetic, Boulder),
            num_housingunits_Synthetic_SF = weighted.mean(num_housingunits_Synthetic, SF),
            num_housingunits_Synthetic_Oakland = weighted.mean(num_housingunits_Synthetic, Oakland),
            num_housingunits_Synthetic_Seattle= weighted.mean(num_housingunits_Synthetic, Seattle),
            pwhite_Synthetic_Philly = weighted.mean(pwhite_Synthetic, Philadelphia),
            pwhite_Synthetic_Boulder = weighted.mean(pwhite_Synthetic, Boulder),
            pwhite_Synthetic_SF = weighted.mean(pwhite_Synthetic, SF),
            pwhite_Synthetic_Oakland = weighted.mean(pwhite_Synthetic, Oakland),
            pwhite_Synthetic_Seattle= weighted.mean(pwhite_Synthetic, Seattle),
            pblack_Synthetic_Philly = weighted.mean(pblack_Synthetic, Philadelphia),
            pblack_Synthetic_Boulder = weighted.mean(pblack_Synthetic, Boulder),
            pblack_Synthetic_SF = weighted.mean(pblack_Synthetic, SF),
            pblack_Synthetic_Oakland = weighted.mean(pblack_Synthetic, Oakland),
            pblack_Synthetic_Seattle= weighted.mean(pblack_Synthetic, Seattle),
            pamindalask_Synthetic_Philly = weighted.mean(pamindalask_Synthetic, Philadelphia),
            pamindalask_Synthetic_Boulder = weighted.mean(pamindalask_Synthetic, Boulder),
            pamindalask_Synthetic_SF = weighted.mean(pamindalask_Synthetic, SF),
            pamindalask_Synthetic_Oakland = weighted.mean(pamindalask_Synthetic, Oakland),
            pamindalask_Synthetic_Seattle= weighted.mean(pamindalask_Synthetic, Seattle),
            pasian_Synthetic_Philly = weighted.mean(pasian_Synthetic, Philadelphia),
            pasian_Synthetic_Boulder = weighted.mean(pasian_Synthetic, Boulder),
            pasian_Synthetic_SF = weighted.mean(pasian_Synthetic, SF),
            pasian_Synthetic_Oakland = weighted.mean(pasian_Synthetic, Oakland),
            pasian_Synthetic_Seattle= weighted.mean(pasian_Synthetic, Seattle),
            phawaii_Synthetic_Philly = weighted.mean(phawaii_Synthetic, Philadelphia),
            phawaii_Synthetic_Boulder = weighted.mean(phawaii_Synthetic, Boulder),
            phawaii_Synthetic_SF = weighted.mean(phawaii_Synthetic, SF),
            phawaii_Synthetic_Oakland = weighted.mean(phawaii_Synthetic, Oakland),
            phawaii_Synthetic_Seattle= weighted.mean(phawaii_Synthetic, Seattle),
            pother_Synthetic_Philly = weighted.mean(pother_Synthetic, Philadelphia),
            pother_Synthetic_Boulder = weighted.mean(pother_Synthetic, Boulder),
            pother_Synthetic_SF = weighted.mean(pother_Synthetic, SF),
            pother_Synthetic_Oakland = weighted.mean(pother_Synthetic, Oakland),
            pother_Synthetic_Seattle= weighted.mean(pother_Synthetic, Seattle),
            phisp_Synthetic_Philly = weighted.mean(phisp_Synthetic, Philadelphia),
            phisp_Synthetic_Boulder = weighted.mean(phisp_Synthetic, Boulder),
            phisp_Synthetic_SF = weighted.mean(phisp_Synthetic, SF),
            phisp_Synthetic_Oakland = weighted.mean(phisp_Synthetic, Oakland),
            phisp_Synthetic_Seattle= weighted.mean(phisp_Synthetic, Seattle),
            poverty_10K_Synthetic_Philly = weighted.mean(poverty_10K_Synthetic, Philadelphia),
            poverty_10K_Synthetic_Boulder = weighted.mean(poverty_10K_Synthetic, Boulder),
            poverty_10K_Synthetic_SF = weighted.mean(poverty_10K_Synthetic, SF),
            poverty_10K_Synthetic_Oakland = weighted.mean(poverty_10K_Synthetic, Oakland),
            poverty_10K_Synthetic_Seattle= weighted.mean(poverty_10K_Synthetic, Seattle),
            age_18to64_Synthetic_Philly = weighted.mean(age_18to64_Synthetic, Philadelphia),
            age_18to64_Synthetic_Boulder = weighted.mean(age_18to64_Synthetic, Boulder),
            age_18to64_Synthetic_SF = weighted.mean(age_18to64_Synthetic, SF),
            age_18to64_Synthetic_Oakland = weighted.mean(age_18to64_Synthetic, Oakland),
            age_18to64_Synthetic_Seattle= weighted.mean(age_18to64_Synthetic, Seattle),
            perc_urban_Synthetic_Philly = weighted.mean(perc_urban_Synthetic, Philadelphia),
            perc_urban_Synthetic_Boulder = weighted.mean(perc_urban_Synthetic, Boulder),
            perc_urban_Synthetic_SF = weighted.mean(perc_urban_Synthetic, SF),
            perc_urban_Synthetic_Oakland = weighted.mean(perc_urban_Synthetic, Oakland),
            perc_urban_Synthetic_Seattle= weighted.mean(perc_urban_Synthetic, Seattle))

#Merging into treated covariate summary stats
Covariates_Prices <- Covariates_Treated
Covariates_Prices$population_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$population_Synthetic_Philly
Covariates_Prices$population_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$population_Synthetic_Boulder
Covariates_Prices$population_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$population_Synthetic_SF
Covariates_Prices$population_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$population_Synthetic_Oakland
Covariates_Prices$population_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$population_Synthetic_Seattle
Covariates_Prices$medHHincome_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$medHHincome_Synthetic_Philly
Covariates_Prices$medHHincome_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$medHHincome_Synthetic_Boulder
Covariates_Prices$medHHincome_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$medHHincome_Synthetic_SF
Covariates_Prices$medHHincome_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$medHHincome_Synthetic_Oakland
Covariates_Prices$medHHincome_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$medHHincome_Synthetic_Seattle
Covariates_Prices$medage_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$medage_Synthetic_Philly
Covariates_Prices$medage_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$medage_Synthetic_Boulder
Covariates_Prices$medage_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$medage_Synthetic_SF
Covariates_Prices$medage_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$medage_Synthetic_Oakland
Covariates_Prices$medage_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$medage_Synthetic_Seattle
Covariates_Prices$num_housingunits_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$num_housingunits_Synthetic_Philly
Covariates_Prices$num_housingunits_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$num_housingunits_Synthetic_Boulder
Covariates_Prices$num_housingunits_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$num_housingunits_Synthetic_SF
Covariates_Prices$num_housingunits_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$num_housingunits_Synthetic_Oakland
Covariates_Prices$num_housingunits_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$num_housingunits_Synthetic_Seattle
Covariates_Prices$pwhite_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$pwhite_Synthetic_Philly
Covariates_Prices$pwhite_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$pwhite_Synthetic_Boulder
Covariates_Prices$pwhite_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$pwhite_Synthetic_SF
Covariates_Prices$pwhite_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$pwhite_Synthetic_Oakland
Covariates_Prices$pwhite_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$pwhite_Synthetic_Seattle
Covariates_Prices$pblack_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$pblack_Synthetic_Philly
Covariates_Prices$pblack_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$pblack_Synthetic_Boulder
Covariates_Prices$pblack_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$pblack_Synthetic_SF
Covariates_Prices$pblack_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$pblack_Synthetic_Oakland
Covariates_Prices$pblack_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$pblack_Synthetic_Seattle
Covariates_Prices$pamindalask_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$pamindalask_Synthetic_Philly
Covariates_Prices$pamindalask_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$pamindalask_Synthetic_Boulder
Covariates_Prices$pamindalask_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$pamindalask_Synthetic_SF
Covariates_Prices$pamindalask_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$pamindalask_Synthetic_Oakland
Covariates_Prices$pamindalask_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$pamindalask_Synthetic_Seattle
Covariates_Prices$pasian_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$pasian_Synthetic_Philly
Covariates_Prices$pasian_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$pasian_Synthetic_Boulder
Covariates_Prices$pasian_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$pasian_Synthetic_SF
Covariates_Prices$pasian_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$pasian_Synthetic_Oakland
Covariates_Prices$pasian_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$pasian_Synthetic_Seattle
Covariates_Prices$phawaii_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$phawaii_Synthetic_Philly
Covariates_Prices$phawaii_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$phawaii_Synthetic_Boulder
Covariates_Prices$phawaii_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$phawaii_Synthetic_SF
Covariates_Prices$phawaii_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$phawaii_Synthetic_Oakland
Covariates_Prices$phawaii_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$phawaii_Synthetic_Seattle
Covariates_Prices$pother_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$pother_Synthetic_Philly
Covariates_Prices$pother_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$pother_Synthetic_Boulder
Covariates_Prices$pother_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$pother_Synthetic_SF
Covariates_Prices$pother_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$pother_Synthetic_Oakland
Covariates_Prices$pother_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$pother_Synthetic_Seattle
Covariates_Prices$phisp_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$phisp_Synthetic_Philly
Covariates_Prices$phisp_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$phisp_Synthetic_Boulder
Covariates_Prices$phisp_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$phisp_Synthetic_SF
Covariates_Prices$phisp_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$phisp_Synthetic_Oakland
Covariates_Prices$phisp_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$phisp_Synthetic_Seattle
Covariates_Prices$poverty_10K_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$poverty_10K_Synthetic_Philly
Covariates_Prices$poverty_10K_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$poverty_10K_Synthetic_Boulder
Covariates_Prices$poverty_10K_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$poverty_10K_Synthetic_SF
Covariates_Prices$poverty_10K_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$poverty_10K_Synthetic_Oakland
Covariates_Prices$poverty_10K_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$poverty_10K_Synthetic_Seattle
Covariates_Prices$age_18to64_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$age_18to64_Synthetic_Philly
Covariates_Prices$age_18to64_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$age_18to64_Synthetic_Boulder
Covariates_Prices$age_18to64_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$age_18to64_Synthetic_SF
Covariates_Prices$age_18to64_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$age_18to64_Synthetic_Oakland
Covariates_Prices$age_18to64_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$age_18to64_Synthetic_Seattle
Covariates_Prices$perc_urban_Prices_Synthetic[1] <- Covariates_Synthetic_Prices$perc_urban_Synthetic_Philly
Covariates_Prices$perc_urban_Prices_Synthetic[2] <- Covariates_Synthetic_Prices$perc_urban_Synthetic_Boulder
Covariates_Prices$perc_urban_Prices_Synthetic[3] <- Covariates_Synthetic_Prices$perc_urban_Synthetic_SF
Covariates_Prices$perc_urban_Prices_Synthetic[4] <- Covariates_Synthetic_Prices$perc_urban_Synthetic_Oakland
Covariates_Prices$perc_urban_Prices_Synthetic[5] <- Covariates_Synthetic_Prices$perc_urban_Synthetic_Seattle

##Merging outcome and covariate data
Quantity_Merged <- left_join(Outcomes_Quantity, Covariates_Quantity,
                             by = c("store_zip3" = "store_zip3"))
Prices_Merged <- left_join(Outcomes_Prices, Covariates_Prices,
                             by = c("store_zip3" = "store_zip3"))

##Melting the data
Quantity_Melted <- melt(Quantity_Merged, id.vars = "store_zip3")
Prices_Melted <- melt(Prices_Merged, id.vars = "store_zip3")

##Changing zip code values to locality names
Quantity_Melted$store_zip3[Quantity_Melted$store_zip3 == 191] <- "Philadelphia"
Quantity_Melted$store_zip3[Quantity_Melted$store_zip3 == 803] <- "Boulder"
Quantity_Melted$store_zip3[Quantity_Melted$store_zip3 == 941] <- "San Francisco"
Quantity_Melted$store_zip3[Quantity_Melted$store_zip3 == 946] <- "Oakland"
Quantity_Melted$store_zip3[Quantity_Melted$store_zip3 == 981] <- "Seattle"

Prices_Melted$store_zip3[Prices_Melted$store_zip3 == 191] <- "Philadelphia"
Prices_Melted$store_zip3[Prices_Melted$store_zip3 == 803] <- "Boulder"
Prices_Melted$store_zip3[Prices_Melted$store_zip3 == 941] <- "San Francisco"
Prices_Melted$store_zip3[Prices_Melted$store_zip3 == 946] <- "Oakland"
Prices_Melted$store_zip3[Prices_Melted$store_zip3 == 981] <- "Seattle"

##Creating variable indicating synthetic vs. treatment
Quantity_Melted$Status <- ifelse(grepl("Treated", Quantity_Melted$variable), "Treated", "Synthetic")
Prices_Melted$Status <- ifelse(grepl("Treated", Prices_Melted$variable), "Treated", "Synthetic")

##Removing "Treated" and "Synthetic" from variable column
Quantity_Melted$variable <- gsub('_Synthetic','',Quantity_Melted$variable)
Quantity_Melted$variable <- gsub('_Treated','',Quantity_Melted$variable)
Quantity_Melted$variable <- gsub('_Quantity','',Quantity_Melted$variable)

Prices_Melted$variable <- gsub('_Synthetic','',Prices_Melted$variable)
Prices_Melted$variable <- gsub('_Treated','',Prices_Melted$variable)
Prices_Melted$variable <- gsub('_Prices','',Prices_Melted$variable)

##Adding indexed values
PretreatmentMeans_Quantity <- group_by(LongDF_Quantity, store_zip3) %>%
  summarise(PretreatmentMean = mean(TotalOz_zipcode)) %>%
  summarise(Min = min(PretreatmentMean),
            Max = max(PretreatmentMean)) %>%
  mutate(Variable = "PretreatmentMean")

PretreatmentMeans_Prices <- group_by(LongDF_Prices, store_zip3) %>%
  summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode)) %>%
  summarise(Min = min(PretreatmentMean),
            Max = max(PretreatmentMean)) %>%
  mutate(Variable = "PretreatmentMean")

Min_Max_medHHincome <- summarise(CensusData,
                                 Max = max(medHHincome),
                                 Min = min(medHHincome)) %>%
  mutate(Variable = "medHHincome")

Min_Max_population <- summarise(CensusData,
                                 Max = max(population),
                                 Min = min(population)) %>%
  mutate(Variable = "population")

Min_Max_medage <- summarise(CensusData,
                                 Max = max(medage),
                                 Min = min(medage)) %>%
  mutate(Variable = "medage")

Min_Max_num_housingunits <- summarise(CensusData,
                                 Max = max(num_housingunits),
                                 Min = min(num_housingunits)) %>%
  mutate(Variable = "num_housingunits")

Min_Max_pwhite <- summarise(CensusData,
                                 Max = max(pwhite),
                                 Min = min(pwhite)) %>%
  mutate(Variable = "pwhite")

Min_Max_pblack <- summarise(CensusData,
                                 Max = max(pblack),
                                 Min = min(pblack)) %>%
  mutate(Variable = "pblack")

Min_Max_pamindalask <- summarise(CensusData,
                                 Max = max(pamindalask),
                                 Min = min(pamindalask)) %>%
  mutate(Variable = "pamindalask")

Min_Max_pasian <- summarise(CensusData,
                                 Max = max(pasian),
                                 Min = min(pasian)) %>%
  mutate(Variable = "pasian")

Min_Max_phawaii <- summarise(CensusData,
                                 Max = max(phawaii),
                                 Min = min(phawaii)) %>%
  mutate(Variable = "phawaii")

Min_Max_pother <- summarise(CensusData,
                                 Max = max(pother),
                                 Min = min(pother)) %>%
  mutate(Variable = "pother")

Min_Max_phisp <- summarise(CensusData,
                                 Max = max(phisp),
                                 Min = min(phisp)) %>%
  mutate(Variable = "phisp")

Min_Max_poverty_10K <- summarise(CensusData,
                                 Max = max(poverty_10K),
                                 Min = min(poverty_10K)) %>%
  mutate(Variable = "poverty_10K")

Min_Max_age_18to64 <- summarise(CensusData,
                                 Max = max(age_18to64),
                                 Min = min(age_18to64)) %>%
  mutate(Variable = "age_18to64")

Min_Max_perc_urban <- summarise(CensusData,
                                 Max = max(perc_urban),
                                 Min = min(perc_urban)) %>%
  mutate(Variable = "perc_urban")

##rbinding all of these values into a quantity and price dataframe separately
Min_Max_Quantities <- rbind(PretreatmentMeans_Quantity, Min_Max_medHHincome,
                            Min_Max_population, Min_Max_medage, Min_Max_num_housingunits,
                            Min_Max_pwhite, Min_Max_pblack, Min_Max_pamindalask, Min_Max_pasian,
                            Min_Max_phawaii, Min_Max_pother, Min_Max_phisp, Min_Max_poverty_10K,
                            Min_Max_age_18to64, Min_Max_perc_urban)

Min_Max_Prices <- rbind(PretreatmentMeans_Prices, Min_Max_medHHincome,
                            Min_Max_population, Min_Max_medage, Min_Max_num_housingunits,
                            Min_Max_pwhite, Min_Max_pblack, Min_Max_pamindalask, Min_Max_pasian,
                            Min_Max_phawaii, Min_Max_pother, Min_Max_phisp, Min_Max_poverty_10K,
                            Min_Max_age_18to64, Min_Max_perc_urban)

##joining these to melted quantity and prices dataframes
Quantity_Melted <- left_join(Quantity_Melted, Min_Max_Quantities, by = c("variable" = "Variable"))
Prices_Melted <- left_join(Prices_Melted, Min_Max_Prices, by = c("variable" = "Variable"))

##Creating indices for each variable
Quantity_Melted <- mutate(Quantity_Melted, value_indexed = 100*(value-Min)/(Max-Min))
Prices_Melted <- mutate(Prices_Melted, value_indexed = 100*(value-Min)/(Max-Min))


#######RESHAPING AND OUTPUTTING IMPORTANT INFO#######

##Reshaping data to be able to calculate difference
Quantity_Treated_Output <- filter(Quantity_Melted, Status == "Treated")
Quantity_Synthetic_Output <- filter(Quantity_Melted, Status == "Synthetic")
Quantity_Output <- left_join(Quantity_Treated_Output, Quantity_Synthetic_Output,
                             by = c("store_zip3" = "store_zip3",
                                    "variable" = "variable"))

Quantity_Output <- dplyr::select(Quantity_Output, -c(Status.x, Status.y, Min.x, Max.x, Min.y, Max.y))
colnames(Quantity_Output) <- c("store_zip3", "variable", "treated_rawval", 
                               "treated_indexedval", "synthetic_rawval", "synthetic_indexedval")

Prices_Treated_Output <- filter(Prices_Melted, Status == "Treated")
Prices_Synthetic_Output <- filter(Prices_Melted, Status == "Synthetic")
Prices_Output <- left_join(Prices_Treated_Output, Prices_Synthetic_Output,
                                by = c("store_zip3" = "store_zip3",
                                       "variable" = "variable"))

Prices_Output <- dplyr::select(Prices_Output, -c(Status.x, Status.y, Min.x, Max.x, Min.y, Max.y))
colnames(Prices_Output) <- c("store_zip3", "variable", "treated_rawval", 
                                  "treated_indexedval", "synthetic_rawval", "synthetic_indexedval")

fwrite(Quantity_Output, "Data/Summary Stats Data/DumbbellData_Consumption.csv")
fwrite(Prices_Output, "Data/Summary Stats Data/DumbbellData_Prices.csv")


#########################
  
Quantity_Output$Diff <- abs(Quantity_Output$treated_indexedval - Quantity_Output$synthetic_indexedval)

#########################

#Computing standard deviations of average price, quantity, and covariates across all pretreatment periods
Outcomes_Quantity_SDs <- filter(LongDF_Quantity, treated == 0 & calendar_time <= 60) %>%
  group_by(store_zip3) %>%
  summarise(PretreatmentMean_Quantity = mean(TotalOz_zipcode)) %>%
  summarise(SD_Quantity = sd(PretreatmentMean_Quantity))

Outcomes_Prices_SDs <- filter(LongDF_Prices, treated == 0 & calendar_time <= 60) %>%
  group_by(store_zip3) %>%
  summarise(PretreatmentMean_Prices = mean(AvgPrice_PerOz_zipcode)) %>%
  summarise(SD_Prices = sd(PretreatmentMean_Prices))

Covariates_SDs <- t(summarise(CensusData, population = sd(population),
            medag = sd(medage),
            num_housingunits = sd(num_housingunits),
            pwhite = sd(pwhite),
            pblack = sd(pblack),
            pamindalask = sd(pamindalask),
            pasian = sd(pasian),
            phawaii = sd(phawaii),
            pother = sd(pother),
            phisp = sd(phisp),
            poverty_10K = sd(poverty_10K),
            age_18to64 = sd(age_18to64),
            perc_urban = sd(perc_urban))) %>% as.data.frame() 
colnames(Covariates_SDs) <- "Standard Deviation"
Covariates_SDs[14,1] <- Outcomes_Quantity_SDs[1,1]
Covariates_SDs[15,1] <- Outcomes_Prices_SDs[1,1]

rownames(Covariates_SDs)[rownames(Covariates_SDs) == "1"] <- "quantity"
rownames(Covariates_SDs)[rownames(Covariates_SDs) == "15"] <- "price"

Covariates_SDs$VarNames <- rownames(Covariates_SDs)
fwrite(Covariates_SDs, "Data/Summary Stats Data/DumbbellData_SDs.csv")

#################################################
###############Quantity Graphs###################
#################################################

#Renaming variables
Quantity_Melted$variable <- as.character(Quantity_Melted$variable)
Quantity_Melted$variable[Quantity_Melted$variable == "PretreatmentMean"] <- "Pretreatment Mean Oz. Purchased"
Quantity_Melted$variable[Quantity_Melted$variable == "population"] <- "Population"
Quantity_Melted$variable[Quantity_Melted$variable == "medage"] <- "Median Age"
Quantity_Melted$variable[Quantity_Melted$variable == "num_housingunits"] <- "# Housing Units"
Quantity_Melted$variable[Quantity_Melted$variable == "pwhite"] <- "% White"
Quantity_Melted$variable[Quantity_Melted$variable == "pblack"] <- "% Black"
Quantity_Melted$variable[Quantity_Melted$variable == "pamindalask"] <- "% American Indian/Alaska Native"
Quantity_Melted$variable[Quantity_Melted$variable == "pasian"] <- "% Asian"
Quantity_Melted$variable[Quantity_Melted$variable == "phawaii"] <- "% Hawaiian"
Quantity_Melted$variable[Quantity_Melted$variable == "pother"] <- "% Other"
Quantity_Melted$variable[Quantity_Melted$variable == "phisp"] <- "% Hispanic"
Quantity_Melted$variable[Quantity_Melted$variable == "poverty_10K"] <- "% <$10K"
Quantity_Melted$variable[Quantity_Melted$variable == "age_18to64"] <- "% Ages 18-64"
Quantity_Melted$variable[Quantity_Melted$variable == "perc_urban"] <- "% Urban"

#ggplot theme
theme_stuff <- theme_bw() +
  theme(legend.position = "none",
        legend.title=element_blank(), 
        legend.text=element_text(size=22),
        plot.title = element_text(hjust = 0.5, size = 22),
        text = element_text(size=12), 
        legend.key.height=unit(2,"line"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0))) 

#Philadelphia
Quantity_Philadelphia <- filter(Quantity_Melted, store_zip3 == "Philadelphia")

Quantity_Philadelphia_Plot <- ggplot(Quantity_Philadelphia, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Philadelphia") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Quantity Dumbbell/Philadelphia.png", width = 40, height = 25, units = "cm")

#Boulder
Quantity_Boulder <- filter(Quantity_Melted, store_zip3 == "Boulder")

Quantity_Boulder_Plot <- ggplot(Quantity_Boulder, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Boulder") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Quantity Dumbbell/Boulder.png", width = 40, height = 25, units = "cm")

#SF
Quantity_SF <- filter(Quantity_Melted, store_zip3 == "San Francisco")

Quantity_SF_Plot <- ggplot(Quantity_SF, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "San Francisco") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Quantity Dumbbell/SF.png", width = 40, height = 25, units = "cm")

#Seattle
Quantity_Seattle <- filter(Quantity_Melted, store_zip3 == "Seattle")

Quantity_Seattle_Plot <- ggplot(Quantity_Seattle, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Seattle") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Quantity Dumbbell/Seattle.png", width = 40, height = 25, units = "cm")

#Oakland
Quantity_Oakland <- filter(Quantity_Melted, store_zip3 == "Oakland")

Quantity_Oakland_Plot <- ggplot(Quantity_Oakland, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Oakland") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Quantity Dumbbell/Oakland.png", width = 40, height = 25, units = "cm")



CombinedGraph_Quantity <- ggpubr::ggarrange(Quantity_Philadelphia_Plot,
                                    Quantity_Boulder_Plot,
                                    Quantity_Oakland_Plot,
                                    Quantity_SF_Plot,
                                    Quantity_Seattle_Plot,
                                    common.legend = TRUE,
                                    legend = "top",
                                    align = "hv",
                                    heights = c(4, 4),
                                    ncol = 2, nrow = 3) 
CombinedGraph_Quantity

ggsave("Figures/Summary Statistics/Quantity Dumbbell/CombinedGraph_Quantity.png", width = 40, height = 30, units = "cm")





#################################################
###############Prices Graphs#####################
#################################################

#Renaming variables
Prices_Melted$variable <- as.character(Prices_Melted$variable)
Prices_Melted$variable[Prices_Melted$variable == "PretreatmentMean"] <- "Pretreatment Mean Price/Oz."
Prices_Melted$variable[Prices_Melted$variable == "population"] <- "Population"
Prices_Melted$variable[Prices_Melted$variable == "medage"] <- "Median Age"
Prices_Melted$variable[Prices_Melted$variable == "num_housingunits"] <- "# Housing Units"
Prices_Melted$variable[Prices_Melted$variable == "pwhite"] <- "% White"
Prices_Melted$variable[Prices_Melted$variable == "pblack"] <- "% Black"
Prices_Melted$variable[Prices_Melted$variable == "pamindalask"] <- "% American Indian/Alaska Native"
Prices_Melted$variable[Prices_Melted$variable == "pasian"] <- "% Asian"
Prices_Melted$variable[Prices_Melted$variable == "phawaii"] <- "% Hawaiian"
Prices_Melted$variable[Prices_Melted$variable == "pother"] <- "% Other"
Prices_Melted$variable[Prices_Melted$variable == "phisp"] <- "% Hispanic"
Prices_Melted$variable[Prices_Melted$variable == "poverty_10K"] <- "% <$10K"
Prices_Melted$variable[Prices_Melted$variable == "age_18to64"] <- "% Ages 18-64"
Prices_Melted$variable[Prices_Melted$variable == "perc_urban"] <- "% Urban"

#ggplot theme
theme_stuff <- theme_bw() +
  theme(legend.position = "none",
        legend.title=element_blank(), 
        legend.text=element_text(size=22),
        plot.title = element_text(hjust = 0.5, size = 22),
        text = element_text(size=12), 
        legend.key.height=unit(2,"line"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0))) 

#Philadelphia
Prices_Philadelphia <- filter(Prices_Melted, store_zip3 == "Philadelphia")

Prices_Philadelphia_Plot <- ggplot(Prices_Philadelphia, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Philadelphia") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Prices Dumbbell/Philadelphia.png", width = 40, height = 25, units = "cm")

#Boulder
Prices_Boulder <- filter(Prices_Melted, store_zip3 == "Boulder")

Prices_Boulder_Plot <- ggplot(Prices_Boulder, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Boulder") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Prices Dumbbell/Boulder.png", width = 40, height = 25, units = "cm")

#SF
Prices_SF <- filter(Prices_Melted, store_zip3 == "San Francisco")

Prices_SF_Plot <- ggplot(Prices_SF, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "San Francisco") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Prices Dumbbell/SF.png", width = 40, height = 25, units = "cm")

#Seattle
Prices_Seattle <- filter(Prices_Melted, store_zip3 == "Seattle")

Prices_Seattle_Plot <- ggplot(Prices_Seattle, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Seattle") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Prices Dumbbell/Seattle.png", width = 40, height = 25, units = "cm")

#Oakland
Prices_Oakland <- filter(Prices_Melted, store_zip3 == "Oakland")

Prices_Oakland_Plot <- ggplot(Prices_Oakland, aes(x = value_indexed, y = as.factor(variable))) +
  geom_line() +
  geom_point(aes(color = Status), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  ggplot2::labs(y = "", x = "Index", title = "Oakland") + 
  scale_x_continuous(labels = comma) +
  #scale_y_continuous(labels = comma) +
  xlim(0,100) + 
  theme_stuff

ggsave("Figures/Summary Statistics/Prices Dumbbell/Oakland.png", width = 40, height = 25, units = "cm")



CombinedGraph_Prices <- ggpubr::ggarrange(Prices_Philadelphia_Plot,
                                            Prices_Boulder_Plot,
                                            Prices_Oakland_Plot,
                                            Prices_SF_Plot,
                                            Prices_Seattle_Plot,
                                            common.legend = TRUE,
                                            legend = "top",
                                            align = "hv",
                                            heights = c(4, 4),
                                            ncol = 2, nrow = 3) 
CombinedGraph_Prices

ggsave("Figures/Summary Statistics/Prices Dumbbell/CombinedGraph_Prices.png", width = 40, height = 30, units = "cm")





########################################
################EXAMPLE#################
########################################

# Seed
set.seed(1)

# Data
customers <- sample(50:150, 10)
potential_customers <- sample(150:500, 10)
company <- LETTERS[1:10]

# Data frame
df2 <- data.frame(company = company,
                  customers = customers,
                  potential_customers = potential_customers)

# Long, ordered data frame
df2 <- melt(df2, id.vars = "company")
df2 <- df2[order(df2$company), ]

ggplot(df2, aes(x = value, y = company)) +
  geom_line() +
  geom_point(aes(color = variable), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme(legend.position = "bottom")





