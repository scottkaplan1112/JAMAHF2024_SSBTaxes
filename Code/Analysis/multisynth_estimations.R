library(tidysynth)
library(data.table)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(lfe)
library(fixest)
library(stargazer)
library(modelr)
library(xtable)
library(purrr)
library(Synth)
library(SCtools)
library(MASS)
library(ggrepel)
library(reshape)
library(reshape2)
library(devtools)
#devtools::install_github("ebenmichael/augsynth")
library(augsynth)

set.seed(05051992)
memory.limit(size = 25000000)

##Setting the working directory for SSB Project

# #FacultyData
# setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis")

#Ceto
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

##ALL SYNTHETIC CONTROL FUNCTIONS IN THIS SOURCE FILE
source("Code/SyntheticControl_Functions.R")

# ##Toggle to drop immediately bordering zip codes in synthetic control estimation
# DropBorderingZips <- 1
# 
# ##Toggle to drop post-COVID data
# DropCovid <- 1
# 
# ##Toggle to just do treated zip codes + those starting with 9 (=0 if just 9, =1 if all)
# Zips_All <- 1
# 
# ##Toggle to 1 if running synthetic control analyses for immediate bordering zip codes
# Border_Analysis <- 0

##Drop Berkeley
Drop_Berkeley <- 1

####################################################
###############QUANTITY DATA########################
####################################################

################################################
###############Reading in Data##################
################################################

NielsenMovement_AllMonths_withCensusData_ZipCode <- fread("Data/NielsenMovement_AllMonths_withCensusData_ZipCode.csv",
                                                          colClasses = c(store_zip3 = "character"))

##Calculating Mean of dep. var.
Mean_OzSoldbyMonth_ZipLevel <- mean(NielsenMovement_AllMonths_withCensusData_ZipCode$TotalOz_zipcode)

#creating a calendar time variable
NielsenMovement_AllMonths_withCensusData_ZipCode <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           calendar_time = ifelse(time_to_treat_months_Berkeley < 25,
                                                                                  time_to_treat_months_Berkeley - min(time_to_treat_months_Berkeley) + 1,
                                                                                  time_to_treat_months_SanFrancisco+73))

#List of taxed and border zip codes (including DC and its border zips)
taxed_zips <- c("946", "981", "803", "191", "200", "947", "941")
border_zips_withDC <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                        "081", "080", "222", "223", "201", "220", "216", "218", "948", "945")
border_zips <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                 "081", "080", "948", "945")

#creating the treated variable for taxed zip codes
NielsenMovement_AllMonths_withCensusData_ZipCode <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           treated = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0 |
                                                                              store_zip3 == "981" & time_to_treat_months_Seattle >= 0 |
                                                                              store_zip3 == "803" & time_to_treat_months_Boulder >= 0 |
                                                                              store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0 |
                                                                              store_zip3 == "946" & time_to_treat_months_Oakland >= 0 |
                                                                              store_zip3 == "200" & time_to_treat_months_DC >= 0 |
                                                                              store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))
# treated_SF = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
# treated_Seattle = ifelse(store_zip3 == "981" & time_to_treat_months_Seattle >= 0, 1, 0),
# treated_Boulder = ifelse(store_zip3 == "803" & time_to_treat_months_Boulder >= 0, 1, 0),
# treated_Philadelphia = ifelse(store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0, 1, 0),
# treated_Oakland = ifelse(store_zip3 == "946" & time_to_treat_months_Oakland >= 0, 1, 0),
# treated_DC = ifelse(store_zip3 == "200" & time_to_treat_months_DC >= 0, 1, 0),
# treated_Berkeley = ifelse(store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))


#creating the overall border, border groupings, and individual taxed city treated variables
NielsenMovement_AllMonths_withCensusData_ZipCode <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           treated_unit_border = ifelse(store_zip3 == "949" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                          store_zip3 == "940" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                          store_zip3 == "980" & time_to_treat_months_Seattle >= 0 | 
                                                                                          store_zip3 == "982" & time_to_treat_months_Seattle >= 0 | 
                                                                                          store_zip3 == "983" & time_to_treat_months_Seattle >= 0 | 
                                                                                          store_zip3 == "984" & time_to_treat_months_Seattle >= 0 |
                                                                                          store_zip3 == "800" & time_to_treat_months_Boulder >= 0 | 
                                                                                          store_zip3 == "804" & time_to_treat_months_Boulder >= 0 | 
                                                                                          store_zip3 == "805" & time_to_treat_months_Boulder >= 0 | 
                                                                                          store_zip3 == "081" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                          store_zip3 == "080" & time_to_treat_months_Philadelphia >= 0 |
                                                                                          store_zip3 == "945" & time_to_treat_months_Oakland >= 0 |
                                                                                          store_zip3 == "948" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                           treated_unit_border_SF = ifelse(store_zip3 == "949" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                             store_zip3 == "940" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
                                                           treated_unit_border_Seattle = ifelse(store_zip3 == "980" & time_to_treat_months_Seattle >= 0 | 
                                                                                                  store_zip3 == "982" & time_to_treat_months_Seattle >= 0 | 
                                                                                                  store_zip3 == "983" & time_to_treat_months_Seattle >= 0 | 
                                                                                                  store_zip3 == "984" & time_to_treat_months_Seattle >= 0, 1, 0),
                                                           treated_unit_border_Boulder = ifelse(store_zip3 == "800" & time_to_treat_months_Boulder >= 0 | 
                                                                                                  store_zip3 == "804" & time_to_treat_months_Boulder >= 0 | 
                                                                                                  store_zip3 == "805" & time_to_treat_months_Boulder >= 0, 1, 0),
                                                           treated_unit_border_Philadelphia = ifelse(store_zip3 == "081" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                                       store_zip3 == "080" & time_to_treat_months_Philadelphia >= 0, 1, 0),
                                                           treated_unit_border_Oakland = ifelse(store_zip3 == "945" & time_to_treat_months_Oakland >= 0 |
                                                                                                  store_zip3 == "948" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                           treated_unit_border_Berkeley = ifelse(store_zip3 == "945" & time_to_treat_months_Berkeley >= 0 |
                                                                                                   store_zip3 == "948" & time_to_treat_months_Berkeley >= 0, 1, 0),
                                                           treated_unit_SF = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
                                                           treated_unit_Seattle = ifelse(store_zip3 == "981" & time_to_treat_months_Seattle >= 0, 1, 0),
                                                           treated_unit_Boulder = ifelse(store_zip3 == "803" & time_to_treat_months_Boulder >= 0, 1, 0),
                                                           treated_unit_Philadelphia = ifelse(store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0, 1, 0),
                                                           treated_unit_Oakland = ifelse(store_zip3 == "946" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                           #treated_unit_DC = ifelse(store_zip3 == "200", 1, 0),
                                                           treated_unit_Berkeley = ifelse(store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))


#Filtering out DC for now
NielsenMovement_AllMonths_withCensusData_ZipCode <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                           !(store_zip3 == "200"))

if(Drop_Berkeley == 1){
  NielsenMovement_AllMonths_withCensusData_ZipCode <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                             !(store_zip3 == "947"))
}

#Including urbanicity variable
CensusData_Combined_Final_3DigitZip <- fread("Data/CensusData_Combined_Final_3DigitZip.csv")
CensusData_Combined_Final_3DigitZip <- dplyr::select(CensusData_Combined_Final_3DigitZip, GEOID_3, perc_urban)

NielsenMovement_AllMonths_withCensusData_ZipCode$store_zip3_numeric <- as.numeric(NielsenMovement_AllMonths_withCensusData_ZipCode$store_zip3)
NielsenMovement_AllMonths_withCensusData_ZipCode <- left_join(NielsenMovement_AllMonths_withCensusData_ZipCode,
                                                              CensusData_Combined_Final_3DigitZip,
                                                              by = c("store_zip3_numeric" = "GEOID_3"))

#################################################
###############PRICE DATA########################
#################################################

################################################
###############Reading in Data##################
################################################

NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- fread("Data/NielsenMovement_AllMonths_withCensusData_ZipCode_Prices.csv",
                                                                 colClasses = c(store_zip3 = "character"))

##Calculating Mean of dep. var.
Mean_AvgPriceperOzbyMonth_ZipLevel <- mean(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices$AvgPrice_PerOz_zipcode)

#creating a calendar time variable
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                  calendar_time = ifelse(time_to_treat_months_Berkeley < 25,
                                                                                         time_to_treat_months_Berkeley - min(time_to_treat_months_Berkeley) + 1,
                                                                                         time_to_treat_months_SanFrancisco+73))

#List of taxed and border zip codes (including DC and its border zips)
taxed_zips <- c("946", "981", "803", "191", "200", "947", "941")
border_zips_withDC <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                        "081", "080", "222", "223", "201", "220", "216", "218", "948", "945")
border_zips <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                 "081", "080", "948", "945")

#creating the treated variable for taxed zip codes
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                  treated = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0 |
                                                                                     store_zip3 == "981" & time_to_treat_months_Seattle >= 0 |
                                                                                     store_zip3 == "803" & time_to_treat_months_Boulder >= 0 |
                                                                                     store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0 |
                                                                                     store_zip3 == "946" & time_to_treat_months_Oakland >= 0 |
                                                                                     store_zip3 == "200" & time_to_treat_months_DC >= 0 |
                                                                                     store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))
# treated_SF = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
# treated_Seattle = ifelse(store_zip3 == "981" & time_to_treat_months_Seattle >= 0, 1, 0),
# treated_Boulder = ifelse(store_zip3 == "803" & time_to_treat_months_Boulder >= 0, 1, 0),
# treated_Philadelphia = ifelse(store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0, 1, 0),
# treated_Oakland = ifelse(store_zip3 == "946" & time_to_treat_months_Oakland >= 0, 1, 0),
# treated_DC = ifelse(store_zip3 == "200" & time_to_treat_months_DC >= 0, 1, 0),
# treated_Berkeley = ifelse(store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))


#creating the overall border, border groupings, and individual taxed city treated variables
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- mutate(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                  treated_unit_border = ifelse(store_zip3 == "949" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                                 store_zip3 == "940" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                                 store_zip3 == "980" & time_to_treat_months_Seattle >= 0 | 
                                                                                                 store_zip3 == "982" & time_to_treat_months_Seattle >= 0 | 
                                                                                                 store_zip3 == "983" & time_to_treat_months_Seattle >= 0 | 
                                                                                                 store_zip3 == "984" & time_to_treat_months_Seattle >= 0 |
                                                                                                 store_zip3 == "800" & time_to_treat_months_Boulder >= 0 | 
                                                                                                 store_zip3 == "804" & time_to_treat_months_Boulder >= 0 | 
                                                                                                 store_zip3 == "805" & time_to_treat_months_Boulder >= 0 | 
                                                                                                 store_zip3 == "081" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                                 store_zip3 == "080" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                                 store_zip3 == "945" & time_to_treat_months_Oakland >= 0 |
                                                                                                 store_zip3 == "948" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                                  treated_unit_border_SF = ifelse(store_zip3 == "949" & time_to_treat_months_SanFrancisco >= 0 | 
                                                                                                    store_zip3 == "940" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
                                                                  treated_unit_border_Seattle = ifelse(store_zip3 == "980" & time_to_treat_months_Seattle >= 0 | 
                                                                                                         store_zip3 == "982" & time_to_treat_months_Seattle >= 0 | 
                                                                                                         store_zip3 == "983" & time_to_treat_months_Seattle >= 0 | 
                                                                                                         store_zip3 == "984" & time_to_treat_months_Seattle >= 0, 1, 0),
                                                                  treated_unit_border_Boulder = ifelse(store_zip3 == "800" & time_to_treat_months_Boulder >= 0 | 
                                                                                                         store_zip3 == "804" & time_to_treat_months_Boulder >= 0 | 
                                                                                                         store_zip3 == "805" & time_to_treat_months_Boulder >= 0, 1, 0),
                                                                  treated_unit_border_Philadelphia = ifelse(store_zip3 == "081" & time_to_treat_months_Philadelphia >= 0 | 
                                                                                                              store_zip3 == "080" & time_to_treat_months_Philadelphia >= 0, 1, 0),
                                                                  treated_unit_border_Oakland = ifelse(store_zip3 == "945" & time_to_treat_months_Oakland >= 0 |
                                                                                                         store_zip3 == "948" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                                  treated_unit_border_Berkeley = ifelse(store_zip3 == "945" & time_to_treat_months_Berkeley >= 0 |
                                                                                                          store_zip3 == "948" & time_to_treat_months_Berkeley >= 0, 1, 0),
                                                                  treated_unit_SF = ifelse(store_zip3 == "941" & time_to_treat_months_SanFrancisco >= 0, 1, 0),
                                                                  treated_unit_Seattle = ifelse(store_zip3 == "981" & time_to_treat_months_Seattle >= 0, 1, 0),
                                                                  treated_unit_Boulder = ifelse(store_zip3 == "803" & time_to_treat_months_Boulder >= 0, 1, 0),
                                                                  treated_unit_Philadelphia = ifelse(store_zip3 == "191" & time_to_treat_months_Philadelphia >= 0, 1, 0),
                                                                  treated_unit_Oakland = ifelse(store_zip3 == "946" & time_to_treat_months_Oakland >= 0, 1, 0),
                                                                  #treated_unit_DC = ifelse(store_zip3 == "200", 1, 0),
                                                                  treated_unit_Berkeley = ifelse(store_zip3 == "947" & time_to_treat_months_Berkeley >= 0, 1, 0))


#Filtering out DC for now
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                  !(store_zip3 == "200"))



if(Drop_Berkeley == 1){
  NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                    !(store_zip3 == "947"))
}

#Including urbanicity variable
CensusData_Combined_Final_3DigitZip <- fread("Data/CensusData_Combined_Final_3DigitZip.csv")
CensusData_Combined_Final_3DigitZip <- dplyr::select(CensusData_Combined_Final_3DigitZip, GEOID_3, perc_urban)

NielsenMovement_AllMonths_withCensusData_ZipCode_Prices$store_zip3_numeric <- as.numeric(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices$store_zip3)
NielsenMovement_AllMonths_withCensusData_ZipCode_Prices <- left_join(NielsenMovement_AllMonths_withCensusData_ZipCode_Prices,
                                                                     CensusData_Combined_Final_3DigitZip,
                                                                     by = c("store_zip3_numeric" = "GEOID_3"))



#########################################################
###############URBANICITY CUTOFFS########################
#########################################################

MeanUrbanicity_Treated <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% taxed_zips) %>%
  summarise(MeanUrbanicity = mean(perc_urban)) %>% as.numeric()

MeanUrbanicity_Borders <- filter(NielsenMovement_AllMonths_withCensusData_ZipCode, store_zip3 %in% border_zips) %>%
  summarise(MeanUrbanicity = mean(perc_urban)) %>% as.numeric()

SDUrbanicity <- summarise(NielsenMovement_AllMonths_withCensusData_ZipCode, 
                          SDUrbanicity = sd(perc_urban)) %>% as.numeric()


###############################################################################
###############SYNTHETIC CONTROL ESTIMATION: MULTISYNTH########################
###############################################################################


#####################################################################
###########################CONSUMPTION###############################
#####################################################################

SCEstimationData_Composite <- NielsenMovement_AllMonths_withCensusData_ZipCode

#Filtering out non-urban donor zip codes
SCEstimationData_Composite <- filter(SCEstimationData_Composite, perc_urban > MeanUrbanicity_Treated-SDUrbanicity | 
                                       store_zip3 %in% taxed_zips |
                                       store_zip3 %in% border_zips)

#Filtering out all border zip codes
SCEstimationData_Composite <- filter(SCEstimationData_Composite,
                                     !(store_zip3 %in% border_zips))


#Adding logged total ounces as a variable
SCEstimationData_Composite <- mutate(SCEstimationData_Composite, log_TotalOz_zipcode = log(TotalOz_zipcode))

#Calculating pretreatment means for all taxed zip codes
PreTreatmentMeans_Taxed <- filter(SCEstimationData_Composite, store_zip3 %in% taxed_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PreTreatmentMeans = mean(TotalOz_zipcode))


#############################################################
#####BALANCED: Including covariates and ridge correction#####
#############################################################

if(Drop_Berkeley == 0){ #38 lags
  # with default nu
  ppool_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
                                      pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                    store_zip3, calendar_time, SCEstimationData_Composite,
                                    fixedeff = T, progfunc="Ridge", scm = T, n_leads = 26, n_lags = 38) #Make lead 26 to ensure period 25 is included
  
  print(ppool_syn_cov_ridge$nu)
  #[1] 0.5104727
}

if(Drop_Berkeley == 1){ #60 lags
  # with default nu
  ppool_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
                                      pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                    store_zip3, calendar_time, SCEstimationData_Composite,
                                    fixedeff = T, progfunc="Ridge", scm = T, n_leads = 26, n_lags = 60) #Make lead 26 to ensure period 25 is included
  
  print(ppool_syn_cov_ridge$nu)
  #[1] 0.5337542
}

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge <- summary(ppool_syn_cov_ridge)
ppool_syn_cov_summ_ridge

#Plotting multisynth results
ppool_syn_cov_plot_ridge <- plot(ppool_syn_cov_summ_ridge, inf = F)
ppool_syn_cov_plot_ridge

ggsave("Figures/Results/Multisynth Results/Consumption/Urbanicity/Multisynth_AllCities.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_AllTaxed <- ppool_syn_cov_ridge$long_df

#extracting att for each placebo run
extraction_att_AllTaxed <- ppool_syn_cov_summ_ridge$att

#extracting weights for each placebo run
extraction_weights_AllTaxed <- ppool_syn_cov_ridge$weights
extraction_weights_AllTaxed <- as.data.frame(extraction_weights_AllTaxed)
extraction_weights_AllTaxed <- tibble::rownames_to_column(extraction_weights_AllTaxed, "store_zip3")
if(Drop_Berkeley == 0){
  colnames(extraction_weights_AllTaxed) <- c("store_zip3", "Philadelphia", "Boulder", "SF", "Oakland",
                                             "Berkeley", "Seattle") #Order determined by att output order
}

if(Drop_Berkeley == 1){
  colnames(extraction_weights_AllTaxed) <- c("store_zip3", "Philadelphia", "Boulder", 
                                             "SF", "Oakland", "Seattle") #Order determined by att output order
}

#Writing the data output
fwrite(extraction_longdf_AllTaxed, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed.csv")
fwrite(extraction_weights_AllTaxed, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_AllTaxed.csv")
fwrite(extraction_att_AllTaxed, "Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed.csv")
fwrite(PreTreatmentMeans_Taxed, "Data/multisynth_placebo_data/Urbanicity/PreTreatmentMeans_Taxed.csv")



###############################################################
#####UNBALANCED: Including covariates and ridge correction#####
###############################################################

# with default nu
ppool_syn_cov_ridge_unbalanced <- multisynth(TotalOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
                                               pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                             store_zip3, calendar_time, SCEstimationData_Composite, 
                                             nu = 0, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 60)

print(ppool_syn_cov_ridge_unbalanced$nu)
#[1] 0.5284903

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_unbalanced <- summary(ppool_syn_cov_ridge_unbalanced)
ppool_syn_cov_summ_ridge_unbalanced

#Plotting multisynth results
ppool_syn_cov_plot_ridge_unbalanced <- plot(ppool_syn_cov_summ_ridge_unbalanced, inf = F)
ppool_syn_cov_plot_ridge_unbalanced

ggsave("Figures/Results/Multisynth Results/Consumption/Urbanicity/Multisynth_AllCities_unbalanced.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_AllTaxed_unbalanced <- ppool_syn_cov_ridge_unbalanced$long_df

#extracting att for each placebo run
extraction_att_AllTaxed_unbalanced <- ppool_syn_cov_summ_ridge_unbalanced$att

#extracting weights for each placebo run
extraction_weights_AllTaxed_unbalanced <- ppool_syn_cov_ridge_unbalanced$weights
extraction_weights_AllTaxed_unbalanced <- as.data.frame(extraction_weights_AllTaxed_unbalanced)
extraction_weights_AllTaxed_unbalanced <- tibble::rownames_to_column(extraction_weights_AllTaxed_unbalanced, "store_zip3")
if(Drop_Berkeley == 0){
  colnames(extraction_weights_AllTaxed_unbalanced) <- c("store_zip3", "Philadelphia", "Boulder", "SF", "Oakland",
                                                        "Berkeley", "Seattle") #Order determined by att output order
}

if(Drop_Berkeley == 1){
  colnames(extraction_weights_AllTaxed_unbalanced) <- c("store_zip3", "Philadelphia", "Boulder", 
                                                        "SF", "Oakland", "Seattle") #Order determined by att output order
}
#Writing the data output
fwrite(extraction_longdf_AllTaxed_unbalanced, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed_unbalanced.csv")
fwrite(extraction_weights_AllTaxed_unbalanced, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_AllTaxed_unbalanced.csv")
fwrite(extraction_att_AllTaxed_unbalanced, "Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed_unbalanced.csv")


# ######################################################
# #####Including covariates and no ridge correction#####
# ######################################################
# 
# # with default nu
# ppool_syn_cov_nocorrection <- multisynth(TotalOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
#                                     pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
#                                   store_zip3, calendar_time, SCEstimationData_Composite, fixedeff = T, progfunc="none", scm = T, n_leads = 25, n_lags = 38)
# 
# print(ppool_syn_cov_nocorrection$nu)
# #[1] 0.540031
# 
# #Gives dataframe of ATT w/ SEs for each treated unit
# ppool_syn_cov_summ_nocorrection <- summary(ppool_syn_cov_nocorrection)
# ppool_syn_cov_summ_nocorrection
# 
# #Plotting multisynth results
# ppool_syn_cov_plot_nocorrection <- plot(ppool_syn_cov_summ_nocorrection, inf = F)
# ppool_syn_cov_plot_nocorrection
# 
# #ggsave("Figures/Results/All Cities Combined/Bias Corrected SCM/Multisynth_nonocorrection.png", width = 40, height = 25, units = "cm")
# 
# #Taking out the average effects
# ATT_cov_nocorrection <- filter(ppool_syn_cov_summ_nocorrection$att, Level == "Average")
# 
# #Again, plotting the average effects
# ppool_syn_cov_plot_avg_nocorrection <- plot(ppool_syn_cov_summ_nocorrection, levels = "Average")
# ppool_syn_cov_plot_avg_nocorrection
# 
# #ggsave("Figures/Results/All Cities Combined/Bias Corrected SCM/Multisynth_noRidge_Average.png", width = 40, height = 25, units = "cm")


#########################################################################
#####Including covariates and ridge correction - PLACEBO ESTIMATIONS#####
#########################################################################

# ##take sample of data for quicker estimation
# set.seed <- 05051992
# placebo_zips <- sample(SCEstimationData_Composite$store_zip3, 5)

placebo_zips <- unique(SCEstimationData_Composite$store_zip3)
placebo_data <- filter(SCEstimationData_Composite, store_zip3 %in% placebo_zips)
placebo_data <- mutate(placebo_data, placebo_col = NA)


####San Francisco####

datalist_multisynth_SF <- list()
datalist_summary_SF <- list()
datalist_pretreatmentmean_SF <- list()

extraction_longdf_SF <- list()
extraction_weights_SF <- list()
extraction_att_SF <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_SF" #change name for different cities
  placebo_data$placebo_SF <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_SanFrancisco >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_SF | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_SF[[j]] <- placebo_syn_cov_ridge
  datalist_summary_SF[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_SF[[j]] <- filter(placebo_data, store_zip3 == i & placebo_SF != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_SF[[j]] <- datalist_multisynth_SF[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_SF[[j]] <- datalist_multisynth_SF[[j]]$weights
  extraction_weights_SF[[j]] <- as.data.frame(extraction_weights_SF[[j]])
  extraction_weights_SF[[j]] <- tibble::rownames_to_column(extraction_weights_SF[[j]], "store_zip3")
  extraction_weights_SF[[j]] <- mutate(extraction_weights_SF[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_SF[[j]] <- datalist_summary_SF[[j]]$att
  extraction_att_SF[[j]] <- as.data.frame(extraction_att_SF[[j]])
  extraction_att_SF[[j]] <- filter(extraction_att_SF[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_SF <- dplyr::select(datalist_multisynth_SF[[length(datalist_multisynth_SF)]]$long_df, -placebo_SF) #only need (last) one since df from each run is the same
extraction_weights_all_SF <- bind_rows(extraction_weights_SF)
extraction_att_all_SF <- bind_rows(extraction_att_SF)
extraction_pretreatmentmean_all_SF <- bind_rows(datalist_pretreatmentmean_SF)

#Writing the data output
fwrite(extraction_longdf_all_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_SF.csv")
fwrite(extraction_weights_all_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_SF.csv")
fwrite(extraction_att_all_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_SF.csv")
fwrite(extraction_pretreatmentmean_all_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_SF.csv")



####Seattle####

datalist_multisynth_Seattle <- list()
datalist_summary_Seattle <- list()
datalist_pretreatmentmean_Seattle <- list()

extraction_longdf_Seattle <- list()
extraction_weights_Seattle <- list()
extraction_att_Seattle <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Seattle" #change name for different cities
  placebo_data$placebo_Seattle <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Seattle >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Seattle | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Seattle[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Seattle[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Seattle[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Seattle != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Seattle[[j]] <- datalist_multisynth_Seattle[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Seattle[[j]] <- datalist_multisynth_Seattle[[j]]$weights
  extraction_weights_Seattle[[j]] <- as.data.frame(extraction_weights_Seattle[[j]])
  extraction_weights_Seattle[[j]] <- tibble::rownames_to_column(extraction_weights_Seattle[[j]], "store_zip3")
  extraction_weights_Seattle[[j]] <- mutate(extraction_weights_Seattle[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Seattle[[j]] <- datalist_summary_Seattle[[j]]$att
  extraction_att_Seattle[[j]] <- as.data.frame(extraction_att_Seattle[[j]])
  extraction_att_Seattle[[j]] <- filter(extraction_att_Seattle[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Seattle <- dplyr::select(datalist_multisynth_Seattle[[length(datalist_multisynth_Seattle)]]$long_df, -placebo_Seattle) #only need (last) one since df from each run is the same
extraction_weights_all_Seattle <- bind_rows(extraction_weights_Seattle)
extraction_att_all_Seattle <- bind_rows(extraction_att_Seattle)
extraction_pretreatmentmean_all_Seattle <- bind_rows(datalist_pretreatmentmean_Seattle)

#Writing the data output
fwrite(extraction_longdf_all_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Seattle.csv")
fwrite(extraction_weights_all_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Seattle.csv")
fwrite(extraction_att_all_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Seattle.csv")
fwrite(extraction_pretreatmentmean_all_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Seattle.csv")



####Boulder####

datalist_multisynth_Boulder <- list()
datalist_summary_Boulder <- list()
datalist_pretreatmentmean_Boulder <- list()

extraction_longdf_Boulder <- list()
extraction_weights_Boulder <- list()
extraction_att_Boulder <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Boulder" #change name for different cities
  placebo_data$placebo_Boulder <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Boulder >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Boulder | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Boulder[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Boulder[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Boulder[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Boulder != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Boulder[[j]] <- datalist_multisynth_Boulder[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Boulder[[j]] <- datalist_multisynth_Boulder[[j]]$weights
  extraction_weights_Boulder[[j]] <- as.data.frame(extraction_weights_Boulder[[j]])
  extraction_weights_Boulder[[j]] <- tibble::rownames_to_column(extraction_weights_Boulder[[j]], "store_zip3")
  extraction_weights_Boulder[[j]] <- mutate(extraction_weights_Boulder[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Boulder[[j]] <- datalist_summary_Boulder[[j]]$att
  extraction_att_Boulder[[j]] <- as.data.frame(extraction_att_Boulder[[j]])
  extraction_att_Boulder[[j]] <- filter(extraction_att_Boulder[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Boulder <- dplyr::select(datalist_multisynth_Boulder[[length(datalist_multisynth_Boulder)]]$long_df, -placebo_Boulder) #only need (last) one since df from each run is the same
extraction_weights_all_Boulder <- bind_rows(extraction_weights_Boulder)
extraction_att_all_Boulder <- bind_rows(extraction_att_Boulder)
extraction_pretreatmentmean_all_Boulder <- bind_rows(datalist_pretreatmentmean_Boulder)

#Writing the data output
fwrite(extraction_longdf_all_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Boulder.csv")
fwrite(extraction_weights_all_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Boulder.csv")
fwrite(extraction_att_all_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Boulder.csv")
fwrite(extraction_pretreatmentmean_all_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Boulder.csv")



####Philadelphia####

datalist_multisynth_Philadelphia <- list()
datalist_summary_Philadelphia <- list()
datalist_pretreatmentmean_Philadelphia <- list()

extraction_longdf_Philadelphia <- list()
extraction_weights_Philadelphia <- list()
extraction_att_Philadelphia <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Philadelphia" #change name for different cities
  placebo_data$placebo_Philadelphia <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Philadelphia >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Philadelphia | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Philadelphia[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Philadelphia[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Philadelphia[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Philadelphia != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Philadelphia[[j]] <- datalist_multisynth_Philadelphia[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Philadelphia[[j]] <- datalist_multisynth_Philadelphia[[j]]$weights
  extraction_weights_Philadelphia[[j]] <- as.data.frame(extraction_weights_Philadelphia[[j]])
  extraction_weights_Philadelphia[[j]] <- tibble::rownames_to_column(extraction_weights_Philadelphia[[j]], "store_zip3")
  extraction_weights_Philadelphia[[j]] <- mutate(extraction_weights_Philadelphia[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Philadelphia[[j]] <- datalist_summary_Philadelphia[[j]]$att
  extraction_att_Philadelphia[[j]] <- as.data.frame(extraction_att_Philadelphia[[j]])
  extraction_att_Philadelphia[[j]] <- filter(extraction_att_Philadelphia[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Philadelphia <- dplyr::select(datalist_multisynth_Philadelphia[[length(datalist_multisynth_Philadelphia)]]$long_df, -placebo_Philadelphia) #only need (last) one since df from each run is the same
extraction_weights_all_Philadelphia <- bind_rows(extraction_weights_Philadelphia)
extraction_att_all_Philadelphia <- bind_rows(extraction_att_Philadelphia)
extraction_pretreatmentmean_all_Philadelphia <- bind_rows(datalist_pretreatmentmean_Philadelphia)

#Writing the data output
fwrite(extraction_longdf_all_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Philadelphia.csv")
fwrite(extraction_weights_all_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Philadelphia.csv")
fwrite(extraction_att_all_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Philadelphia.csv")
fwrite(extraction_pretreatmentmean_all_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Philadelphia.csv")



####Oakland####

datalist_multisynth_Oakland <- list()
datalist_summary_Oakland <- list()
datalist_pretreatmentmean_Oakland <- list()

extraction_longdf_Oakland <- list()
extraction_weights_Oakland <- list()
extraction_att_Oakland <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Oakland" #change name for different cities
  placebo_data$placebo_Oakland <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Oakland >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Oakland | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Oakland[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Oakland[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Oakland[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Oakland != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Oakland[[j]] <- datalist_multisynth_Oakland[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Oakland[[j]] <- datalist_multisynth_Oakland[[j]]$weights
  extraction_weights_Oakland[[j]] <- as.data.frame(extraction_weights_Oakland[[j]])
  extraction_weights_Oakland[[j]] <- tibble::rownames_to_column(extraction_weights_Oakland[[j]], "store_zip3")
  extraction_weights_Oakland[[j]] <- mutate(extraction_weights_Oakland[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Oakland[[j]] <- datalist_summary_Oakland[[j]]$att
  extraction_att_Oakland[[j]] <- as.data.frame(extraction_att_Oakland[[j]])
  extraction_att_Oakland[[j]] <- filter(extraction_att_Oakland[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Oakland <- dplyr::select(datalist_multisynth_Oakland[[length(datalist_multisynth_Oakland)]]$long_df, -placebo_Oakland) #only need (last) one since df from each run is the same
extraction_weights_all_Oakland <- bind_rows(extraction_weights_Oakland)
extraction_att_all_Oakland <- bind_rows(extraction_att_Oakland)
extraction_pretreatmentmean_all_Oakland <- bind_rows(datalist_pretreatmentmean_Oakland)

#Writing the data output
fwrite(extraction_longdf_all_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Oakland.csv")
fwrite(extraction_weights_all_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Oakland.csv")
fwrite(extraction_att_all_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Oakland.csv")
fwrite(extraction_pretreatmentmean_all_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Oakland.csv")



if(Drop_Berkeley == 0){
  
  ####Berkeley####
  
  datalist_multisynth_Berkeley <- list()
  datalist_summary_Berkeley <- list()
  datalist_pretreatmentmean_Berkeley <- list()
  
  extraction_longdf_Berkeley <- list()
  extraction_weights_Berkeley <- list()
  extraction_att_Berkeley <- list()
  
  j <- 1
  
  for(i in placebo_zips){
    colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Berkeley" #change name for different cities
    placebo_data$placebo_Berkeley <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Berkeley >= 0), 1, 0) #change time for different cities
    
    # with default nu
    placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Berkeley | medHHincome + population + medage + num_housingunits + #change name for different cities
                                          pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                        store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
    
    placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
    
    datalist_multisynth_Berkeley[[j]] <- placebo_syn_cov_ridge
    datalist_summary_Berkeley[[j]] <- placebo_syn_cov_summ_ridge
    datalist_pretreatmentmean_Berkeley[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Berkeley != 1) %>% #change name for different cities
      summarise(PretreatmentMean = mean(TotalOz_zipcode),
                store_zip3 = unique(store_zip3))
    
    #extracting long_df for each placebo run
    extraction_longdf_Berkeley[[j]] <- datalist_multisynth_Berkeley[[j]]$long_df
    
    #extracting weights for each placebo run
    extraction_weights_Berkeley[[j]] <- datalist_multisynth_Berkeley[[j]]$weights
    extraction_weights_Berkeley[[j]] <- as.data.frame(extraction_weights_Berkeley[[j]])
    extraction_weights_Berkeley[[j]] <- tibble::rownames_to_column(extraction_weights_Berkeley[[j]], "store_zip3")
    extraction_weights_Berkeley[[j]] <- mutate(extraction_weights_Berkeley[[j]], placebo_zip = i)
    
    #extracting att for each placebo run
    extraction_att_Berkeley[[j]] <- datalist_summary_Berkeley[[j]]$att
    extraction_att_Berkeley[[j]] <- as.data.frame(extraction_att_Berkeley[[j]])
    extraction_att_Berkeley[[j]] <- filter(extraction_att_Berkeley[[j]], Level != "Average")
    j <- j+1
  }
  
  #Extracting meaningful information
  extraction_longdf_all_Berkeley <- dplyr::select(datalist_multisynth_Berkeley[[length(datalist_multisynth_Berkeley)]]$long_df, -placebo_Berkeley) #only need (last) one since df from each run is the same
  extraction_weights_all_Berkeley <- bind_rows(extraction_weights_Berkeley)
  extraction_att_all_Berkeley <- bind_rows(extraction_att_Berkeley)
  extraction_pretreatmentmean_all_Berkeley <- bind_rows(datalist_pretreatmentmean_Berkeley)
  
  #Writing the data output
  fwrite(extraction_longdf_all_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Berkeley.csv")
  fwrite(extraction_weights_all_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Berkeley.csv")
  fwrite(extraction_att_all_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Berkeley.csv")
  fwrite(extraction_pretreatmentmean_all_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Berkeley.csv")
  
}



#################################################################
###########################BORDERS###############################
#################################################################

SCEstimationData_Composite_Borders <- NielsenMovement_AllMonths_withCensusData_ZipCode

#Filtering out non-urban donor zip codes
SCEstimationData_Composite_Borders <- filter(SCEstimationData_Composite_Borders, perc_urban > MeanUrbanicity_Borders-SDUrbanicity | 
                                       store_zip3 %in% taxed_zips |
                                       store_zip3 %in% border_zips)

#Filtering out all taxed zip codes
SCEstimationData_Composite_Borders <- filter(SCEstimationData_Composite_Borders,
                                             !(store_zip3 %in% taxed_zips))


#Adding logged total ounces as a variable
SCEstimationData_Composite_Borders <- mutate(SCEstimationData_Composite_Borders, log_TotalOz_zipcode = log(TotalOz_zipcode))

#Calculating pretreatment means for all border zip codes
PreTreatmentMeans_Borders <- filter(SCEstimationData_Composite_Borders, store_zip3 %in% border_zips & treated_unit_border == 0) %>%
  group_by(store_zip3) %>%
  summarise(PreTreatmentMeans = mean(TotalOz_zipcode))


##############################
#####ALL BORDERS TOGETHER#####
##############################

#############################################################
#####BALANCED: Including covariates and ridge correction#####
#############################################################

if(Drop_Berkeley == 0){
  # with default nu
  ppool_syn_cov_ridge_Borders <- multisynth(TotalOz_zipcode ~ treated_unit_border | medHHincome + population + medage + num_housingunits +
                                              pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                            store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 26, n_lags = 38)
  
  print(ppool_syn_cov_ridge_Borders$nu)
  #[1] 0.4765759
}

if(Drop_Berkeley == 1){
  # with default nu
  ppool_syn_cov_ridge_Borders <- multisynth(TotalOz_zipcode ~ treated_unit_border | medHHincome + population + medage + num_housingunits +
                                              pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                            store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 26, n_lags = 60)
  
  print(ppool_syn_cov_ridge_Borders$nu)
  #[1]0.4709868
}

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_Borders <- summary(ppool_syn_cov_ridge_Borders)
ppool_syn_cov_summ_ridge_Borders

#Plotting multisynth results
ppool_syn_cov_plot_ridge_Borders <- plot(ppool_syn_cov_summ_ridge_Borders, inf = F)
ppool_syn_cov_plot_ridge_Borders

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_AllCities_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_Borders <- ppool_syn_cov_ridge_Borders$long_df

#extracting att for each placebo run
extraction_att_Borders <- ppool_syn_cov_summ_ridge_Borders$att

#extracting weights for each placebo run
extraction_weights_Borders <- ppool_syn_cov_ridge_Borders$weights
extraction_weights_Borders <- as.data.frame(extraction_weights_Borders)
extraction_weights_Borders <- tibble::rownames_to_column(extraction_weights_Borders, "store_zip3")
colnames(extraction_weights_Borders) <- c("store_zip3", "Philadelphia_080", "Philadelphia_081", 
                                          "Boulder_800", "Boulder_804", "Boulder_805", 
                                          "SF_940", "Oakland_945", "Oakland_948", "SF_949", "Seattle_980",
                                          "Seattle_982", "Seattle_983", "Seattle_984") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders.csv")
fwrite(extraction_weights_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders.csv")
fwrite(extraction_att_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders.csv")
fwrite(PreTreatmentMeans_Borders, "Data/multisynth_placebo_data/Urbanicity/PreTreatmentMeans_Borders.csv")


###############################################################
#####UNBALANCED: Including covariates and ridge correction#####
###############################################################

# with default nu
ppool_syn_cov_ridge_unbalanced_Borders <- multisynth(TotalOz_zipcode ~ treated_unit_border | medHHincome + population + medage + num_housingunits +
                                                       pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                     store_zip3, calendar_time, SCEstimationData_Composite_Borders, 
                                                     nu = 0, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 60)

print(ppool_syn_cov_ridge_unbalanced_Borders$nu)
#[1] 0.4770159

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_unbalanced_Borders <- summary(ppool_syn_cov_ridge_unbalanced_Borders)
ppool_syn_cov_summ_ridge_unbalanced_Borders

#Plotting multisynth results
ppool_syn_cov_plot_ridge_unbalanced_Borders <- plot(ppool_syn_cov_summ_ridge_unbalanced_Borders, inf = F)
ppool_syn_cov_plot_ridge_unbalanced_Borders

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_AllCities_unbalanced_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_unbalanced_Borders <- ppool_syn_cov_ridge_unbalanced_Borders$long_df

#extracting att for each placebo run
extraction_att_unbalanced_Borders <- ppool_syn_cov_summ_ridge_unbalanced_Borders$att

#extracting weights for each placebo run
extraction_weights_unbalanced_Borders <- ppool_syn_cov_ridge_unbalanced_Borders$weights
extraction_weights_unbalanced_Borders <- as.data.frame(extraction_weights_unbalanced_Borders)
extraction_weights_unbalanced_Borders <- tibble::rownames_to_column(extraction_weights_unbalanced_Borders, "store_zip3")
colnames(extraction_weights_unbalanced_Borders) <- c("store_zip3", "Philadelphia_080", "Philadelphia_081", 
                                                     "Boulder_800", "Boulder_804", "Boulder_805", 
                                                     "SF_940", "Oakland_945", "Oakland_948", "SF_949", "Seattle_980",
                                                     "Seattle_982", "Seattle_983", "Seattle_984") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_unbalanced_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_unbalanced_Borders.csv")
fwrite(extraction_weights_unbalanced_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_unbalanced_Borders.csv")
fwrite(extraction_att_unbalanced_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_unbalanced_Borders.csv")



###################
#####SF BORDER#####
###################

###################################################
#####Including covariates and ridge correction#####
###################################################

# with default nu
ppool_syn_cov_ridge_Borders_SF <- multisynth(TotalOz_zipcode ~ treated_unit_border_SF | medHHincome + population + medage + num_housingunits +
                                               pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                             store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T)

print(ppool_syn_cov_ridge_Borders_SF$nu)
#[1] 0.8728

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_Borders_SF <- summary(ppool_syn_cov_ridge_Borders_SF)
ppool_syn_cov_summ_ridge_Borders_SF

#Plotting multisynth results
ppool_syn_cov_plot_ridge_Borders_SF <- plot(ppool_syn_cov_summ_ridge_Borders_SF, inf = F)
ppool_syn_cov_plot_ridge_Borders_SF

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_SF_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_Borders_SF <- ppool_syn_cov_ridge_Borders_SF$long_df

#extracting att for each placebo run
extraction_att_Borders_SF <- ppool_syn_cov_summ_ridge_Borders_SF$att

#extracting weights for each placebo run
extraction_weights_Borders_SF <- ppool_syn_cov_ridge_Borders_SF$weights
extraction_weights_Borders_SF <- as.data.frame(extraction_weights_Borders_SF)
extraction_weights_Borders_SF <- tibble::rownames_to_column(extraction_weights_Borders_SF, "store_zip3")
colnames(extraction_weights_Borders_SF) <- c("store_zip3", "SF_940", "SF_949") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_Borders_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders_SF.csv")
fwrite(extraction_weights_Borders_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders_SF.csv")
fwrite(extraction_att_Borders_SF, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders_SF.csv")


########################
#####SEATTLE BORDER#####
########################

###################################################
#####Including covariates and ridge correction#####
###################################################

# with default nu
ppool_syn_cov_ridge_Borders_Seattle <- multisynth(TotalOz_zipcode ~ treated_unit_border_Seattle | medHHincome + population + medage + num_housingunits +
                                                    pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                  store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T)

print(ppool_syn_cov_ridge_Borders_Seattle$nu)
#[1] 0.8706676

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_Borders_Seattle <- summary(ppool_syn_cov_ridge_Borders_Seattle)
ppool_syn_cov_summ_ridge_Borders_Seattle

#Plotting multisynth results
ppool_syn_cov_plot_ridge_Borders_Seattle <- plot(ppool_syn_cov_summ_ridge_Borders_Seattle, inf = F)
ppool_syn_cov_plot_ridge_Borders_Seattle

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_Seattle_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_Borders_Seattle <- ppool_syn_cov_ridge_Borders_Seattle$long_df

#extracting att for each placebo run
extraction_att_Borders_Seattle <- ppool_syn_cov_summ_ridge_Borders_Seattle$att

#extracting weights for each placebo run
extraction_weights_Borders_Seattle <- ppool_syn_cov_ridge_Borders_Seattle$weights
extraction_weights_Borders_Seattle <- as.data.frame(extraction_weights_Borders_Seattle)
extraction_weights_Borders_Seattle <- tibble::rownames_to_column(extraction_weights_Borders_Seattle, "store_zip3")
colnames(extraction_weights_Borders_Seattle) <- c("store_zip3", "Seattle_980",
                                                  "Seattle_982", "Seattle_983", "Seattle_984") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_Borders_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders_Seattle.csv")
fwrite(extraction_weights_Borders_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders_Seattle.csv")
fwrite(extraction_att_Borders_Seattle, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders_Seattle.csv")


########################
#####BOULDER BORDER#####
########################

###################################################
#####Including covariates and ridge correction#####
###################################################

# with default nu
ppool_syn_cov_ridge_Borders_Boulder <- multisynth(TotalOz_zipcode ~ treated_unit_border_Boulder | medHHincome + population + medage + num_housingunits +
                                                    pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                  store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T)

print(ppool_syn_cov_ridge_Borders_Boulder$nu)
#[1] 0.6721443

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_Borders_Boulder <- summary(ppool_syn_cov_ridge_Borders_Boulder)
ppool_syn_cov_summ_ridge_Borders_Boulder

#Plotting multisynth results
ppool_syn_cov_plot_ridge_Borders_Boulder <- plot(ppool_syn_cov_summ_ridge_Borders_Boulder, inf = F)
ppool_syn_cov_plot_ridge_Borders_Boulder

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_Boulder_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_Borders_Boulder <- ppool_syn_cov_ridge_Borders_Boulder$long_df

#extracting att for each placebo run
extraction_att_Borders_Boulder <- ppool_syn_cov_summ_ridge_Borders_Boulder$att

#extracting weights for each placebo run
extraction_weights_Borders_Boulder <- ppool_syn_cov_ridge_Borders_Boulder$weights
extraction_weights_Borders_Boulder <- as.data.frame(extraction_weights_Borders_Boulder)
extraction_weights_Borders_Boulder <- tibble::rownames_to_column(extraction_weights_Borders_Boulder, "store_zip3")
colnames(extraction_weights_Borders_Boulder) <- c("store_zip3", "Boulder_800",
                                                  "Boulder_804", "Boulder_805") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_Borders_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders_Boulder.csv")
fwrite(extraction_weights_Borders_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders_Boulder.csv")
fwrite(extraction_att_Borders_Boulder, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders_Boulder.csv")


########################
#####PHILLY BORDER######
########################

###################################################
#####Including covariates and ridge correction#####
###################################################

# with default nu
ppool_syn_cov_ridge_Borders_Philadelphia <- multisynth(TotalOz_zipcode ~ treated_unit_border_Philadelphia | medHHincome + population + medage + num_housingunits +
                                                         pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                       store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T)

print(ppool_syn_cov_ridge_Borders_Philadelphia$nu)
#[1] 0.9568504

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_Borders_Philadelphia <- summary(ppool_syn_cov_ridge_Borders_Philadelphia)
ppool_syn_cov_summ_ridge_Borders_Philadelphia

#Plotting multisynth results
ppool_syn_cov_plot_ridge_Borders_Philadelphia <- plot(ppool_syn_cov_summ_ridge_Borders_Philadelphia, inf = F)
ppool_syn_cov_plot_ridge_Borders_Philadelphia

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_Philadelphia_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_Borders_Philadelphia <- ppool_syn_cov_ridge_Borders_Philadelphia$long_df

#extracting att for each placebo run
extraction_att_Borders_Philadelphia <- ppool_syn_cov_summ_ridge_Borders_Philadelphia$att

#extracting weights for each placebo run
extraction_weights_Borders_Philadelphia <- ppool_syn_cov_ridge_Borders_Philadelphia$weights
extraction_weights_Borders_Philadelphia <- as.data.frame(extraction_weights_Borders_Philadelphia)
extraction_weights_Borders_Philadelphia <- tibble::rownames_to_column(extraction_weights_Borders_Philadelphia, "store_zip3")
colnames(extraction_weights_Borders_Philadelphia) <- c("store_zip3", "Philadelphia_080",
                                                       "Philadelphia_081") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_Borders_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders_Philadelphia.csv")
fwrite(extraction_weights_Borders_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders_Philadelphia.csv")
fwrite(extraction_att_Borders_Philadelphia, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders_Philadelphia.csv")




#########################
#####OAKLAND BORDER######
#########################

###################################################
#####Including covariates and ridge correction#####
###################################################

# with default nu
ppool_syn_cov_ridge_Borders_Oakland <- multisynth(TotalOz_zipcode ~ treated_unit_border_Oakland | medHHincome + population + medage + num_housingunits +
                                                    pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                  store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T)

print(ppool_syn_cov_ridge_Borders_Oakland$nu)
#[1] 1

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_Borders_Oakland <- summary(ppool_syn_cov_ridge_Borders_Oakland)
ppool_syn_cov_summ_ridge_Borders_Oakland

#Plotting multisynth results
ppool_syn_cov_plot_ridge_Borders_Oakland <- plot(ppool_syn_cov_summ_ridge_Borders_Oakland, inf = F)
ppool_syn_cov_plot_ridge_Borders_Oakland

ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_Oakland_Borders.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_Borders_Oakland <- ppool_syn_cov_ridge_Borders_Oakland$long_df

#extracting att for each placebo run
extraction_att_Borders_Oakland <- ppool_syn_cov_summ_ridge_Borders_Oakland$att

#extracting weights for each placebo run
extraction_weights_Borders_Oakland <- ppool_syn_cov_ridge_Borders_Oakland$weights
extraction_weights_Borders_Oakland <- as.data.frame(extraction_weights_Borders_Oakland)
extraction_weights_Borders_Oakland <- tibble::rownames_to_column(extraction_weights_Borders_Oakland, "store_zip3")
colnames(extraction_weights_Borders_Oakland) <- c("store_zip3", "Oakland_948") #Order determined by att output order

#Writing the data output
fwrite(extraction_longdf_Borders_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders_Oakland.csv")
fwrite(extraction_weights_Borders_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders_Oakland.csv")
fwrite(extraction_att_Borders_Oakland, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders_Oakland.csv")


if(Drop_Berkeley == 0){
  
  ##########################
  #####BERKELEY BORDER######
  ##########################
  
  ###################################################
  #####Including covariates and ridge correction#####
  ###################################################
  
  # with default nu
  ppool_syn_cov_ridge_Borders_Berkeley <- multisynth(TotalOz_zipcode ~ treated_unit_border_Berkeley | medHHincome + population + medage + num_housingunits +
                                                       pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                     store_zip3, calendar_time, SCEstimationData_Composite_Borders, fixedeff = T, progfunc="Ridge", scm = T)
  
  print(ppool_syn_cov_ridge_Borders_Berkeley$nu)
  #[1] 1
  
  #Gives dataframe of ATT w/ SEs for each treated unit
  ppool_syn_cov_summ_ridge_Borders_Berkeley <- summary(ppool_syn_cov_ridge_Borders_Berkeley)
  ppool_syn_cov_summ_ridge_Borders_Berkeley
  
  #Plotting multisynth results
  ppool_syn_cov_plot_ridge_Borders_Berkeley <- plot(ppool_syn_cov_summ_ridge_Borders_Berkeley, inf = F)
  ppool_syn_cov_plot_ridge_Borders_Berkeley
  
  ggsave("Figures/Results/Multisynth Results/Borders/Urbanicity/Multisynth_Berkeley_Borders.png", width = 40, height = 25, units = "cm")
  
  #extracting long_df for each placebo run
  extraction_longdf_Borders_Berkeley <- ppool_syn_cov_ridge_Borders_Berkeley$long_df
  
  #extracting att for each placebo run
  extraction_att_Borders_Berkeley <- ppool_syn_cov_summ_ridge_Borders_Berkeley$att
  
  #extracting weights for each placebo run
  extraction_weights_Borders_Berkeley <- ppool_syn_cov_ridge_Borders_Berkeley$weights
  extraction_weights_Borders_Berkeley <- as.data.frame(extraction_weights_Borders_Berkeley)
  extraction_weights_Borders_Berkeley <- tibble::rownames_to_column(extraction_weights_Borders_Berkeley, "store_zip3")
  colnames(extraction_weights_Borders_Berkeley) <- c("store_zip3", "Berkeley_948") #Order determined by att output order
  
  #Writing the data output
  fwrite(extraction_longdf_Borders_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders_Berkeley.csv")
  fwrite(extraction_weights_Borders_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_Borders_Berkeley.csv")
  fwrite(extraction_att_Borders_Berkeley, "Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders_Berkeley.csv")
  
}


#########################################################################
#####Including covariates and ridge correction - PLACEBO ESTIMATIONS#####
#########################################################################

# ##take sample of data for quicker estimation
# set.seed <- 05051992
# placebo_zips <- sample(SCEstimationData_Composite_Borders$store_zip3, 5)

placebo_zips <- unique(SCEstimationData_Composite_Borders$store_zip3)
placebo_data <- filter(SCEstimationData_Composite_Borders, store_zip3 %in% placebo_zips)
placebo_data <- mutate(placebo_data, placebo_col = NA)


####San Francisco####

datalist_multisynth_SF <- list()
datalist_summary_SF <- list()
datalist_pretreatmentmean_SF <- list()

extraction_longdf_SF <- list()
extraction_weights_SF <- list()
extraction_att_SF <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_SF" #change name for different cities
  placebo_data$placebo_SF <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_SanFrancisco >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_SF | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_SF[[j]] <- placebo_syn_cov_ridge
  datalist_summary_SF[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_SF[[j]] <- filter(placebo_data, store_zip3 == i & placebo_SF != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_SF[[j]] <- datalist_multisynth_SF[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_SF[[j]] <- datalist_multisynth_SF[[j]]$weights
  extraction_weights_SF[[j]] <- as.data.frame(extraction_weights_SF[[j]])
  extraction_weights_SF[[j]] <- tibble::rownames_to_column(extraction_weights_SF[[j]], "store_zip3")
  extraction_weights_SF[[j]] <- mutate(extraction_weights_SF[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_SF[[j]] <- datalist_summary_SF[[j]]$att
  extraction_att_SF[[j]] <- as.data.frame(extraction_att_SF[[j]])
  extraction_att_SF[[j]] <- filter(extraction_att_SF[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_SF_Borders <- dplyr::select(datalist_multisynth_SF[[length(datalist_multisynth_SF)]]$long_df, -placebo_SF) #only need (last) one since df from each run is the same
extraction_weights_all_SF_Borders <- bind_rows(extraction_weights_SF)
extraction_att_all_SF_Borders <- bind_rows(extraction_att_SF)
extraction_pretreatmentmean_all_SF_Borders <- bind_rows(datalist_pretreatmentmean_SF)

#Writing the data output
fwrite(extraction_longdf_all_SF_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_SF_Borders.csv")
fwrite(extraction_weights_all_SF_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_SF_Borders.csv")
fwrite(extraction_att_all_SF_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_SF_Borders.csv")
fwrite(extraction_pretreatmentmean_all_SF_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_SF_Borders.csv")



####Seattle####

datalist_multisynth_Seattle <- list()
datalist_summary_Seattle <- list()
datalist_pretreatmentmean_Seattle <- list()

extraction_longdf_Seattle <- list()
extraction_weights_Seattle <- list()
extraction_att_Seattle <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Seattle" #change name for different cities
  placebo_data$placebo_Seattle <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Seattle >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Seattle | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Seattle[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Seattle[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Seattle[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Seattle != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Seattle[[j]] <- datalist_multisynth_Seattle[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Seattle[[j]] <- datalist_multisynth_Seattle[[j]]$weights
  extraction_weights_Seattle[[j]] <- as.data.frame(extraction_weights_Seattle[[j]])
  extraction_weights_Seattle[[j]] <- tibble::rownames_to_column(extraction_weights_Seattle[[j]], "store_zip3")
  extraction_weights_Seattle[[j]] <- mutate(extraction_weights_Seattle[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Seattle[[j]] <- datalist_summary_Seattle[[j]]$att
  extraction_att_Seattle[[j]] <- as.data.frame(extraction_att_Seattle[[j]])
  extraction_att_Seattle[[j]] <- filter(extraction_att_Seattle[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Seattle_Borders <- dplyr::select(datalist_multisynth_Seattle[[length(datalist_multisynth_Seattle)]]$long_df, -placebo_Seattle) #only need (last) one since df from each run is the same
extraction_weights_all_Seattle_Borders <- bind_rows(extraction_weights_Seattle)
extraction_att_all_Seattle_Borders <- bind_rows(extraction_att_Seattle)
extraction_pretreatmentmean_all_Seattle_Borders <- bind_rows(datalist_pretreatmentmean_Seattle)

#Writing the data output
fwrite(extraction_longdf_all_Seattle_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Seattle_Borders.csv")
fwrite(extraction_weights_all_Seattle_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Seattle_Borders.csv")
fwrite(extraction_att_all_Seattle_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Seattle_Borders.csv")
fwrite(extraction_pretreatmentmean_all_Seattle_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Seattle_Borders.csv")



####Boulder####

datalist_multisynth_Boulder <- list()
datalist_summary_Boulder <- list()
datalist_pretreatmentmean_Boulder <- list()

extraction_longdf_Boulder <- list()
extraction_weights_Boulder <- list()
extraction_att_Boulder <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Boulder" #change name for different cities
  placebo_data$placebo_Boulder <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Boulder >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Boulder | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Boulder[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Boulder[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Boulder[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Boulder != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Boulder[[j]] <- datalist_multisynth_Boulder[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Boulder[[j]] <- datalist_multisynth_Boulder[[j]]$weights
  extraction_weights_Boulder[[j]] <- as.data.frame(extraction_weights_Boulder[[j]])
  extraction_weights_Boulder[[j]] <- tibble::rownames_to_column(extraction_weights_Boulder[[j]], "store_zip3")
  extraction_weights_Boulder[[j]] <- mutate(extraction_weights_Boulder[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Boulder[[j]] <- datalist_summary_Boulder[[j]]$att
  extraction_att_Boulder[[j]] <- as.data.frame(extraction_att_Boulder[[j]])
  extraction_att_Boulder[[j]] <- filter(extraction_att_Boulder[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Boulder_Borders <- dplyr::select(datalist_multisynth_Boulder[[length(datalist_multisynth_Boulder)]]$long_df, -placebo_Boulder) #only need (last) one since df from each run is the same
extraction_weights_all_Boulder_Borders <- bind_rows(extraction_weights_Boulder)
extraction_att_all_Boulder_Borders <- bind_rows(extraction_att_Boulder)
extraction_pretreatmentmean_all_Boulder_Borders <- bind_rows(datalist_pretreatmentmean_Boulder)

#Writing the data output
fwrite(extraction_longdf_all_Boulder_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Boulder_Borders.csv")
fwrite(extraction_weights_all_Boulder_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Boulder_Borders.csv")
fwrite(extraction_att_all_Boulder_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Boulder_Borders.csv")
fwrite(extraction_pretreatmentmean_all_Boulder_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Boulder_Borders.csv")



####Philadelphia####

datalist_multisynth_Philadelphia <- list()
datalist_summary_Philadelphia <- list()
datalist_pretreatmentmean_Philadelphia <- list()

extraction_longdf_Philadelphia <- list()
extraction_weights_Philadelphia <- list()
extraction_att_Philadelphia <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Philadelphia" #change name for different cities
  placebo_data$placebo_Philadelphia <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Philadelphia >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Philadelphia | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Philadelphia[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Philadelphia[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Philadelphia[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Philadelphia != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Philadelphia[[j]] <- datalist_multisynth_Philadelphia[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Philadelphia[[j]] <- datalist_multisynth_Philadelphia[[j]]$weights
  extraction_weights_Philadelphia[[j]] <- as.data.frame(extraction_weights_Philadelphia[[j]])
  extraction_weights_Philadelphia[[j]] <- tibble::rownames_to_column(extraction_weights_Philadelphia[[j]], "store_zip3")
  extraction_weights_Philadelphia[[j]] <- mutate(extraction_weights_Philadelphia[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Philadelphia[[j]] <- datalist_summary_Philadelphia[[j]]$att
  extraction_att_Philadelphia[[j]] <- as.data.frame(extraction_att_Philadelphia[[j]])
  extraction_att_Philadelphia[[j]] <- filter(extraction_att_Philadelphia[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Philadelphia_Borders <- dplyr::select(datalist_multisynth_Philadelphia[[length(datalist_multisynth_Philadelphia)]]$long_df, -placebo_Philadelphia) #only need (last) one since df from each run is the same
extraction_weights_all_Philadelphia_Borders <- bind_rows(extraction_weights_Philadelphia)
extraction_att_all_Philadelphia_Borders <- bind_rows(extraction_att_Philadelphia)
extraction_pretreatmentmean_all_Philadelphia_Borders <- bind_rows(datalist_pretreatmentmean_Philadelphia)

#Writing the data output
fwrite(extraction_longdf_all_Philadelphia_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Philadelphia_Borders.csv")
fwrite(extraction_weights_all_Philadelphia_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Philadelphia_Borders.csv")
fwrite(extraction_att_all_Philadelphia_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Philadelphia_Borders.csv")
fwrite(extraction_pretreatmentmean_all_Philadelphia_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Philadelphia_Borders.csv")



####Oakland####

datalist_multisynth_Oakland <- list()
datalist_summary_Oakland <- list()
datalist_pretreatmentmean_Oakland <- list()

extraction_longdf_Oakland <- list()
extraction_weights_Oakland <- list()
extraction_att_Oakland <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Oakland" #change name for different cities
  placebo_data$placebo_Oakland <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Oakland >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Oakland | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Oakland[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Oakland[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Oakland[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Oakland != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(TotalOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Oakland[[j]] <- datalist_multisynth_Oakland[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Oakland[[j]] <- datalist_multisynth_Oakland[[j]]$weights
  extraction_weights_Oakland[[j]] <- as.data.frame(extraction_weights_Oakland[[j]])
  extraction_weights_Oakland[[j]] <- tibble::rownames_to_column(extraction_weights_Oakland[[j]], "store_zip3")
  extraction_weights_Oakland[[j]] <- mutate(extraction_weights_Oakland[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Oakland[[j]] <- datalist_summary_Oakland[[j]]$att
  extraction_att_Oakland[[j]] <- as.data.frame(extraction_att_Oakland[[j]])
  extraction_att_Oakland[[j]] <- filter(extraction_att_Oakland[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Oakland_Borders <- dplyr::select(datalist_multisynth_Oakland[[length(datalist_multisynth_Oakland)]]$long_df, -placebo_Oakland) #only need (last) one since df from each run is the same
extraction_weights_all_Oakland_Borders <- bind_rows(extraction_weights_Oakland)
extraction_att_all_Oakland_Borders <- bind_rows(extraction_att_Oakland)
extraction_pretreatmentmean_all_Oakland_Borders <- bind_rows(datalist_pretreatmentmean_Oakland)

#Writing the data output
fwrite(extraction_longdf_all_Oakland_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Oakland_Borders.csv")
fwrite(extraction_weights_all_Oakland_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Oakland_Borders.csv")
fwrite(extraction_att_all_Oakland_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Oakland_Borders.csv")
fwrite(extraction_pretreatmentmean_all_Oakland_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Oakland_Borders.csv")


if(Drop_Berkeley == 0){
  
  
  ####Berkeley####
  
  datalist_multisynth_Berkeley <- list()
  datalist_summary_Berkeley <- list()
  datalist_pretreatmentmean_Berkeley <- list()
  
  extraction_longdf_Berkeley <- list()
  extraction_weights_Berkeley <- list()
  extraction_att_Berkeley <- list()
  
  j <- 1
  
  for(i in placebo_zips){
    colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Berkeley" #change name for different cities
    placebo_data$placebo_Berkeley <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Berkeley >= 0), 1, 0) #change time for different cities
    
    # with default nu
    placebo_syn_cov_ridge <- multisynth(TotalOz_zipcode ~ placebo_Berkeley | medHHincome + population + medage + num_housingunits + #change name for different cities
                                          pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                        store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
    
    placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
    
    datalist_multisynth_Berkeley[[j]] <- placebo_syn_cov_ridge
    datalist_summary_Berkeley[[j]] <- placebo_syn_cov_summ_ridge
    datalist_pretreatmentmean_Berkeley[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Berkeley != 1) %>% #change name for different cities
      summarise(PretreatmentMean = mean(TotalOz_zipcode),
                store_zip3 = unique(store_zip3))
    
    #extracting long_df for each placebo run
    extraction_longdf_Berkeley[[j]] <- datalist_multisynth_Berkeley[[j]]$long_df
    
    #extracting weights for each placebo run
    extraction_weights_Berkeley[[j]] <- datalist_multisynth_Berkeley[[j]]$weights
    extraction_weights_Berkeley[[j]] <- as.data.frame(extraction_weights_Berkeley[[j]])
    extraction_weights_Berkeley[[j]] <- tibble::rownames_to_column(extraction_weights_Berkeley[[j]], "store_zip3")
    extraction_weights_Berkeley[[j]] <- mutate(extraction_weights_Berkeley[[j]], placebo_zip = i)
    
    #extracting att for each placebo run
    extraction_att_Berkeley[[j]] <- datalist_summary_Berkeley[[j]]$att
    extraction_att_Berkeley[[j]] <- as.data.frame(extraction_att_Berkeley[[j]])
    extraction_att_Berkeley[[j]] <- filter(extraction_att_Berkeley[[j]], Level != "Average")
    j <- j+1
  }
  
  #Extracting meaningful information
  extraction_longdf_all_Berkeley_Borders <- dplyr::select(datalist_multisynth_Berkeley[[length(datalist_multisynth_Berkeley)]]$long_df, -placebo_Berkeley) #only need (last) one since df from each run is the same
  extraction_weights_all_Berkeley_Borders <- bind_rows(extraction_weights_Berkeley)
  extraction_att_all_Berkeley_Borders <- bind_rows(extraction_att_Berkeley)
  extraction_pretreatmentmean_all_Berkeley_Borders <- bind_rows(datalist_pretreatmentmean_Berkeley)
  
  #Writing the data output
  fwrite(extraction_longdf_all_Berkeley_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Berkeley_Borders.csv")
  fwrite(extraction_weights_all_Berkeley_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Berkeley_Borders.csv")
  fwrite(extraction_att_all_Berkeley_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Berkeley_Borders.csv")
  fwrite(extraction_pretreatmentmean_all_Berkeley_Borders, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Berkeley_Borders.csv")
  
}



################################################################
###########################PRICES###############################
################################################################

SCEstimationData_Composite_Prices <- NielsenMovement_AllMonths_withCensusData_ZipCode_Prices

#Filtering out non-urban donor zip codes
SCEstimationData_Composite_Prices <- filter(SCEstimationData_Composite_Prices, perc_urban > MeanUrbanicity_Treated-SDUrbanicity | 
                                               store_zip3 %in% taxed_zips |
                                               store_zip3 %in% border_zips)

#Filtering out all border zip codes
SCEstimationData_Composite_Prices <- filter(SCEstimationData_Composite_Prices,
                                            !(store_zip3 %in% border_zips))


#Adding logged total ounces as a variable
SCEstimationData_Composite_Prices <- mutate(SCEstimationData_Composite_Prices, log_AvgPrice_PerOz_zipcode = log(AvgPrice_PerOz_zipcode))

#Calculating pretreatment means for all taxed zip codes
PreTreatmentMeans_Taxed_Prices <- filter(SCEstimationData_Composite_Prices, store_zip3 %in% taxed_zips & treated == 0) %>%
  group_by(store_zip3) %>%
  summarise(PreTreatmentMeans = mean(AvgPrice_PerOz_zipcode))


#############################################################
#####BALANCED: Including covariates and ridge correction#####
#############################################################

if(Drop_Berkeley == 0){
  # with default nu
  ppool_syn_cov_ridge_prices <- multisynth(AvgPrice_PerOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
                                             pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                           store_zip3, calendar_time, SCEstimationData_Composite_Prices, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 26, n_lags = 38) #Make lead 26 to ensure period 25 is included
  
  print(ppool_syn_cov_ridge_prices$nu)
  #[1] 0.3726607
}

if(Drop_Berkeley == 1){
  # with default nu
  ppool_syn_cov_ridge_prices <- multisynth(AvgPrice_PerOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
                                             pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                           store_zip3, calendar_time, SCEstimationData_Composite_Prices, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 26, n_lags = 60) #Make lead 26 to ensure period 25 is included
  
  print(ppool_syn_cov_ridge_prices$nu)
  #[1] 0.4249873
}

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_prices <- summary(ppool_syn_cov_ridge_prices)
ppool_syn_cov_summ_ridge_prices

#Plotting multisynth results
ppool_syn_cov_plot_ridge_prices <- plot(ppool_syn_cov_summ_ridge_prices, inf = F)
ppool_syn_cov_plot_ridge_prices

ggsave("Figures/Results/Multisynth Results/Prices/Urbanicity/Multisynth_AllCities_prices.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_AllTaxed_prices <- ppool_syn_cov_ridge_prices$long_df

#extracting att for each placebo run
extraction_att_AllTaxed_prices <- ppool_syn_cov_summ_ridge_prices$att

#extracting weights for each placebo run
extraction_weights_AllTaxed_prices <- ppool_syn_cov_ridge_prices$weights
extraction_weights_AllTaxed_prices <- as.data.frame(extraction_weights_AllTaxed_prices)
extraction_weights_AllTaxed_prices <- tibble::rownames_to_column(extraction_weights_AllTaxed_prices, "store_zip3")
if(Drop_Berkeley == 0){
  colnames(extraction_weights_AllTaxed_prices) <- c("store_zip3", "Philadelphia", "Boulder", "SF", "Oakland",
                                                    "Berkeley", "Seattle") #Order determined by att output order 
}
if(Drop_Berkeley == 1){
  colnames(extraction_weights_AllTaxed_prices) <- c("store_zip3", "Philadelphia", "Boulder", 
                                                    "SF", "Oakland", "Seattle") #Order determined by att output order 
}

#Writing the data output
fwrite(extraction_longdf_AllTaxed_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed_prices.csv")
fwrite(extraction_weights_AllTaxed_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_AllTaxed_prices.csv")
fwrite(extraction_att_AllTaxed_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed_prices.csv")
fwrite(PreTreatmentMeans_Taxed_Prices, "Data/multisynth_placebo_data/Urbanicity/PreTreatmentMeans_Taxed_prices.csv")



###############################################################
#####UNBALANCED: Including covariates and ridge correction#####
###############################################################

# with default nu
ppool_syn_cov_ridge_unbalanced_prices <- multisynth(AvgPrice_PerOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
                                                      pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                                    store_zip3, calendar_time, SCEstimationData_Composite_Prices, 
                                                    nu = 0, fixedeff = T, progfunc="Ridge", scm = T, n_leads = 60)

print(ppool_syn_cov_ridge_unbalanced_prices$nu)
#[1] 0.4242538

#Gives dataframe of ATT w/ SEs for each treated unit
ppool_syn_cov_summ_ridge_unbalanced_prices <- summary(ppool_syn_cov_ridge_unbalanced_prices)
ppool_syn_cov_summ_ridge_unbalanced_prices

#Plotting multisynth results
ppool_syn_cov_plot_ridge_unbalanced_prices <- plot(ppool_syn_cov_summ_ridge_unbalanced_prices, inf = F)
ppool_syn_cov_plot_ridge_unbalanced_prices

ggsave("Figures/Results/Multisynth Results/Prices/Urbanicity/Multisynth_AllCities_unbalanced_prices.png", width = 40, height = 25, units = "cm")

#extracting long_df for each placebo run
extraction_longdf_AllTaxed_unbalanced_prices <- ppool_syn_cov_ridge_unbalanced_prices$long_df

#extracting att for each placebo run
extraction_att_AllTaxed_unbalanced_prices <- ppool_syn_cov_summ_ridge_unbalanced_prices$att

#extracting weights for each placebo run
extraction_weights_AllTaxed_unbalanced_prices <- ppool_syn_cov_ridge_unbalanced_prices$weights
extraction_weights_AllTaxed_unbalanced_prices <- as.data.frame(extraction_weights_AllTaxed_unbalanced_prices)
extraction_weights_AllTaxed_unbalanced_prices <- tibble::rownames_to_column(extraction_weights_AllTaxed_unbalanced_prices, "store_zip3")
if(Drop_Berkeley == 0){
  colnames(extraction_weights_AllTaxed_unbalanced_prices) <- c("store_zip3", "Philadelphia", "Boulder", "SF", "Oakland",
                                                               "Berkeley", "Seattle") #Order determined by att output order
}

if(Drop_Berkeley == 1){
  colnames(extraction_weights_AllTaxed_unbalanced_prices) <- c("store_zip3", "Philadelphia", "Boulder", 
                                                               "SF", "Oakland","Seattle") #Order determined by att output order
}
#Writing the data output
fwrite(extraction_longdf_AllTaxed_unbalanced_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed_unbalanced_prices.csv")
fwrite(extraction_weights_AllTaxed_unbalanced_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_AllTaxed_unbalanced_prices.csv")
fwrite(extraction_att_AllTaxed_unbalanced_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed_unbalanced_prices.csv")


# ######################################################
# #####Including covariates and no ridge correction#####
# ######################################################
# 
# # with default nu
# ppool_syn_cov_nocorrection <- multisynth(TotalOz_zipcode ~ treated | medHHincome + population + medage + num_housingunits +
#                                     pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
#                                   store_zip3, calendar_time, SCEstimationData_Composite, fixedeff = T, progfunc="none", scm = T, n_leads = 25, n_lags = 38)
# 
# print(ppool_syn_cov_nocorrection$nu)
# #[1] 0.540031
# 
# #Gives dataframe of ATT w/ SEs for each treated unit
# ppool_syn_cov_summ_nocorrection <- summary(ppool_syn_cov_nocorrection)
# ppool_syn_cov_summ_nocorrection
# 
# #Plotting multisynth results
# ppool_syn_cov_plot_nocorrection <- plot(ppool_syn_cov_summ_nocorrection, inf = F)
# ppool_syn_cov_plot_nocorrection
# 
# #ggsave("Figures/Results/All Cities Combined/Bias Corrected SCM/Multisynth_nonocorrection.png", width = 40, height = 25, units = "cm")
# 
# #Taking out the average effects
# ATT_cov_nocorrection <- filter(ppool_syn_cov_summ_nocorrection$att, Level == "Average")
# 
# #Again, plotting the average effects
# ppool_syn_cov_plot_avg_nocorrection <- plot(ppool_syn_cov_summ_nocorrection, levels = "Average")
# ppool_syn_cov_plot_avg_nocorrection
# 
# #ggsave("Figures/Results/All Cities Combined/Bias Corrected SCM/Multisynth_noRidge_Average.png", width = 40, height = 25, units = "cm")


#########################################################################
#####Including covariates and ridge correction - PLACEBO ESTIMATIONS#####
#########################################################################

# ##take sample of data for quicker estimation
# set.seed <- 05051992
# placebo_zips <- sample(SCEstimationData_Composite$store_zip3, 5)

placebo_zips <- unique(SCEstimationData_Composite_Prices$store_zip3)
placebo_data <- filter(SCEstimationData_Composite_Prices, store_zip3 %in% placebo_zips)
placebo_data <- mutate(placebo_data, placebo_col = NA)


####San Francisco####

datalist_multisynth_SF <- list()
datalist_summary_SF <- list()
datalist_pretreatmentmean_SF <- list()

extraction_longdf_SF <- list()
extraction_weights_SF <- list()
extraction_att_SF <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_SF" #change name for different cities
  placebo_data$placebo_SF <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_SanFrancisco >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(AvgPrice_PerOz_zipcode ~ placebo_SF | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_SF[[j]] <- placebo_syn_cov_ridge
  datalist_summary_SF[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_SF[[j]] <- filter(placebo_data, store_zip3 == i & placebo_SF != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_SF[[j]] <- datalist_multisynth_SF[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_SF[[j]] <- datalist_multisynth_SF[[j]]$weights
  extraction_weights_SF[[j]] <- as.data.frame(extraction_weights_SF[[j]])
  extraction_weights_SF[[j]] <- tibble::rownames_to_column(extraction_weights_SF[[j]], "store_zip3")
  extraction_weights_SF[[j]] <- mutate(extraction_weights_SF[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_SF[[j]] <- datalist_summary_SF[[j]]$att
  extraction_att_SF[[j]] <- as.data.frame(extraction_att_SF[[j]])
  extraction_att_SF[[j]] <- filter(extraction_att_SF[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_SF_prices <- dplyr::select(datalist_multisynth_SF[[length(datalist_multisynth_SF)]]$long_df, -placebo_SF) #only need (last) one since df from each run is the same
extraction_weights_all_SF_prices <- bind_rows(extraction_weights_SF)
extraction_att_all_SF_prices <- bind_rows(extraction_att_SF)
extraction_pretreatmentmean_all_SF_prices <- bind_rows(datalist_pretreatmentmean_SF)

#Writing the data output
fwrite(extraction_longdf_all_SF_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_SF_prices.csv")
fwrite(extraction_weights_all_SF_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_SF_prices.csv")
fwrite(extraction_att_all_SF_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_SF_prices.csv")
fwrite(extraction_pretreatmentmean_all_SF_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_SF_prices.csv")



####Seattle####

datalist_multisynth_Seattle <- list()
datalist_summary_Seattle <- list()
datalist_pretreatmentmean_Seattle <- list()

extraction_longdf_Seattle <- list()
extraction_weights_Seattle <- list()
extraction_att_Seattle <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Seattle" #change name for different cities
  placebo_data$placebo_Seattle <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Seattle >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(AvgPrice_PerOz_zipcode ~ placebo_Seattle | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Seattle[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Seattle[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Seattle[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Seattle != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Seattle[[j]] <- datalist_multisynth_Seattle[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Seattle[[j]] <- datalist_multisynth_Seattle[[j]]$weights
  extraction_weights_Seattle[[j]] <- as.data.frame(extraction_weights_Seattle[[j]])
  extraction_weights_Seattle[[j]] <- tibble::rownames_to_column(extraction_weights_Seattle[[j]], "store_zip3")
  extraction_weights_Seattle[[j]] <- mutate(extraction_weights_Seattle[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Seattle[[j]] <- datalist_summary_Seattle[[j]]$att
  extraction_att_Seattle[[j]] <- as.data.frame(extraction_att_Seattle[[j]])
  extraction_att_Seattle[[j]] <- filter(extraction_att_Seattle[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Seattle_prices <- dplyr::select(datalist_multisynth_Seattle[[length(datalist_multisynth_Seattle)]]$long_df, -placebo_Seattle) #only need (last) one since df from each run is the same
extraction_weights_all_Seattle_prices <- bind_rows(extraction_weights_Seattle)
extraction_att_all_Seattle_prices <- bind_rows(extraction_att_Seattle)
extraction_pretreatmentmean_all_Seattle_prices <- bind_rows(datalist_pretreatmentmean_Seattle)

#Writing the data output
fwrite(extraction_longdf_all_Seattle_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Seattle_prices.csv")
fwrite(extraction_weights_all_Seattle_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Seattle_prices.csv")
fwrite(extraction_att_all_Seattle_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Seattle_prices.csv")
fwrite(extraction_pretreatmentmean_all_Seattle_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Seattle_prices.csv")



####Boulder####

datalist_multisynth_Boulder <- list()
datalist_summary_Boulder <- list()
datalist_pretreatmentmean_Boulder <- list()

extraction_longdf_Boulder <- list()
extraction_weights_Boulder <- list()
extraction_att_Boulder <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Boulder" #change name for different cities
  placebo_data$placebo_Boulder <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Boulder >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(AvgPrice_PerOz_zipcode ~ placebo_Boulder | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Boulder[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Boulder[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Boulder[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Boulder != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Boulder[[j]] <- datalist_multisynth_Boulder[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Boulder[[j]] <- datalist_multisynth_Boulder[[j]]$weights
  extraction_weights_Boulder[[j]] <- as.data.frame(extraction_weights_Boulder[[j]])
  extraction_weights_Boulder[[j]] <- tibble::rownames_to_column(extraction_weights_Boulder[[j]], "store_zip3")
  extraction_weights_Boulder[[j]] <- mutate(extraction_weights_Boulder[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Boulder[[j]] <- datalist_summary_Boulder[[j]]$att
  extraction_att_Boulder[[j]] <- as.data.frame(extraction_att_Boulder[[j]])
  extraction_att_Boulder[[j]] <- filter(extraction_att_Boulder[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Boulder_prices <- dplyr::select(datalist_multisynth_Boulder[[length(datalist_multisynth_Boulder)]]$long_df, -placebo_Boulder) #only need (last) one since df from each run is the same
extraction_weights_all_Boulder_prices <- bind_rows(extraction_weights_Boulder)
extraction_att_all_Boulder_prices <- bind_rows(extraction_att_Boulder)
extraction_pretreatmentmean_all_Boulder_prices <- bind_rows(datalist_pretreatmentmean_Boulder)

#Writing the data output
fwrite(extraction_longdf_all_Boulder_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Boulder_prices.csv")
fwrite(extraction_weights_all_Boulder_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Boulder_prices.csv")
fwrite(extraction_att_all_Boulder_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Boulder_prices.csv")
fwrite(extraction_pretreatmentmean_all_Boulder_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Boulder_prices.csv")



####Philadelphia####

datalist_multisynth_Philadelphia <- list()
datalist_summary_Philadelphia <- list()
datalist_pretreatmentmean_Philadelphia <- list()

extraction_longdf_Philadelphia <- list()
extraction_weights_Philadelphia <- list()
extraction_att_Philadelphia <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Philadelphia" #change name for different cities
  placebo_data$placebo_Philadelphia <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Philadelphia >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(AvgPrice_PerOz_zipcode ~ placebo_Philadelphia | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Philadelphia[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Philadelphia[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Philadelphia[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Philadelphia != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Philadelphia[[j]] <- datalist_multisynth_Philadelphia[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Philadelphia[[j]] <- datalist_multisynth_Philadelphia[[j]]$weights
  extraction_weights_Philadelphia[[j]] <- as.data.frame(extraction_weights_Philadelphia[[j]])
  extraction_weights_Philadelphia[[j]] <- tibble::rownames_to_column(extraction_weights_Philadelphia[[j]], "store_zip3")
  extraction_weights_Philadelphia[[j]] <- mutate(extraction_weights_Philadelphia[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Philadelphia[[j]] <- datalist_summary_Philadelphia[[j]]$att
  extraction_att_Philadelphia[[j]] <- as.data.frame(extraction_att_Philadelphia[[j]])
  extraction_att_Philadelphia[[j]] <- filter(extraction_att_Philadelphia[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Philadelphia_prices <- dplyr::select(datalist_multisynth_Philadelphia[[length(datalist_multisynth_Philadelphia)]]$long_df, -placebo_Philadelphia) #only need (last) one since df from each run is the same
extraction_weights_all_Philadelphia_prices <- bind_rows(extraction_weights_Philadelphia)
extraction_att_all_Philadelphia_prices <- bind_rows(extraction_att_Philadelphia)
extraction_pretreatmentmean_all_Philadelphia_prices <- bind_rows(datalist_pretreatmentmean_Philadelphia)

#Writing the data output
fwrite(extraction_longdf_all_Philadelphia_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Philadelphia_prices.csv")
fwrite(extraction_weights_all_Philadelphia_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Philadelphia_prices.csv")
fwrite(extraction_att_all_Philadelphia_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Philadelphia_prices.csv")
fwrite(extraction_pretreatmentmean_all_Philadelphia_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Philadelphia_prices.csv")



####Oakland####

datalist_multisynth_Oakland <- list()
datalist_summary_Oakland <- list()
datalist_pretreatmentmean_Oakland <- list()

extraction_longdf_Oakland <- list()
extraction_weights_Oakland <- list()
extraction_att_Oakland <- list()

j <- 1

for(i in placebo_zips){
  colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Oakland" #change name for different cities
  placebo_data$placebo_Oakland <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Oakland >= 0), 1, 0) #change time for different cities
  
  # with default nu
  placebo_syn_cov_ridge <- multisynth(AvgPrice_PerOz_zipcode ~ placebo_Oakland | medHHincome + population + medage + num_housingunits + #change name for different cities
                                        pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                      store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
  
  placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
  
  datalist_multisynth_Oakland[[j]] <- placebo_syn_cov_ridge
  datalist_summary_Oakland[[j]] <- placebo_syn_cov_summ_ridge
  datalist_pretreatmentmean_Oakland[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Oakland != 1) %>% #change name for different cities
    summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode),
              store_zip3 = unique(store_zip3))
  
  #extracting long_df for each placebo run
  extraction_longdf_Oakland[[j]] <- datalist_multisynth_Oakland[[j]]$long_df
  
  #extracting weights for each placebo run
  extraction_weights_Oakland[[j]] <- datalist_multisynth_Oakland[[j]]$weights
  extraction_weights_Oakland[[j]] <- as.data.frame(extraction_weights_Oakland[[j]])
  extraction_weights_Oakland[[j]] <- tibble::rownames_to_column(extraction_weights_Oakland[[j]], "store_zip3")
  extraction_weights_Oakland[[j]] <- mutate(extraction_weights_Oakland[[j]], placebo_zip = i)
  
  #extracting att for each placebo run
  extraction_att_Oakland[[j]] <- datalist_summary_Oakland[[j]]$att
  extraction_att_Oakland[[j]] <- as.data.frame(extraction_att_Oakland[[j]])
  extraction_att_Oakland[[j]] <- filter(extraction_att_Oakland[[j]], Level != "Average")
  j <- j+1
}

#Extracting meaningful information
extraction_longdf_all_Oakland_prices <- dplyr::select(datalist_multisynth_Oakland[[length(datalist_multisynth_Oakland)]]$long_df, -placebo_Oakland) #only need (last) one since df from each run is the same
extraction_weights_all_Oakland_prices <- bind_rows(extraction_weights_Oakland)
extraction_att_all_Oakland_prices <- bind_rows(extraction_att_Oakland)
extraction_pretreatmentmean_all_Oakland_prices <- bind_rows(datalist_pretreatmentmean_Oakland)

#Writing the data output
fwrite(extraction_longdf_all_Oakland_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Oakland_prices.csv")
fwrite(extraction_weights_all_Oakland_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Oakland_prices.csv")
fwrite(extraction_att_all_Oakland_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Oakland_prices.csv")
fwrite(extraction_pretreatmentmean_all_Oakland_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Oakland_prices.csv")


if(Drop_Berkeley == 0){
  
  
  ####Berkeley####
  
  datalist_multisynth_Berkeley <- list()
  datalist_summary_Berkeley <- list()
  datalist_pretreatmentmean_Berkeley <- list()
  
  extraction_longdf_Berkeley <- list()
  extraction_weights_Berkeley <- list()
  extraction_att_Berkeley <- list()
  
  j <- 1
  
  for(i in placebo_zips){
    colnames(placebo_data)[which(names(placebo_data) == "placebo_col")] <- "placebo_Berkeley" #change name for different cities
    placebo_data$placebo_Berkeley <- ifelse((placebo_data$store_zip3 == i) & (placebo_data$time_to_treat_months_Berkeley >= 0), 1, 0) #change time for different cities
    
    # with default nu
    placebo_syn_cov_ridge <- multisynth(AvgPrice_PerOz_zipcode ~ placebo_Berkeley | medHHincome + population + medage + num_housingunits + #change name for different cities
                                          pwhite + pblack + pamindalask + pasian + phawaii + pother + phisp + poverty_10K + age_18to64,
                                        store_zip3, calendar_time, placebo_data, fixedeff = T, progfunc="Ridge", scm = T)
    
    placebo_syn_cov_summ_ridge <- summary(placebo_syn_cov_ridge)
    
    datalist_multisynth_Berkeley[[j]] <- placebo_syn_cov_ridge
    datalist_summary_Berkeley[[j]] <- placebo_syn_cov_summ_ridge
    datalist_pretreatmentmean_Berkeley[[j]] <- filter(placebo_data, store_zip3 == i & placebo_Berkeley != 1) %>% #change name for different cities
      summarise(PretreatmentMean = mean(AvgPrice_PerOz_zipcode),
                store_zip3 = unique(store_zip3))
    
    #extracting long_df for each placebo run
    extraction_longdf_Berkeley[[j]] <- datalist_multisynth_Berkeley[[j]]$long_df
    
    #extracting weights for each placebo run
    extraction_weights_Berkeley[[j]] <- datalist_multisynth_Berkeley[[j]]$weights
    extraction_weights_Berkeley[[j]] <- as.data.frame(extraction_weights_Berkeley[[j]])
    extraction_weights_Berkeley[[j]] <- tibble::rownames_to_column(extraction_weights_Berkeley[[j]], "store_zip3")
    extraction_weights_Berkeley[[j]] <- mutate(extraction_weights_Berkeley[[j]], placebo_zip = i)
    
    #extracting att for each placebo run
    extraction_att_Berkeley[[j]] <- datalist_summary_Berkeley[[j]]$att
    extraction_att_Berkeley[[j]] <- as.data.frame(extraction_att_Berkeley[[j]])
    extraction_att_Berkeley[[j]] <- filter(extraction_att_Berkeley[[j]], Level != "Average")
    j <- j+1
  }
  
  #Extracting meaningful information
  extraction_longdf_all_Berkeley_prices <- dplyr::select(datalist_multisynth_Berkeley[[length(datalist_multisynth_Berkeley)]]$long_df, -placebo_Berkeley) #only need (last) one since df from each run is the same
  extraction_weights_all_Berkeley_prices <- bind_rows(extraction_weights_Berkeley)
  extraction_att_all_Berkeley_prices <- bind_rows(extraction_att_Berkeley)
  extraction_pretreatmentmean_all_Berkeley_prices <- bind_rows(datalist_pretreatmentmean_Berkeley)
  
  #Writing the data output
  fwrite(extraction_longdf_all_Berkeley_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Berkeley_prices.csv")
  fwrite(extraction_weights_all_Berkeley_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_weights_all_Berkeley_prices.csv")
  fwrite(extraction_att_all_Berkeley_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Berkeley_prices.csv")
  fwrite(extraction_pretreatmentmean_all_Berkeley_prices, "Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Berkeley_prices.csv")
  
}