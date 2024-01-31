library(tidysynth)
library(data.table)
library(dplyr)
library(haven)
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
library(gghighlight)

memory.limit(size = 25000000)

##Setting the working directory for SSB Project

##Data Drive
#setwd("S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis")

##Ceto Local Disk
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

source("Code/multisynth_funs.R")

##Whether to drop Berkeley
Drop_Berkeley <- 1

##Whether to weight treatment units by their Census population
Population_Weighted <- 0


#######Bringing in Census Data for Weights#######
PopulationCensusData <- fread("Data/CensusData_Combined_Final_3DigitZip_Population.csv",
                              colClasses = c("character", "numeric", "numeric"))
PopulationCensusData <- dplyr::select(PopulationCensusData,
                                      GEOID_3, population) %>% distinct()
colnames(PopulationCensusData) <- c("store_zip3", "population")


#############################################################
###############TREATED CONSUMPTION ANALYSES##################
#############################################################

#########################
#####Reading in Data#####
#########################

#All Taxed Data
PreTreatmentMeans_AllTaxed <- fread("Data/multisynth_placebo_data/Urbanicity/PreTreatmentMeans_Taxed.csv",
                                    colClasses = c("character", "numeric"))
ATT_AllTaxed <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed.csv")
ATT_AllTaxed_Unbalanced <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed_unbalanced.csv")
LongDF_AllTaxed <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed.csv")

#SF Placebos
PreTreatmentMeans_SF <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_SF.csv",
                              colClasses = c("numeric", "numeric"))
ATT_SF <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_SF.csv")
LongDF_SF <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_SF.csv")

#Seattle Placebos
PreTreatmentMeans_Seattle <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Seattle.csv",
                                   colClasses = c("numeric", "numeric"))
ATT_Seattle <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Seattle.csv")
LongDF_Seattle <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Seattle.csv")

#Boulder Placebos
PreTreatmentMeans_Boulder <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Boulder.csv",
                                   colClasses = c("numeric", "numeric"))
ATT_Boulder <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Boulder.csv")
LongDF_Boulder <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Boulder.csv")

#Philadelphia Placebos
PreTreatmentMeans_Philadelphia <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Philadelphia.csv",
                                        colClasses = c("numeric", "numeric"))
ATT_Philadelphia <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Philadelphia.csv")
LongDF_Philadelphia <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Philadelphia.csv")

#Oakland Placebos
PreTreatmentMeans_Oakland <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Oakland.csv",
                                   colClasses = c("numeric", "numeric"))
ATT_Oakland <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Oakland.csv")
LongDF_Oakland <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Oakland.csv")

#Berkeley Placebos
PreTreatmentMeans_Berkeley <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Berkeley.csv",
                                    colClasses = c("numeric", "numeric"))
ATT_Berkeley <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Berkeley.csv")
LongDF_Berkeley <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Berkeley.csv")


###########################################################################
###############Generating Placebo Dataframe by Taxed City##################
###########################################################################

set.seed(05051992)

##Outputting placebo dataframes
SanFrancisco_Placebo <- PlaceboDataGenFun(ATT_SF, "941", PreTreatmentMeans_SF, LongDF_SF, 72, ATT_AllTaxed_Unbalanced)
Seattle_Placebo <- PlaceboDataGenFun(ATT_Seattle, "981", PreTreatmentMeans_Seattle, LongDF_Seattle, 72, ATT_AllTaxed_Unbalanced)
Boulder_Placebo <- PlaceboDataGenFun(ATT_Boulder, "803", PreTreatmentMeans_Boulder, LongDF_Boulder, 66, ATT_AllTaxed_Unbalanced)
Philadelphia_Placebo <- PlaceboDataGenFun(ATT_Philadelphia, "191", PreTreatmentMeans_Philadelphia, LongDF_Philadelphia, 60, ATT_AllTaxed_Unbalanced)
Oakland_Placebo <- PlaceboDataGenFun(ATT_Oakland, "946", PreTreatmentMeans_Oakland, LongDF_Oakland, 66, ATT_AllTaxed_Unbalanced)
if(Drop_Berkeley == 0){
  Berkeley_Placebo <- PlaceboDataGenFun(ATT_Berkeley, "947", PreTreatmentMeans_Berkeley, LongDF_Berkeley, 38, ATT_AllTaxed_Unbalanced)
}

##Outputting Average placebo dataframe
Average_Placebo <- PlaceboDataGenFun_Average(ATT_AllTaxed, PreTreatmentMeans_AllTaxed, LongDF_AllTaxed)

##Plots for each city
SanFrancisco_Placebo_Plot <- PlaceboPlotFun(SanFrancisco_Placebo[[1]], SanFrancisco_Placebo[[2]],
                                            "941", "San Francisco", "Consumption")
Seattle_Placebo_Plot <- PlaceboPlotFun(Seattle_Placebo[[1]], Seattle_Placebo[[2]],
                                       "981", "Seattle", "Consumption")
Boulder_Placebo_Plot <- PlaceboPlotFun(Boulder_Placebo[[1]], Boulder_Placebo[[2]],
                                       "803", "Boulder", "Consumption")
Philadelphia_Placebo_Plot <- PlaceboPlotFun(Philadelphia_Placebo[[1]], Philadelphia_Placebo[[2]],
                                            "191", "Philadelphia", "Consumption")
Oakland_Placebo_Plot <- PlaceboPlotFun(Oakland_Placebo[[1]], Oakland_Placebo[[2]],
                                       "946", "Oakland", "Consumption")
if(Drop_Berkeley == 0){
  Berkeley_Placebo_Plot <- PlaceboPlotFun(Berkeley_Placebo[[1]], Berkeley_Placebo[[2]],
                                          "947", "Berkeley", "Consumption")
}
Average_Placebo_Plot <- PlaceboPlotFun(Average_Placebo[[1]], Average_Placebo[[2]],
                                       "Average", "Average", "Consumption")


#####################################################################
###############Summarizing SC Effects for Each City##################
#####################################################################

if(Population_Weighted == 1){
  #Taking individual city effects from unbalanced estimations
  ATT_AllTaxed_Unbalanced <- left_join(ATT_AllTaxed_Unbalanced, PopulationCensusData,
                                       by = c("Level" = "store_zip3"))
  totaloz_ate_summary <- filter(ATT_AllTaxed_Unbalanced, Time >= 0 & Level != "Average") %>% group_by(Level) %>%
    summarise(ATE = weighted.mean(Estimate, population, na.rm = TRUE))
  
  #Taking average effect from balanced estimation
  ATT_AllTaxed <- left_join(ATT_AllTaxed, PopulationCensusData,
                            by = c("Level" = "store_zip3"))
  Average_ATE <- filter(ATT_AllTaxed, is.na(Time) & Level != "Average") %>%
    summarise(ATE = weighted.mean(Estimate, population, na.rm = TRUE)) %>%
    mutate(Level = "Average")
  
  #rbinding them
  totaloz_ate_summary <- rbind(totaloz_ate_summary, Average_ATE)
  
  totaloz_ate_summary <- left_join(totaloz_ate_summary, PreTreatmentMeans_AllTaxed,
                                   by = c("Level" = "store_zip3"))
  totaloz_ate_summary <- left_join(totaloz_ate_summary, PopulationCensusData,
                                   by = c("Level" = "store_zip3"))
  
  #Calculating weighted average pre-treatment mean
  PreTreatmentMean_PopulationWeighted <- weighted.mean(totaloz_ate_summary$PreTreatmentMeans, totaloz_ate_summary$population, na.rm = TRUE)
  totaloz_ate_summary$PreTreatmentMeans[nrow(totaloz_ate_summary)] <- PreTreatmentMean_PopulationWeighted
  totaloz_ate_summary <- mutate(totaloz_ate_summary, ATE_normalized = ATE/PreTreatmentMeans)
  
  #bringing in p-values
  if(Drop_Berkeley == 0){
    pval <- c(Philadelphia_Placebo[[2]], Boulder_Placebo[[2]], SanFrancisco_Placebo[[2]], Oakland_Placebo[[2]],
              Berkeley_Placebo[[2]], Seattle_Placebo[[2]], Average_Placebo[[2]])
  }
  if(Drop_Berkeley == 1){
    pval <- c(Philadelphia_Placebo[[2]], Boulder_Placebo[[2]], SanFrancisco_Placebo[[2]], 
              Oakland_Placebo[[2]], Seattle_Placebo[[2]], Average_Placebo[[2]])
  }
  totaloz_ate_summary <- cbind(totaloz_ate_summary, pval)
  
  fwrite(totaloz_ate_summary, "Figures/Results/Multisynth Results/Consumption/Urbanicity/multisynth_ATE_DF_pvals.csv")
  
}


if(Population_Weighted == 0){
  
  #Taking individual city effects from unbalanced estimations
  totaloz_ate_summary <- filter(ATT_AllTaxed_Unbalanced, Time >= 0 & Level != "Average") %>% group_by(Level) %>%
    summarise(ATE = mean(Estimate, na.rm = TRUE))
  
  #Taking average effect from balanced estimation
  Average_ATE <- filter(ATT_AllTaxed, Time >= 0 & Level == "Average") %>% group_by(Level) %>%
    summarise(ATE = mean(Estimate, na.rm = TRUE))
  
  #rbinding them
  totaloz_ate_summary <- rbind(totaloz_ate_summary, Average_ATE)
  
  totaloz_ate_summary <- left_join(totaloz_ate_summary, PreTreatmentMeans_AllTaxed,
                                   by = c("Level" = "store_zip3"))
  
  totaloz_ate_summary$PreTreatmentMeans[nrow(totaloz_ate_summary)] <- mean(totaloz_ate_summary$PreTreatmentMeans[1:nrow(totaloz_ate_summary)], na.rm = TRUE)
  totaloz_ate_summary <- mutate(totaloz_ate_summary, ATE_normalized = ATE/PreTreatmentMeans)
  
  #bringing in p-values
  if(Drop_Berkeley == 0){
    pval <- c(Philadelphia_Placebo[[2]], Boulder_Placebo[[2]], SanFrancisco_Placebo[[2]], Oakland_Placebo[[2]],
              Berkeley_Placebo[[2]], Seattle_Placebo[[2]], Average_Placebo[[2]])
  }
  if(Drop_Berkeley == 1){
    pval <- c(Philadelphia_Placebo[[2]], Boulder_Placebo[[2]], SanFrancisco_Placebo[[2]], 
              Oakland_Placebo[[2]], Seattle_Placebo[[2]], Average_Placebo[[2]])
  }
  
  totaloz_ate_summary <- cbind(totaloz_ate_summary, pval)
  
  fwrite(totaloz_ate_summary, "Figures/Results/Multisynth Results/Consumption/Urbanicity/multisynth_ATE_DF_pvals.csv")
  
}

#Test to ensure composite estimate is an unweighted average of the change in ounces sold in each treated locality
test <- group_by(ATT_AllTaxed, Time) %>% summarise(mean_est = mean(Estimate))

######################################################
###############PASS-THROUGH ANALYSES##################
######################################################

#########################
#####Reading in Data#####
#########################

#All Taxed Data
PreTreatmentMeans_AllTaxed_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/PreTreatmentMeans_Taxed_prices.csv",
                                           colClasses = c("character", "numeric"))
ATT_AllTaxed_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed_prices.csv")
ATT_AllTaxed_Unbalanced_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_AllTaxed_unbalanced_prices.csv")
LongDF_AllTaxed_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed_prices.csv")

#SF Placebos
PreTreatmentMeans_SF_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_SF_prices.csv",
                                     colClasses = c("numeric", "numeric"))
ATT_SF_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_SF_prices.csv")
LongDF_SF_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_SF_prices.csv")

#Seattle Placebos
PreTreatmentMeans_Seattle_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Seattle_prices.csv",
                                          colClasses = c("numeric", "numeric"))
ATT_Seattle_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Seattle_prices.csv")
LongDF_Seattle_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Seattle_prices.csv")

#Boulder Placebos
PreTreatmentMeans_Boulder_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Boulder_prices.csv",
                                          colClasses = c("numeric", "numeric"))
ATT_Boulder_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Boulder_prices.csv")
LongDF_Boulder_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Boulder_prices.csv")

#Philadelphia Placebos
PreTreatmentMeans_Philadelphia_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Philadelphia_prices.csv",
                                               colClasses = c("numeric", "numeric"))
ATT_Philadelphia_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Philadelphia_prices.csv")
LongDF_Philadelphia_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Philadelphia_prices.csv")

#Oakland Placebos
PreTreatmentMeans_Oakland_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Oakland_prices.csv",
                                          colClasses = c("numeric", "numeric"))
ATT_Oakland_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Oakland_prices.csv")
LongDF_Oakland_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Oakland_prices.csv")

#Berkeley Placebos
PreTreatmentMeans_Berkeley_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Berkeley_prices.csv",
                                           colClasses = c("numeric", "numeric"))
ATT_Berkeley_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Berkeley_prices.csv")
LongDF_Berkeley_Prices <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Berkeley_prices.csv")


###########################################################################
###############Generating Placebo Dataframe by Taxed City##################
###########################################################################

##Outputting placebo dataframes
SanFrancisco_Placebo_Prices <- PlaceboDataGenFun(ATT_SF_Prices, "941", PreTreatmentMeans_SF_Prices, LongDF_SF_Prices, 72, ATT_AllTaxed_Unbalanced_Prices)
Seattle_Placebo_Prices <- PlaceboDataGenFun(ATT_Seattle_Prices, "981", PreTreatmentMeans_Seattle_Prices, LongDF_Seattle_Prices, 72, ATT_AllTaxed_Unbalanced_Prices)
Boulder_Placebo_Prices <- PlaceboDataGenFun(ATT_Boulder_Prices, "803", PreTreatmentMeans_Boulder_Prices, LongDF_Boulder_Prices, 66, ATT_AllTaxed_Unbalanced_Prices)
Philadelphia_Placebo_Prices <- PlaceboDataGenFun(ATT_Philadelphia_Prices, "191", PreTreatmentMeans_Philadelphia_Prices, LongDF_Philadelphia_Prices, 60, ATT_AllTaxed_Unbalanced_Prices)
Oakland_Placebo_Prices <- PlaceboDataGenFun(ATT_Oakland_Prices, "946", PreTreatmentMeans_Oakland_Prices, LongDF_Oakland_Prices, 66, ATT_AllTaxed_Unbalanced_Prices)
if(Drop_Berkeley == 0){
  Berkeley_Placebo_Prices <- PlaceboDataGenFun(ATT_Berkeley_Prices, "947", PreTreatmentMeans_Berkeley_Prices, LongDF_Berkeley_Prices, 38, ATT_AllTaxed_Unbalanced_Prices)
}

##Outputting Average placebo dataframe
Average_Placebo_Prices <- PlaceboDataGenFun_Average_Prices(ATT_AllTaxed_Prices, PreTreatmentMeans_AllTaxed_Prices, LongDF_AllTaxed_Prices)

##Plots for each city
SanFrancisco_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(SanFrancisco_Placebo_Prices[[1]], SanFrancisco_Placebo_Prices[[2]],
                                                   "941", "San Francisco", "Prices")
Seattle_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(Seattle_Placebo_Prices[[1]], Seattle_Placebo_Prices[[2]],
                                              "981", "Seattle", "Prices")
Boulder_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(Boulder_Placebo_Prices[[1]], Boulder_Placebo_Prices[[2]],
                                              "803", "Boulder", "Prices")
Philadelphia_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(Philadelphia_Placebo_Prices[[1]], Philadelphia_Placebo_Prices[[2]],
                                                   "191", "Philadelphia", "Prices")
Oakland_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(Oakland_Placebo_Prices[[1]], Oakland_Placebo_Prices[[2]],
                                              "946", "Oakland", "Prices")
if(Drop_Berkeley == 0){
  Berkeley_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(Berkeley_Placebo_Prices[[1]], Berkeley_Placebo_Prices[[2]],
                                                 "947", "Berkeley", "Prices")
}
Average_Placebo_Plot_Prices <- PlaceboPlotFun_Prices(Average_Placebo_Prices[[1]], Average_Placebo_Prices[[2]],
                                              "Average", "Average", "Prices")


#####################################################################
###############Summarizing SC Effects for Each City##################
#####################################################################

if(Population_Weighted == 1){
  
  #Taking individual city effects from unbalanced estimations
  ATT_AllTaxed_Unbalanced_Prices <- left_join(ATT_AllTaxed_Unbalanced_Prices, PopulationCensusData,
                                              by = c("Level" = "store_zip3"))
  totaloz_ate_summary_Prices <- filter(ATT_AllTaxed_Unbalanced_Prices, Time >= 0 & Level != "Average") %>% group_by(Level) %>%
    summarise(ATE = weighted.mean(Estimate, population, na.rm = TRUE))
  
  #Taking average effect from balanced estimation
  ATT_AllTaxed_Prices <- left_join(ATT_AllTaxed_Prices, PopulationCensusData,
                                   by = c("Level" = "store_zip3"))
  Average_ATE_Prices <- filter(ATT_AllTaxed_Prices, is.na(Time) & Level != "Average") %>%
    summarise(ATE = weighted.mean(Estimate, population, na.rm = TRUE)) %>%
    mutate(Level = "Average")
  
  #rbinding them
  totaloz_ate_summary_Prices <- rbind(totaloz_ate_summary_Prices, Average_ATE_Prices)
  
  totaloz_ate_summary_Prices <- left_join(totaloz_ate_summary_Prices, PreTreatmentMeans_AllTaxed_Prices,
                                          by = c("Level" = "store_zip3"))
  totaloz_ate_summary_Prices <- left_join(totaloz_ate_summary_Prices, PopulationCensusData,
                                          by = c("Level" = "store_zip3"))
  
  #Calculating weighted average pre-treatment mean
  PreTreatmentMean_PopulationWeighted_Prices <- weighted.mean(totaloz_ate_summary_Prices$PreTreatmentMeans, totaloz_ate_summary_Prices$population, na.rm = TRUE)
  totaloz_ate_summary_Prices$PreTreatmentMeans[nrow(totaloz_ate_summary_Prices)] <- PreTreatmentMean_PopulationWeighted_Prices
  totaloz_ate_summary_Prices <- mutate(totaloz_ate_summary_Prices, ATE_normalized = ATE/PreTreatmentMeans)
  
  #bringing in p-values
  if(Drop_Berkeley == 0){
    pval_prices <- c(Philadelphia_Placebo_Prices[[2]], Boulder_Placebo_Prices[[2]], SanFrancisco_Placebo_Prices[[2]], 
                     Oakland_Placebo_Prices[[2]], Berkeley_Placebo_Prices[[2]], Seattle_Placebo_Prices[[2]], Average_Placebo_Prices[[2]])
  }
  if(Drop_Berkeley == 1){
    pval_prices <- c(Philadelphia_Placebo_Prices[[2]], Boulder_Placebo_Prices[[2]], SanFrancisco_Placebo_Prices[[2]], 
                     Oakland_Placebo_Prices[[2]], Seattle_Placebo_Prices[[2]], Average_Placebo_Prices[[2]])
  }
  totaloz_ate_summary_Prices <- cbind(totaloz_ate_summary_Prices, pval_prices)
  
  fwrite(totaloz_ate_summary_Prices, "Figures/Results/Multisynth Results/Prices/Urbanicity/multisynth_ATE_DF_pvals_Prices.csv")
  
}


if(Population_Weighted == 0){
  
  #Taking individual city effects from unbalanced estimations
  totaloz_ate_summary_Prices <- filter(ATT_AllTaxed_Unbalanced_Prices, Time >= 0 & Level != "Average") %>% group_by(Level) %>%
    summarise(ATE = mean(Estimate, na.rm = TRUE))
  
  #Taking average effect from balanced estimation
  Average_ATE_Prices <- filter(ATT_AllTaxed_Prices, Time >= 0 & Level == "Average") %>% group_by(Level) %>%
    summarise(ATE = mean(Estimate, na.rm = TRUE))
  
  #rbinding them
  totaloz_ate_summary_Prices <- rbind(totaloz_ate_summary_Prices, Average_ATE_Prices)
  
  totaloz_ate_summary_Prices <- left_join(totaloz_ate_summary_Prices, PreTreatmentMeans_AllTaxed_Prices,
                                          by = c("Level" = "store_zip3"))
  
  totaloz_ate_summary_Prices$PreTreatmentMeans[nrow(totaloz_ate_summary_Prices)] <- mean(totaloz_ate_summary_Prices$PreTreatmentMeans[1:nrow(totaloz_ate_summary_Prices)], na.rm = TRUE)
  totaloz_ate_summary_Prices <- mutate(totaloz_ate_summary_Prices, ATE_normalized = ATE/PreTreatmentMeans)
  
  #bringing in p-values
  if(Drop_Berkeley == 0){
    pval_prices <- c(Philadelphia_Placebo_Prices[[2]], Boulder_Placebo_Prices[[2]], SanFrancisco_Placebo_Prices[[2]], 
                     Oakland_Placebo_Prices[[2]], Berkeley_Placebo_Prices[[2]], Seattle_Placebo_Prices[[2]], Average_Placebo_Prices[[2]])
  }
  if(Drop_Berkeley == 1){
    pval_prices <- c(Philadelphia_Placebo_Prices[[2]], Boulder_Placebo_Prices[[2]], SanFrancisco_Placebo_Prices[[2]], 
                     Oakland_Placebo_Prices[[2]], Seattle_Placebo_Prices[[2]], Average_Placebo_Prices[[2]])
  }
  
  totaloz_ate_summary_Prices <- cbind(totaloz_ate_summary_Prices, pval_prices)
  
  fwrite(totaloz_ate_summary_Prices, "Figures/Results/Multisynth Results/Prices/Urbanicity/multisynth_ATE_DF_pvals_Prices.csv")
  
}


################################################
###############BORDER ANALYSES##################
################################################

#########################
#####Reading in Data#####
#########################

#All Taxed Data
PreTreatmentMeans_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/PreTreatmentMeans_Borders.csv",
                                   colClasses = c("character", "numeric"))
ATT_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_Borders.csv")
ATT_Borders_Unbalanced <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_unbalanced_Borders.csv")
LongDF_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_Borders.csv")

#SF Placebos
PreTreatmentMeans_SF_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_SF_Borders.csv",
                                      colClasses = c("numeric", "numeric"))
ATT_SF_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_SF_Borders.csv")
LongDF_SF_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_SF_Borders.csv")

#Seattle Placebos
PreTreatmentMeans_Seattle_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Seattle_Borders.csv",
                                           colClasses = c("numeric", "numeric"))
ATT_Seattle_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Seattle_Borders.csv")
LongDF_Seattle_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Seattle_Borders.csv")

#Boulder Placebos
PreTreatmentMeans_Boulder_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Boulder_Borders.csv",
                                           colClasses = c("numeric", "numeric"))
ATT_Boulder_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Boulder_Borders.csv")
LongDF_Boulder_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Boulder_Borders.csv")

#Philadelphia Placebos
PreTreatmentMeans_Philadelphia_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Philadelphia_Borders.csv",
                                                colClasses = c("numeric", "numeric"))
ATT_Philadelphia_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Philadelphia_Borders.csv")
LongDF_Philadelphia_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Philadelphia_Borders.csv")

#Oakland Placebos
PreTreatmentMeans_Oakland_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Oakland_Borders.csv",
                                           colClasses = c("numeric", "numeric"))
ATT_Oakland_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Oakland_Borders.csv")
LongDF_Oakland_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Oakland_Borders.csv")

#Berkeley Placebos
PreTreatmentMeans_Berkeley_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_pretreatmentmean_all_Berkeley_Borders.csv",
                                            colClasses = c("numeric", "numeric"))
ATT_Berkeley_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_att_all_Berkeley_Borders.csv")
LongDF_Berkeley_Borders <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_all_Berkeley_Borders.csv")


###########################################################################
###############Generating Placebo Dataframe by Taxed City##################
###########################################################################

##Outputting placebo dataframes
SanFrancisco_Placebo_Borders <- PlaceboDataGenFun_Borders(ATT_SF_Borders, c("940", "949"), PreTreatmentMeans_SF_Borders, LongDF_SF_Borders, 72, ATT_Borders_Unbalanced)
Seattle_Placebo_Borders <- PlaceboDataGenFun_Borders(ATT_Seattle_Borders, c("980", "982", "983", "984"), PreTreatmentMeans_Seattle_Borders, LongDF_Seattle_Borders, 72, ATT_Borders_Unbalanced)
Boulder_Placebo_Borders <- PlaceboDataGenFun_Borders(ATT_Boulder_Borders, c("800", "804", "805"), PreTreatmentMeans_Boulder_Borders, LongDF_Boulder_Borders, 66, ATT_Borders_Unbalanced)
Philadelphia_Placebo_Borders <- PlaceboDataGenFun_Borders(ATT_Philadelphia_Borders, c("80", "81"), PreTreatmentMeans_Philadelphia_Borders, LongDF_Philadelphia_Borders, 60, ATT_Borders_Unbalanced)
Oakland_Placebo_Borders <- PlaceboDataGenFun_Borders(ATT_Oakland_Borders, c("945", "948"), PreTreatmentMeans_Oakland_Borders, LongDF_Oakland_Borders, 66, ATT_Borders_Unbalanced)
if(Drop_Berkeley == 0){
  Berkeley_Placebo_Borders <- PlaceboDataGenFun_Borders(ATT_Berkeley_Borders, c("945", "948"), PreTreatmentMeans_Berkeley_Borders, LongDF_Berkeley_Borders, 38, ATT_Borders_Unbalanced)
}

##Outputting Average placebo dataframe
Average_Placebo_Borders <- PlaceboDataGenFun_Average_Borders(ATT_Borders, PreTreatmentMeans_Borders, LongDF_Borders)

##Plots for each city
SanFrancisco_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(SanFrancisco_Placebo_Borders[[1]], SanFrancisco_Placebo_Borders[[2]],
                                                            c("940", "949"), "San Francisco", "Borders")
Seattle_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(Seattle_Placebo_Borders[[1]], Seattle_Placebo_Borders[[2]],
                                                       c("980", "982", "983", "984"), "Seattle", "Borders")
Boulder_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(Boulder_Placebo_Borders[[1]], Boulder_Placebo_Borders[[2]],
                                                       c("800", "804", "805"), "Boulder", "Borders")
Philadelphia_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(Philadelphia_Placebo_Borders[[1]], Philadelphia_Placebo_Borders[[2]],
                                                            c("80", "81"), "Philadelphia", "Borders")
Oakland_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(Oakland_Placebo_Borders[[1]], Oakland_Placebo_Borders[[2]],
                                                       c("945", "948"), "Oakland", "Borders")
if(Drop_Berkeley == 0){
  Berkeley_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(Berkeley_Placebo_Borders[[1]], Berkeley_Placebo_Borders[[2]],
                                                          c("945", "948"), "Berkeley", "Borders")
}
Average_Placebo_Plot_Borders <- PlaceboPlotFun_Borders(Average_Placebo_Borders[[1]], Average_Placebo_Borders[[2]],
                                                       "Average", "Average", "Borders")


#####################################################################
###############Summarizing SC Effects for Each City##################
#####################################################################

if(Population_Weighted == 1){
  
  #Taking individual city effects from unbalanced estimations
  ATT_Borders_Unbalanced <- left_join(ATT_Borders_Unbalanced, PopulationCensusData,
                                      by = c("Level" = "store_zip3"))
  totaloz_ate_summary_Borders <- filter(ATT_Borders_Unbalanced, Time >= 0 & Level != "Average") %>% group_by(Level) %>%
    summarise(ATE = weighted.mean(Estimate, population, na.rm = TRUE))
  
  totaloz_ate_summary_Borders <- left_join(totaloz_ate_summary_Borders, PreTreatmentMeans_Borders,
                                           by = c("Level" = "store_zip3"))
  totaloz_ate_summary_Borders <- left_join(totaloz_ate_summary_Borders, PopulationCensusData,
                                           by = c("Level" = "store_zip3"))
  
  #grouping by taxed zip code
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "080"] <- "Philadelphia"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "081"] <- "Philadelphia"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "800"] <- "Boulder"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "804"] <- "Boulder"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "805"] <- "Boulder"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "940"] <- "San Francisco"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "949"] <- "San Francisco"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "948"] <- "Oakland"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "945"] <- "Oakland"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "980"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "982"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "983"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "984"] <- "Seattle"
  #totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "Average"] <- "Average"
  
  #grouping by taxed zip code
  totaloz_ate_summary_Borders <- group_by(totaloz_ate_summary_Borders, TaxedZip) %>%
    summarise(ATE = weighted.mean(ATE, population), 
              PreTreatmentMeans = weighted.mean(PreTreatmentMeans, population), 
              ATE_normalized = ATE/PreTreatmentMeans,
              population = sum(population))
  
  #Taking average effect from balanced estimation
  ATT_Borders <- left_join(ATT_Borders, PopulationCensusData,
                           by = c("Level" = "store_zip3"))
  Average_ATE_Borders <- filter(ATT_Borders, is.na(Time) & Level != "Average") %>%
    summarise(ATE = weighted.mean(Estimate, population, na.rm = TRUE)) %>%
    mutate(Level = "Average")
  
  #Calculating weighted average pre-treatment mean
  PreTreatmentMean_PopulationWeighted_Borders <- weighted.mean(totaloz_ate_summary_Borders$PreTreatmentMeans, totaloz_ate_summary_Borders$population, na.rm = TRUE)
  
  #Creating "Average" row
  Average_ATE_Borders <- t(c("Average", Average_ATE_Borders$ATE, PreTreatmentMean_PopulationWeighted_Borders,
                             Average_ATE_Borders$ATE/PreTreatmentMean_PopulationWeighted_Borders, NA))
  colnames(Average_ATE_Borders) <- names(totaloz_ate_summary_Borders)
  
  #rbinding them
  totaloz_ate_summary_Borders <- rbind(totaloz_ate_summary_Borders, Average_ATE_Borders)
  
  #bringing in p-values
  if(Drop_Berkeley == 0){
    pval_Borders <- c(Berkeley_Placebo_Borders[[2]], Boulder_Placebo_Borders[[2]],
                      Oakland_Placebo_Borders[[2]], Philadelphia_Placebo_Borders[[2]], SanFrancisco_Placebo_Borders[[2]],
                      Seattle_Placebo_Borders[[2]],
                      Average_Placebo_Borders[[2]])
  }
  if(Drop_Berkeley == 1){
    pval_Borders <- c(Boulder_Placebo_Borders[[2]],
                      Oakland_Placebo_Borders[[2]], Philadelphia_Placebo_Borders[[2]], 
                      SanFrancisco_Placebo_Borders[[2]], Seattle_Placebo_Borders[[2]],
                      Average_Placebo_Borders[[2]])
  }
  totaloz_ate_summary_Borders <- cbind(totaloz_ate_summary_Borders, pval_Borders)
  
  fwrite(totaloz_ate_summary_Borders, "Figures/Results/Multisynth Results/Borders/Urbanicity/multisynth_ATE_DF_pvals_Borders.csv")
  
}


if(Population_Weighted == 0){
  
  #Taking individual city effects from unbalanced estimations
  totaloz_ate_summary_Borders <- filter(ATT_Borders_Unbalanced, Time >= 0 & Level != "Average") %>% group_by(Level) %>%
    summarise(ATE = mean(Estimate, na.rm = TRUE))
  
  #Taking average effect from balanced estimation
  Average_ATE_Borders <- filter(ATT_Borders, Time >= 0 & Level == "Average") %>% group_by(Level) %>%
    summarise(ATE = mean(Estimate, na.rm = TRUE))
  
  #rbinding them
  totaloz_ate_summary_Borders <- rbind(totaloz_ate_summary_Borders, Average_ATE_Borders)
  
  totaloz_ate_summary_Borders <- left_join(totaloz_ate_summary_Borders, PreTreatmentMeans_Borders,
                                           by = c("Level" = "store_zip3"))
  
  totaloz_ate_summary_Borders$PreTreatmentMeans[nrow(totaloz_ate_summary_Borders)] <- mean(totaloz_ate_summary_Borders$PreTreatmentMeans[1:nrow(totaloz_ate_summary_Borders)], na.rm = TRUE)
  totaloz_ate_summary_Borders <- mutate(totaloz_ate_summary_Borders, ATE_normalized = ATE/PreTreatmentMeans)
  
  #grouping by taxed zip code
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "080"] <- "Philadelphia"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "081"] <- "Philadelphia"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "800"] <- "Boulder"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "804"] <- "Boulder"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "805"] <- "Boulder"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "940"] <- "San Francisco"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "949"] <- "San Francisco"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "948"] <- "Oakland"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "945"] <- "Oakland"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "980"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "982"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "983"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "984"] <- "Seattle"
  totaloz_ate_summary_Borders$TaxedZip[totaloz_ate_summary_Borders$Level == "Average"] <- "Average"
  
  if(Drop_Berkeley == 0){
    #bringing in 948 and 945 for Berkeley
    totaloz_ate_Berkeley_Borders <- filter(Berkeley_Placebo_Borders[[1]], (Level == "948" & Time >= 0) | (Level == "945" & Time >= 0)) %>% 
      group_by(Level) %>%
      summarise(ATE = mean(Estimate, na.rm = TRUE))
    totaloz_ate_Berkeley_Borders <- left_join(totaloz_ate_Berkeley_Borders, filter(PreTreatmentMeans_Berkeley_Borders, 
                                                                                   store_zip3 == "948" | store_zip3 == "945"),
                                              by = c("Level" = "store_zip3"))
    totaloz_ate_Berkeley_Borders <- mutate(totaloz_ate_Berkeley_Borders, ATE_normalized = ATE/PretreatmentMean)
    totaloz_ate_Berkeley_Borders <- mutate(totaloz_ate_Berkeley_Borders, TaxedZip = "Berkeley")
    names(totaloz_ate_Berkeley_Borders)[names(totaloz_ate_Berkeley_Borders) == "PretreatmentMean"] <- "PreTreatmentMeans"
    totaloz_ate_summary_Borders <- rbind(totaloz_ate_summary_Borders, totaloz_ate_Berkeley_Borders)
  }
  
  #grouping by taxed zip code
  totaloz_ate_summary_Borders <- group_by(totaloz_ate_summary_Borders, TaxedZip) %>%
    summarise(ATE = mean(ATE), 
              PreTreatmentMeans = mean(PreTreatmentMeans), 
              ATE_normalized = mean(ATE)/mean(PreTreatmentMeans))
  
  #bringing in p-values
  if(Drop_Berkeley == 0){
    pval_Borders <- c(Average_Placebo_Borders[[2]], Berkeley_Placebo_Borders[[2]], Boulder_Placebo_Borders[[2]],
                      Oakland_Placebo_Borders[[2]], Philadelphia_Placebo_Borders[[2]], SanFrancisco_Placebo_Borders[[2]],
                      Seattle_Placebo_Borders[[2]])
  }
  if(Drop_Berkeley == 1){
    pval_Borders <- c(Average_Placebo_Borders[[2]], Boulder_Placebo_Borders[[2]],
                      Oakland_Placebo_Borders[[2]], Philadelphia_Placebo_Borders[[2]], 
                      SanFrancisco_Placebo_Borders[[2]], Seattle_Placebo_Borders[[2]])
  }
  totaloz_ate_summary_Borders <- cbind(totaloz_ate_summary_Borders, pval_Borders)
  
  fwrite(totaloz_ate_summary_Borders, "Figures/Results/Multisynth Results/Borders/Urbanicity/multisynth_ATE_DF_pvals_Borders.csv")
  
}
