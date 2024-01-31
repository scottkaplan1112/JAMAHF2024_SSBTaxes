options(digits = 3)

##Placebo Data Generation Function
PlaceboDataGenFun <- function(ATTData, zip3, PreTreatmentMeans_Data, LongDF, treatment_time, ATTAllTaxed_DF){
  
  # ATTData <- ATT_SF
  # zip3 <- "941"
  # PreTreatmentMeans_Data <- PreTreatmentMeans_SF
  # LongDF <- LongDF_SF
  # treatment_time <- 72
  # ATTAllTaxed_DF <- ATT_AllTaxed_Unbalanced
  
  ##Filter out treated city zip code
  PlaceboData <- filter(ATTData, Level != zip3)
  
  ##Bring in treated city zip code info from Taxed ATT (Unbalanced)
  ATT_Taxed <- filter(ATTAllTaxed_DF, Level == zip3 & !(is.na(Estimate)))
  PlaceboData <- rbind(PlaceboData, ATT_Taxed)
  
  ##Normalize Data
  PlaceboData$Level <- as.numeric(PlaceboData$Level)
  PlaceboData <- left_join(PlaceboData, PreTreatmentMeans_Data,
                           by = c("Level" = "store_zip3"))
  PlaceboData <- mutate(PlaceboData, Estimate_Normalized = Estimate/PretreatmentMean)
  
  
  ##Calculate MSPE
  PlaceboData <- mutate(PlaceboData, PrePeriod = ifelse(Time < 0, 1, 0))
  MSPE <- group_by(PlaceboData, PrePeriod, Level) %>%
    summarise(MSPE = sqrt(mean((Estimate)^2, na.rm = TRUE)),
              MSPE_Normalized = sqrt(mean((Estimate)^2, na.rm = TRUE))/PretreatmentMean) %>%
    distinct() %>%
    filter(!(is.na(PrePeriod)))
  
  ##Calculating the RMSPE
  MSPE <- group_by(MSPE, Level) %>% mutate(RMSPE = MSPE/dplyr::lead(MSPE,1))
  
  PlaceboData <- left_join(PlaceboData, MSPE,
                           by = c("Level" = "Level",
                                  "PrePeriod" = "PrePeriod")) 
  
  ##Calculating the ranking
  MSPE_Ranking <- filter(PlaceboData, PrePeriod == 0) %>%
    distinct(Level, RMSPE) %>%
    mutate(RMSPE_rank = rank(desc(RMSPE)))
  
  ##Calculating p-value
  pval <- filter(MSPE_Ranking, Level == zip3) %>%
    distinct(RMSPE_rank/length(unique(PlaceboData$Level))) %>% as.numeric
  
  ##Pruning placebos that have a relatively low pre-period MSPE
  
  ##Filtering out pre-period normalized MSPEs 5x or more larger than Average pre-period normalized MSPE
  PrePeriod_Normalized_MSPE <- filter(MSPE, PrePeriod == 1 & Level == zip3) %>% distinct()
  PrePeriod_Normalized_MSPE <- PrePeriod_Normalized_MSPE$MSPE_Normalized
  MSPE_Pruned <- filter(MSPE, PrePeriod == 1 & MSPE_Normalized < 5*PrePeriod_Normalized_MSPE)
  
  ##Only keeping valid placebos
  PlaceboData_Pruned <- filter(PlaceboData, Level %in% MSPE_Pruned$Level)
  
  ##Taking a sample of 100 valid placebos
  Placebo_Zips <- unique(PlaceboData_Pruned$Level)
  Sample_Placebos <- c(sample(Placebo_Zips, 100), zip3)
  PlaceboData_Pruned_Sample <- filter(PlaceboData_Pruned, Level %in% Sample_Placebos) %>% ungroup()
  
  List_Out <- list(PlaceboData_Pruned_Sample, pval)
  return(List_Out)
  
  
}


##Placebo Data Generation Function - BORDERS
PlaceboDataGenFun_Borders <- function(ATTData, zip3, PreTreatmentMeans_Data, LongDF, treatment_time, ATTAllTaxed_DF){
  
  # ATTData <- ATT_Philadelphia_Borders
  # zip3 <- c("80", "81")
  # PreTreatmentMeans_Data <- PreTreatmentMeans_Philadelphia_Borders
  # LongDF <- LongDF_Philadelphia_Borders
  # treatment_time <- 60
  # ATTAllTaxed_DF <- ATT_Borders_Unbalanced
  
  #Changing levels for Philly to 80 and 81
  ATTAllTaxed_DF$Level[ATTAllTaxed_DF$Level == "080"] <- "80"
  ATTAllTaxed_DF$Level[ATTAllTaxed_DF$Level == "081"] <- "81"
  
  ##Filter out border city zip codes
  PlaceboData <- filter(ATTData, !(Level %in% zip3))
  
  ##Bring in treated city zip code info from Taxed ATT (Unbalanced)
  ATT_Taxed <- filter(ATTAllTaxed_DF, Level %in% zip3 & !(is.na(Estimate)))
  if(treatment_time == 38){
    ATT_Taxed$Time <- ATT_Taxed$Time+28
  }
  
  PlaceboData <- rbind(PlaceboData, ATT_Taxed)
  
  ##Normalize Data
  PlaceboData$Level <- as.numeric(PlaceboData$Level)
  PlaceboData <- left_join(PlaceboData, PreTreatmentMeans_Data,
                           by = c("Level" = "store_zip3"))
  PlaceboData <- mutate(PlaceboData, Estimate_Normalized = Estimate/PretreatmentMean)
  
  
  ##Calculate MSPE
  PlaceboData <- mutate(PlaceboData, PrePeriod = ifelse(Time < 0, 1, 0))
  MSPE <- group_by(PlaceboData, PrePeriod, Level) %>%
    summarise(MSPE = sqrt(mean((Estimate)^2, na.rm = TRUE)),
              MSPE_Normalized = sqrt(mean((Estimate)^2, na.rm = TRUE))/PretreatmentMean) %>%
    distinct() %>%
    filter(!(is.na(PrePeriod)))
  
  ##Calculating the RMSPE
  MSPE <- group_by(MSPE, Level) %>% mutate(RMSPE = MSPE/dplyr::lead(MSPE,1))
  
  PlaceboData <- left_join(PlaceboData, MSPE,
                           by = c("Level" = "Level",
                                  "PrePeriod" = "PrePeriod")) 
  
  ##Calculating the ranking
  MSPE_Ranking <- filter(PlaceboData, PrePeriod == 0) %>%
    distinct(Level, RMSPE) %>%
    mutate(RMSPE_rank = rank(desc(RMSPE)))
  
  ##Calculating p-value
  pval <- filter(MSPE_Ranking, Level %in% zip3) %>%
    distinct(pval = RMSPE_rank/length(unique(PlaceboData$Level))) %>%
    summarise(pval = mean(pval)) %>% as.numeric()
  
  ##Pruning placebos that have a relatively low pre-period MSPE
  
  ##Filtering out pre-period normalized MSPEs 5x or more larger than Average pre-period normalized MSPE
  PrePeriod_Normalized_MSPE <- filter(MSPE, PrePeriod == 1 & Level %in% zip3) %>% distinct()
  PrePeriod_Normalized_MSPE <- PrePeriod_Normalized_MSPE$MSPE_Normalized
  MSPE_Pruned <- filter(MSPE, PrePeriod == 1 & MSPE_Normalized < 5*max(PrePeriod_Normalized_MSPE))
  
  ##Only keeping valid placebos
  PlaceboData_Pruned <- filter(PlaceboData, Level %in% MSPE_Pruned$Level)
  
  ##Taking a sample of 100 valid placebos
  Placebo_Zips <- unique(PlaceboData_Pruned$Level)
  Sample_Placebos <- c(sample(Placebo_Zips, 100), zip3)
  PlaceboData_Pruned_Sample <- filter(PlaceboData_Pruned, Level %in% Sample_Placebos) %>% ungroup()
  
  List_Out <- list(PlaceboData_Pruned_Sample, pval)
  return(List_Out)
  
  
}


##Placebo Data Generation Function - CONSUMPTION - AVERAGE ESTIMATES
PlaceboDataGenFun_Average <- function(ATTData, PreTreatmentMeans_Data, LongDF){
  
  # ATTData <- ATT_AllTaxed
  # PreTreatmentMeans_Data <- PreTreatmentMeans_AllTaxed
  # LongDF <- LongDF_AllTaxed
  
  if(Population_Weighted == 1){
    ##Calculate WEIGHTED average effect based on population
    ATTData <- left_join(ATTData, PopulationCensusData,
                         by = c("Level" = "store_zip3"))
    AverageData <- filter(ATTData, !(Level == "Average")) %>%
      group_by(Time) %>%
      summarise(Estimate = weighted.mean(Estimate, population)) %>%
      mutate(Level = "Average")
  }
  
  if(Population_Weighted == 0){
    ##Filter to just average effects
    AverageData <- filter(ATTData, Level == "Average")
  }
  
  if(Population_Weighted == 1){
    ##Calculate WEIGHTED average pretreatment mean of taxed zip codes
    PreTreatmentMeans_Data <- left_join(PreTreatmentMeans_Data, PopulationCensusData,
                                        by = c("store_zip3" = "store_zip3"))
    PretreatmentMean <- summarise(PreTreatmentMeans_Data, Pretreatmentmean = weighted.mean(PreTreatmentMeans, population)) %>% as.numeric
    AverageData <- cbind(AverageData, PretreatmentMean)
    AverageData <- mutate(AverageData, Estimate_Normalized = Estimate/PretreatmentMean)
  }
  
  if(Population_Weighted == 0){
    ##Calculate average pretreatment mean of taxed zip codes
    PretreatmentMean <- summarise(PreTreatmentMeans_Data, Pretreatmentmean = mean(PreTreatmentMeans)) %>% as.numeric
    AverageData <- cbind(AverageData, PretreatmentMean)
    AverageData <- mutate(AverageData, Estimate_Normalized = Estimate/PretreatmentMean)
  }
  
  ##Mutate "treated city" column to distinguish between treated cities
  AverageData <- mutate(AverageData, Treated_City = "Average")
  
  ##Bring in and Normalize Placebo Data for each of the 6 treated cities
  
  #SF
  Placebo_SF <- ATT_SF 
  Placebo_SF$Level <- as.numeric(Placebo_SF$Level)
  Placebo_SF <- left_join(Placebo_SF, PreTreatmentMeans_SF,
                          by = c("Level" = "store_zip3"))
  Placebo_SF <- mutate(Placebo_SF, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_SF <- mutate(Placebo_SF, Treated_City = "San Francisco")
  
  #Seattle
  Placebo_Seattle <- ATT_Seattle 
  Placebo_Seattle$Level <- as.numeric(Placebo_Seattle$Level)
  Placebo_Seattle <- left_join(Placebo_Seattle, PreTreatmentMeans_Seattle,
                               by = c("Level" = "store_zip3"))
  Placebo_Seattle <- mutate(Placebo_Seattle, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Seattle <- mutate(Placebo_Seattle, Treated_City = "Seattle")
  
  #Boulder
  Placebo_Boulder <- ATT_Boulder 
  Placebo_Boulder$Level <- as.numeric(Placebo_Boulder$Level)
  Placebo_Boulder <- left_join(Placebo_Boulder, PreTreatmentMeans_Boulder,
                               by = c("Level" = "store_zip3"))
  Placebo_Boulder <- mutate(Placebo_Boulder, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Boulder <- mutate(Placebo_Boulder, Treated_City = "Boulder")
  
  #Philadelphia
  Placebo_Philadelphia <- ATT_Philadelphia 
  Placebo_Philadelphia$Level <- as.numeric(Placebo_Philadelphia$Level)
  Placebo_Philadelphia <- left_join(Placebo_Philadelphia, PreTreatmentMeans_Philadelphia,
                                    by = c("Level" = "store_zip3"))
  Placebo_Philadelphia <- mutate(Placebo_Philadelphia, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Philadelphia <- mutate(Placebo_Philadelphia, Treated_City = "Philadelphia")
  
  #Oakland
  Placebo_Oakland <- ATT_Oakland 
  Placebo_Oakland$Level <- as.numeric(Placebo_Oakland$Level)
  Placebo_Oakland <- left_join(Placebo_Oakland, PreTreatmentMeans_Oakland,
                               by = c("Level" = "store_zip3"))
  Placebo_Oakland <- mutate(Placebo_Oakland, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Oakland <- mutate(Placebo_Oakland, Treated_City = "Oakland")
  
  if(Drop_Berkeley == 0){
    #Berkeley
    Placebo_Berkeley <- ATT_Berkeley 
    Placebo_Berkeley$Level <- as.numeric(Placebo_Berkeley$Level)
    Placebo_Berkeley <- left_join(Placebo_Berkeley, PreTreatmentMeans_Berkeley,
                                  by = c("Level" = "store_zip3"))
    Placebo_Berkeley <- mutate(Placebo_Berkeley, Estimate_Normalized = Estimate/PretreatmentMean)
    Placebo_Berkeley <- mutate(Placebo_Berkeley, Treated_City = "Berkeley")
  }
  
  ##Rbind all placebo data
  if(Drop_Berkeley == 0){
    PlaceboData <- rbind(Placebo_SF, Placebo_Seattle, Placebo_Boulder, Placebo_Philadelphia, Placebo_Oakland, Placebo_Berkeley)
  }
  if(Drop_Berkeley == 1){
    PlaceboData <- rbind(Placebo_SF, Placebo_Seattle, Placebo_Boulder, Placebo_Philadelphia, Placebo_Oakland)
  }
  
  if(Population_Weighted == 1){
    PlaceboData <- dplyr::select(PlaceboData, -c(Std.Error, lower_bound, upper_bound))
  }
  
  ##Rbind placebo data to average data
  Placebo_Average_Data <- rbind(AverageData, PlaceboData)
  
  ##Filter to just balanced time periods
  if(Drop_Berkeley == 0){
    Placebo_Average_Data_Balanced <- filter(Placebo_Average_Data, Time >= -38 & Time <= 25)
  }
  if(Drop_Berkeley == 1){
    Placebo_Average_Data_Balanced <- filter(Placebo_Average_Data, Time >= -60 & Time <= 25)
  }
  
  ##Creating unique ids for each Level by Treated City placebo combination
  Placebo_Average_Data_Balanced <- mutate(Placebo_Average_Data_Balanced, Placebo_ID = dplyr::group_indices(Placebo_Average_Data_Balanced, .dots=c("Level", "Treated_City")))
  Placebo_Average_Data_Balanced$Placebo_ID[Placebo_Average_Data_Balanced$Level == "Average"] <- "Average"
  
  
  ##Calculate MSPE
  Placebo_Average_Data_Balanced <- mutate(Placebo_Average_Data_Balanced, PrePeriod = ifelse(Time < 0, 1, 0))
  MSPE <- group_by(Placebo_Average_Data_Balanced, PrePeriod, Placebo_ID) %>%
    summarise(MSPE = sqrt(mean((Estimate)^2, na.rm = TRUE)),
              MSPE_Normalized = sqrt(mean((Estimate)^2, na.rm = TRUE))/PretreatmentMean) %>%
    distinct()
  
  ##Calculating the RMSPE
  MSPE <- group_by(MSPE, Placebo_ID) %>% mutate(RMSPE = MSPE/dplyr::lead(MSPE,1))
  
  Placebo_Average_Data_Balanced <- left_join(Placebo_Average_Data_Balanced, MSPE,
                                             by = c("Placebo_ID" = "Placebo_ID",
                                                    "PrePeriod" = "PrePeriod")) 
  
  ##Calculating the ranking
  MSPE_Ranking <- filter(Placebo_Average_Data_Balanced, PrePeriod == 0) %>%
    distinct(Placebo_ID, RMSPE) %>%
    mutate(RMSPE_rank = rank(desc(RMSPE)))
  
  ##Calculating p-value
  pval <- filter(MSPE_Ranking, Placebo_ID == "Average") %>%
    distinct(RMSPE_rank/length(unique(Placebo_Average_Data_Balanced$Placebo_ID))) %>% as.numeric
  
  ##Pruning placebos that have a relatively low pre-period MSPE
  
  ##Filtering out pre-period normalized MSPEs 5x or more larger than Average pre-period normalized MSPE
  PrePeriod_Normalized_MSPE <- filter(MSPE, PrePeriod == 1 & Placebo_ID == "Average") %>% distinct()
  PrePeriod_Normalized_MSPE <- PrePeriod_Normalized_MSPE$MSPE_Normalized
  MSPE_Pruned <- filter(MSPE, PrePeriod == 1 & MSPE_Normalized < 5*PrePeriod_Normalized_MSPE)
  
  ##Only keeping valid placebos
  PlaceboData_Pruned <- filter(Placebo_Average_Data_Balanced, Placebo_ID %in% MSPE_Pruned$Placebo_ID)
  
  ##Taking a sample of 100 valid placebos
  Placebo_Zips <- unique(PlaceboData_Pruned$Placebo_ID)
  Sample_Placebos <- c(sample(Placebo_Zips, 100), "Average")
  PlaceboData_Pruned_Sample <- filter(PlaceboData_Pruned, Placebo_ID %in% Sample_Placebos) %>% ungroup()
  
  
  List_Out <- list(PlaceboData_Pruned_Sample, pval)
  return(List_Out)
  
}


##Placebo Data Generation Function - PASSTHROUGH - AVERAGE ESTIMATES
PlaceboDataGenFun_Average_Prices <- function(ATTData, PreTreatmentMeans_Data, LongDF){
  
  # ATTData <- ATT_AllTaxed_Prices
  # PreTreatmentMeans_Data <- PreTreatmentMeans_AllTaxed_Prices
  # LongDF <- LongDF_AllTaxed_Prices
  
  if(Population_Weighted == 1){
    ##Calculate WEIGHTED average effect based on population
    ATTData <- left_join(ATTData, PopulationCensusData,
                         by = c("Level" = "store_zip3"))
    AverageData <- filter(ATTData, !(Level == "Average")) %>%
      group_by(Time) %>%
      summarise(Estimate = weighted.mean(Estimate, population)) %>%
      mutate(Level = "Average")
  }
  
  if(Population_Weighted == 0){
    ##Filter to just average effects
    AverageData <- filter(ATTData, Level == "Average")
  }
  
  if(Population_Weighted == 1){
    ##Calculate WEIGHTED average pretreatment mean of taxed zip codes
    PreTreatmentMeans_Data <- left_join(PreTreatmentMeans_Data, PopulationCensusData,
                                        by = c("store_zip3" = "store_zip3"))
    PretreatmentMean <- summarise(PreTreatmentMeans_Data, Pretreatmentmean = weighted.mean(PreTreatmentMeans, population)) %>% as.numeric
    AverageData <- cbind(AverageData, PretreatmentMean)
    AverageData <- mutate(AverageData, Estimate_Normalized = Estimate/PretreatmentMean)
  }
  
  if(Population_Weighted == 0){
    ##Calculate average pretreatment mean of taxed zip codes
    PretreatmentMean <- summarise(PreTreatmentMeans_Data, Pretreatmentmean = mean(PreTreatmentMeans)) %>% as.numeric
    AverageData <- cbind(AverageData, PretreatmentMean)
    AverageData <- mutate(AverageData, Estimate_Normalized = Estimate/PretreatmentMean)
  }
  
  ##Mutate "treated city" column to distinguish between treated cities
  AverageData <- mutate(AverageData, Treated_City = "Average")
  
  ##Bring in and Normalize Placebo Data for each of the 6 treated cities
  
  #SF
  Placebo_SF <- ATT_SF_Prices 
  Placebo_SF$Level <- as.numeric(Placebo_SF$Level)
  Placebo_SF <- left_join(Placebo_SF, PreTreatmentMeans_SF_Prices,
                          by = c("Level" = "store_zip3"))
  Placebo_SF <- mutate(Placebo_SF, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_SF <- mutate(Placebo_SF, Treated_City = "San Francisco")
  
  #Seattle
  Placebo_Seattle <- ATT_Seattle_Prices  
  Placebo_Seattle$Level <- as.numeric(Placebo_Seattle$Level)
  Placebo_Seattle <- left_join(Placebo_Seattle, PreTreatmentMeans_Seattle_Prices,
                               by = c("Level" = "store_zip3"))
  Placebo_Seattle <- mutate(Placebo_Seattle, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Seattle <- mutate(Placebo_Seattle, Treated_City = "Seattle")
  
  #Boulder
  Placebo_Boulder <- ATT_Boulder_Prices  
  Placebo_Boulder$Level <- as.numeric(Placebo_Boulder$Level)
  Placebo_Boulder <- left_join(Placebo_Boulder, PreTreatmentMeans_Boulder_Prices,
                               by = c("Level" = "store_zip3"))
  Placebo_Boulder <- mutate(Placebo_Boulder, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Boulder <- mutate(Placebo_Boulder, Treated_City = "Boulder")
  
  #Philadelphia
  Placebo_Philadelphia <- ATT_Philadelphia_Prices  
  Placebo_Philadelphia$Level <- as.numeric(Placebo_Philadelphia$Level)
  Placebo_Philadelphia <- left_join(Placebo_Philadelphia, PreTreatmentMeans_Philadelphia_Prices,
                                    by = c("Level" = "store_zip3"))
  Placebo_Philadelphia <- mutate(Placebo_Philadelphia, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Philadelphia <- mutate(Placebo_Philadelphia, Treated_City = "Philadelphia")
  
  #Oakland
  Placebo_Oakland <- ATT_Oakland_Prices  
  Placebo_Oakland$Level <- as.numeric(Placebo_Oakland$Level)
  Placebo_Oakland <- left_join(Placebo_Oakland, PreTreatmentMeans_Oakland_Prices,
                               by = c("Level" = "store_zip3"))
  Placebo_Oakland <- mutate(Placebo_Oakland, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Oakland <- mutate(Placebo_Oakland, Treated_City = "Oakland")
  
  if(Drop_Berkeley == 0){
    #Berkeley
    Placebo_Berkeley <- ATT_Berkeley_Prices  
    Placebo_Berkeley$Level <- as.numeric(Placebo_Berkeley$Level)
    Placebo_Berkeley <- left_join(Placebo_Berkeley, PreTreatmentMeans_Berkeley_Prices,
                                  by = c("Level" = "store_zip3"))
    Placebo_Berkeley <- mutate(Placebo_Berkeley, Estimate_Normalized = Estimate/PretreatmentMean)
    Placebo_Berkeley <- mutate(Placebo_Berkeley, Treated_City = "Berkeley")
  }
  
  ##Rbind all placebo data
  if(Drop_Berkeley == 0){
    PlaceboData <- rbind(Placebo_SF, Placebo_Seattle, Placebo_Boulder, Placebo_Philadelphia, Placebo_Oakland, Placebo_Berkeley)
  }
  if(Drop_Berkeley == 1){
    PlaceboData <- rbind(Placebo_SF, Placebo_Seattle, Placebo_Boulder, Placebo_Philadelphia, Placebo_Oakland)
  }
  
  if(Population_Weighted == 1){
    PlaceboData <- dplyr::select(PlaceboData, -c(Std.Error, lower_bound, upper_bound))
  }
  
  ##Rbind placebo data to average data
  Placebo_Average_Data <- rbind(AverageData, PlaceboData)
  
  ##Filter to just balanced time periods
  if(Drop_Berkeley == 0){
    Placebo_Average_Data_Balanced <- filter(Placebo_Average_Data, Time >= -38 & Time <= 25)
  }
  if(Drop_Berkeley == 1){
    Placebo_Average_Data_Balanced <- filter(Placebo_Average_Data, Time >= -60 & Time <= 25)
  }
  
  ##Creating unique ids for each Level by Treated City placebo combination
  Placebo_Average_Data_Balanced <- mutate(Placebo_Average_Data_Balanced, Placebo_ID = dplyr::group_indices(Placebo_Average_Data_Balanced, .dots=c("Level", "Treated_City")))
  Placebo_Average_Data_Balanced$Placebo_ID[Placebo_Average_Data_Balanced$Level == "Average"] <- "Average"
  
  
  ##Calculate MSPE
  Placebo_Average_Data_Balanced <- mutate(Placebo_Average_Data_Balanced, PrePeriod = ifelse(Time < 0, 1, 0))
  MSPE <- group_by(Placebo_Average_Data_Balanced, PrePeriod, Placebo_ID) %>%
    summarise(MSPE = sqrt(mean((Estimate)^2, na.rm = TRUE)),
              MSPE_Normalized = sqrt(mean((Estimate)^2, na.rm = TRUE))/PretreatmentMean) %>%
    distinct()
  
  ##Calculating the RMSPE
  MSPE <- group_by(MSPE, Placebo_ID) %>% mutate(RMSPE = MSPE/dplyr::lead(MSPE,1))
  
  Placebo_Average_Data_Balanced <- left_join(Placebo_Average_Data_Balanced, MSPE,
                                             by = c("Placebo_ID" = "Placebo_ID",
                                                    "PrePeriod" = "PrePeriod")) 
  
  ##Calculating the ranking
  MSPE_Ranking <- filter(Placebo_Average_Data_Balanced, PrePeriod == 0) %>%
    distinct(Placebo_ID, RMSPE) %>%
    mutate(RMSPE_rank = rank(desc(RMSPE)))
  
  ##Calculating p-value
  pval <- filter(MSPE_Ranking, Placebo_ID == "Average") %>%
    distinct(RMSPE_rank/length(unique(Placebo_Average_Data_Balanced$Placebo_ID))) %>% as.numeric
  
  ##Pruning placebos that have a relatively low pre-period MSPE
  
  ##Filtering out pre-period normalized MSPEs 5x or more larger than Average pre-period normalized MSPE
  PrePeriod_Normalized_MSPE <- filter(MSPE, PrePeriod == 1 & Placebo_ID == "Average") %>% distinct()
  PrePeriod_Normalized_MSPE <- PrePeriod_Normalized_MSPE$MSPE_Normalized
  MSPE_Pruned <- filter(MSPE, PrePeriod == 1 & MSPE_Normalized < 5*PrePeriod_Normalized_MSPE)
  
  ##Only keeping valid placebos
  PlaceboData_Pruned <- filter(Placebo_Average_Data_Balanced, Placebo_ID %in% MSPE_Pruned$Placebo_ID)
  
  ##Taking a sample of 100 valid placebos
  Placebo_Zips <- unique(PlaceboData_Pruned$Placebo_ID)
  Sample_Placebos <- c(sample(Placebo_Zips, 100), "Average")
  PlaceboData_Pruned_Sample <- filter(PlaceboData_Pruned, Placebo_ID %in% Sample_Placebos) %>% ungroup()
  
  
  List_Out <- list(PlaceboData_Pruned_Sample, pval)
  return(List_Out)
  
}


##Placebo Data Generation Function - BORDERS - AVERAGE ESTIMATES
PlaceboDataGenFun_Average_Borders <- function(ATTData, PreTreatmentMeans_Data, LongDF){
  
  # ATTData <- ATT_Borders
  # PreTreatmentMeans_Data <- PreTreatmentMeans_Borders
  # LongDF <- LongDF_Borders
  
  if(Population_Weighted == 1){
    ##Calculate WEIGHTED average effect based on population
    ATTData <- left_join(ATTData, PopulationCensusData,
                         by = c("Level" = "store_zip3"))
    AverageData <- filter(ATTData, !(Level == "Average")) %>%
      group_by(Time) %>%
      summarise(Estimate = weighted.mean(Estimate, population)) %>%
      mutate(Level = "Average")
  }
  
  if(Population_Weighted == 0){
    ##Filter to just average effects
    AverageData <- filter(ATTData, Level == "Average")
  }
  
  if(Population_Weighted == 1){
    ##Calculate WEIGHTED average pretreatment mean of taxed zip codes
    PreTreatmentMeans_Data <- left_join(PreTreatmentMeans_Data, PopulationCensusData,
                                        by = c("store_zip3" = "store_zip3"))
    PretreatmentMean <- summarise(PreTreatmentMeans_Data, Pretreatmentmean = weighted.mean(PreTreatmentMeans, population)) %>% as.numeric
    AverageData <- cbind(AverageData, PretreatmentMean)
    AverageData <- mutate(AverageData, Estimate_Normalized = Estimate/PretreatmentMean)
  }
  
  if(Population_Weighted == 0){
    ##Calculate average pretreatment mean of taxed zip codes
    PretreatmentMean <- summarise(PreTreatmentMeans_Data, Pretreatmentmean = mean(PreTreatmentMeans)) %>% as.numeric
    AverageData <- cbind(AverageData, PretreatmentMean)
    AverageData <- mutate(AverageData, Estimate_Normalized = Estimate/PretreatmentMean)
  }
  
  ##Mutate "treated city" column to distinguish between treated cities
  AverageData <- mutate(AverageData, Treated_City = "Average")
  
  ##Bring in and Normalize Placebo Data for each of the 6 treated cities
  
  #SF
  Placebo_SF <- ATT_SF_Borders
  Placebo_SF$Level <- as.numeric(Placebo_SF$Level)
  Placebo_SF <- left_join(Placebo_SF, PreTreatmentMeans_SF_Borders,
                          by = c("Level" = "store_zip3"))
  Placebo_SF <- mutate(Placebo_SF, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_SF <- mutate(Placebo_SF, Treated_City = "San Francisco")
  
  #Seattle
  Placebo_Seattle <- ATT_Seattle_Borders
  Placebo_Seattle$Level <- as.numeric(Placebo_Seattle$Level)
  Placebo_Seattle <- left_join(Placebo_Seattle, PreTreatmentMeans_Seattle_Borders,
                               by = c("Level" = "store_zip3"))
  Placebo_Seattle <- mutate(Placebo_Seattle, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Seattle <- mutate(Placebo_Seattle, Treated_City = "Seattle")
  
  #Boulder
  Placebo_Boulder <- ATT_Boulder_Borders  
  Placebo_Boulder$Level <- as.numeric(Placebo_Boulder$Level)
  Placebo_Boulder <- left_join(Placebo_Boulder, PreTreatmentMeans_Boulder_Borders,
                               by = c("Level" = "store_zip3"))
  Placebo_Boulder <- mutate(Placebo_Boulder, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Boulder <- mutate(Placebo_Boulder, Treated_City = "Boulder")
  
  #Philadelphia
  Placebo_Philadelphia <- ATT_Philadelphia_Borders  
  Placebo_Philadelphia$Level <- as.numeric(Placebo_Philadelphia$Level)
  Placebo_Philadelphia <- left_join(Placebo_Philadelphia, PreTreatmentMeans_Philadelphia_Borders,
                                    by = c("Level" = "store_zip3"))
  Placebo_Philadelphia <- mutate(Placebo_Philadelphia, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Philadelphia <- mutate(Placebo_Philadelphia, Treated_City = "Philadelphia")
  
  #Oakland
  Placebo_Oakland <- ATT_Oakland_Borders  
  Placebo_Oakland$Level <- as.numeric(Placebo_Oakland$Level)
  Placebo_Oakland <- left_join(Placebo_Oakland, PreTreatmentMeans_Oakland_Borders,
                               by = c("Level" = "store_zip3"))
  Placebo_Oakland <- mutate(Placebo_Oakland, Estimate_Normalized = Estimate/PretreatmentMean)
  Placebo_Oakland <- mutate(Placebo_Oakland, Treated_City = "Oakland")
  
  if(Drop_Berkeley == 0){
    #Berkeley
    Placebo_Berkeley <- ATT_Berkeley_Borders  
    Placebo_Berkeley$Level <- as.numeric(Placebo_Berkeley$Level)
    Placebo_Berkeley <- left_join(Placebo_Berkeley, PreTreatmentMeans_Berkeley_Borders,
                                  by = c("Level" = "store_zip3"))
    Placebo_Berkeley <- mutate(Placebo_Berkeley, Estimate_Normalized = Estimate/PretreatmentMean)
    Placebo_Berkeley <- mutate(Placebo_Berkeley, Treated_City = "Berkeley")
  }
  
  ##Rbind all placebo data
  if(Drop_Berkeley == 0){
    PlaceboData <- rbind(Placebo_SF, Placebo_Seattle, Placebo_Boulder, Placebo_Philadelphia, Placebo_Oakland, Placebo_Berkeley)
  }
  if(Drop_Berkeley == 1){
    PlaceboData <- rbind(Placebo_SF, Placebo_Seattle, Placebo_Boulder, Placebo_Philadelphia, 
                         Placebo_Oakland)
  }
  
  if(Population_Weighted == 1){
    PlaceboData <- dplyr::select(PlaceboData, -c(Std.Error, lower_bound, upper_bound))
  }
  
  ##Rbind placebo data to average data
  Placebo_Average_Data <- rbind(AverageData, PlaceboData)
  
  ##Filter to just balanced time periods
  if(Drop_Berkeley == 0){
    Placebo_Average_Data_Balanced <- filter(Placebo_Average_Data, Time >= -38 & Time <= 25)
  }
  if(Drop_Berkeley == 1){
    Placebo_Average_Data_Balanced <- filter(Placebo_Average_Data, Time >= -60 & Time <= 25)
  }
  
  ##Creating unique ids for each Level by Treated City placebo combination
  Placebo_Average_Data_Balanced <- mutate(Placebo_Average_Data_Balanced, Placebo_ID = dplyr::group_indices(Placebo_Average_Data_Balanced, .dots=c("Level", "Treated_City")))
  Placebo_Average_Data_Balanced$Placebo_ID[Placebo_Average_Data_Balanced$Level == "Average"] <- "Average"
  
 
  ##Calculate MSPE
  Placebo_Average_Data_Balanced <- mutate(Placebo_Average_Data_Balanced, PrePeriod = ifelse(Time < 0, 1, 0))
  MSPE <- group_by(Placebo_Average_Data_Balanced, PrePeriod, Placebo_ID) %>%
    summarise(MSPE = sqrt(mean((Estimate)^2, na.rm = TRUE)),
              MSPE_Normalized = sqrt(mean((Estimate)^2, na.rm = TRUE))/PretreatmentMean) %>%
    distinct()
  
  ##Calculating the RMSPE
  MSPE <- group_by(MSPE, Placebo_ID) %>% mutate(RMSPE = MSPE/dplyr::lead(MSPE,1))
  
  Placebo_Average_Data_Balanced <- left_join(Placebo_Average_Data_Balanced, MSPE,
                                             by = c("Placebo_ID" = "Placebo_ID",
                                                    "PrePeriod" = "PrePeriod")) 
  
  ##Calculating the ranking
  MSPE_Ranking <- filter(Placebo_Average_Data_Balanced, PrePeriod == 0) %>%
    distinct(Placebo_ID, RMSPE) %>%
    mutate(RMSPE_rank = rank(desc(RMSPE)))
  
  ##Calculating p-value
  pval <- filter(MSPE_Ranking, Placebo_ID == "Average") %>%
    distinct(RMSPE_rank/length(unique(Placebo_Average_Data_Balanced$Placebo_ID))) %>% as.numeric
  
  ##Pruning placebos that have a relatively low pre-period MSPE
  
  ##Filtering out pre-period normalized MSPEs 5x or more larger than Average pre-period normalized MSPE
  PrePeriod_Normalized_MSPE <- filter(MSPE, PrePeriod == 1 & Placebo_ID == "Average") %>% distinct()
  PrePeriod_Normalized_MSPE <- PrePeriod_Normalized_MSPE$MSPE_Normalized
  MSPE_Pruned <- filter(MSPE, PrePeriod == 1 & MSPE_Normalized < 5*PrePeriod_Normalized_MSPE)
  
  ##Only keeping valid placebos
  PlaceboData_Pruned <- filter(Placebo_Average_Data_Balanced, Placebo_ID %in% MSPE_Pruned$Placebo_ID)
  
  ##Taking a sample of 100 valid placebos
  Placebo_Zips <- unique(PlaceboData_Pruned$Placebo_ID)
  Sample_Placebos <- c(sample(Placebo_Zips, 100), "Average")
  PlaceboData_Pruned_Sample <- filter(PlaceboData_Pruned, Placebo_ID %in% Sample_Placebos) %>% ungroup()
  
  
  List_Out <- list(PlaceboData_Pruned_Sample, pval)
  return(List_Out)
  
}


##Plot Function - CONSUMPTION 
PlaceboPlotFun <- function(PlaceboData, pvalData, zip3, LocationName, Type){
  
  # PlaceboData <- Oakland_Placebo[[1]]
  # zip3 <- "946"
  # LocationName <- "Oakland"
  # pvalData <- Oakland_Placebo[[2]]
  # Type <- "Consumption"
  
  
  PlaceboData <- filter(PlaceboData, !(is.na(Time)))
  
  MeanEstimate <- filter(PlaceboData, Level == zip3 & PrePeriod == 0) %>%
    summarise(MeanEstimate = mean(Estimate_Normalized)) %>% as.numeric
  
  syn_bc_plot_formatted <- ggplot(data = filter(PlaceboData, Level != zip3)) + ggplot2::geom_hline(yintercept = 0, size = 1.1, color = "red", linetype = "solid") + 
    ggplot2::geom_vline(xintercept = 0, size = 1.1, color = "red", linetype = "dashed") + 
    ggplot2::geom_line(aes(x=Time, y=Estimate_Normalized, group = as.factor(Level)), size = 1.5, alpha = 0.4, color = "grey") + 
    ggplot2::geom_line(data = filter(PlaceboData, Level == zip3), 
                       aes(x=Time, y=Estimate_Normalized), size = 1.5, alpha = 1, color = "purple") + 
    #ggplot2::geom_line(aes(x=Time, y=100*(lower_bound/BiasedSCMObject$PreTreatMean_ATE_DF$`Pretreatment Mean`)), size = 1, alpha = 1, color = "grey", linetype = "dashed") + 
    #ggplot2::geom_line(aes(x=Time, y=100*(upper_bound/BiasedSCMObject$PreTreatMean_ATE_DF$`Pretreatment Mean`)), size = 1, alpha = 1, color = "grey", linetype = "dashed") + 
    #ggplot2::geom_point(aes(x=Time, y=Estimate), color = "#b41e7c") + 
    annotate("text", color = "red", size = 8, x = -22, 
             y = 1, label = "Start of Tax") +
    geom_segment(aes(x = -15, y = 1,
                     xend = -1, yend = 1),
                 color = "red", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
    geom_segment(aes(x = 0, y = MeanEstimate, 
                     xend = max(Time), yend = MeanEstimate),
                 color = "blue", size = 1) + 
    geom_segment(aes(x = -25, y = -0.6,
                     xend = -1, yend = MeanEstimate),
                 color = "blue", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("label", color = "black", size = 10, x = -37, 
             y = -0.6, 
             label = paste0("Effect Estimate:\n", sprintf('%0.1f', round(MeanEstimate*100, digits = 2)), "% (p=", round(pvalData, 3),")")) +
    ggplot2::labs(color = "", linetype = "", #title = paste0(LocationName), 
                  y = "Scaled Treatment Effect (in Percent)", x = "Event-Time (Months)") + 
    ggplot2::theme_minimal() +
    scale_x_continuous(breaks = seq(min(PlaceboData$Time, na.rm = TRUE), 
                                    max(PlaceboData$Time, na.rm = TRUE), by = 12)) +
    scale_y_continuous(labels = comma) +
    theme_bw() + 
    theme_classic() +
    theme(legend.title=element_blank(), 
          #panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 30),
          text = element_text(size=26), 
          legend.key.height=unit(2,"line"),
          axis.text.x = element_text(size=32),
          axis.text.y = element_text(size=32),
          axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 32, margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    ylim(-1,1) 
  
  ggsave(paste0("Figures/Results/Multisynth Results/", Type, "/Urbanicity/multisynth_placebos_", LocationName, "_", Type, ".pdf"), width = 40, height = 25, units = "cm")
  
  
  return(syn_bc_plot_formatted)
  
}


##Plot Function - CONSUMPTION 
PlaceboPlotFun_Prices <- function(PlaceboData, pvalData, zip3, LocationName, Type){
  
  # PlaceboData <- SanFrancisco_Placebo_Prices[[1]]
  # zip3 <- "941"
  # LocationName <- "San Francisco"
  # pvalData <- SanFrancisco_Placebo_Prices[[2]]
  # Type <- "Prices"
  
  
  PlaceboData <- filter(PlaceboData, !(is.na(Time)))
  
  MeanEstimate <- filter(PlaceboData, Level == zip3 & PrePeriod == 0) %>%
    summarise(MeanEstimate = mean(Estimate_Normalized)) %>% as.numeric
  
  syn_bc_plot_formatted <- ggplot(data = filter(PlaceboData, Level != zip3)) + ggplot2::geom_hline(yintercept = 0, size = 1.1, color = "red", linetype = "solid") + 
    ggplot2::geom_vline(xintercept = 0, size = 1.1, color = "red", linetype = "dashed") + 
    ggplot2::geom_line(aes(x=Time, y=Estimate_Normalized, group = as.factor(Level)), size = 1.5, alpha = 0.4, color = "grey") + 
    ggplot2::geom_line(data = filter(PlaceboData, Level == zip3), 
                       aes(x=Time, y=Estimate_Normalized), size = 1.5, alpha = 1, color = "purple") + 
    #ggplot2::geom_line(aes(x=Time, y=100*(lower_bound/BiasedSCMObject$PreTreatMean_ATE_DF$`Pretreatment Mean`)), size = 1, alpha = 1, color = "grey", linetype = "dashed") + 
    #ggplot2::geom_line(aes(x=Time, y=100*(upper_bound/BiasedSCMObject$PreTreatMean_ATE_DF$`Pretreatment Mean`)), size = 1, alpha = 1, color = "grey", linetype = "dashed") + 
    #ggplot2::geom_point(aes(x=Time, y=Estimate), color = "#b41e7c") + 
    annotate("text", color = "red", size = 8, x = -22, 
             y = 1, label = "Start of Tax") +
    geom_segment(aes(x = -15, y = 1,
                     xend = -1, yend = 1),
                 color = "red", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
    geom_segment(aes(x = 0, y = MeanEstimate, 
                     xend = max(Time), yend = MeanEstimate),
                 color = "blue", size = 1) + 
    geom_segment(aes(x = -25, y = 0.5,
                     xend = -1, yend = MeanEstimate),
                 color = "blue", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("label", color = "black", size = 10, x = -37, 
             y = 0.5, 
             label = paste0("Effect Estimate:\n", sprintf('%0.1f', round(MeanEstimate*100, digits = 2)), "% (p=", round(pvalData, 3),")")) +
    ggplot2::labs(color = "", linetype = "", #title = paste0(LocationName), 
                  y = "Scaled Treatment Effect (in Percent)", x = "Event-Time (Months)") + 
    ggplot2::theme_minimal() +
    scale_x_continuous(breaks = seq(min(PlaceboData$Time, na.rm = TRUE), 
                                    max(PlaceboData$Time, na.rm = TRUE), by = 12)) +
    scale_y_continuous(labels = comma) +
    theme_bw() + 
    theme_classic() +
    theme(legend.title=element_blank(), 
          #panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 30),
          text = element_text(size=26), 
          legend.key.height=unit(2,"line"),
          axis.text.x = element_text(size=32),
          axis.text.y = element_text(size=32),
          axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 32, margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    ylim(-1,1) 
  
  ggsave(paste0("Figures/Results/Multisynth Results/", Type, "/Urbanicity/multisynth_placebos_", LocationName, "_", Type, ".pdf"), width = 40, height = 25, units = "cm")
  
  return(syn_bc_plot_formatted)
  
}

##Plot Function - BORDERS
PlaceboPlotFun_Borders <- function(PlaceboData, pvalData, zip3, LocationName, Type){
  
  # PlaceboData <- Seattle_Placebo_Borders[[1]]
  # zip3 <- c("980", "982", "983", "984")
  # LocationName <- "Seattle"
  # pvalData <- Seattle_Placebo_Borders[[2]]
  # Type <- "Borders"
  
  
  PlaceboData <- filter(PlaceboData, !(is.na(Time)))
  
  MeanEstimate <- filter(PlaceboData, Level %in% zip3 & PrePeriod == 0) %>%
    summarise(MeanEstimate = mean(Estimate)/mean(PretreatmentMean)) %>% as.numeric() 
  
  syn_bc_plot_formatted <- ggplot(data = filter(PlaceboData, !(Level %in% zip3))) + ggplot2::geom_hline(yintercept = 0, size = 1.1, color = "red", linetype = "solid") + 
    ggplot2::geom_vline(xintercept = 0, size = 1.1, color = "red", linetype = "dashed") + 
    ggplot2::geom_line(aes(x=Time, y=Estimate_Normalized, group = as.factor(Level)), size = 1.5, alpha = 0.4, color = "grey") + 
    ggplot2::geom_line(data = filter(PlaceboData, Level %in% zip3) %>% 
                         group_by(Time) %>% 
                         summarise(Estimate_Normalized = mean(Estimate)/mean(PretreatmentMean)), 
                       aes(x=Time, y=Estimate_Normalized), size = 1.5, alpha = 1, color = "purple") + 
    #ggplot2::geom_line(aes(x=Time, y=100*(lower_bound/BiasedSCMObject$PreTreatMean_ATE_DF$`Pretreatment Mean`)), size = 1, alpha = 1, color = "grey", linetype = "dashed") + 
    #ggplot2::geom_line(aes(x=Time, y=100*(upper_bound/BiasedSCMObject$PreTreatMean_ATE_DF$`Pretreatment Mean`)), size = 1, alpha = 1, color = "grey", linetype = "dashed") + 
    #ggplot2::geom_point(aes(x=Time, y=Estimate), color = "#b41e7c") + 
    annotate("text", color = "red", size = 8, x = -22, 
             y = 1, label = "Start of Tax") +
    geom_segment(aes(x = -15, y = 1,
                     xend = -1, yend = 1),
                 color = "red", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
    geom_segment(aes(x = 0, y = MeanEstimate, 
                     xend = max(Time), yend = MeanEstimate),
                 color = "blue", size = 1) + 
    geom_segment(aes(x = -15, y = -0.5,
                     xend = 2, yend = MeanEstimate-.10),
                 color = "blue", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("label", color = "black", size = 10, x = -27, 
             y = -0.5, 
             label = paste0("Effect Estimate:\n", sprintf('%0.1f', round(MeanEstimate*100, digits = 2)), "% (p=", round(pvalData, 3),")")) +
    ggplot2::labs(color = "", linetype = "", #title = paste0(LocationName), 
                  y = "Scaled Treatment Effect (in Percent)", x = "Event-Time (Months)") + 
    ggplot2::theme_minimal() +
    scale_x_continuous(breaks = seq(min(PlaceboData$Time, na.rm = TRUE), 
                                    max(PlaceboData$Time, na.rm = TRUE), by = 12)) +
    scale_y_continuous(labels = comma) +
    theme_bw() + 
    theme_classic() +
    theme(legend.title=element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 30),
          text = element_text(size=26), 
          legend.key.height=unit(2,"line"),
          axis.text.x = element_text(size=32),
          axis.text.y = element_text(size=32),
          axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 32, margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    ylim(-1,1) 
  
  ggsave(paste0("Figures/Results/Multisynth Results/", Type, "/Urbanicity/multisynth_placebos_", LocationName, "_", Type, ".pdf"), width = 40, height = 25, units = "cm")
  
  return(syn_bc_plot_formatted)
  
}

