###################################################################
###############Initializing Synthetic Control Object###############
###################################################################

SynthControlGeneration_Fun <- function(data, treated_unit, time_min){
    
    data %>%
    
    # initial the synthetic control object
    synthetic_control(outcome = TotalOz_zipcode, # outcome
                      unit = store_zip3, # unit index in the panel data
                      i_time = 0, # time period when the intervention occurred
                      time = treated_months, # time index in the panel data
                      i_unit = treated_unit, # unit where the intervention occurred
                      generate_placebos=T # generate placebo synthetic controls (for inference)
    ) %>%
    
    # Generate the aggregate predictors used to fit the weights
    
    # average median HH income, proportion < 10K income, and proportion of the
    # population between 18 and 64 years of age from 2015-2020
    generate_predictor(time_window = time_min:-1,
                       medHHincome = mean(medHHincome, na.rm = T),
                       poverty_10K = mean(poverty_10K, na.rm = T),
                       age_18to64 = mean(age_18to64, na.rm = T),
                       population = mean(population, na.rm = T),
                       medage = mean(medage, na.rm = T),
                       num_housingunits = mean(num_housingunits, na.rm = T),
                       pwhite = mean(pwhite, na.rm = T),
                       pblack = mean(pblack, na.rm = T),
                       pamindalask = mean(pamindalask, na.rm = T),
                       pasian = mean(pasian, na.rm = T),
                       phawaii = mean(phawaii, na.rm = T),
                       pother = mean(pother, na.rm = T),
                       phisp = mean(phisp, na.rm = T)
    ) %>%
    
    # # Don't need different time-window predictors
    # # average beer consumption in the donor pool from 1984 - 1988
    # generate_predictor(time_window = 1984:1988,
    #                    beer_sales = mean(beer, na.rm = T)) %>%
    # 
    # # Lagged cigarette sales 
    # generate_predictor(time_window = 1975,
    #                    cigsale_1975 = cigsale) %>%
    # generate_predictor(time_window = 1980,
    #                    cigsale_1980 = cigsale) %>%
  # generate_predictor(time_window = 1988,
  #                    cigsale_1988 = cigsale) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = time_min:-1, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
    
    # Generate the synthetic control
    generate_control()
  
}


####################################################
###############Synthetic Control Plot###############
####################################################

SynthControlPlot_Fun <- function(OriginalData, SynthData){
  
plot_trends <- SynthData %>% plot_trends()
plot_trends <- plot_trends +
  ylab("Total Oz. Sold by Zip Code by Month") + xlab("Months from Tax Implementation") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(min(OriginalData$treated_months), max(OriginalData$treated_months), by = 10)) +
  scale_y_continuous(labels = comma) +
  labs(caption = "") +
  theme_bw() + 
  theme(legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=32), 
        legend.key.height=unit(2,"line"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 
plot_trends

}


################################################################
###############Synthetic Control Differences Plot###############
################################################################

SynthControlDifferencesPlot_Fun <- function(OriginalData, SynthData){
  
plot_differences <- SynthData %>% plot_differences()
plot_differences <- plot_differences +
  ylab("Difference in Total Oz. Sold by Zip Code by Month") + xlab("Months from Tax Implementation") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(min(OriginalData$treated_months), max(OriginalData$treated_months), by = 10)) +
  scale_y_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=26), 
        legend.key.height=unit(2,"line"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 
plot_differences

}



#######################################################################
###############Synthetic Control Donor Weights Figure##################
#######################################################################

SynthControlWeights_Fun <- function(SynthData){
  
plot_weights_fun <- function(data, weight_threshold) 
{
  dplyr::bind_rows(grab_unit_weights(data, placebo = FALSE) %>% 
                     dplyr::mutate(type = "Control Unit Weights (W)"), 
                   grab_predictor_weights(data, placebo = FALSE) %>% dplyr::mutate(type = "Variable Weights (V)") %>% 
                     dplyr::rename(unit = variable)) %>% dplyr::arrange(weight) %>% 
    dplyr::mutate(unit = forcats::fct_reorder(unit, weight)) %>% filter(weight >= weight_threshold) %>%
    ggplot2::ggplot(ggplot2::aes(unit, weight, fill = type, 
                                 color = type)) + ggplot2::geom_col(show.legend = FALSE, 
                                                                    alpha = 0.65) + ggplot2::coord_flip() + ggplot2::labs(x = "") + 
    ggplot2::facet_wrap(~type, ncol = 2, scales = "free") + 
    ggplot2::theme_minimal() + ggplot2::scale_fill_manual(values = c("#b41e7c", 
                                                                     "grey60")) + ggplot2::scale_color_manual(values = c("#b41e7c", 
                                                                                                                         "grey60")) + ggplot2::theme(text = ggplot2::element_text(size = 14))
}

chosen_weight_threshold <- 0.0000001
plot_weights <- SynthData %>% plot_weights_fun(chosen_weight_threshold)
plot_weights <- plot_weights +
  ylab("Weight") + xlab("") +
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26),
        text = element_text(size = 32),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 
plot_weights

}




#######################################################################
###############Synthetic Control Placebo Graph#########################
#######################################################################

SynthControlPlacebos_Fun <- function(OriginalData, SynthData){
  
##Outputting the placebos in the DIFFERENCES synthetic control plot
plot_placebos_modified <- function (data, time_window = NULL, prune = TRUE) 
{
  if (!(".meta" %in% colnames(data))) {
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  unit_index <- data$.meta[[1]]$unit_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  if (is.null(time_window)) {
    time_window <- unique(data$.original_data[[1]][[time_index]])
  }
  plot_data <- data %>% grab_synthetic_control(placebo = TRUE) %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% 
    dplyr::filter(time_unit %in% time_window) %>% 
    dplyr::mutate(type_text = ifelse(.placebo == 0, treatment_unit, "Control Units"), 
                  type_text = factor(type_text, levels = c(treatment_unit, "Control Units")))
  caption <- ""
  if (prune) {
    sig_data = data %>% grab_signficance(time_window = time_window)
    thres <- sig_data %>% dplyr::filter(type == "Treated") %>%
      dplyr::pull(pre_mspe) %>% sqrt(.)
    retain_ <- sig_data %>% dplyr::select(unit_name, pre_mspe) %>%
      dplyr::filter(sqrt(pre_mspe) <= thres * 2) %>% dplyr::pull(unit_name)
    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
    caption <- "Pruned all placebo cases with a pre-period RMSPE exceeding two times the treated unit's pre-period RMSPE."
  }
  plot_data %>% ggplot2::ggplot(ggplot2::aes(time_unit, diff,
                                             group = .id, color = type_text, alpha = type_text, size = type_text)) +
    ggplot2::geom_hline(yintercept = 0, color = "black",
                        linetype = 2) + ggplot2::geom_vline(xintercept = trt_time,
                                                            color = "black", linetype = 3) + ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("#b41e7c",
                                           "grey60")) + ggplot2::scale_alpha_manual(values = c(1,
                                                                                               0.4)) + ggplot2::scale_size_manual(values = c(1, 0.5)) +
    ggplot2::labs(color = "", alpha = "", size = "",
                  y = outcome_name, x = time_index, title = paste0("Difference of each '",
                                                                   unit_index, "' in the donor pool"), caption = caption) +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
}

plot_placebos <- SynthData %>% plot_placebos_modified()
plot_placebos <- plot_placebos +
  ylab("Difference in Total Oz. Sold by Zip Code by Month") + xlab("Months from Tax Implementation") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(min(OriginalData$treated_months), max(OriginalData$treated_months), by = 10)) +
  scale_y_continuous(labels = comma) +
  labs(caption = "", legend = "") +
  theme_bw() + 
  theme(legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=26), 
        legend.key.height=unit(2,"line"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) 
plot_placebos

}




############################################################################
###############Synthetic Control MSPE Figure################################
############################################################################

##Construct p-values (confidence intervals) for the synthetic control fit. 
##Adabie et al. 2010 outline a way of constructing Fishers Exact P-values by dividing the post-intervention MSPE by the pre-intervention MSPE 
##and then ranking all the cases by this ratio in descending order. A p-value is then constructed by taking the rank/total.
##The idea is that if the synthetic control fits the observed time series well (low MSPE in the pre-period) 
##and diverges in the post-period (high MSPE in the post-period) then there is a meaningful effect due to the intervention. 
##If the intervention had no effect, then the post-period and pre-period should continue to map onto one another fairly well, 
##yielding a ratio close to 1. If the placebo units fit the data similarly, then we cant reject the null hypothesis 
##that there is no effect brought about by the intervention.

SynthControlMSPE_Fun <- function(SynthData, treated_unit){
  
##using a slightly modified plot_mspe_ratio() function
plot_mspe_ratio_fun <- function (data, threshold, treated_unit, time_window = NULL) 
{
  data %>% grab_signficance(time_window = time_window) %>% 
    dplyr::mutate(unit_name = forcats::fct_reorder(as.character(unit_name), 
                                                   mspe_ratio)) %>% filter(mspe_ratio >= threshold | unit_name == treated_unit) %>% ggplot2::ggplot(ggplot2::aes(unit_name, 
                                                                                                                                     mspe_ratio, fill = type)) + ggplot2::geom_col(alpha = 0.65) + 
    ggplot2::coord_flip() + ggplot2::labs(y = "Post-Period MSPE / Pre-Period MSPE", 
                                          x = "", fill = "", color = "", title = "Ratio of the pre and post intervention period mean squared predictive error") + 
    ggplot2::scale_fill_manual(values = c("grey50", 
                                          "#b41e7c")) + ggplot2::scale_color_manual(values = c("grey50", 
                                                                                               "#b41e7c")) + ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
}

mspe_threshold <- 10
plot_mspe_ratio <- SynthData %>% plot_mspe_ratio_fun(mspe_threshold, treated_unit)
plot_mspe_ratio <- plot_mspe_ratio +
  ylab("Post-Period MSPE / Pre-Period MSPE") + xlab("") +
  ggtitle("Ratio of the Pre and Post Intervention Period Mean Squared Predictive Error") +
  theme(legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 26),
        legend.key.height=unit(2,"line"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 20, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 24))
plot_mspe_ratio

}




###################################################################################
###############Synthetic Control Significance Table################################
###################################################################################

SynthControlSignificance_Fun <- function(SynthData, file_name){
  
##Can also generate significance table (showing exact p-values)
grab_signficance <- SynthData %>% grab_signficance() %>% filter(fishers_exact_pvalue <= .1 | type == "Treated") %>% as.data.frame()
grab_signficance
grab_signficance <- mutate(grab_signficance, pre_mspe = pre_mspe/1000000000000,
                           post_mspe = post_mspe/1000000000000)
sigtable <- xtable(grab_signficance, digits = 2)
colnames(sigtable) <- c("Zip", "Type", "Pre MSPE", "Post MSPE", "MSPE Ratio", "Rank", "p-value", "z-score")

print(sigtable, file = file_name)

}



##Can grab other outputs using the "grab_" functions. 
## # A tibble: 8 x 2
##   Function             Description                                              
##   <chr>                <chr>                                                    
## 1 grab_outcome()       Extract the outcome variable generated by synthetic_cont~
## 2 grab_predictors()    Extract the aggregate-level covariates generated by gene~
## 3 grab_unit_weights()  Extract the unit weights generated by generate_weights().
## 4 grab_predictor_weig~ Extract the predictor variable weights generated by gene~
## 5 grab_loss()          Extract the RMSE loss of the optimized weights generated~
## 6 grab_synthetic_cont~ Extract the synthetic control generated using generate_c~
## 7 grab_signficance()   Generate inferential statistics comparing the rarity of ~
## 8 grab_balance_table() Compare the distributions of the aggregate-level predict~


#######################################################################################
###############Synthetic Control Return Full Placebo DF################################
#######################################################################################

SynthControlFullPlaceboDF_Fun <- function(SynthData, time_window = NULL, prune = TRUE) {
  
  ####Outputting the full placebo data dataframe to examine specific placebo lines
  
  if (!(".meta" %in% colnames(SynthData))) {
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- SynthData$.meta[[1]]$treatment_time[1]
  time_index <- SynthData$.meta[[1]]$time_index[1]
  treatment_unit <- SynthData$.meta[[1]]$treatment_unit[1]
  unit_index <- SynthData$.meta[[1]]$unit_index[1]
  outcome_name <- SynthData$.meta[[1]]$outcome[1]
  if (is.null(time_window)) {
    time_window <- unique(SynthData$.original_data[[1]][[time_index]])
  }
  plot_data <- SynthData %>% grab_synthetic_control(placebo = TRUE) %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% 
    dplyr::filter(time_unit %in% time_window) %>% 
    dplyr::mutate(type_text = ifelse(.placebo == 0, treatment_unit, "control units"), 
                  type_text = factor(type_text, levels = c(treatment_unit, "control units")))
  # caption <- ""
  if (prune) {
    sig_data = SynthData %>% grab_signficance(time_window = time_window)
    thres <- sig_data %>% dplyr::filter(type == "Treated") %>%
      dplyr::pull(pre_mspe) %>% sqrt(.)
    retain_ <- sig_data %>% dplyr::select(unit_name, pre_mspe) %>%
      dplyr::filter(sqrt(pre_mspe) <= thres * 2) %>% dplyr::pull(unit_name)
    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
  }
}

