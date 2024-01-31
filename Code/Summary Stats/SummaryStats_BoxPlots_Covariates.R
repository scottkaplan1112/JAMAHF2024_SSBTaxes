# devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
library(reshape)
library(tm)
#library(ggpubr)
library(egg)
library(RColorBrewer)
library(devtools)
library(NatParksPalettes)

#Ceto
setwd("C:/Users/skaplan/Backed Up Data/SSB Taxes")

##Long DF - Quantity
LongDF_Quantity <- fread("Data/multisynth_placebo_data/Urbanicity/extraction_longdf_AllTaxed.csv")

##Census Data
CensusData <- fread("Data/CensusData_Combined_Final_3DigitZip.csv")
names(CensusData)[names(CensusData) == "GEOID_3"] <- "store_zip3"

#List of taxed and border zip codes 
taxed_zips <- c("946", "981", "803", "191", "941")
border_zips <- c("949", "940", "980", "982", "983", "984", "800", "804", "805",
                 "81", "80", "948", "945")

#Get means of all variables for each treated zip code
CensusData_TreatedZips <- filter(CensusData, store_zip3 %in% taxed_zips)

#Filter to zip codes in primary urbanicity analysis
CensusData_UrbanZips <- filter(CensusData, store_zip3 %in% LongDF_Quantity$store_zip3)
  
#Create long data for treated zips
CensusData_TreatedZips_Transposed <- as.data.frame(t(CensusData_TreatedZips))
CensusData_TreatedZips_Transposed$variable <- rownames(CensusData_TreatedZips_Transposed)
colnames(CensusData_TreatedZips_Transposed) <- c("Philadelphia", "Boulder", "San Francisco", 
                                                 "Oakland", "Seattle", "covariate")

CensusData_TreatedZips_Melted <- melt(CensusData_TreatedZips_Transposed, id.vars = "covariate")
CensusData_TreatedZips_Melted <- filter(CensusData_TreatedZips_Melted, covariate != "store_zip3")

#Create long data for all urban zips
CensusData_UrbanZips_Transposed <- as.data.frame(t(CensusData_UrbanZips))
CensusData_UrbanZips_Transposed$variable <- rownames(CensusData_UrbanZips_Transposed)
colnames(CensusData_UrbanZips_Transposed) <- paste0("store_zip_",CensusData_UrbanZips_Transposed[1,])
names(CensusData_UrbanZips_Transposed)[names(CensusData_UrbanZips_Transposed) == "store_zip_store_zip3"] <- "covariate"

CensusData_UrbanZips_Melted <- melt(CensusData_UrbanZips_Transposed, id.vars = "covariate")
CensusData_UrbanZips_Melted <- filter(CensusData_UrbanZips_Melted, covariate != "store_zip3")

#Generating max and min for index values for each covariate
Max_Mins_Covariates <- group_by(CensusData_UrbanZips_Melted, covariate) %>%
  summarise(Max = max(value),
            Min = min(value))
CensusData_UrbanZips_Melted <- left_join(CensusData_UrbanZips_Melted, Max_Mins_Covariates,
                                         by = c("covariate" = "covariate"))

#Generating indexed values
CensusData_UrbanZips_Melted <- mutate(CensusData_UrbanZips_Melted, value_indexed = 100*(value-Min)/(Max-Min))

ggplot(CensusData_UrbanZips_Melted, aes(x = covariate, y = value_indexed)) +            # Applying ggplot function
  geom_boxplot()





#renaming treated zip code values to names
CensusData_TreatedZips$store_zip3[CensusData_TreatedZips$store_zip3 == "191"] <- "Philadelphia"
CensusData_TreatedZips$store_zip3[CensusData_TreatedZips$store_zip3 == "803"] <- "Boulder"
CensusData_TreatedZips$store_zip3[CensusData_TreatedZips$store_zip3 == "941"] <- "SF"
CensusData_TreatedZips$store_zip3[CensusData_TreatedZips$store_zip3 == "946"] <- "Oakland"
CensusData_TreatedZips$store_zip3[CensusData_TreatedZips$store_zip3 == "981"] <- "Seattle"


##################################
##############GRAPHS##############
##################################

#medHHincome
BoxPlots_medHHincome <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                     covariate == "medHHincome"), 
                              aes(y = value, x = "Median HH Income")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = medHHincome, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 19),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=15, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_medHHincome

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_medHHincome.png", width = 35, height = 18, units = "cm")


#population
BoxPlots_population <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "population"), 
                               aes(y = value, x = "Population")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = population, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_population

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_population.png", width = 35, height = 18, units = "cm")

#medage
BoxPlots_medage <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "medage"), 
                               aes(y = value, x = "Median Age")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = medage, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_medage

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_medage.png", width = 35, height = 18, units = "cm")


#num_housingunits
BoxPlots_num_housingunits <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "num_housingunits"), 
                               aes(y = value, x = "# Housing Units")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = num_housingunits, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_num_housingunits

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_num_housingunits.png", width = 35, height = 18, units = "cm")


#pwhite
BoxPlots_pwhite <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "pwhite"), 
                               aes(y = value, x = "% White")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = pwhite, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_pwhite

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_pwhite.png", width = 35, height = 18, units = "cm")


#pblack
BoxPlots_pblack <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "pblack"), 
                               aes(y = value, x = "% Black")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = pblack, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_pblack

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_pblack.png", width = 35, height = 18, units = "cm")

#pamindalask
BoxPlots_pamindalask <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "pamindalask"), 
                               aes(y = value, x = "% Am. Indian/Ala. Native")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = pamindalask, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 14),
                     axis.title.x = element_text(size = 28, margin = margin(t=0, r=15, b=-20, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_pamindalask

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_pamindalask.png", width = 35, height = 18, units = "cm")


#pasian
BoxPlots_pasian <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "pasian"), 
                               aes(y = value, x = "% Asian")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = pasian, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_pasian

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_pasian.png", width = 35, height = 18, units = "cm")


#phawaii
BoxPlots_phawaii <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "phawaii"), 
                               aes(y = value, x = "% Hawaiian")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = phawaii, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_phawaii

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_phawaii.png", width = 35, height = 18, units = "cm")


#pother
BoxPlots_pother <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "pother"), 
                               aes(y = value, x = "% Other")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = pother, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=0, r=0, b=-20, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_pother

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_pother.png", width = 35, height = 18, units = "cm")


#phisp
BoxPlots_phisp <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "phisp"), 
                               aes(y = value, x = "% Hispanic")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = phisp, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=0, r=0, b=-20, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_phisp

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_phisp.png", width = 35, height = 18, units = "cm")


#poverty_10K
BoxPlots_poverty_10K <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "poverty_10K"), 
                               aes(y = value, x = "% <$10K")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = poverty_10K, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_poverty_10K

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_poverty_10K.png", width = 35, height = 18, units = "cm")


#age_18to64
BoxPlots_age_18to64 <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "age_18to64"), 
                               aes(y = value, x = "% Age 18-64")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = age_18to64, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_age_18to64

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_age_18to64.png", width = 35, height = 18, units = "cm")


#perc_urban
BoxPlots_perc_urban <- ggplot(filter(CensusData_UrbanZips_Melted, 
                                      covariate == "perc_urban"), 
                               aes(y = value, x = "% Urban")) + 
  stat_boxplot(geom ='errorbar', color="darkblue", coef = 1.5) + 
  geom_boxplot(outlier.shape = NA, fill='lightblue', width = 0.9, color="darkblue") + 
  geom_point(data = CensusData_TreatedZips, aes(y = perc_urban, colour = store_zip3), size = 4) +
  scale_color_manual(values = c("gold", "red", "purple", "brown", "black")) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(labels = comma) +
  #ggtitle("Average Prices by Difference in Win Probability") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20),
                     text = element_text(size=16),
                     axis.title.y = element_text(size=22, margin = margin(t=0, r=15, b=0, l=0)),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 26),
                     axis.text.y = element_text(size = 22),
                     axis.text.x = element_text(size = 22),
                     axis.title.x = element_text(size = 28, margin = margin(t=15, r=0, b=0, l=0)),
                     legend.key.height=unit(2,"line"),
                     plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
BoxPlots_perc_urban

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_perc_urban.png", width = 35, height = 18, units = "cm")

#CombinedGraph
CombinedGraph <- ggpubr::ggarrange(BoxPlots_num_housingunits, 
                                   BoxPlots_medHHincome, 
                                   BoxPlots_medage,
                                   BoxPlots_poverty_10K,
                                   BoxPlots_age_18to64,
                                   BoxPlots_perc_urban,
                                   BoxPlots_pwhite,
                                   BoxPlots_pblack,
                                   BoxPlots_pasian,
                                   BoxPlots_pamindalask,
                                   BoxPlots_pother,
                                   BoxPlots_phisp,
                                   common.legend = TRUE,
                                   legend = "bottom",
                                   align = "hv",
                                   #heights = c(12, 12, 12),
                                   #widths = c(8, 8, 8),
                                   ncol = 3, nrow = 4) 
                           

ggsave("Figures/Summary Statistics/Covariate Box Plots/BoxPlots_Combined.png", width = 30, height = 45, units = "cm")
