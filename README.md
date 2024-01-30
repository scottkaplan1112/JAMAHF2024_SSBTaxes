# Kaplan et al. (JAMA Health Forum, 2024)
This repository provides R code allowing a user with access to the Nielsen Retail Scanner Data provided by the Kilts Center for Marketing at the Chicago Booth School of Business to replicate the results found in this paper: 
Kaplan S, White JS, Madsen KA, Basu S, Villas-Boas SB, Schillinger D. Evaluation of Changes in Prices and Purchases Following Implementation of Sugar-Sweetened Beverage Taxes Across the US. *JAMA Health Forum*. 2024;5(1):e234737. doi:10.1001/jamahealthforum.2023.4737

The document is split up into four primary sections of files: data preparation for analysis, analysis, summary statistics, and miscellaneous. The data preparation for analysis section numbers files in the order they should be used. The other sections include notes on file ordering, when relevant. Each file is accompanied by a short description of i) the primary objective of the file and ii) any important data or analysis outputs from the file.

I.	Data Preparation for Analysis

1.	1_Nielsen_Cleaning_Aggregating_RawData.R
  a.	This file brings in raw Nielsen data containing beverage products and organizes it by year (from 2012-2020). **It requires access to the Kilts Center for Marketing Nielsen retail scanner data repository (a paid subscription).**
  b.	The final outputs from this file are yearly-level files of raw Nielsen data titled “Nielsen_YEAR_Aggregated_FirstPass.csv.”
2.	2_Nielsen_UniqueUPC_Generation.R
  a.	This file takes all of the raw data from 1) and outputs a list of unique UPCs from the collected Nielsen data. 
  b.	The final output from this file is a single file with a list of all unique UPCs in the raw Nielsen data, titled “Nielsen_Aggregate_UniqueUPCs.csv.”
3.	3_CensusData_Extraction.R
  a.	This file outputs relevant Census data by 3-digit zip code (for both unweighted and population weighted analyses).
  b.	The final output from this file is “CensusData_Combined_Final_3DigitZip.csv” (there is also a separate file outputted and used for the population-weighted robustness analyses found in the Supplementary Online Material in eFigures 8 and 9).
4.	4_merge_nielsen_upcs_with_nutrition_SK.do
  a.	This file merges the set of unique Nielsen UPCs generated in 2) with nutrition data from handcoded sources and Label Insights. It requires the datasets “LabelInsight_short_Dec2020.dta” and “nutrition_by_upc.dta.”
  b.	The final outputs from this file are the unique Nielsen UPCs dataset (“upc_list_nielsen.dta”) and Nielsen UPCs with matches (or lack thereof) to nutrition data (“Nielsen_upcs_with_nutrition_short_SK.dta”).
5.	5_ObtainMatchedUPCs_fromNielsenRaw.R
  a.	This file takes the matched data from 4) and outputs i) a list of Nielsen UPCs that were matched to nutrition data and ii) the raw Nielsen data (by year) that was matched.
  b.	The final outputs from this file are “Matched UPC Characteristics_LI_and_Handcoded.csv” and “Nielsen_YEAR_MatchedData_NoCharacteristics_NoStores.csv” for each year.
6.	6_Nielsen_Stores_and_ProdCharacteristics.R
  a.	This file takes additional product characteristics files provided by Nielsen and matches it with the file generated in 5) (“Matched UPC Characteristics_LI_and_Handcoded.csv”). It also combines information about stores provided by Nielsen and Census data generated        in 3).
  b.	The final outputs from this file are a couple of different files of matched product characteristics (“Matched UPC Characteristics_NielsenProductFiles.csv” and “Matched UPC Characteristics_ALL.csv”) as well as a file combining both store and Census characteristics        (“Store_Census_Combined.csv”)
7.	7_Nielsen_Filtering_justSSBs.R
  a.	This file takes the “Matched UPC Characteristics_ALL.csv” file outputted in 6) and filters the raw yearly Nielsen data to just UPCs that were matched and denoted SSBs. 
  b.	The final output from this file are yearly raw Nielsen data files that is just SSB UPCs (that have been matched), titled “NielsenMovement_YEAR_SSBs.csv.”
8.	8_DID_and_Event_Study_DataPrep.R
  a.	This file takes matched UPC product characteristics data and the store/Census data, along with the data outputs from 7), and does some work to modify it to be in a format useful for a difference-in-differences/event study estimation.
  b.	The final outputs from this file are yearly level Nielsen data files at the month level (“NielsenMovement_YEAR_SSBs_MonthLevel.csv”).
9.	9_SyntheticControl_DataPrep.R
  a.	This file takes the data outputs from 8) and modifies them further to be ready for a synthetic control estimation. 
  b.	The final outputs from this file are yearly level Nielsen data files (titled “NielsenMovement_YEAR_SSBs_MonthLevel_SCEstimation.csv”).
10.	10_SyntheticControl_DataPrep_ZipCodeLevel.R
  a.	This file takes all of the data outputted in 9) and aggregates it to the month-by-zip code level and combine it into a single dataset. 
  b.	There are two final outputs from this file, one dataset for volume sold (“NielsenMovement_AllMonths_withCensusData_ZipCode.csv”) and one for shelf prices (“NielsenMovement_AllMonths_withCensusData_ZipCode_Prices.csv”).

II.	Analysis

1.	multisynth_estimations.R
  a.	This file runs the augmented synthetic control analyses and generates placebo estimates for each of the three separate analyses: volume sold, shelf prices, and volume sold in bordering zip codes. It relies on data outputted in 10) as well as the Census data output       from 3) to classify and filter based on urbanicity. NOTE that this code needs to be re-run in its entirety when changing the urbanicity cutoff criteria, and takes several hours to run.
  b.	The final outputs from this file are numerous, which are broken down by specific estimation. These files include estimation results, placebo estimation results, weights on different donors, and pretreatment means. 
2.	multisynth_funs.R
  a.	This file contains functions used in generating the graphs and data table outputs in the “multisynth_placebographs.R” file.
3.	multisynth_placebographs.R
  a.	This file generates the primary results for the study using the estimation outputs from 1) in the Analysis code section.
  b.	Final outputs from this file include several figures and other estimation information used in generating the final results seen in the study.

III.	Summary Statistics

1.	SummaryStats_forPaper.R
  a.	Outputs general summary statistics seen in Table 1. Requires data outputs from 10) in Data Preparation for Analysis section as well as the “CensusData_Combined_Final_3DigitZip.csv” and “Store_Census_Combined.csv" datasets.
2.	SummaryStats_BoxPlots_Covariates.R
  a.	This file produces the box plot figure seen in the Supplementary Online Information (eFigure 2). It requires the “CensusData_Combined_Final_3DigitZip.csv” as well as a data output from 1) in the Analysis section (titled “extraction_longdf_AllTaxed.csv”) in order         to the 3-digit zip codes used in the primary analysis.
3.	SummaryStats_DumbbellPlot.R
  a.	Produces (a less nice version) of the dumbbell plots seen in Figure 1 in main text (for volume purchases) and eFigure 1 in supplementary info (for shelf prices). Data needed for figure creation is read in at the top of the file and includes outputs from 10) in           Data Preparation for Analysis section as well as the “CensusData_Combined_Final_3DigitZip.csv” data output.
4.	Population by Subjurisdictions.xlsx
  a.	File to determine % of treated vs. untreated population within a 3-digit zip code (eTable 2 in Supplementary Online Information).
5.	UPC Coverage.xlsx
  a.	Spreadsheet version of UPC coverage table (eTable 1) in Supplementary Online Information.

IV.	Miscellaneous

1.	TWFE_Comparison.R
  a.	This file estimates two-way fixed effects models to compare to the augmented synthetic control results. It requires outputs from 10) in the Data Preparation for Analysis section as well as the “CensusData_Combined_Final_3DigitZip.csv” data output.
  b.	The final outputs from this file are the estimates and figures displayed in eTable3 and eFigures 16-19.
2.	UPC Coverage_Matched Products.R
  a.	This file calculates the total number of units (and thus, ounces) of each UPC, separately for the UPCs that were not matched with nutrition data (and thus not used in the analysis) and for the UPCs that were matched with nutrition data and included in the                analysis. It requires each of the yearly raw Nielsen data files outputted in 1) in the Data Preparation for Analysis section (“Nielsen_YEAR_Aggregated_FirstPass.csv”) as well as the data files outputted in 4) in the Data Preparation for Analysis section                  (“upc_list_nielsen.dta” and “Nielsen_upcs_with_nutrition_short_SK.dta”).
  b.	The final outputs from this file are total units by UPC for unmatched UPCs (“TotalUnits_byUPC_All_Unmatched_NoCharacteristics.csv”) and total units by UPC for matched UPCs (“TotalUnits_byUPC_All_Matched_NoCharacteristics.csv”).
3.	UPC Coverage_Matched Products_IncludingProductCharacteristics.R
  a.	This file takes the outputs from 2) and integrates it with important product characteristics found in the “products.tsv” file provided by Nielsen.
  b.	This file outputs important coverage estimates seen in the paper (the “84.0%” match estimate found in the 2nd paragraph of the Limitations subsection). It also provides an estimate of the % of matched volume that is classified as SSBs.
4.	SyntheticControl_Functions.R
  a.	This file contains miscellaneous functions for synthetic control analyses NOT using the augsynth R package. It was not necessary in producing the final results/deliverables for the study, but may be useful for others conducting synthetic control analyses.
