* Merge Label Insight nutrition data with list of Nielsen beverage UPCs from Scott 
* Authors: Scott Kaplan and Justin White
* Last updated: May 31, 2023


// Set directories

*local user Justin
*local user Scott
*local user ScottPersonal
local user ScottCeto

if "`user'"=="Justin" {
	global main		"~/Library/CloudStorage/Box-Box/Research/Soda tax eval"
	global dataI 	"$main/Data/IRi data"
	global dataN 	"$main/Data/Nielsen data"
	global do 		"$main/Do-files"
	global log 		"$main/Log"
	global temp 	"$main/Data/temp"
	global figures 	"$main/Output/Figures"
	global tables 	"$main/Output/Tables"
	}
	
	if "`user'"=="Scott" {
	global main		"S:/Kaplan/SSB Taxes/SF Synthetic Control Analysis"
	global dataI 	"$main/Processed Data/Nutrition and Label Insights Data"
	global dataN 	"$main/Processed Data/Unique Nielsen UPCs"
	*global do 		"$main/Do-files"
	*global log 	"$main/Log"
	global temp 	"$dataN/temp"
	global figures 	"$main/Output/Figures"
	global tables 	"$main/Output/Tables"
	}
	
	if "`user'"=="ScottPersonal" {
	global main		"Z:/Kaplan/SSB Taxes/SF Synthetic Control Analysis"
	global dataI 	"$main/Processed Data/Nutrition and Label Insights Data"
	global dataN 	"$main/Processed Data/Unique Nielsen UPCs"
	*global do 		"$main/Do-files"
	*global log 	"$main/Log"
	global temp 	"$dataN/temp"
	global figures 	"$main/Output/Figures"
	global tables 	"$main/Output/Tables"
	}
	
	if "`user'"=="ScottCeto" {
	global main		"C:/Users/skaplan/Backed Up Data/SSB Taxes"
	global dataI 	"$main/Processed Data/Nutrition and Label Insights Data"
	global dataN 	"$main/Processed Data/Unique Nielsen UPCs"
	*global do 		"$main/Do-files"
	*global log 	"$main/Log"
	global temp 	"$dataN/temp"
	global figures 	"$main/Output/Figures"
	global tables 	"$main/Output/Tables"
	}
	
cd "$dataN"


* Save temp file with Nielsen data
***********************************

import delimited "$dataN/Nielsen_Aggregate_UniqueUPCs.csv", stringc(1) clear varnames(1)

* Remove first TWO characters
gen upc10 = substr(upc,3,length(upc)) // string upc var for merge - take off first two digits instead since nielsen does not have a check digit and pads with an additional leading 0 (https://helpdesk.labelinsight.com/hc/en-us/articles/360021181853-Conventional-Product-Data-GTIN-Formats)

duplicates list upc10 // check for duplicates
duplicates tag upc10, gen(dup) // tag duplicates
duplicates drop upc10, force // drop all but first occurrence of duplicate upc10

save "$temp/upc_list_nielsen.dta", replace

*import delimited "$main/Label Insight/LabelInsight_export_Dec2020.csv", clear varnames(1)



* Prepare Label Insight data for merge
***************************************

use "$dataI/LabelInsight_short_Dec2020.dta", clear

* Remove first and last characters
gen upc10 = substr(upc_12,2,length(upc_12)-2)

duplicates list upc10 // check for duplicates
duplicates tag upc10, gen(dup) // tag duplicates
sort upc10 ssb3_li
*browse if dup == 1
duplicates drop upc10, force // drop all but first occurrence of duplicate upc10

merge 1:1 upc10 using "$temp/upc_list_nielsen.dta", generate(merge_li)

/* 
	Result                      Number of obs
    -----------------------------------------
    Not matched                       158,462
        from master                   121,219  (merge_li==1)
        from using                     37,243  (merge_li==2)

    Matched                            15,341  (merge_li==3)
    -----------------------------------------

*/

save "$temp/nielsen_LI_merge.dta", replace


* Bev type if matched

tab bev_type_li if merge_li == 3

/*
                         bev_type_li |      Freq.     Percent        Cum.
-------------------------------------+-----------------------------------
                    100% fruit juice |        802        5.43        5.43
                       Bottled water |        739        5.00       10.43
                              Coffee |          5        0.03       10.47
                           Diet soda |        637        4.31       14.78
                        Energy drink |         34        0.23       15.01
                      Flavored water |      2,656       17.98       33.00
                         Fruit drink |      4,166       28.21       61.21
                                Milk |         46        0.31       61.52
                    Milk alternative |        355        2.40       63.92
                Nutrition supplement |        100        0.68       64.60
                               Other |        868        5.88       70.48
                                Soda |      3,565       24.14       94.62
                        Sports drink |        517        3.50       98.12
                                 Tea |         21        0.14       98.26
Unflavored Club Soda and Tonic Water |        257        1.74      100.00
-------------------------------------+-----------------------------------
                               Total |     14,768      100.00


*/

* Bev type if no match

tab bev_type_li if merge_li == 1

/* 
                         bev_type_li |      Freq.     Percent        Cum.
-------------------------------------+-----------------------------------
                    100% fruit juice |      1,033        2.10        2.10
                       Bottled water |      1,026        2.09        4.19
                              Coffee |     11,170       22.72       26.91
                           Diet soda |        601        1.22       28.13
                        Energy drink |        337        0.69       28.81
                      Flavored water |      2,559        5.20       34.02
                         Fruit drink |      3,777        7.68       41.70
                                Milk |      3,705        7.54       49.24
                    Milk alternative |        771        1.57       50.80
                Nutrition supplement |     10,942       22.25       73.06
                               Other |      2,829        5.75       78.81
                                Soda |      3,095        6.29       85.11
                        Sports drink |        110        0.22       85.33
                                 Tea |      6,906       14.05       99.38
Unflavored Club Soda and Tonic Water |        307        0.62      100.00
-------------------------------------+-----------------------------------
                               Total |     49,168      100.00

*/

* SSB status if match

tab ssb3_li if merge_li == 3

/*
    ssb3_li |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     10,892       71.00       71.00
          1 |      4,449       29.00      100.00
------------+-----------------------------------
      Total |     15,341      100.00

*/

* SSB status if no match

tab ssb3_li if merge_li == 1

/*
    ssb3_li |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     97,945       80.80       80.80
          1 |     23,274       19.20      100.00
------------+-----------------------------------
      Total |    121,219      100.00

*/




* Merge with hand-coded nutrition data
***************************************

use "$dataI/nutrition_by_upc.dta", clear

* Create 10-digit UPC
gen prod2=substr(upc_id,-5,.)
gen vendor2=substr(upc_id,-11,5)
* combine to 10-digit upc
gen upc10=vendor2+prod2

duplicates list upc10 // check for duplicates
duplicates tag upc10, gen(dup) // tag duplicates
sort upc10 ssb 
sort upc10 ssb
duplicates drop upc10, force // drop all but first occurrence of duplicate upc10

merge 1:1 upc10 using "$temp/nielsen_LI_merge.dta", generate(merge_handcoded)

/*
	Result                      Number of obs
    -----------------------------------------
    Not matched                       164,400
        from master                     3,732  (merge_handcoded==1)
        from using                    160,668  (merge_handcoded==2)

    Matched                            13,135  (merge_handcoded==3)
    -----------------------------------------


*/

tab merge_handcoded merge_li, mi

/*
Matching result from |         Matching result from merge
                merge | Master on  Using onl  Matched (          . |     Total
----------------------+--------------------------------------------+----------
      Master only (1) |         0          0          0      3,732 |     3,732 
       Using only (2) |   117,091     34,113      9,464          0 |   160,668 
          Matched (3) |     4,128      3,130      5,877          0 |    13,135 
----------------------+--------------------------------------------+----------
                Total |   121,219     37,243     15,341      3,732 |   177,535 


*/

* 3130 UPCs matched on handcoded data but not LI data
* 4128 UPCs matched between handcoded data and LI data, but NOT nielsen data

* Total UPCs matched on: 22599
di 13135 - 4128 + 15341 - 5877


* Bev type if matched

tab bev_type if merge_handcoded == 3

/*

        Product category - handcoded |      Freq.     Percent        Cum.
-------------------------------------+-----------------------------------
                    100% fruit juice |        979       10.31       10.31
                       Bottled water |        493        5.19       15.50
                              Coffee |        306        3.22       18.73
                        Energy drink |        555        5.85       24.57
                      Flavored Water |          2        0.02       24.59
                      Flavored water |        795        8.37       32.97
                         Fruit drink |      1,865       19.64       52.61
                                Milk |        263        2.77       55.38
                    Milk alternative |        158        1.66       57.05
                Nutrition supplement |         29        0.31       57.35
                               Other |        195        2.05       59.41
                                Soda |      2,233       23.52       82.93
                        Sports drink |        541        5.70       88.62
                                 Tea |        829        8.73       97.36
Unflavored Club Soda and Tonic Water |        182        1.92       99.27
                    `00% fruit juice |          2        0.02       99.29
                         fruit drink |         26        0.27       99.57
                    milk alternative |          2        0.02       99.59
                               other |          1        0.01       99.60
                                soda |         38        0.40      100.00
-------------------------------------+-----------------------------------
                               Total |      9,494      100.00



*/

* Bev type if no match

tab bev_type if merge_handcoded == 1

/* 

        Product category - handcoded |      Freq.     Percent        Cum.
-------------------------------------+-----------------------------------
                    100% fruit juice |         28        2.00        2.00
                       Bottled water |        207       14.79       16.79
                              Coffee |        109        7.79       24.57
                        Energy drink |        175       12.50       37.07
                      Flavored water |         31        2.21       39.29
                         Fruit drink |         66        4.71       44.00
                                Milk |         26        1.86       45.86
                    Milk alternative |         27        1.93       47.79
                Nutrition supplement |          7        0.50       48.29
                               Other |         35        2.50       50.79
                                Soda |        290       20.71       71.50
                        Sports drink |         19        1.36       72.86
                                 Tea |        367       26.21       99.07
Unflavored Club Soda and Tonic Water |         13        0.93      100.00
-------------------------------------+-----------------------------------
                               Total |      1,400      100.00


*/

* SSB status if match

tab ssb if merge_handcoded == 3

/*

Sugar-sweet |
       ened |
   beverage |      Freq.     Percent        Cum.
------------+-----------------------------------
         No |      4,797       50.04       50.04
        Yes |      4,789       49.96      100.00
------------+-----------------------------------
      Total |      9,586      100.00


*/

* SSB status if no match

tab ssb if merge_handcoded == 1

/*

Sugar-sweet |
       ened |
   beverage |      Freq.     Percent        Cum.
------------+-----------------------------------
         No |      1,005       56.81       56.81
        Yes |        764       43.19      100.00
------------+-----------------------------------
      Total |      1,769      100.00


*/


* Format nutrition data
************************

* Update variables with LI values
foreach var of varlist added_sugar cals_per_serving size serving_size tot_sugar {
	replace `var' = `var'_li if `var'_li != .
	}

* Create new bev type
clonevar bev_type_new = bev_type
replace	bev_type_new = bev_type_li if bev_type_li ~= ""
replace bev_type_new = "Club soda/tonic water" if bev_type_new == "Unflavored Club Soda and Tonic Water"

tab brand if bev_type_new == ""

* Impute bev type based on brand and subcategory
replace bev_type_new = "Soda" if bev_type_new == "" & subcat == "REGULAR SOFT DRINKS"

replace bev_type_new = "Energy drink" if bev_type_new == "" & subcat == "SS ENERGY DRINKS NON-ASEPTIC"
replace bev_type_new = "Energy drink" if bev_type_new == "" & subcat == "SS ENERGY SHOT"
replace bev_type_new = "Energy drink" if bev_type_new == "" & subcat == "SS ENERGY DRINK MIX"
replace bev_type_new = "Energy drink" if bev_type_new == "" & subcat == "OCEAN SPRAY CRAN ENERGY"

replace bev_type_new = "Sports drink" if bev_type_new == "" & subcat == "SS SPORTS DRINK MIX"
replace bev_type_new = "Sports drink" if bev_type_new == "" & subcat == "SS SPORTS DRINKS NON-ASEPTIC"

replace bev_type_new = "Fruit drink" if bev_type_new == "" & subcat == "SS BOTTLED FRUIT DRINKS"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & subcat == "RFG FRUIT DRINK"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & brand == "OCEAN SPRAY DIET"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & brand == "NAKED PROTEIN ZONE"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & brand == "ODWALLA"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & brand == "NAKED WELL BEING"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & brand == "SIMPLY LEMONADE"
replace bev_type_new = "Fruit drink" if bev_type_new == "" & brand == "SIMPLY LIMEADE"

replace bev_type_new = "100% fruit juice" if bev_type_new == "" & subcat == "RFG ORANGE JUICE"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "TROPICANA PURE PREMIUM"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "NAKED"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "NAKED PRESSED"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "ODWALLA C MONSTER"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "ODWALLA SUPERFOOD"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "POM WONDERFUL"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "PRESSED JUICERY"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "SIMPLY ORANGE"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "SIMPLY GRAPEFRUIT"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "SUJA ESSENTIALS"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "KEDEM"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "KEDEM FRESH"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "KEDEM ORGANIC"
replace bev_type_new = "100% fruit juice" if bev_type_new == "" & brand == "IZZE"

replace bev_type_new = "Coffee" if bev_type_new == "" & subcat == "RFG READY-TO-DRINK COFFEE"
replace bev_type_new = "Coffee" if bev_type_new == "" & subcat == "CAPPUCINO/ICED COFFEE"

replace bev_type_new = "Tea" if bev_type_new == "" & subcat == "RFG TEAS"
replace bev_type_new = "Tea" if bev_type_new == "" & subcat == "CANNED AND BOTTLED TEA"

replace bev_type_new = "Milk" if bev_type_new == "" & subcat == "RFG WHOLE MILK"
replace bev_type_new = "Milk" if bev_type_new == "" & subcat == "RFG SKIM/LOWFAT MILK"

replace bev_type_new = "Milk alternative" if bev_type_new == "" & subcat == "RFG ALMOND MILK"
replace bev_type_new = "Milk alternative" if bev_type_new == "" & subcat == "RFG SOY MILK"

replace bev_type_new = "Bottled water" if bev_type_new == "" & subcat == "CONVENIENCE/PET STILL WATER"
replace bev_type_new = "Bottled water" if bev_type_new == "" & subcat == "JUG/BULK STILL WATER"

replace bev_type_new = "Club soda/tonic water" if bev_type_new == "" & subcat == "TONIC WATER/CLUB SODA"
replace bev_type_new = "Club soda/tonic water" if bev_type_new == "" & subcat == "TONIC WATER/CLUB SODA"
replace bev_type_new = "Club soda/tonic water" if bev_type_new == "" & brand == "PERRIER"
replace bev_type_new = "Club soda/tonic water" if bev_type_new == "" & brand == "ADIRONDACK CLEAR N NATURAL"

replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "LA CROIX"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "LA CROIX CURATE"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "ORIGINAL NEW YORK SELTZER"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "SPARKLING ICE"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "SPARKLING ICE LEMONADE"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "ARROWHEAD"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "AYALAS HERBAL WATER"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "SO CLEAR"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "YERBAE"
replace bev_type_new = "Flavored water" if bev_type_new == "" & brand == "ZEVIA"

replace bev_type_new = "Diet soda" if subcat == "LOW CALORIE SOFT DRINKS"

* Impute SSB status based on brand and subcategory
replace ssb = 1 if ssb == . & brand == "TAMPICO"
replace ssb = 1 if ssb == . & brand == "SIMPLY LEMONADE"
replace ssb = 1 if ssb == . & brand == "SIMPLY LIMEADE"

replace ssb = 1 if ssb == . & subcat == "REGULAR SOFT DRINKS"

replace ssb = 0 if ssb == . & brand == "TROPICANA PURE PREMIUM"
replace ssb = 0 if ssb == . & brand == "LA CROIX"
replace ssb = 0 if ssb == . & brand == "LA CROIX CURATE"
replace ssb = 0 if ssb == . & brand == "SPARKLING ICE"
replace ssb = 0 if ssb == . & brand == "SPARKLING ICE LEMONADE"
replace ssb = 0 if ssb == . & brand == "OCEAN SPRAY CRAN ENERGY"
replace ssb = 0 if ssb == . & brand == "OCEAN SPRAY DIET"
replace ssb = 0 if ssb == . & brand == "ODWALLA"
replace ssb = 0 if ssb == . & brand == "ODWALLA C MONSTER"
replace ssb = 0 if ssb == . & brand == "ODWALLA SUPERFOOD"
replace ssb = 0 if ssb == . & brand == "POM WONDERFUL"
replace ssb = 0 if ssb == . & brand == "PRESSED JUICERY"
replace ssb = 0 if ssb == . & brand == "SIMPLY ORANGE"
replace ssb = 0 if ssb == . & brand == "SIMPLY GRAPEFRUIT"
replace ssb = 0 if ssb == . & brand == "SUJA ESSENTIALS"
replace ssb = 0 if ssb == . & brand == "KEDEM"
replace ssb = 0 if ssb == . & brand == "KEDEM FRESH"
replace ssb = 0 if ssb == . & brand == "KEDEM ORGANIC"
replace ssb = 0 if ssb == . & brand == "IZZE"
replace ssb = 0 if ssb == . & brand == "ARROWHEAD"
replace ssb = 0 if ssb == . & brand == "ADIRONDACK CLEAR N NATURAL"
replace ssb = 0 if ssb == . & brand == "AYALAS HERBAL WATER"
replace ssb = 0 if ssb == . & brand == "SO CLEAR"
replace ssb = 0 if ssb == . & brand == "YERBAE"

replace ssb = 0 if ssb == . & subcat == "RFG ORANGE JUICE"


replace ssb = 0 if 	bev_type_new == "Milk" | ///
					bev_type_new == "Milk alternative" | /// 
					bev_type_new == "Nutrition supplement" | ///
					bev_type_new == "Bottled water" | ///
					bev_type_new == "Club soda/tonic water" | ///
					bev_type_new == "100% fruit juice" | ///
					bev_type_new == "Diet soda"

* Create new SSB variables
foreach i of numlist 1 2 3 {
	gen 	ssb`i' = ssb`i'_li
	replace ssb`i' = ssb if ssb`i' == .
	}

* Fill in calories per serving based on bev type
replace cals_per_serving = 0 if subcat == "JUG/BULK STILL WATER" | subcat == "BOTTLED WATER" | bev_type_new == "Bottled water"

* Convert size into ounces
gen 	sizelab = substr(upc_nm, -2, .)
tab		sizelab
replace size = size*16 if size_li == . & sizelab == "PT"


// Save file
	save	"$dataN/nielsen_upcs_with_nutrition_SK.dta", replace

// Save short file

	* Restrict to subset of variables 
	keep 	upc10 cals_per_serving tot_sugar added_sugar serving_size bev_type_new ssb3 size merge_li merge_handcoded

	rename bev_type_new bev_type
	rename ssb3 ssb

	* Save
	save	"$dataN/nielsen_upcs_with_nutrition_short_SK.dta", replace


	