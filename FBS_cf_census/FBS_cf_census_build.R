##RScript which actually runs the main R script and then builds the reports.


#Choose the crop year of interest here
crop_year <- 2022
#Publication link - this should work automatically, but worth clicking it in the produced report to make sure it works.
publication_link <- paste0("https://www.gov.scot/publications/scottish-farm-business-income-annual-estimates-",crop_year,"-",crop_year+1,"/")

#These two variables automatically update based on crop_year. "year" is used for importing the census data.
#"datayear" is used for importing the FBS data.
year <- crop_year - 2000
datayear <- crop_year+1
#Variables for FBS thresholds. The exchange rate is from 2013, since that is when standard outputs were determined.
FBS_euro_threshold <- 25000
exchange_rate <- 0.8526
FBS_slr_threshold <- 0.5
title_var <- paste0("Farm Business Survey in relation to the June Agricultural Census - ", crop_year, "-", year+1)
output_file_title <- paste0("FBS_cf_census/Farm Business Survey ",crop_year,"-",year+1," - Data requests - FBS ",year,"-",year+1," in relation to ",crop_year," census")
source('FBS_cf_census/FBS cf census.R')
rmarkdown::render('FBS_cf_census/FBS_cf_census.Rmd', output_file = paste0(output_file_title,".html"))
rmarkdown::render('FBS_cf_census/FBS_cf_census_with_BRN.Rmd', output_file = paste0(output_file_title," with BRNs.html"))
