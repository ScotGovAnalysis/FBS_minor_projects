# sample characteristics

# settings ---

#Choose the crop year of interest here
crop_year <- 2022
#Publication link - this should work automatically, but worth clicking it in the produced report to make sure it works.
#publication_link <- paste0("https://www.gov.scot/publications/scottish-farm-business-income-annual-estimates-",crop_year,"-",crop_year+1,"/")

#These two variables automatically update based on crop_year. "year" is used for importing the census data.
#"datayear" is used for importing the FBS data.
year <- crop_year - 2000
datayear <- crop_year+1
#Variables for FBS thresholds. The exchange rate is from 2013, since that is when standard outputs were determined.
FBS_euro_threshold <- 25000
exchange_rate <- 0.8526
FBS_slr_threshold <- 0.5
title_var <- paste0("Farm Business Survey in relation to the June Agricultural Census - ", crop_year, "-", year+1)
output_file_title <- paste0("Farm Business Survey ",crop_year,"-",year+1," - Data requests - FBS ",year,"-",year+1," in relation to ",crop_year," census")

#import ----
source('FBS_cf_census/FBS cf census.R')

# import weights 
FBS_weights_file <- paste0("new_weights.sas7bdat")
FBS_weights <- tryCatch({
  FBS_weights <- read_sas(FBS_weights_file)
},
error = function(e)
{
  file.copy(paste0(FBS_directory_path, FBS_weights_file), getwd())
  return(read_sas(FBS_weights_file))
})
names(FBS_weights) <- tolower(names(FBS_weights))
for (x in colnames(FBS_weights)) {
  attr(FBS_weights[[deparse(as.name(x))]], "format.sas") = NULL
}



# merge with FBS data file
fbs_data_process <- FBS_data %>% filter(fa_id%%10000==datayear) %>% 
  left_join(., FBS_weights, by = "fa_id")


sum_wt = sum(fbs_data_process$fbswt)

# sample numbers
#census- weighted sample numbers
census_pop <-  census_fbs_threshold %>% 
  group_by(census_type) %>% 
  summarise(`Number of holdings: June Survey 2022` =n()) %>% 
  rename(type = census_type)

#clac type 14
#calc type 14
pop_all <- census_fbs_threshold %>% 
  summarise(type = 14,
            `Number of holdings: June Survey 2022` = n())

#bind

census_pop <- rbind(census_pop, pop_all)
  
# fbs- sample numbers
fbs_pop <- fbs_data_process %>% 
  group_by(type) %>% 
  summarise(`Number of businesses: FBS sample` = n())

#calc type 14
pop_all <- fbs_data_process %>% 
  summarise(type = 14,
    `Number of businesses: FBS sample` = n())

#bind
fbs_pop <- rbind(fbs_pop, pop_all)

#join
pop <- full_join( fbs_pop,census_pop, by = "type")


# area
#census fbs threshold farms 
census_area <- census_fbs_threshold%>% select(census_type,
                           total_area) %>% 
  group_by(census_type) %>% 
  summarise(area = round(mean(total_area), digits = 0)#,
            #sample_size = n()
            )

#calc type 14
area_all <- census_fbs_threshold%>% select(census_type,
                                   total_area) %>% 
  summarise(census_type = 14,
    area = round(mean(total_area), digits = 0))



census_area <- rbind(census_area, area_all) %>% 
  rename(`Average total area (hectares): June Survey 2022` = area,
         type = census_type
  ) 


#FBS sample area
fbs_sample_area <- fbs_data_process %>% select(type, fa_aua, fbswt) %>%  
  mutate(area = (fa_aua*fbswt)) %>% 
  group_by(type) %>% 
  summarise(tot_area = sum(area),
           tot_wt = sum(fbswt), # get total weight for each farm type 
           wt_area = round((tot_area/tot_wt), digits = 0)#, # calc average area by dividing weighted areas by sum of weights for each farm type 
            #sample_size = n
           ) # for QA purposes

#calc type 14
area_all <- fbs_data_process %>% select(type, fa_aua, fbswt)%>% 
  mutate(area = (fa_aua*fbswt)) %>% 
  summarise(type = 14,
    tot_area = sum(area),
            tot_wt = sum(fbswt), # get total weight for each farm type 
            wt_area = round((tot_area/tot_wt), digits = 0))

# bind with all types 
fbs_sample_area <- rbind(fbs_sample_area, area_all) %>% select (type, wt_area) %>% 
  rename (`Average total area (hectares): FBS sample` = wt_area)

#join
area <- full_join( fbs_sample_area, census_area, by = "type")



# slr
#census 
slr_census <- census_fbs_threshold %>% 
  group_by(census_type) %>%
  summarise(slr = round(mean(slr),digits=0))
  
 
#calc type14
slr_all <- census_fbs_threshold %>% 
  summarise(census_type = 14,
            slr = round(mean(slr),digits=0))

#bind to all farm types
slr_census <- rbind(slr_census, slr_all) %>% 
rename(`Average size of holding by Standard Labour Requirement: June Survey 2022` = slr,
 type = census_type)


# fbs sample slr 
slr_fbs<- fbs_data_process %>% select(type, fa_tslr, fbswt) %>%  
mutate(slr = ((fa_tslr/1900) *fbswt)) %>% 
  group_by(type) %>% 
  summarise(tot_slr = sum(slr),
            tot_wt = sum(fbswt), # get total weight for each farm type 
            wt_slr = round((tot_slr/tot_wt), digits = 0)#, # calc average slr by dividing weighted areas by sum of weights for each farm type 
            ) %>%  select(type, wt_slr) 

#calc type 14
slr_all <- fbs_data_process %>% select(type, fa_tslr, fbswt) %>%  
  mutate(slr = ((fa_tslr/1900) *fbswt)) %>% 
  summarise(tot_slr = sum(slr),
            tot_wt = sum(fbswt), # get total weight for each farm type 
            wt_slr = round((tot_slr/tot_wt), digits = 0),
            type = 14#, # calc average slr by dividing weighted areas by sum of weights for each farm type 
  ) %>% 
  select(type, wt_slr)

#bind to all farm types
slr_fbs <- rbind(slr_fbs, slr_all) %>% 
  rename(`Average size of business by Standard Labour Requirement: FBS sample` = wt_slr) 
  

#join
slr <- full_join(slr_fbs, slr_census, by = 'type')


# join all together
workbook <- pop %>% full_join(slr, by = "type") %>% 
  full_join(area, by = "type") %>% 
  rename (`Farm type` = type) 

#change all type to 14 and join to get farm type names
fbs_type_tab[9,1] <- 14
fbs_type_tab <- fbs_type_tab %>% rename(`Farm type` = `fbs_type_numbers`)

workbook <- workbook %>% left_join(fbs_type_tab, by = "Farm type")
workbook <- workbook %>% select(-`Farm type`) %>% select(fbs_type_words, everything()) %>% 
  rename(`Farm type` = fbs_type_words)
workbook <- as.data.frame(workbook)
#create spreadsheet
wb <- createWorkbook()
# #cover sheet
# cover_sheet_text <- read.csv(
#   "cover.csv",
#   header = FALSE,
#   sep = "\t", 
#   fileEncoding = "UTF-8-BOM")
# 
# #links
# cover_sheet_links <- data.frame(
#   rows = c(14, 15, 17, 18,20),
#   links = c("https://www.gov.scot/publications/scottish-farm-business-income-annual-estimates-methodology/pages/introduction/", 
#             "https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-june-2023-methodology-report/",
#             "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/",
#             "https://www.gov.scot/collections/june-scottish-agricultural-census/",
#             "mailto:agric.stats@gov.scot"))
# 
# 
# create_cover_sheet(
#   wb = wb,
#   text_df = cover_sheet_text,
#   subheadings = c(2,4,6, 8,10, 13,16),
#   hyperlinks = cover_sheet_links# rows to apply bold font
# )
#table
create_data_table_tab(
  wb = wb,
  df = workbook,
  tab_name = "FBS2022-23",
  column_width = rep(30, ncol(workbook)),
  #tableName = paste("table", data_year, sep = "_"),
  heading = "Farm Business Survey 2022-23: Sample Characteristics",
  no_decimal = c(1:7)
)

openXL(wb)
saveWorkbook(wb, "FBS_cf_census/FBS Sample Characteristics.xlsx",
             overwrite = T)

