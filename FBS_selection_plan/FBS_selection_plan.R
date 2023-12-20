##FBS selection plan code
#The idea is to replicate Table 4 of the old FADN Selection Plan process (eRDM reference A31170539), using R.
#Where possible, I'll simplify the process, but will likely keep the same Table names, so some tables may appear to be missing below.

#Packages
library(tidyverse)
library(haven)
#Manual variables
datayear=2022
census_year=datayear-1
FBS_directory_path <- '//s0177a/sasdata1/ags/fas/'
FBS_data_file <- paste0("so_y", datayear, "_fa",".sas7bdat")
weights_file <- "new_weights.sas7bdat"
census_directory_path <- '//s0177a/sasdata1/ags/census/agscens/'
census_data_file <- paste0("june", census_year, ".sas7bdat")
size_band_A <- 25000
size_band_B <- 100000
size_band_C <- 500000
exchange_rate <- 0.8526
FADN_sample <- 380

##Read in FBS data
#First, try reading it in from the project folder, and if not found, copy it from the SAS drive and then read it in.
FBS_data <- tryCatch(
  {
    FBS_data <- read_sas(FBS_data_file)
  },
  error = function(e)
  {
    file.copy(paste0(FBS_directory_path,FBS_data_file), getwd())
    return(read_sas(FBS_data_file))
  }
)
names(FBS_data) <- tolower(names(FBS_data))
for (x in colnames(FBS_data)){
  attr(FBS_data[[deparse(as.name(x))]],"format.sas")=NULL
}

##Read in JAC data
census_data <- tryCatch(
  {
    census_data <- read_sas(census_data_file)
  },
  error = function(e)
  {
    file.copy(paste0(census_directory_path, census_data_file), getwd())
    return(read_sas(census_data_file))
  }
)
names(census_data) <- tolower(names(census_data))
for (x in colnames(census_data)){
  attr(census_data[[deparse(as.name(x))]],"format.sas")=NULL
}

#Import weights file
weights <- tryCatch(
  {
    weights <- read_sas(weights_file)
  },
  error = function(e)
  {
    file.copy(paste0(FBS_directory_path, weights_file), getwd())
    return(read_sas(weights_file))
  }
)
names(weights) <- tolower(names(weights))
for (x in colnames(weights)){
  attr(weights[[deparse(as.name(x))]],"format.sas")=NULL
}


##Process FBS data
#Default all farms to the smallest sizeband group (6,7) and first farmtype group (15,16).
#Other groupings will be changed below
FBS_sample <- FBS_data %>% 
  filter(fa_id%%10000==datayear, sumsoecu >= size_band_A) %>%  
  mutate(typmed = typmed%%100) %>%
  mutate(size_band = as.factor("6,7")) %>%
  mutate(FADN_group = as.factor("15,16")) %>% 
  select(fa_id, typmed, sumsoecu, size_band, FADN_group)

##Use FADN's size groupings (6 and 7 merged, 8 and 9 merged, 10+ merged)
levels(FBS_sample$size_band) <- c("6,7", "8,9", "10+")
##USe FADN's farmtype groupings:
#15, 16 - cereals and general cropping
#45, 47 - Dairy and LFA Cattle
#46 - Non-LFA Cattle
#48 - LFA Sheep and non-LFA Sheep
#73,74,83,84 - Mixed and miscellaneous
levels(FBS_sample$FADN_group) <- c("15,16", "45,47", "46", "48", "73,74,83,84")

#Change sizeband and farmtype groups
FBS_sample$size_band[FBS_sample$sumsoecu >= size_band_B] = "8,9"
FBS_sample$size_band[FBS_sample$sumsoecu >= size_band_C] = "10+"

FBS_sample$FADN_group[FBS_sample$typmed %in% c(45,47)] = "45,47"
FBS_sample$FADN_group[FBS_sample$typmed == 46] = "46"
FBS_sample$FADN_group[FBS_sample$typmed == 48] = "48"
FBS_sample$FADN_group[FBS_sample$typmed >= 73] = "73,74,83,84"

#Count number of farms in each sizeband/farmtype grouping in the FBS sample
FBS_summary <- FBS_sample %>% 
  group_by(FADN_group, size_band) %>% 
  summarise(FBS_count=n())

##Process census data
#Filter to only farms with standard output size group 6 or greater
#Combine the LFA and non-LFA categories (these are distinguished in the census dataset with a 77 or 99 at the start of the typmed. So dividing by 100 and 
#taking the modulus is sufficient to combine)
census_process <- census_data %>% 
  filter(sumso >= size_band_A*exchange_rate) %>% 
  mutate(typmed = typmed%%100)
#Filter to appropriate farmtypes, convert standard output to euros
##Use FADN sizeband and farmtype groupings, as was done for FBS dataset
census_process <- census_process %>% 
  filter(typmed %in% c(15,16,45:48,73,74,83,84)) %>% 
  mutate(sumsoecu=sumso/exchange_rate) %>% 
  mutate(size_band=as.factor("6,7")) %>% 
  mutate(FADN_group=as.factor("15,16")) %>% 
  select(typmed, sumsoecu, size_band, FADN_group)
levels(census_process$size_band) <- levels(FBS_sample$size_band)
levels(census_process$FADN_group) <- levels(FBS_sample$FADN_group)
census_process$size_band[census_process$sumsoecu >= size_band_B] = "8,9"
census_process$size_band[census_process$sumsoecu >= size_band_C] = "10+"

census_process$FADN_group[census_process$typmed %in% c(15,16)] = "15,16"
census_process$FADN_group[census_process$typmed %in% c(45,47)] = "45,47"
census_process$FADN_group[census_process$typmed == 46] = "46"
census_process$FADN_group[census_process$typmed == 48] = "48"
census_process$FADN_group[census_process$typmed %in% c(73,74,83,84)] = "73,74,83,84"

census_process <- census_process %>% 
  group_by(FADN_group, size_band) %>% 
  summarise(census_count=n())

FBS_census <- census_process %>% 
  left_join(FBS_summary, by=c("FADN_group","size_band"))

FBS_census[is.na(FBS_census)]=0



FBS_census <- mutate(FBS_census, proportional_allocation = FADN_sample*census_count/sum(FBS_census$census_count))
FBS_census <- mutate(FBS_census, difference = FBS_count-proportional_allocation)
FBS_census <- mutate(FBS_census, remove_minus = ifelse(difference <1, FBS_count, proportional_allocation))
FBS_census <- mutate(FBS_census, number_remaining = ifelse(difference <1, 0, census_count))
FBS_census <- mutate(FBS_census, proportional_allocation_remaining = ifelse(difference <1, 0, census_count/sum(FBS_census$number_remaining)))
FBS_census <- mutate(FBS_census, secondary_proportional = ifelse(difference <1, 0, round(proportional_allocation_remaining*(FADN_sample-sum(FBS_census$remove_minus)),0)))
FBS_census <- mutate(FBS_census, check_shortfall = ifelse(remove_minus+secondary_proportional <= FBS_count, 0, remove_minus+secondary_proportional-FBS_count))
FBS_census <- mutate(FBS_census, number_remaining_II = ifelse(secondary_proportional == 0 | check_shortfall != 0, 0, number_remaining))
FBS_census <- mutate(FBS_census, tertiary_proportional = round(number_remaining_II*(round(sum(FBS_census$check_shortfall))/sum(FBS_census$number_remaining_II))))
FBS_census <- mutate(FBS_census, result_secondary = ifelse(check_shortfall==0, round(remove_minus+secondary_proportional), FBS_count))
FBS_census <- mutate(FBS_census, final_result = result_secondary + tertiary_proportional)
FBS_census <- mutate(FBS_census, FADN_weight = ifelse(FBS_count==0, 0, census_count/final_result))
FBS_census <- select(FBS_census, FADN_group, size_band, census_count, FBS_count, final_result, FADN_weight)

##At this point we have replicated the automated sections of Table 4 of the spreadsheet.
#However, there is also a manual portion on the end, to account for occasions where the "final result" allocates more farms than there are in the FBS sample.
#The first step is to make sure final_result is <= FBS_count. 
FBS_census <- mutate(FBS_census, final_result = ifelse(final_result > FBS_count, FBS_count, final_result))
##Recalculate FADN_weight
FBS_census <- mutate(FBS_census, FADN_weight = ifelse(FBS_count==0, 0, census_count/final_result))

#Repeat until sum(final_result) = FADN_sample
while(sum(FBS_census$final_result) < FADN_sample){
  #Then work out which strata have spare farms
  FBS_census <- mutate(FBS_census, spare = ifelse(FBS_count - final_result > 0 , 1, 0))
  #Then figure out which of the strata with spare farms has the highest FADN weight, and add one to its final_result
  FBS_census <- FBS_census[order(-FBS_census$spare, -FBS_census$FADN_weight),]
  FBS_census$final_result[1] <- FBS_census$final_result[1] + 1
  ##Recalculate FADN_weight
  FBS_census <- mutate(FBS_census, FADN_weight = ifelse(FBS_count==0, 0, census_count/final_result))
}

##Sort final table
FBS_census <- FBS_census %>% 
  select(FADN_group, size_band, census_count, FBS_count, final_result, FADN_weight) %>% 
  arrange(FADN_group, size_band)

##Create actual 'selection' from FBS sample
##Do this based on R-based weightings for strata where FBS_count > final_result
##Is this a feasible option? I.e, would this process happen before or after R weights are produced?

FADN_selection <- FBS_sample %>% 
  left_join(weights, by="fa_id") %>% 
  arrange(desc(fbswt))
FADN_selection_list <- NULL
for (i in 1:nrow(FBS_census)){
  temp <- FADN_selection %>% 
    filter(FADN_group==FBS_census$FADN_group[[i]], size_band==FBS_census$size_band[[i]]) %>% 
    slice(1:FBS_census$final_result[[i]]) %>%
    select(fa_id)
  FADN_selection_list <- FADN_selection_list %>% 
    rbind(temp)
  }
FADN_selection <- FADN_selection %>% 
  right_join(FADN_selection_list, by="fa_id") %>% 
  left_join(select(FBS_census, FADN_group, size_band, FADN_weight), by=c("FADN_group", "size_band"))


##Create output tables

output_table_1 <- FBS_census %>% 
  mutate(census_percent = census_count * 100/sum(FBS_census$census_count),
         FBS_percent = FBS_count * 100/sum(FBS_census$FBS_count),
         selection_percent = final_result * 100/FBS_count)

output_table_2 <- FADN_selection

#output_table_3 will be a multi-year FADN weight comparison

ggplot(data = output_table_2, aes(x = FADN_weight, y = fbswt))+
  geom_point()

check <- output_table_2 %>% 
  group_by(FADN_weight) %>% 
  summarise(avg_fbswt = mean(fbswt))
