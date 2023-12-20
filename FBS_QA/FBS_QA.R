library(tidyverse)
library(haven)
library(ggplot2)
library(data.table)
datayear = 2023

fbs_type_numbers <- c(1:9)
fbs_type_words <- c("Cereals","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed","All farm types")
FBS_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)

#Input data folders
FBS_directory_path<- '//s0177a/sasdata1/ags/fas/'
agstemp_path <- '//s0177a/sasdata1/ags/census/agstemp/'

#Fucntions
read_sas2 <- function(directory, filename){
  dataset <- tryCatch(
    {
      dataset <- read_sas(filename)
    },
    error = function(e)
    {
      file.copy(paste0(directory, filename), getwd())
      return(read_sas(filename))
    }
  )
}
# Function which calls above function, then does minor cleaning
import_sas <-function(directory_path, filename) {
  dataset <- read_sas2(directory_path, filename)
  names(dataset) <- tolower(names(dataset))
  for (var in colnames(dataset)) {
    attr(dataset[[deparse(as.name(var))]],"format.sas")=NULL
  }
  return(dataset)
}



#Read in farm_account data
FBS_fa_data_file <- paste0("so_y", datayear,"_fa.sas7bdat")
FBS_fa_data <- import_sas(FBS_directory_path, FBS_fa_data_file)
#Basic cleaning of fa data - convert all column names to lower case and strip sas formatting

FBS_fa_data_process <- FBS_fa_data %>% 
  mutate(sampyear = fa_id %% 10000) %>% 
  mutate(f_number = floor(fa_id/10000) )

select_variables <- c("fa_id", "sampyear","f_number", "fa_fbi", "fa_input", "fa_outpt", "fa_gsub")

FBS_fa_current_year <- FBS_fa_data_process %>% 
  filter(sampyear==datayear) %>% 
  select(all_of(select_variables), type)

FBS_fa_prev_year <- FBS_fa_data_process %>% 
  filter(sampyear==datayear-1)%>% 
  select(all_of(select_variables))

FBS_fa_match <- FBS_fa_current_year %>% 
  inner_join(FBS_fa_prev_year, by="f_number") %>% 
  select(f_number)

FBS_fa_match_sample <- FBS_fa_data_process %>% 
  filter(f_number %in% FBS_fa_match$f_number) %>% 
  select(all_of(select_variables), type)
FBS_types_summary <- FBS_fa_match_sample %>% 
  group_by(sampyear, type) %>% 
  summarise(across(fa_fbi:fa_outpt, ~mean(.x))) %>% 
  gather(fa_fbi:fa_outpt, key=measure, value=value) %>% 
  spread(key=sampyear, value=value) %>% 
  mutate(ratio=`2023`/`2022`,
         pc_change = 100*(`2023`-`2022`)/`2022`)

ggplot(data = FBS_fa_match_sample) +
  geom_point(mapping = aes(x=fa_input, y=fa_outpt, colour=as.factor(type)))


FBS_fa_match_wide <- FBS_fa_match %>% 
  left_join(FBS_fa_current_year, by="f_number") %>% 
  left_join(FBS_fa_prev_year, by="f_number") %>% 
  mutate(FBI_diff = fa_fbi.x - fa_fbi.y,
         FBI_ratio = fa_fbi.x/fa_fbi.y,
         Input_diff = fa_input.x - fa_input.y,
         Input_ratio = fa_input.x/fa_input.y,
         Output_diff = fa_outpt.x - fa_outpt.y,
         Output_ratio = fa_outpt.x/fa_outpt.y,
         Subs_diff = fa_gsub.x - fa_gsub.y,
         Subs_ratio = fa_gsub.x/fa_gsub.y)

all_row <- FBS_fa_match_wide %>% 
  select(f_number, type, 
         "Mean input 2021/22"=fa_input.y, "Mean input 2022/23"=fa_input.x, 
         "Mean output 2021/22"=fa_outpt.y,  "Mean output 2022/23"=fa_outpt.x, 
         "Mean subsidies 2021/22"=fa_gsub.y, "Mean subsidies 2022/23" = fa_gsub.x,
         "Mean FBI 2021/22"=fa_fbi.y,  "Mean FBI 2022/23"= fa_fbi.x) %>% 
  summarise(across(`Mean input 2021/22`:`Mean FBI 2022/23`, ~ mean(.x)),
            count=n()) %>% 
  mutate(type=9)

FBS_match_summary <- FBS_fa_match_wide %>% 
  select(f_number, type, 
         "Mean input 2021/22"=fa_input.y, "Mean input 2022/23"=fa_input.x, 
         "Mean output 2021/22"=fa_outpt.y,  "Mean output 2022/23"=fa_outpt.x, 
         "Mean subsidies 2021/22"=fa_gsub.y, "Mean subsidies 2022/23" = fa_gsub.x,
         "Mean FBI 2021/22"=fa_fbi.y,  "Mean FBI 2022/23"= fa_fbi.x) %>% 
  group_by(type) %>% 
  summarise(across(`Mean input 2021/22`:`Mean FBI 2022/23`, ~ mean(.x)),
            count=n()) %>% 
  bind_rows(all_row) %>% 
  mutate("Input change (%)" = 100*(`Mean input 2022/23`-`Mean input 2021/22`)/`Mean input 2021/22`,
         "Output change (%)" = 100*(`Mean output 2022/23`-`Mean output 2021/22`)/`Mean output 2021/22`,
         "Subsidies change (%)" = 100*(`Mean subsidies 2022/23`-`Mean subsidies 2021/22`)/`Mean subsidies 2021/22`,
         "FBI change (%)" = 100*(`Mean FBI 2022/23`-`Mean FBI 2021/22`)/`Mean FBI 2021/22`
)
setkey(setDT(FBS_match_summary),type)
FBS_match_summary[setDT(FBS_type_tab),farmtype:=i.fbs_type_words]



FBS_match_summary <- FBS_match_summary %>% 
  select(farmtype, count, everything()) %>% 
  select(-type)
write.csv(FBS_match_summary, file = "FBS_match_summary.csv")

ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=Output_diff, y=FBI_diff, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=Input_diff, y=Output_diff, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=Input_diff, y=FBI_diff, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=fa_outpt.y, y=Output_ratio, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=fa_input.y, y=Input_ratio, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=fa_fbi.y, y=FBI_ratio, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=fa_input.x, y=Input_ratio, colour=as.factor(type)))
ggplot(data = FBS_fa_match_wide) +
  geom_point(mapping = aes(x=Subs_ratio, y= FBI_diff, colour=as.factor(type)))







#Normalised input/outputs
#Code to calculate values from FBS data such as sheep output/ewe, tattie output/area of tattie, feed input/grazing livestock unit.
#Considers only the farmtypes specialising in that enterprise. So only dairy for milk etc.


#Read in livestock account
FBS_al_data_file <- paste0("SO_Y", datayear, "_AL.sas7bdat")
FBS_al_data <- import_sas(FBS_directory_path, FBS_al_data_file)
#Read in livestock group account
FBS_alg_data_file <- paste0("SO_Y", datayear, "_ALG.sas7bdat")
FBS_alg_data <- import_sas(FBS_directory_path, FBS_alg_data_file)
#Read in crop group account
FBS_acg_data_file <- paste0("SO_Y", datayear, "_ACG.sas7bdat")
FBS_acg_data <- import_sas(FBS_directory_path, FBS_acg_data_file)

#Process and merge datasets
Livestock_numbers <- FBS_al_data %>% 
  spread(key=l_code, value=al_numb) %>% 
  group_by(fa_id) %>% 
  summarise(number_ewes = sum(LSVE, na.rm=T)+sum(LSVU, na.rm=T),
            number_suckler_cattle = sum(LCVR, na.rm=T),
            number_dairy_cattle = sum(LCVD, na.rm=T))
Total_cattle <- FBS_alg_data %>% 
  spread(key=lg_code, value=algnumb) %>% 
  group_by(fa_id) %>% 
  summarise(number_cattle = sum(LGCA, na.rm=T))

Livestock_outputs <- FBS_alg_data %>% 
  spread(key=lg_code, value = algout) %>% 
  group_by(fa_id) %>% 
  summarise(sheep_output = sum(LGSH, na.rm=T),
            cattle_output = sum(LGCA, na.rm=T))

FBS_fa_data_process2 <- FBS_fa_data %>% 
  select(fa_id, type, milk_output=fa_miout, fa_ifeed, fa_iseed, GLU=fa_glu)

Crop_areas <- FBS_acg_data %>% 
  spread(key=cg_code, value = acgarea) %>% 
  group_by(fa_id) %>% 
  summarise(cereal_area = sum(CGCA, na.rm=T),
            tattie_area = sum(CGPA, na.rm=T))
Crop_outputs <- FBS_acg_data %>% 
  spread(key=cg_code, value = acgout) %>% 
  group_by(fa_id) %>% 
  summarise(cereal_output = sum(CGCV, na.rm=T),
            tattie_output = sum(CGPV, na.rm=T))

Merged_dataset <- Livestock_numbers %>% 
  left_join(Total_cattle, by="fa_id") %>% 
  left_join(Livestock_outputs, by="fa_id") %>% 
  left_join(FBS_fa_data_process2, by="fa_id") %>% 
  left_join(Crop_areas, by="fa_id") %>% 
  left_join(Crop_outputs, by="fa_id") %>% 
  select(fa_id, type, everything()) %>% 
  mutate(crop_area = cereal_area + tattie_area)

#Function to calculate normalised values
Normalised_value <- function(types, name, numerator, denominator){
  varname <- name
  Normalised_sheep_output <- Merged_dataset %>% 
    # filter(floor(fa_id/10000)==12843) %>% 
    mutate(year = fa_id%%10000) %>% 
    group_by(year) %>% 
    filter(type %in% types) %>% 
    summarise(!!name := sum({{numerator}})/sum({{denominator}}))
}
norm_sheep <- Normalised_value(c(4:8), "sheep output per ewe", sheep_output, number_ewes)
norm_cattle <- Normalised_value(c(4:8), "cattle output per cattle", cattle_output, number_cattle)
norm_cereal <- Normalised_value(c(1:2,8), "cereal output per hectare", cereal_output, cereal_area)
norm_tattie <- Normalised_value(c(2), "tattie output per hectare", tattie_output, tattie_area)
norm_milk <- Normalised_value(3, "milk output per dairy cow", milk_output, number_dairy_cattle)
norm_feed <- Normalised_value(c(4:8), "Feed input per GLU (non-dairy)", fa_ifeed, GLU)
norm_feed_dairy <- Normalised_value(3, "feed input per GLU (dairy)", fa_ifeed, GLU)
norm_seed <- Normalised_value(c(1,2,8), "Seed input per crop hectare", fa_iseed, crop_area)

Merged_summary <- norm_sheep %>% 
  full_join(norm_cattle, by=c("year")) %>% 
  full_join(norm_milk, by=c("year"))%>% 
  full_join(norm_cereal, by=c("year"))%>% 
  full_join(norm_tattie, by=c("year"))%>% 
  full_join(norm_feed, by=c("year"))%>% 
  full_join(norm_feed_dairy, by=c("year"))%>% 
  full_join(norm_seed, by=c("year"))
Merged_summary_t <- data.frame(t(Merged_summary))
colnames(Merged_summary_t) <- c("2022", "2023")
Merged_summary_t <- Merged_summary_t %>% 
  mutate("Percentage change"= 100*(`2023`-`2022`)/`2022`)

write.csv(Merged_summary_t, file = "Normalised inputs and outputs - summary.csv")