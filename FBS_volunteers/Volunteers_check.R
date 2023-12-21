library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

#Use an environment variable to specify the FBS data path.
#See https://csgillespie.github.io/efficientR/set-up.html#renviron
#The path here is to the FAS folder on the SAS drive.
FBS_directory_path <- Sys.getenv("FBS_directory_path")
datayear=2023
sampyear=2023

FBS_data_file <- paste0("so_y", datayear, "_fa",".sas7bdat")
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
FBS_data_process <- FBS_data %>% 
  filter(ys_year==sampyear) %>% 
  select(fa_id, f_number, fa_outpt, fa_input, fa_fbi, type) #%>% 
#  mutate(volunteer=0)

# volunteers <- c(11806,
#                 11807,
#                 11809,
#                 11810,
#                 11812,
#                 11813,
#                 11814,
#                 11816,
#                 11818,
#                 12941,
#                 12942,
#                 12943,
#                 12944,
#                 12946,
#                 13837,
#                 13838,
#                 13839,
#                 13840,
#                 13841,
#                 13842,
#                 13843,
#                 13844,
#                 13845,
#                 13846,
#                 13847,
#                 13848,
#                 13849,
#                 13850,
#                 13851,
#                 13852,
#                 13853,
#                 13854,
#                 13856
# )

# FBS_data_process <- FBS_data_process %>% 
#   mutate(volunteer = case_when(f_number %in% volunteers ~ "Yes",
#                                !(f_number %in% volunteers) ~ "No"))

#Update code to reflect volunteer flag added to raw data
FBS_info_file <- paste0("so_y", datayear, "_fi",".sas7bdat")
FBS_info <- tryCatch(
  {
    FBS_data <- read_sas(FBS_info_file)
  },
  error = function(e)
  {
    file.copy(paste0(FBS_directory_path,FBS_info_file), getwd())
    return(read_sas(FBS_info_file))
  }
)
names(FBS_info) <- tolower(names(FBS_info))
for (x in colnames(FBS_info)){
  attr(FBS_info[[deparse(as.name(x))]],"format.sas")=NULL
}
FBS_info_process <- FBS_info %>% 
  select(fa_id, volunteer=fi_volunteer)

#Join datasets to add volunteer flag to FBS_data_process
FBS_data_process <- FBS_data_process %>% 
  left_join(FBS_info_process, by='fa_id')


FBI <- FBS_data_process %>% 
  group_by(type, volunteer) %>% 
  summarise(fbi=mean(fa_fbi),count=n())

All_farms <- FBS_data_process %>% 
  group_by(volunteer) %>% 
  summarise(fbi=mean(fa_fbi), count=n())
FBI_both <- FBI %>% 
  group_by(type) %>% 
  summarise(fbi = weighted.mean(fbi, count), count=sum(count)) %>% 
  mutate(volunteer="both")
col_order <- c("type", "volunteer", "fbi", "count")
FBI_both <- FBI_both[, col_order]

FBI_merge <- bind_rows(FBI, FBI_both)

FBI_all_both <- All_farms %>% 
  summarise(fbi = weighted.mean(fbi,count), count=sum(count)) %>% 
  mutate (volunteer="both")
All_merge <- bind_rows(All_farms, FBI_all_both)

for (i in 1:8){
  bin_number <- 2*sqrt(FBI_both$count[FBI_both$type==i])
  hist <- ggplot(subset(FBS_data_process, type==i), aes(fa_fbi, fill=volunteer)) +
    geom_histogram(bins=bin_number) +
    labs(title=i)
  scatter <- ggplot(subset(FBS_data_process, type==i), aes(x=fa_outpt, y=fa_fbi, colour=volunteer)) +
    geom_point() +
    labs(title=i)
  assign(paste("hist",i,sep = "_"), hist)
  assign(paste("scatter",i,sep = "_"), scatter)
}
bin_number = 2*sqrt(All_merge$count[All_merge$volunteer=="both"])
plot_all <- ggplot(FBS_data_process, aes(fa_fbi, fill=as.factor(volunteer))) +
  geom_histogram(bins=bin_number) +
  labs(title="All")
scatter_all <- ggplot(FBS_data_process, aes(x=fa_input, y=fa_outpt, colour=volunteer)) +
  geom_point() +
  labs(title="All")
hist_1
scatter_1
hist_2
scatter_2
hist_3
scatter_3
hist_4
scatter_4
hist_5
scatter_5
hist_6
scatter_6
hist_7
scatter_7
hist_8
scatter_8
plot_all
scatter_all

ggplot(FBS_data_process, aes(fa_fbi, fill=as.factor(type))) +
  geom_histogram(bins=bin_number) +
  labs(title="All")


##Further analysis on matched sample

weights_file <- paste0("new_weights.sas7bdat")
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

Year_two_sample <- FBS_data %>% 
  filter(fa_id %% 10000 == sampyear,
         fa_id != 137682022,
         fa_id != 137172022) %>% 
  select(f_number, fa_id, ys_year, fa_input, fa_outpt, fa_fbi, type) %>% 
  left_join(weights, by="fa_id")
Year_one_sample <- FBS_data %>% 
  filter(fa_id %% 10000 == sampyear-1)%>% 
  select(f_number, fa_id, ys_year, fa_input, fa_outpt, fa_fbi, type) %>% 
  left_join(weights, by="fa_id")

Matched_sample <- Year_one_sample %>% 
  left_join(Year_two_sample, by = "f_number") %>% 
  filter(type.x==type.y)
Year_one_matched <- Year_one_sample %>% 
  filter(f_number %in% Matched_sample$f_number)
Year_two_matched <- Year_two_sample %>% 
  filter(f_number %in% Matched_sample$f_number)

Year_one_fbi <- Year_one_sample %>% 
  group_by(type) %>% 
  summarise(FBI_year_one = weighted.mean(fa_fbi, fbswt))
Year_two_fbi <- Year_two_sample %>% 
  group_by(type) %>% 
  summarise(FBI_year_two = weighted.mean(fa_fbi, fbswt))

FBI <- Year_one_fbi %>% 
  left_join(Year_two_fbi, by="type")

Year_one_fbi_match <- Year_one_matched %>% 
  group_by(type) %>% 
  summarise(FBI_year_one = weighted.mean(fa_fbi, fbswt))
Year_two_fbi_match <- Year_two_matched %>% 
  group_by(type) %>% 
  summarise(FBI_year_two = weighted.mean(fa_fbi, fbswt))

FBI_match <- Year_one_fbi_match %>% 
  left_join(Year_two_fbi_match, by="type")

FBI <- FBI %>% 
  pivot_longer(cols=c("FBI_year_one", "FBI_year_two"),
               names_to="Year",
               values_to="FBI")

FBI_match <- FBI_match %>% 
  pivot_longer(cols=c("FBI_year_one", "FBI_year_two"),
               names_to="Year",
               values_to="FBI")

min <- min(c(Year_one_sample$fa_fbi, Year_two_sample$fa_fbi))
max <- max(c(Year_one_sample$fa_fbi, Year_two_sample$fa_fbi))

ggplot(FBI, aes(x=type, y=FBI, fill = Year)) +
  geom_col(position='dodge')+
  ylim(0,200000)
ggplot(FBI_match, aes(x=type, y=FBI, fill = Year)) +
  geom_col(position='dodge')+
  ylim(0,200000)

ggplot(Matched_sample, aes(x=fa_fbi.x, y=fa_fbi.y)) +
  geom_point() +
  ylim(min,max)+
  xlim(min,max)

#Separate check for whether farms which have been noted as potential outliers due to structure changes should be excluded

outliers_check <- FBS_data_process %>% 
  mutate(outlier_flag = 0)
outliers_check$outlier_flag[outliers_check$outlier_flag=="All other farms" & outliers_check$type==5] = "All other LFA Cattle farms"
outliers_check$farmtype[outliers_check$type==5] = "LFA Cattle"

ggplot(subset(outliers_check, type%in%c(4,5)), aes(fa_fbi, fill = as.factor(outlier_flag))) +
  geom_histogram() +
  scale_x_continuous(labels = function(x) format(x, scientific=FALSE)) +
  facet_wrap(~ farmtype)

ggplot(subset(outliers_check, type%in%c(4,5)), aes(x=fa_input, y=fa_outpt, colour = as.factor(outlier_flag))) +
  geom_point() +
  scale_x_continuous(labels = function(x) format(x, scientific=FALSE)) +
  facet_wrap(~farmtype)
