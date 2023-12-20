library(haven)
library(spatstat)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(tidyverse)

source("resas_theme.R", encoding = "UTF-8")

# # excluded_farms <- c(
#   136782022,
#   137072022
# )


sampyear_range <- 2016:2022
for (sampyear in sampyear_range){
  if(sampyear==max(sampyear_range)){
    datayear=sampyear
  }
  else {
    datayear=sampyear + 1
  }
  
  if(sampyear==min(sampyear_range)){
    AllYears_support <- data.frame(
      decile = c(1:10))
    AllYears_profit <- AllYears_support
    AllYears_efficiency <- AllYears_support
    AllFarms <- NULL
    AllSubsidies <- NULL
  }
  #Read in farm_account data
  
  FBS_directory_path <- '//s0177a/sasdata1/ags/fas/'
  FBS_fa_data_file <- paste0("so_y", datayear, "_fa",".sas7bdat")
  FBS_fa_data <- tryCatch(
    {
      FBS_data <- read_sas(FBS_fa_data_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_fa_data_file), getwd())
      return(read_sas(FBS_fa_data_file))
    }
  )
  names(FBS_fa_data) <- tolower(names(FBS_fa_data))
  for (x in colnames(FBS_fa_data)){
    attr(FBS_fa_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  
  FBS_dsec1_data_file <- paste0("so_y", datayear, "_dsec1",".sas7bdat")
  FBS_dsec1_data <- tryCatch(
    {
      FBS_dsec1_data <- read_sas(FBS_dsec1_data_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_dsec1_data_file), getwd())
      return(read_sas(FBS_dsec1_data_file))
    }
  )
  names(FBS_dsec1_data) <- tolower(names(FBS_dsec1_data))
  for (x in colnames(FBS_dsec1_data)){
    attr(FBS_dsec1_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  FBS_dsec2_data_file <- paste0("so_y", datayear, "_dsec2",".sas7bdat")
  FBS_dsec2_data <- tryCatch(
    {
      FBS_dsec2_data <- read_sas(FBS_dsec2_data_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_dsec2_data_file), getwd())
      return(read_sas(FBS_dsec2_data_file))
    }
  )
  names(FBS_dsec2_data) <- tolower(names(FBS_dsec2_data))
  for (x in colnames(FBS_dsec2_data)){
    attr(FBS_dsec2_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  FBS_weights_file <- paste0("new_weights.sas7bdat")
  FBS_weights <- tryCatch(
    {
      FBS_weights <- read_sas(FBS_weights_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_weights_file), getwd())
      return(read_sas(FBS_weights_file))
    }
  )
  names(FBS_weights) <- tolower(names(FBS_weights))
  for (x in colnames(FBS_weights)){
    attr(FBS_weights[[deparse(as.name(x))]],"format.sas")=NULL
  }
  
  FBS_subsidy_data_file <- paste0("so_y", datayear, "_asu",".sas7bdat")
  FBS_subsidy_data <- tryCatch(
    {
      FBS_subsidy_data <- read_sas(FBS_subsidy_data_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_subsidy_data_file), getwd())
      return(read_sas(FBS_subsidy_data_file))
    }
  )
  names(FBS_subsidy_data) <- tolower(names(FBS_subsidy_data))
  for (x in colnames(FBS_subsidy_data)){
    attr(FBS_subsidy_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  
  
  
  FBS_fa_data_process <- FBS_fa_data %>% 
    filter(ys_year==sampyear)%>%
    select(fa_id, fa_gsub, fa_fbi, fa_input, fa_outpt, type, fa_tarea)  
  FBS_dsec1_data_process <- FBS_dsec1_data %>% 
    filter(fa_id%%10000==sampyear) %>%
    filter(ds1acode==11300) %>%
    group_by(fa_id) %>% 
    dplyr::summarise(dsec1_total = sum(ds1margn)) 
  FBS_dsec2_data_process <- FBS_dsec2_data %>% 
    filter(fa_id%%10000==sampyear) %>% 
    filter(ds2acode==21300) %>%
    group_by(fa_id) %>% 
    dplyr::summarise(dsec2_total = sum(ds2margn))
  FBS_weights_process <- FBS_weights %>% 
    filter(fa_id%%10000==sampyear)
  
  
  FBS_merged_dataset <- FBS_fa_data_process %>%
    left_join(FBS_dsec1_data_process, by='fa_id') %>% 
    left_join(FBS_dsec2_data_process, by='fa_id') %>% 
    inner_join(FBS_weights_process, by='fa_id') %>% 
    # filter(!fa_id %in% excluded_farms) %>% 
    mutate(decile=0)
  
  FBS_merged_dataset$fbswt[is.na(FBS_merged_dataset$fbswt)] <- mean(FBS_merged_dataset$fbswt, na.rm = T)
  
  #set all weights =1, for testing
  # FBS_merged_dataset$fbswt=1
  
  FBS_merged_dataset$dsec2_total[is.na(FBS_merged_dataset$dsec2_total)] <- 0
  FBS_merged_dataset <- FBS_merged_dataset %>% 
    mutate(FBI_exc_dsec2=(fa_fbi-(dsec2_total))) %>% 
    mutate(FBI_exc_dsec = (fa_fbi-(dsec2_total+dsec1_total))) %>% 
    mutate(FBI_exc_dsec2_subs = (fa_fbi-(fa_gsub+dsec2_total))) %>%
    mutate(FBI_exc_dsec_subs = (fa_fbi-(fa_gsub+dsec1_total+dsec2_total))) %>% 
    mutate(FBI_ag_only = (fa_outpt-fa_input))
  
  #weighted.median(FBS_merged_dataset$FBI_exc_dsec2, FBS_merged_dataset$fbswt)
  Deciles <- weighted.quantile(FBS_merged_dataset$FBI_exc_dsec2_subs , FBS_merged_dataset$fbswt, probs=seq(0, 1, 0.1))
  # Deciles <- rev(Deciles)
  #check <- FBS_merged_dataset %>% 
  #  filter(Deciles[1] <= FBI_exc_dsec2 & FBI_exc_dsec2 < Deciles[2])
  
  for (i in 1:10){
    FBS_merged_dataset$decile[FBS_merged_dataset$FBI_exc_dsec2_subs >= Deciles[i]]=i
  }
  
  #paste(sampyear," ", mean(FBS_merged_dataset$fa_fbi))
  #paste(sampyear," ", weighted.mean(FBS_merged_dataset$fa_fbi, FBS_merged_dataset$fbswt))
  #paste(sampyear," ", weighted.mean(FBS_merged_dataset$dsec2_total, FBS_merged_dataset$fbswt))
  
  Summary <- FBS_merged_dataset %>% 
    group_by(decile) %>% 
    summarise(count=n(),
              sumwt=sum(fbswt))
  
  
  Support <- FBS_merged_dataset %>% 
    group_by(decile) %>% 
    summarise(avg=weighted.mean(fa_gsub, fbswt))
  colnames(Support) = c('decile', paste0("Y",sampyear))
  Profits <- FBS_merged_dataset %>% 
    group_by(decile) %>% 
    summarise(Profit=weighted.mean(FBI_exc_dsec2_subs , fbswt))
  colnames(Profits) = c('decile', paste0("Y", sampyear))
  AllYears_support <- AllYears_support %>% 
    left_join(Support, by='decile')
  AllYears_profit <- AllYears_profit %>% 
    left_join(Profits, by='decile')
  Efficiency <- FBS_merged_dataset %>% 
    group_by(decile) %>% 
    summarise(Efficiency = weighted.mean((fa_outpt-fa_gsub)/fa_input, fbswt))
  
  colnames(Efficiency) = c('decile', paste0("Y", sampyear))
  AllYears_efficiency <- AllYears_efficiency %>% 
    left_join(Efficiency, by="decile")
    
  
  
  Year_totals <- FBS_merged_dataset %>% 
    summarise(Sampyear = sampyear,
              FBI_with_subs=weighted.mean(FBI_exc_dsec2, fbswt),
              Total_subs = weighted.mean(fa_gsub, fbswt),
              FBI_no_subs = weighted.mean((FBI_exc_dsec2_subs), fbswt))
  
  AllFarms <- AllFarms %>% 
    rbind(Year_totals)
  
  FBS_subsidy_data_process <- FBS_subsidy_data %>% 
    filter(fa_id%%10000 == sampyear) %>% 
    select(fa_id, subsidy = su_code, value=asuv) %>% 
    spread(key = subsidy, value = value) %>% 
    left_join(FBS_weights, by='fa_id')
  FBS_subsidy_data_process[is.na(FBS_subsidy_data_process)]=0

  Subsidy_summary <- FBS_subsidy_data_process %>% 
    summarise(across(starts_with('S'), ~weighted.mean(., fbswt))) %>% 
    mutate(sampyear=sampyear)
  AllSubsidies <- AllSubsidies %>% 
    bind_rows(Subsidy_summary)
  
}


#Figure 3
AllFarms_longdata <- AllFarms %>% 
  pivot_longer(
    cols=c('FBI_with_subs', 'Total_subs', 'FBI_no_subs'),
    names_to = 'Measure',
    values_to = 'Total'
  )
ggplot(AllFarms_longdata, aes(x=Sampyear, y=Total, colour = Measure)) +
  geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=(max(sampyear_range)-min(sampyear_range))/2))


#Figure 4
AllSubsidies[is.na(AllSubsidies)]=0
AllSubsidies_longdata <- AllSubsidies %>% 
  select(sampyear, everything()) %>% 
  pivot_longer(
    cols=c(2:length(colnames(AllSubsidies))), names_to = "Subsidy",
    values_to = "Value")


ggplot(AllSubsidies_longdata, aes(x=Subsidy, y=Value, fill=as.factor(sampyear)))+
  geom_col()

#Figure 5
Fig5a <- AllYears_profit %>% 
  filter(decile %in% c(1,10)) %>% 
  mutate(Measure="Without support")
Fig5b <- AllYears_support %>% 
  filter(decile %in% c(1,10)) %>% 
  mutate(Measure="Support")
Fig5c <- setNames(data.frame(t(AllFarms[ , -1])), colnames(Fig5b[c(2:(2+max(sampyear_range)-min(sampyear_range)))] )) %>% 
  mutate(decile="All")
Fig5c$Measure <- revalue(row.names(Fig5c), c("FBI_no_subs" = "Without support", "Total_subs" = "Support"))
Fig5c <- Fig5c %>% 
  filter(Measure %in% c("Support", "Without support"))
row.names(Fig5c) <- NULL

Fig5d <- Fig5a %>% 
  rbind(Fig5b, Fig5c) %>% 
  group_by(decile) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate(Measure="With support")

Fig5 <- Fig5d %>%
  rbind(Fig5a, Fig5b, Fig5c) %>% 
  filter(Measure %in% c("With support", "Without support"))
  

Fig5 <- Fig5 %>% 
  pivot_longer(
    cols=colnames(Fig5[-c(1,length(sampyear_range)+2)]),
    names_to = "Year",
    values_to = "Value"
  )
ggplot(filter(Fig5, Year=="Y2019"), aes(x=decile, y=Value, fill=Measure)) +
  geom_col(position='dodge')


ggplot(Fig5, aes(x=decile, y=Value, fill=Year, colour=Measure)) +
 geom_col(position='dodge')

# Figure 6
ggplot(FBS_merged_dataset, aes(x=FBI_exc_dsec2, y=..density.., weight=fbswt)) +
  geom_histogram(bins=100)+
  xlim(-750000, 750000)
ggplot(FBS_merged_dataset, aes(x=FBI_exc_dsec2_subs, y=..density.., weight =fbswt)) +#
  geom_histogram(bins=100)+
  xlim(-750000, 750000)

#Figure7
ggplot(AllYears_profit, aes(x=decile, y= Y2022)) +
  geom_col()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))

LongData_profit <- AllYears_profit %>%
  pivot_longer(
    cols = colnames(AllYears_profit[-c(1)]),
    names_to = 'Year',
    values_to = 'Profit'
  )
ggplot(LongData_profit, aes(x=decile, y=Profit, fill=Year))+
  geom_col()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))

#Figure8
ggplot(AllYears_support, aes(x=decile, y=Y2022))+
  geom_col()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))
LongData_support <- AllYears_support%>%
  pivot_longer(
    cols=colnames(AllYears_support[-c(1)]),
    names_to='Year',
    values_to='Support')

ggplot(LongData_support, aes(x=decile, y=Support, fill=Year))+
  geom_col() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))+
  labs(x="Profitability decile", y="Support payments (£)")

#Figure 9
ggplot(Efficiency, aes(x=decile, y=Y2022)) +
  geom_col()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))
Efficiency_longdata <- AllYears_efficiency %>% 
  pivot_longer(
    cols=colnames(AllYears_efficiency[-c(1)]),
    names_to = "Year",
    values_to = "Efficiency")
ggplot(Efficiency_longdata, aes(x=decile, y= Efficiency/(max(sampyear_range)-min(sampyear_range)), fill = Year)) + 
  geom_col()+
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))



