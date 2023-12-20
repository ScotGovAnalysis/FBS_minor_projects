# Code for calculating the average:
#   Quantities of fertiliser purchased and used (total, and per hectare)
#   Amount of money spent on fertiliser (total, and per hectare)
#   Price per kg
#   The ratio of N:P:K used by each farmtype
#   Some preliminary analysis of fuel/electricity usage/spending
# each year by farms in the Farm Business Survey (broken down by farmtype).
# The averages are weighted using the normal R-calculated weightings.


# NB: The quantities are NOT per tonne of raw fertiliser, as this data isn't gathered.
# Instead they are the "N value", "P value" and "K value" multiplied by mass of fertiliser/100.
# This effectively gives the mass of active ingredient.
# See https://en.wikipedia.org/wiki/Labeling_of_fertilizer for more information.
# Best to speak to SACC with any queries.

#Load necessary libraries. Haven is used for importing SAS datasets, data.table to match numerical farmtype (1,2,3...) to wordy farmtypes (Cereals, Cropping...), 
#ade4 is used to create triangle plots
library(tidyverse)
library(haven)
library(data.table)
library(ade4)
# library(purrr)
source("resas_theme.R", encoding = "UTF-8")

#Sample years to be run on. 2015 is the earliest year we have data for.
years <- (2019:2022)
#Years where carbon audit data is available are 2019 onwards. 2019 only for pilot (~1/4 of farms), so
#won't be truly comparable.
carbon_audit_years <- (2019:2022)
#define directory path
directory_path <- '//s0177a/sasdata1/ags/fas'
#If only interested in a particular farmtype, select here. For all farms, set to 9.
farmtype <- 9
#Formatting for farmtypes
type_numbers <- c(1:9)
type_words <- c("Cereals","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed","All farm types")
type_tab <- data.frame(type_numbers,type_words)

#Function to perform minor data cleaning when reading in SAS datasets
#Reads in SAS dataset, converts column names to lower case and strips "format.sas" attributes
#Only works for datasets with name in format so_Y2021_anpk, so doesn't work for the weights file
import_sas <-function(directory_path, datayear, suffix) {
  dataset <- read_sas(paste0(directory_path, "/so_y", datayear, "_", suffix, ".sas7bdat"))
  names(dataset) <- tolower(names(dataset))
  for (var in colnames(dataset)) {
    attr(dataset[[deparse(as.name(var))]],"format.sas")=NULL
  }
  return(dataset)
}
#Import weights file
weights_file <- paste(directory_path, "/new_weights.sas7bdat", sep='')
weights <- read_sas(weights_file)
names(weights) <- tolower(names(weights))
for (x in colnames(weights)){
  attr(weights[[deparse(as.name(x))]],"format.sas")=NULL
}


#Loop over all sampyears. For most cases, datayear will equal sampyear-1.
#However, in the most recently gathered data, datayear will equal sampyear.
main_code <- function(sampyear){
  if(sampyear==max(years)){
    datayear <- sampyear
  } else {
    datayear <- sampyear+1
  }
  #Code to allow selection of individual farmtypes, or all.
  if(farmtype==9){
    typ_low <- 1
    typ_high <- 8
  } else {
    typ_low <- farmtype
    typ_high <- farmtype
  }
  
  #Import SAS datasets, except weights file
  farm_account <- import_sas(directory_path, datayear, "fa")
  fert_quantity <- import_sas(directory_path, datayear, "anpk")
  if(sampyear %in% carbon_audit_years){
    carbon_audit <- import_sas(directory_path, datayear, "carbon")
    #Gather manure data from the carbon audit dataset
    carbon_audit <- carbon_audit %>% 
      select(fa_id, no_mants, no_orgmants, de_d, de_e, de_of) %>% 
      mutate(total_fuels = de_d + de_e + de_of)
  }
  #Gather fertiliser quantity data 
  fert_quantity <- fert_quantity %>% 
    # filter(fa_id !=117352022) %>% 
    select(fa_id, npk_code, npkpur, npktot) %>% 
    mutate(npktot = npktot/1000) %>%
    mutate(npkpur = npkpur/1000)  %>% 
    rename(tonnes_bought = npkpur) %>% 
    rename(tonnes_used = npktot)
  
  #Calculate the total fertiliser quantity for each farm,  
  fert_totals <- fert_quantity %>% 
    select(fa_id, tonnes_bought, tonnes_used) %>% 
    group_by(fa_id) %>% 
    summarise(
      tonnes_bought = sum(tonnes_bought),
      tonnes_used = sum(tonnes_used)) %>% 
    mutate(npk_code = "NPKT")
  
  #Append totals to fert_quantity, rearrange columns and change format to one row per farmid
  fert_quantity <- fert_quantity %>% 
    rbind(fert_totals) %>% 
    arrange(fa_id, npk_code) %>% 
    pivot_wider(id_cols = fa_id,
                names_from = npk_code,
                values_from = c("tonnes_used","tonnes_bought"))
  
  
  
  #Keep only farm id, farm type and fertiliser value from Farm Account 
  farm_account <- farm_account %>% 
    select(fa_id, type, fa_ifert, fa_ifuel, fa_aua, fa_cgare)
  
  #Join input datasets
  merged_data <- farm_account %>% 
    left_join(weights, by="fa_id") %>% 
    left_join(fert_quantity, by="fa_id")
  if(sampyear %in% carbon_audit_years){
    merged_data <- merged_data %>% 
      left_join(carbon_audit, by="fa_id")
  }
  
  
  ## Set missing weights to be 1 (useful for preliminary data)
  merged_data$fbswt[is.na(merged_data$fbswt)]=1
  ## Set any 0 values of crop/grass areas to be non-zero but negligibly small, to prevent division by zero errors.
  ## There must be a cleaner way of achieving this, but it works.
  merged_data$fa_cgare[merged_data$fa_cgare==0]=0.00001
  
  #Add sample year, filter to farmtype selected at top, replace any "NA"s with zeroes.
  merged_data <- merged_data %>%
    mutate(ys_year = (fa_id%%10000)) %>% 
    filter(ys_year==sampyear) %>% 
    filter(type %in% (typ_low:typ_high)) %>% 
    #filter(npk_code=="NPKN") %>% 
    mutate_all(~replace_na(.,0))
  
  #Create an output table with weighted means for each farm type
  ##Note that the weight for the "per hectare" variables is (weight * area).
  if(sampyear %in% carbon_audit_years){
    output_table <- merged_data %>% 
      group_by(type) %>% 
      summarise(avg_fert_spend = weighted.mean(fa_ifert, fbswt),
                avg_fert_spend_ha = weighted.mean(fa_ifert/fa_cgare, fbswt*fa_cgare, na.rm=T),
                avg_tonnes_used_NPKN = weighted.mean(tonnes_used_NPKN, fbswt),
                avg_tonnes_bought_NPKN = weighted.mean(tonnes_bought_NPKN, fbswt),
                avg_tonnes_used_NPKP = weighted.mean(tonnes_used_NPKP, fbswt),
                avg_tonnes_bought_NPKP = weighted.mean(tonnes_bought_NPKP, fbswt),
                avg_tonnes_used_NPKK = weighted.mean(tonnes_used_NPKK, fbswt),
                avg_tonnes_bought_NPKK = weighted.mean(tonnes_bought_NPKK, fbswt),
                avg_tonnes_used_NPKT = weighted.mean(tonnes_used_NPKT, fbswt),
                avg_tonnes_bought_NPKT = weighted.mean(tonnes_bought_NPKT, fbswt),
                avg_tonnes_used_ha_NPKT = weighted.mean(tonnes_used_NPKT/fa_cgare, fbswt*fa_cgare, na.rm=T),
                avg_tonnes_bought_ha_NPKT = weighted.mean(tonnes_bought_NPKT/fa_cgare, fbswt*fa_cgare, na.rm=T),
                avg_N_inorg = weighted.mean(no_mants, fbswt),
                avg_N_org = weighted.mean(no_orgmants, fbswt),
                avg_diesel = weighted.mean(de_d, fbswt),
                avg_electricity = weighted.mean(de_e, fbswt),
                avg_other_fuels = weighted.mean(de_of, fbswt),
                avg_total_fuels = weighted.mean(total_fuels, fbswt),
                avg_fuel_spend = weighted.mean(fa_ifuel, fbswt),
                ys_year = sampyear,
                sum_weight = sum(fbswt)
      )} else {
        output_table <- merged_data %>% 
          group_by(type) %>% 
          summarise(avg_fert_spend = weighted.mean(fa_ifert, fbswt),
                    avg_fert_spend_ha = weighted.mean(fa_ifert/fa_cgare, fbswt*fa_cgare, na.rm=T),
                    avg_tonnes_used_NPKN = weighted.mean(tonnes_used_NPKN, fbswt),
                    avg_tonnes_bought_NPKN = weighted.mean(tonnes_bought_NPKN, fbswt),
                    avg_tonnes_used_NPKP = weighted.mean(tonnes_used_NPKP, fbswt),
                    avg_tonnes_bought_NPKP = weighted.mean(tonnes_bought_NPKP, fbswt),
                    avg_tonnes_used_NPKK = weighted.mean(tonnes_used_NPKK, fbswt),
                    avg_tonnes_bought_NPKK = weighted.mean(tonnes_bought_NPKK, fbswt),
                    avg_tonnes_used_NPKT = weighted.mean(tonnes_used_NPKT, fbswt),
                    avg_tonnes_bought_NPKT = weighted.mean(tonnes_bought_NPKT, fbswt),
                    avg_tonnes_used_ha_NPKT = weighted.mean(tonnes_used_NPKT/fa_cgare, fbswt*fa_cgare, na.rm = T),
                    avg_tonnes_bought_ha_NPKT = weighted.mean(tonnes_bought_NPKT/fa_cgare, fbswt*fa_cgare, na.rm = T),
                    avg_fuel_spend = weighted.mean(fa_ifuel, fbswt),
                    ys_year = sampyear,
                    sum_weight = sum(fbswt)
          )
      }
  
  #Calculate weighted averages of tonnes used, tonnes bought and money spent for all farms
  global_totals <- output_table %>% 
    summarise_at(vars(avg_fert_spend:avg_fuel_spend),~weighted.mean(.,w=sum_weight)) %>% 
    mutate(ys_year=sampyear) %>% 
    mutate(type=9)
  
  #Append the all farms totals to the output table. Add column for price per tonne of fertiliser 
  output_table <- output_table %>% 
    select(-sum_weight) %>% 
    rbind(global_totals) %>% 
    mutate(pounds_per_kg = avg_fert_spend/(avg_tonnes_bought_NPKT*1000))
  if(sampyear %in% carbon_audit_years){
    output_table <- output_table %>% 
      mutate(inorg_N_ratio = avg_N_inorg/(avg_N_org+avg_N_inorg))}
  
  #Add farmtype names to the output table.
  #Adapted from: https://stackoverflow.com/questions/28948168/equivalent-of-sas-format-in-r
  setkey(setDT(output_table),type)
  output_table[setDT(type_tab),farmtype:=i.type_words]
  #Reorder columns
  if(sampyear %in% carbon_audit_years){
    output_table <- output_table %>%
      select(type, farmtype, ys_year,
             avg_tonnes_used_NPKN, avg_tonnes_bought_NPKN,
             avg_tonnes_used_NPKP, avg_tonnes_bought_NPKP,
             avg_tonnes_used_NPKK, avg_tonnes_bought_NPKK,
             avg_tonnes_used_NPKT, avg_tonnes_bought_NPKT,
             avg_tonnes_used_ha_NPKT, avg_tonnes_bought_ha_NPKT,
             avg_fert_spend, avg_fert_spend_ha, pounds_per_kg, inorg_N_ratio,
             avg_fuel_spend, avg_total_fuels, avg_diesel,
             avg_electricity, avg_other_fuels)
  }else{
    output_table <- output_table %>% 
      select(type, farmtype, ys_year,
             avg_tonnes_used_NPKN, avg_tonnes_bought_NPKN,
             avg_tonnes_used_NPKP, avg_tonnes_bought_NPKP,
             avg_tonnes_used_NPKK, avg_tonnes_bought_NPKK,
             avg_tonnes_used_NPKT, avg_tonnes_bought_NPKT,
             avg_tonnes_used_ha_NPKT, avg_tonnes_bought_ha_NPKT,
             avg_fert_spend, avg_fert_spend_ha, pounds_per_kg)
  }
  
  #Store dataset with sampyear appended to name
  assign(paste("output_table",sampyear,sep="_"),output_table)
  return(output_table)
}
output_all <- NULL
for(sampyear in years){
  output_all <- output_all %>% 
    bind_rows(main_code(sampyear))%>% 
  mutate(cropyear=ys_year-1)
}

#Take only the fuel related variables and reformat the table to make a bar chart easier to plot.
output_fuel <- output_all %>%
  filter(ys_year %in% carbon_audit_years) %>% 
  select(type, farmtype, ys_year, cropyear, "Fuel spend"=avg_fuel_spend, "Emissions (diesel)"=avg_diesel, "Emissions (electricity)"=avg_electricity, "Emissions (other fuels)"=avg_other_fuels) %>% 
  gather("Fuel spend", "Emissions (diesel)", "Emissions (electricity)", "Emissions (other fuels)", key="key", value="value")


#Manually create table for nutrient price data from John Nix.
#The year on the front of each John Nix Pocketbook is the year they are intended for use during, so data refers to the previous year.
#For example, the 2017 handbook was published in Sept 2016, so fertiliser price data most closely matches crop year 2016.
nutrient_data <- data.frame(matrix(ncol=4, nrow=0))
colnames(nutrient_data) <- c("handbookyear", "nutrient_price_N", "nutrient_price_P", "nutrient_price_K")
nutrient_data <- nutrient_data %>% 
  rbind(list(handbookyear=2013, nutrient_price_N=0.870, nutrient_price_P=0.826, nutrient_price_K=0.558)) %>% 
  rbind(list(handbookyear=2014, nutrient_price_N=0.797, nutrient_price_P=0.707, nutrient_price_K=0.542)) %>% 
  rbind(list(handbookyear=2015, nutrient_price_N=0.768, nutrient_price_P=0.598, nutrient_price_K=0.458)) %>% 
  rbind(list(handbookyear=2016, nutrient_price_N=0.696, nutrient_price_P=0.685, nutrient_price_K=0.442)) %>% 
  rbind(list(handbookyear=2017, nutrient_price_N=0.493, nutrient_price_P=0.543, nutrient_price_K=0.400)) %>% 
  rbind(list(handbookyear=2018, nutrient_price_N=0.551, nutrient_price_P=0.587, nutrient_price_K=0.417)) %>% 
  rbind(list(handbookyear=2019, nutrient_price_N=0.652, nutrient_price_P=0.641, nutrient_price_K=0.450)) %>% 
  rbind(list(handbookyear=2020, nutrient_price_N=0.754, nutrient_price_P=0.674, nutrient_price_K=0.467)) %>%
  rbind(list(handbookyear=2021, nutrient_price_N=0.580, nutrient_price_P=0.576, nutrient_price_K=0.417)) %>% 
  rbind(list(handbookyear=2022, nutrient_price_N=0.797, nutrient_price_P=0.870, nutrient_price_K=0.417)) %>% 
  mutate(cropyear=handbookyear-1)
  
#Calculate the average nutrient price, using the tonnes NPK bought and the individual nutrient prices.
nutrient_data <- nutrient_data %>% 
  full_join(output_all, by="cropyear") %>% 
  filter(type==9) %>% 
  select(cropyear, nutrient_price_N, nutrient_price_P, nutrient_price_K, 
         avg_tonnes_bought_NPKN, avg_tonnes_bought_NPKP, avg_tonnes_bought_NPKK, avg_tonnes_bought_NPKT) %>% 
  mutate(nutrient_price_T = (avg_tonnes_bought_NPKN*nutrient_price_N+avg_tonnes_bought_NPKP*nutrient_price_P+avg_tonnes_bought_NPKK*nutrient_price_K)/avg_tonnes_bought_NPKT) %>% 
  filter(is.na(nutrient_price_T)==F)

#Manually create table for total nutrient usage in Scotland, using British Survey of Fertiliser Practice data.
#Data up to 2021 from here: https://www.gov.uk/government/statistics/british-survey-of-fertiliser-practice-2021
# Future data should be available by changing the date in the url 
## Usage values below are in tonnes/ha (nb: BSFP table is in kg/ha)
BSFP_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(BSFP_data) <- c("cropyear", "BSFP_usage_N", "BSFP_usage_K", "BSFP_usage_P")
BSFP_data <- BSFP_data %>% 
  rbind(list(cropyear = 2014, BSFP_usage_N = 0.087, BSFP_usage_P = 0.026, BSFP_usage_K = 0.035)) %>%
  rbind(list(cropyear = 2015, BSFP_usage_N = 0.089, BSFP_usage_P = 0.027, BSFP_usage_K = 0.034)) %>%
  rbind(list(cropyear = 2016, BSFP_usage_N = 0.086, BSFP_usage_P = 0.027, BSFP_usage_K = 0.036)) %>%
  rbind(list(cropyear = 2017, BSFP_usage_N = 0.086, BSFP_usage_P = 0.029, BSFP_usage_K = 0.039)) %>%
  rbind(list(cropyear = 2018, BSFP_usage_N = 0.085, BSFP_usage_P = 0.026, BSFP_usage_K = 0.038)) %>%
  rbind(list(cropyear = 2019, BSFP_usage_N = 0.082, BSFP_usage_P = 0.024, BSFP_usage_K = 0.034)) %>%
  rbind(list(cropyear = 2020, BSFP_usage_N = 0.073, BSFP_usage_P = 0.021, BSFP_usage_K = 0.030)) %>%
  rbind(list(cropyear = 2021, BSFP_usage_N = 0.077, BSFP_usage_P = 0.023, BSFP_usage_K = 0.031))
BSFP_data$BSFP_usage_T = rowSums(BSFP_data[, c("BSFP_usage_N","BSFP_usage_P","BSFP_usage_K")])


#Graph of Average tonnes of fertiliser used by farmtype
ggplot(data=output_all) +
  geom_point(mapping=aes(x=cropyear, y=avg_tonnes_used_NPKT, colour=farmtype)) +
  ylim(0,70)+
  xlab("Year") +
  ylab("Average NPK used (tonnes)")

#Graph of Average tonnes of fertiliser used per hectare, by farmtype
ggplot(data=output_all) +
  geom_point(BSFP_data, mapping = aes(x=cropyear, y=BSFP_usage_T, shape="BSFP data"),size=2) +
  geom_point(mapping=aes(x=cropyear, y=avg_tonnes_used_ha_NPKT, colour=farmtype)) +
  xlab("Year") +
  ylim(0,0.4) +
  ylab("Average NPK used (tonnes/ha)")

#Graph of Average tonnes of fertiliser bought by farmtype
ggplot(data=output_all) +
  geom_point(mapping=aes(x=cropyear, y=avg_tonnes_bought_NPKT, colour=farmtype)) +
  ylim(0,70) +
  xlab("Year") +
  ylab("Average NPK bought (tonnes)")

#Graph of Average spend on fertiliser by farmtype
ggplot(data=output_all) +
  geom_point(mapping=aes(x=cropyear, y=avg_fert_spend, colour=farmtype)) +
  xlab("Year") +
  ylab("Average spend (£)")

#Graph of Average fertiliser surplus (amount bought minus amount used)
ggplot(data=output_all) +
  geom_point(mapping=aes(x=cropyear, y=(avg_tonnes_bought_ha_NPKT-avg_tonnes_used_ha_NPKT), colour=farmtype)) +
  xlab("Year") +
  # ylim(-ceiling(abs(max(max(output_all$avg_tonnes_bought_NPKT - output_all$avg_tonnes_used_NPKT),min(output_all$avg_tonnes_bought_NPKT - output_all$avg_tonnes_used_NPKT)))),ceiling(abs(max(max(output_all$avg_tonnes_bought_NPKT - output_all$avg_tonnes_used_NPKT),min(output_all$avg_tonnes_bought_NPKT - output_all$avg_tonnes_used_NPKT)))))+
  ylab("Average surplus (tonnes)")

#Graph of price per kg
ggplot(data=output_all) +
  geom_point(nutrient_data, mapping = aes(x=cropyear, y=nutrient_price_T, size=8), shape=18) +
  geom_point(mapping=aes(x=cropyear, y=pounds_per_kg, colour=farmtype)) +
  ylim(0,1) +
  ylab("Nutrient price (£/kg)") +
  xlab("Year")

#Graphs of fuel data
ggplot(data=output_fuel,aes(x=cropyear, y=value, fill = key)) +
  geom_bar(position="dodge", stat="identity")
  

#Graph showing fraction of nitrogen manure which is from inorganic fertiliser, by year and farmtype.
#Note, from carbon audit, so data only starts in 2019.
ggplot(data=filter(output_all, ys_year %in% carbon_audit_years)) +
  geom_point(mapping=aes(x=cropyear, y=inorg_N_ratio, colour=farmtype)) +
  xlab ("Year") +
  ylim(0,1) +
  ylab ("Fraction of nitrogen from inorganic manure") +
  theme(legend.position = "right")

#Create triangle plots showing the proportion of N,P,K used on each farmtype.
par(mfrow=c(3,3))
for (i in 1:9){
  triangle_table <- output_all %>% 
    filter(type==i) %>%
    select("N"=avg_tonnes_used_NPKN,"P"=avg_tonnes_used_NPKP,"K"=avg_tonnes_used_NPKK,type)
  row.names(triangle_table) <- paste(years)
  triplot <- triangle.plot(select(triangle_table,-type),sub=type_words[i], csub=2, possub="bottomleft", addmean=TRUE,scale=FALSE,label=row.names(triangle_table),clab=0)
}

par(mfrow=c(3,3))
for (i in 1:9){
  triangle_table <- output_all %>% 
    filter(type==i) %>% 
    filter(ys_year %in% carbon_audit_years) %>%
    select("Diesel"=avg_diesel,"Electricity"=avg_electricity,"Other"=avg_other_fuels,type)
  row.names(triangle_table) <- paste(carbon_audit_years)
  triplot <- triangle.plot(select(triangle_table,-type),sub=type_words[i], csub=2, possub="bottomleft", addmean=TRUE,scale=FALSE,label=row.names(triangle_table),clab=0)
}