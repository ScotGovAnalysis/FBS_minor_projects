##A few different bits of code for analysing farm business income data, and which may be useful for QA purposes.
library(haven)
library(spatstat)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(tidyverse)
library(data.table)

##Formatting for farm types
fbs_type_numbers <- c(1:9)
fbs_type_words <-
  c(
    "Cereals",
    "General Cropping",
    "Dairy",
    "LFA Sheep",
    "LFA Cattle",
    "LFA Cattle and Sheep",
    "Lowland Livestock",
    "Mixed",
    "All farm types"
  )
fbs_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)
apply_type_formats <- function(table_name) {
  setkey(setDT(table_name), type)
  table_name[setDT(fbs_type_tab), farmtype := i.fbs_type_words]
  return(table_name)
}

#Define sampyear - this is the cropping year you want to analyse.
sampyear <- 2022
# Calculate datayear (the year in which data was received from SACC).
# For the most recently received sampyear of data, data is provisional and datayear=sampyear.
# For previous years, data is final and datayear = sampyear + 1.
if (sampyear == 2022) {
  datayear <- sampyear
} else{
  datayear <- sampyear + 1
}

#Read in farm_account data from SAS drive.
#This is the FBS folder on the SAS drive; I'm not sure who has access to this.
FBS_directory_path <- '//s0177a/sasdata1/ags/fas/'
FBS_fa_data_file <- paste0("so_y", datayear, "_fa", ".sas7bdat")
#Check if the farm_account dataset already exists in the current work directory, and download it from
#the SAS drive if it doesn't
FBS_fa_data <- tryCatch({
  FBS_data <- read_sas(FBS_fa_data_file)
},
error = function(e)
{
  file.copy(paste0(FBS_directory_path, FBS_fa_data_file), getwd())
  return(read_sas(FBS_fa_data_file))
})
#Some basic data cleaning. Convert column names to lower case; remove "sas" formatting.
names(FBS_fa_data) <- tolower(names(FBS_fa_data))
for (x in colnames(FBS_fa_data)) {
  attr(FBS_fa_data[[deparse(as.name(x))]], "format.sas") = NULL
}

#Repeat above for the weights file
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

#Bring in diversification data
FBS_dsec_file <- paste0("so_y",datayear,"_dsec2",".sas7bdat")
FBS_dsec_data <- tryCatch({
  FBS_dsec_data <- read_sas(FBS_dsec_file)
},
error = function(e)
{
  file.copy(paste0(FBS_directory_path, FBS_dsec_file), getwd())
  return(read_sas(FBS_dsec_file))
})
names(FBS_dsec_data) <- tolower(names(FBS_dsec_data))
for (x in colnames(FBS_dsec_data)) {
  attr(FBS_dsec_data[[deparse(as.name(x))]], "format.sas") = NULL
}

##Data processing
##Any variable you want to analyse in the plots below needs to go into the select line here.
FBS_fa_data_process <- FBS_fa_data %>%
  filter(ys_year == sampyear) %>%
  select(fa_id, fa_gsub, fa_fbi, fa_input, fa_outpt, type, fa_tarea, fa_aua, fa_glu)
FBS_dsec_data_process <- FBS_dsec_data %>% 
  filter(fa_id%%10000==sampyear,
         ds2acode==21300) %>% 
  group_by(fa_id) %>% 
  summarise(dsec_costs = sum(ds2over, ds2refin, ds2mp_depn, ds2plc, ds2pmacc, ds2vcost),
            dsec_output = sum(ds2oput),
            dsec_margin = sum(ds2margn)
  ) %>% 
  right_join(select(FBS_fa_data_process, fa_id), by='fa_id')
FBS_dsec_data_process <- FBS_dsec_data_process %>% 
  mutate_all(~replace(., is.na(.),0))
FBS_merged_dataset <- FBS_fa_data_process %>%
  inner_join(FBS_weights, by = 'fa_id') %>% 
  left_join(FBS_dsec_data_process, by = 'fa_id')
FBS_merged_dataset <- apply_type_formats(FBS_merged_dataset)


#Weighted and unweighted FBI histograms - originally for FBS methodology document, may be useful for profitability
#report later.
#Calculate weighted and unweighted mean and median FBI
FBI_mean = weighted.mean(FBS_merged_dataset$fa_fbi, w = FBS_merged_dataset$fbswt)
FBI_median = weighted.median(FBS_merged_dataset$fa_fbi, w = FBS_merged_dataset$fbswt)
FBI_unwt_mean = mean(FBS_merged_dataset$fa_fbi)
FBI_unwt_median = median(FBS_merged_dataset$fa_fbi)
#Calculate sum of fbswt and the number of farms, for normalising density curves
sumweights <- sum(FBS_merged_dataset$fbswt)
FBS_sample_size <- nrow(FBS_merged_dataset)

#Add (weighted) mean and median by farmtype
FBS_merged_dataset <- FBS_merged_dataset %>% 
  group_by(farmtype) %>% 
  mutate(farmtype_Q1 = weighted.quantile(fa_fbi, fbswt, 0.25),
         farmtype_Q2 = weighted.median(fa_fbi, fbswt),
         farmtype_Q3 = weighted.quantile(fa_fbi, fbswt, 0.75))
#Scale weights by farmtype
FBS_merged_dataset <- FBS_merged_dataset %>% 
  group_by(farmtype) %>% 
  mutate(type_wt_sum = sum(fbswt))
FBS_merged_dataset <- FBS_merged_dataset %>% 
  mutate(type_fbswt = fbswt*sumweights/type_wt_sum)
  

##Plot histograms
g = ggplot(FBS_merged_dataset) +
  geom_histogram(bins = 50, aes(
    x = fa_fbi,
    y = (100 * ..count..),
    weight = type_fbswt/sumweights,
    fill = as.factor(quartile)
  )) +
  geom_vline(aes(xintercept = farmtype_Q1, group=farmtype, colour = "red")) +
  geom_vline(aes(xintercept = farmtype_Q2, group=farmtype, colour = "red")) +
  geom_vline(aes(xintercept = farmtype_Q3, group=farmtype, colour = "red")) +
  # geom_vline(xintercept = FBI_median, colour = "blue") +
  xlab("Farm business income (£)") +
  ylab("Fraction of farms (%)") +
  scale_x_continuous(
    # breaks = seq(-300000, 300000, 100000),
    labels = function(l) {
      trans = l / 1000
      paste0(trans, "k")
    }
  ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  theme(
    axis.ticks.x = element_line(colour = "grey90"),
    axis.line.x = element_line(colour = "grey90"),
    panel.grid.major.x = element_line(colour = "grey90")
  )+ facet_wrap(~farmtype)
g 
# +
#   annotate(
#     x = FBI_mean,
#     y = 3,
#     vjust = -0,
#     hjust = 0,
#     colour = "red",
#     label = paste0("Mean FBI = £", round(FBI_mean,-2)),
#     geom = "label"
#   ) +
#   annotate(
#     x = FBI_median,
#     y = 2,
#     vjust = -0,
#     hjust = 0,
#     colour = "blue",
#     label = paste0("Median FBI = £", round(FBI_median,-2)),
#     geom = "label"
#   )

g = ggplot(FBS_merged_dataset) +
  geom_histogram(bins = 100, aes(x = fa_fbi, y = (100 * ..count.. / FBS_sample_size))) +
  geom_vline(xintercept = FBI_mean, colour = "red") +
  geom_vline(xintercept = FBI_median, colour = "blue") +
  xlab("Farm business income (£)") +
  ylab("Fraction of farms (%)") +
  scale_x_continuous(
    breaks = seq(-300000, 500000, 100000),
    labels = function(l) {
      trans = l / 1000
      paste0(trans, "k")
    }
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) +
  theme(
    axis.ticks.x = element_line(colour = "grey90"),
    axis.line.x = element_line(colour = "grey90"),
    panel.grid.major.x = element_line(colour = "grey90")
  )
g +
  annotate(
    x = FBI_unwt_mean,
    y = 10,
    vjust = -0,
    colour = "red",
    label = paste0("Unweighted mean FBI = £", round(FBI_unwt_mean,-2)),
    geom = "label"
  ) +
  annotate(
    x = FBI_unwt_median,
    y = 11,
    vjust = -0,
    colour = "blue",
    label = paste0("Unweighted median FBI = £", round(FBI_unwt_median,-2)),
    geom = "label"
  )


#Define function to calculate quartile values for a specified dataset and variable, then assign each farm to a quartile group.
quartiles_variable <- function(dataset, variable="fa_fbi", type) {
  if(i != 9){
    type_range <- c(i)
  } else
  {
    type_range <- c(1:8)
  }
  Q1 <- weighted.quantile(filter(dataset,type%in%type_range)[[variable]], filter(dataset, type%in%type_range)[["fbswt"]], 0.25)
  Q2 <- weighted.quantile(filter(dataset,type%in%type_range)[[variable]], filter(dataset, type%in%type_range)[["fbswt"]], 0.5)
  Q3 <- weighted.quantile(filter(dataset,type%in%type_range)[[variable]], filter(dataset, type%in%type_range)[["fbswt"]], 0.75)
  # print(Q1)
  # print(Q2)
  # print(Q3)
  FBS_merged_dataset$quartile = 1
  FBS_merged_dataset$quartile[dataset[[variable]] > Q1] <- 2
  FBS_merged_dataset$quartile[dataset[[variable]] > Q2] <- 3
  FBS_merged_dataset$quartile[dataset[[variable]] > Q3] <- 4
  
  FBS_quartile_1_sumwt <-
    sum(FBS_merged_dataset$fbswt[FBS_merged_dataset$quartile == 1])
  FBS_quartile_2_sumwt <-
    sum(FBS_merged_dataset$fbswt[FBS_merged_dataset$quartile == 2])
  FBS_quartile_3_sumwt <-
    sum(FBS_merged_dataset$fbswt[FBS_merged_dataset$quartile == 3])
  FBS_quartile_4_sumwt <-
    sum(FBS_merged_dataset$fbswt[FBS_merged_dataset$quartile == 4])
  invisible(FBS_merged_dataset)
}
#Use the function above with your chosen variable. Eg., fa_tarea is total farm area and fa_outpt is total farm output
#The selected variable will need to have been selected when FBS_data_process was created above.
for (i in 1:9){
  FBS_merged_dataset <-
  quartiles_variable(FBS_merged_dataset, "fa_fbi", i)

#Plot density curves for each of the calculated quartiles, with FBI on the x-axis.
#Quartile 1 is the lowest quartile (so e.g., the 25% of farms with the lowest total area)
#Quartile 4 is the upper quartile (e.g., the 25% of farms with the highest total area)
#The quartile calculations are weighted, so there may not be an equal number of farms in each quartile (but the sum of fbswt in each group should be roughly equal)
  if(i != 9){
    type_range <- c(i)
  } else
  {
    type_range <- c(1:8)
  }
plot <- ggplot(data=filter(FBS_merged_dataset, type%in%type_range),
               aes(
    x = fa_fbi,
    y = fa_glu,
    # weight = fbswt,
    colour = farmtype
  )) +
  geom_point()+
  geom_smooth(method='lm', formula=y ~ x)+
  xlab("agricultural area (ha)")+
  ylab("Grazing livestock units") +
  # scale_y_continuous(limits = c(0, NULL))+
  # labs(title=paste0(fbs_type_words[[i]]))+
  facet_wrap(~farmtype)

print(plot)
ggsave(filename = paste0("Outputs/Output_area_scatter_facet",fbs_type_words[[i]],".png"), plot=plot)


# plot2 <- ggplot(filter(FBS_merged_dataset, type%in%type_range))+
#   geom_point(aes(
#     x = fa_aua,
#     y=fa_fbi,
#     colour = as.factor(farmtype)
#   ))+
#   labs(colour = "Farm type", title="Agricultural area")
# print(plot2)
# # ggsave()
# 
# plot3 <- ggplot(filter(FBS_merged_dataset, type%in%type_range))+
#   geom_point(aes(
#     x = fa_glu/fa_aua,
#     y = fa_fbi,
#     colour = as.factor(farmtype)
#   ))+
#   labs(colour = "Farm type", title="Intensity (GLU/ha)")
# print(plot3)
# 
# 
# plot4 <- ggplot(filter(FBS_merged_dataset, type%in%type_range))+
#   geom_point(aes(
#     x = fa_outpt/fa_aua,
#     y = fa_fbi,
#     colour = as.factor(farmtype)
#   ))+
#   labs(colour = "Farm type", title="Output(£)/hectare")
# print(plot4)
# 
# plot5 <- ggplot(filter(FBS_merged_dataset, type%in%type_range, dsec_margin !=0))+
#   geom_point(aes(
#     x = dsec_margin,
#     y = fa_fbi,
#     colour = as.factor(farmtype)
#   ))+
#   labs(colour = "Farm type", title="Diversification margin")
# print(plot5)
}

check <- FBS_merged_dataset %>% 
  mutate(dsec_prop = dsec_margin/fa_fbi) %>% 
  group_by(farmtype, quartile) %>% 
  summarise(dsec_margin=weighted.mean(dsec_margin),
            fa_fbi=weighted.mean(fa_fbi))


plot = ggplot(filter(check)) +
  geom_point(aes(
    y = dsec_margin,
    x = fa_aua,
    colour = farmtype,
    size = quartile
  )) +
  # scale_x_continuous(
  #   breaks = seq(-300000, 500000, 100000),
  #   labels = function(l) {
  #     trans = l / 1000
  #     paste0(trans, "k")
  #   }
  # ) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) +
  theme(
    axis.ticks.x = element_line(colour = "grey90"),
    axis.line.x = element_line(colour = "grey90"),
    panel.grid.major.x = element_line(colour = "grey90")
  )
print(plot)
ggsave(filename = paste0("Outputs/Diversification_margin_scatter_.png"), plot=plot)


check <- FBS_merged_dataset %>% 
  select(fa_id, fa_aua, fa_outpt, fbswt, type, farmtype)
write_xlsx(check,'area_output_correlation.xlsx')
