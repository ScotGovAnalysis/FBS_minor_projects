##Import packages
library(tidyverse)
library(haven)
library(data.table)
library(writexl)
library(RtoSQLServer)
library(rapid.spreadsheets)
library(openxlsx)

#Function to apply name formats for farm types
apply_type_formats <- function(table_name) {
  setkey(setDT(table_name),census_type)
  table_name[setDT(census_type_tab),farmtype:=i.census_type_words]
  return(table_name)
}
#Variables for farmtype names and numbering
fbs_type_numbers <- c(1:9)
fbs_type_words <- c("Cereals","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed","All farm types")
fbs_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)
census_type_numbers <- c(1:16)
census_type_words <- c("Cereals","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed",
                       "Specialist horticulture & permanent crops", "Specialist pigs", "Specialist poultry", "General cropping - forage", 
                       "Unclassified", "All farm types", "FBS farm types only", "FBS farm types only, and meeting FBS thresholds")
census_type_tab <- data.frame(census_type_numbers, census_type_words)

#Identify the locations and names of the datasets to be imported

#Use an environment variable to specify the FBS and census data paths.
#See https://csgillespie.github.io/efficientR/set-up.html#renviron
#The path here is to the FAS/asgscens folder on the SAS drive.
FBS_directory_path <- Sys.getenv("FBS_directory_path")
census_directory_path <- Sys.getenv("Census_directory_path")
census_data_file <- paste0("june",year,".sas7bdat")
FBS_data_file <- paste0("so_y", datayear, "_fa",".sas7bdat")

## Read in the census data for the relevant crop year. The code first looks in the project folder and reads in the data from there if the
## file has already been downloaded. If it's not found there, it copies the file from the SAS drive then reads it in.

# census_data <- tryCatch(
#   {
#     census_data <- read_sas(census_data_file)
#   },
#   error = function(e)
#   {
#     file.copy(paste0(census_directory_path, census_data_file), getwd())
#     return(read_sas(census_data_file))
#   }
# )

#adm census
# #2022
server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

census_data <- read_table_from_db(server=server,
                                  database=database,
                                  schema=schema,
                                  table_name="JAC22_10_01_24")

#Read in FBS data. First, try reading it in from the project folder, and if not found, copy it from the SAS drive and then read it in.
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
#Cleaning of column names
names(census_data) <- tolower(names(census_data))
for (x in colnames(census_data)){
  attr(census_data[[deparse(as.name(x))]],"format.sas")=NULL
}
names(FBS_data) <- tolower(names(FBS_data))
for (x in colnames(FBS_data)){
  attr(FBS_data[[deparse(as.name(x))]],"format.sas")=NULL
}

#Copy the main datasets for processing
FBS_data_process <- FBS_data %>% 
  select(fa_id, type) %>% 
  filter(fa_id%%10000==datayear)
census_data_process <- census_data

## The next lines are used for calculating the number of farms with employees, rather than the number of employees.
## Leave them commented out if calculating actual number of workers.
##UPDATE: No longer believe the above comment is true. The numbers in the raw data for these columns seem to indicate a category,
# rather than a number of employees (since all range between 1-4)
special_cols <- c("item177","item178","item179","item182","item183","item184")
census_data_process[,special_cols][census_data_process[,special_cols]>1]=1
census_data_process[is.na(census_data_process)]=0
#Identify farm types based on FADN typology (typhigh, typmed, typlow, robust_new)
census_data_process <- census_data_process %>% 
  mutate(totlab = rowSums(census_data_process[c("item177","item178","item179","item182","item183","item184", "item200")], na.rm=F)) %>% 
  select(typhigh, typmed, typlow, robust_new, sumso, slr, total_area=item12, totlab, lfatemp, saf_completed, brn, num_holdings, sheep=item145) %>% 
  mutate(census_type=case_when(
    typhigh %in% c(6:8)~8,
    robust_new==1~1,
    robust_new==2~2,
    robust_new==6~3,
    robust_new==7 & typlow %in% c(481,483,484)~4,
    typmed==9946|typmed==9947~5,
    robust_new==8~7,
    typlow==482~6,
    robust_new==9&typlow==484~8,
    robust_new==3~9,
    robust_new==4~10,
    robust_new==5~11,
    robust_new==10~12,
    robust_new==11~13
  )) %>%
  #Default to FBS criteria being met - farms not meeting criteria will have this set to 0 below
  mutate(FBS_thresholds_met=1) %>% 
  filter(is.na(census_type)==FALSE)

#Set the FBS thresholds flag to 0 for holdings not meeting the various criteria
census_data_process$FBS_thresholds_met[census_data_process$sumso < FBS_euro_threshold*exchange_rate]=0
census_data_process$FBS_thresholds_met[census_data_process$slr<0.5]=0
census_data_process$FBS_thresholds_met[!(census_data_process$census_type %in% c(1:8))]=0
#Assume that number of holdings=1 if farms don't have a BRN.
census_data_process$num_holdings[is.na(census_data_process$brn)]=1

##For holdings with a BRN, count the number of holdings each business has
BRN_holdings <- census_data_process %>%
  filter(is.na(brn)==F) %>% 
  group_by(census_type) %>% 
  summarise(holdings=n(), businesses=n_distinct(brn))
##Repeat, but for only those holdings meeting FBS thresholds
BRN_holdings_threshold <- census_data_process %>% 
  filter(is.na(brn)==F, FBS_thresholds_met==1) %>% 
  group_by(census_type) %>% 
  summarise(holdings=n(), businesses=n_distinct(brn))
##Total row for "all farms"
BRN_holdings_all_farms_total <- census_data_process %>% 
  summarise(census_type=14, holdings=sum(!is.na(brn)), businesses=n_distinct(brn[!is.na(brn)]), holdings_no_BRN=sum(is.na(brn)))
##Total row for "all farms (FBS types only)"
BRN_holdings_FBS_types_total <- census_data_process %>% 
  filter(census_type<9) %>% 
  summarise(census_type=15, holdings=sum(!is.na(brn)), businesses=n_distinct(brn[!is.na(brn)]), holdings_no_BRN=sum(is.na(brn)))
##Total row for "all farms (FBS types only and meeting other FBS criteria)"
BRN_holdings_threshold_row <- census_data_process %>% 
  filter(FBS_thresholds_met==1) %>% 
  summarise(census_type=16, holdings=sum(!is.na(brn)), businesses=n_distinct(brn[!is.na(brn)]), holdings_no_BRN=sum(is.na(brn)))

## Count holdings with no BRN, by farmtype - used in Table A
BRN_holdings_NA <- census_data_process %>%
  filter(is.na(brn)==T) %>%
  group_by(census_type) %>%
  summarise(holdings_no_BRN=n())

# Count farms with no BRN which meet FBS criteria, by farmtype - used in Table A1
BRN_holdings_NA_threshold <- census_data_process %>% 
  filter(is.na(brn)==T, FBS_thresholds_met==1) %>% 
  group_by(census_type) %>% 
  summarise(holdings_no_BRN=n())
# Add total row
BRN_holdings_NA_threshold <- BRN_holdings_NA_threshold %>%
  bind_rows(c(census_type=15, holdings_no_BRN=sum(BRN_holdings_NA_threshold$holdings_no_BRN[BRN_holdings_NA_threshold$census_type %in% c(1:8)])))


##Create Table A - businesses and holdings for all farm types 
Table_A <- BRN_holdings %>% 
  full_join(BRN_holdings_NA, by="census_type")
Table_A <- Table_A %>% 
 rbind(BRN_holdings_all_farms_total, BRN_holdings_FBS_types_total, BRN_holdings_threshold_row)
Table_A <- Table_A %>% 
  mutate(businesses=businesses) %>% 
  mutate(holdings=holdings) %>% 
  mutate(holdings_per_business=holdings/businesses)

##Create Table A1 - businesses and holdings that meet FBS criteria
Table_A1 <- BRN_holdings_threshold %>% 
  full_join(BRN_holdings_NA_threshold, by="census_type") %>% 
  filter(census_type < 9)
Table_A1[is.na(Table_A1)==T]=0
Table_A1 <- Table_A1 %>% 
  bind_rows(mutate(BRN_holdings_threshold_row, census_type=14))





##Now to create Table B - counting the number of holdings of each farmtype businesses have

##Count the number of holdings of each farmtype that each business has
BRN_type <- census_data_process %>% 
  group_by(brn,census_type) %>% 
  summarise(count=n())

#Loop over the different farmtypes, counting number of businesses with numbers of holdings in the given range 
for (i in 1:13){
  BRN_type_filter <- BRN_type %>%
    filter(census_type==i,is.na(brn)==F)
  BRN_type_filter <- as.data.frame(table(cut(BRN_type_filter$count,breaks=c(0,1,2,5,10,Inf))))
  names(BRN_type_filter)[1:2] <- c("Number range",paste0(census_type_words[i]))
  #output this as a dataframe with the census type in its name
  assign(paste("BRN_type",i,sep="_"), BRN_type_filter)
}
#Total for "All farm types"
BRN_type_14 <- census_data_process %>%
  filter(is.na(brn)==F) %>% 
  group_by(brn) %>% 
  summarise(count=n())
BRN_type_14 <- as.data.frame(table(cut(BRN_type_14$count,breaks=c(0,1,2,5,10,Inf))))
names(BRN_type_14)[1:2] <- c("Number range",paste0(census_type_words[14]))
#Total for "FBS farm types only"
BRN_type_15 <- census_data_process %>% 
  filter(is.na(brn)==F, census_type %in% 1:8) %>% 
  group_by(brn) %>% 
  summarise(count=n())
BRN_type_15 <- as.data.frame(table(cut(BRN_type_15$count,breaks=c(0,1,2,5,10,Inf))))
names(BRN_type_15)[1:2] <- c("Number range",paste0(census_type_words[15]))
#Total for "FBS farm types only, and meeting FBS thresholds"
BRN_type_16 <- census_data_process %>% 
  filter(FBS_thresholds_met==1, is.na(brn)==F) %>%
  group_by(brn) %>% 
  summarise(count=n())
BRN_type_16 <- as.data.frame(table(cut(BRN_type_16$count,breaks=c(0,1,2,5,10,Inf))))
names(BRN_type_16)[1:2] <- c("Number range",census_type_words[16])

#Combine the above into a single dataframe
#Steps are: create a list of the tables to be combined; combine them using "reduce"; remove the first column; transpose; convert to data.frame; add column names
Table_B <- lapply(paste0("BRN_type_",1:16), get)
Table_B <- Table_B %>% 
  reduce(full_join, by="Number range")
Table_B <- Table_B[2:17]
Table_B <- t(Table_B)
Table_B <- as.data.frame(Table_B)
colnames(Table_B) <- c("1","2","3-5","6-10","11+")







##Repeat Table B steps, but only for famrs meeting FBS criteria
# Filter the census dataset to only those holdings which meet FBS criteria
#This is also used later to create columns in Tables 1-5
census_fbs_threshold <- census_data_process %>%
  filter(FBS_thresholds_met==1)

BRN_type <- census_fbs_threshold %>% 
  group_by(brn, census_type) %>% 
  summarise(count=n())
#Loop over the different farmtypes, counting number of businesses with numbers of holdings in the given range 
for (i in 1:8){
  BRN_type_filter <- BRN_type %>%
    filter(census_type==i,is.na(brn)==F)
  BRN_type_filter <- as.data.frame(table(cut(BRN_type_filter$count,breaks=c(0,1,2,5,10,Inf))))
  names(BRN_type_filter)[1:2] <- c("Number range",paste0(census_type_words[i]))
  #output this as a dataframe with the census type in its name
  assign(paste("BRN_type",i,sep="_"), BRN_type_filter)
}
#Total for "All farm types"
BRN_type_14 <- census_fbs_threshold %>%
  filter(is.na(brn)==F) %>% 
  group_by(brn) %>% 
  summarise(count=n())
BRN_type_14 <- as.data.frame(table(cut(BRN_type_14$count,breaks=c(0,1,2,5,10,Inf))))
names(BRN_type_14)[1:2] <- c("Number range",paste0(census_type_words[14]))

#Combine the above into a single dataframe
#Steps are: create a list of the tables to be combined; combine them using "reduce"; remove the first column; transpose; convert to data.frame; add column names
Table_B1 <- lapply(paste0("BRN_type_",c(1:8,14)), get)
Table_B1 <- Table_B1 %>% 
  reduce(full_join, by="Number range")
Table_B1 <- Table_B1[2:10]
Table_B1 <- t(Table_B1)
Table_B1 <- as.data.frame(Table_B1)
colnames(Table_B1) <- c("1","2","3-5","6-10","11+")


#Calculate the number of businesses operating holdings of different types, and the number of holdings they own 
multiple_types <- census_data_process %>% 
  filter(is.na(brn)==F) %>% 
  group_by(brn) %>% 
  #the holdings calculation is a mean rather than a sum, as each row already has the number of holdings for the business as a whole
  summarise(count=n_distinct(census_type), holdings=mean(num_holdings)) %>% 
  filter(count>1)
businesses_multi_types_businesses <- nrow(multiple_types)
businesses_multi_types_holdings <- sum(multiple_types$holdings)
multiple_types_threshold <- census_fbs_threshold %>% 
  filter(is.na(brn)==F) %>% 
  group_by(brn) %>% 
  summarise(count=n_distinct(census_type), holdings=mean(num_holdings)) %>% 
  filter(count>1)
businesses_multi_types_businesses_threshold <- nrow(multiple_types_threshold)
##Need to join the multiple_types_threshold dataframe with the census data and count the rows.
##Using mean(num_holdings) would include holdings operated by these businesses buth which don't meet FBS thresholds
business_multi_types_threshold <- multiple_types_threshold %>% 
 left_join(census_fbs_threshold, by="brn", multiple="all")
businesses_multi_types_holdings_threshold <- nrow(business_multi_types_threshold)


# Change any NA values in the census_data_process dataframe to be 0s.
census_data_process[is.na(census_data_process)]=0

#Count the number of holdings which have/haven't completed a SAF form
saf_completed <- census_data_process %>% 
  group_by(saf_completed) %>% 
  summarise(count=n())
#Extract the number which have completed, for use in the RMarkdown documents
saf_holdings <- saf_completed$count[saf_completed==1]



#Extract the (rounded) number of farms who completed a cutdown census form, for use in the RMarkdown docs
#Reverse engineered the calculation for this from Kirsty Naylor's original report; best to speak to the census team with any questions
survtype <- census_data %>%
  group_by(survtype) %>%
  summarise(count=n())
cutdown_census <- round(sum(survtype$count[survtype$survtype %in% c("SAF", "SAF only")]), digits=-3)




##Create Table 1 - number of JAC farms meeting FBS and number in FBS sample
#Table 1a is the number in the census meeting the FBS threshold
Table_1a <- census_fbs_threshold %>% 
  group_by(census_type) %>% 
  summarise(above_fbs_threshold = n())
#Table 1b is the number in the FBS sample
Table_1b <- FBS_data_process %>% 
  group_by(type) %>% 
  summarise(fbs_sample = n()) %>% 
  rename(census_type=type)
#Table 1 combines Table 1a and 1b and adds a total row and a % column
Table_1 <- Table_1a %>% 
  full_join(Table_1b, by = "census_type") %>% 
  bind_rows(c(census_type=14, colSums(Table_1a[,2]), colSums(Table_1b[,2]))) %>% 
  mutate(percentage_of_census = round(100*fbs_sample/above_fbs_threshold, digits = 0))

##Create Table 2 - the number of holdings of each type which meet FBS criteria, as a percentage of the total of that type
#Table 2a is the total number of each farm type
Table_2a <- census_data_process %>% 
  group_by(census_type) %>% 
  summarise(census_population = n())
#Table 2b is the number of each type which meets FBS criteria
Table_2b <- census_fbs_threshold %>%
  group_by(census_type) %>% 
  summarise(above_fbs_threshold = n())
#Combine the two, add total rows (including and excluding non-FBS farmtypes), and a % column
Table_2 <- Table_2a %>% 
  full_join(Table_2b, by ="census_type") %>% 
  bind_rows(c(census_type=14,colSums(Table_2a[,2]),colSums(Table_2b[,2]))) %>% 
  bind_rows(c(census_type=15,colSums(filter(Table_2a,census_type<9)[,2]),colSums(Table_2b[,2]))) %>% 
  mutate(percentage_above_threshold = round(100*above_fbs_threshold/census_population, digits=0))

##Table 3 - as Table 2, but for farm areas
Table_3a <- census_data_process %>% 
  group_by(census_type) %>% 
  summarise(census_population=round(sum(total_area),digits=0))
Table_3b <- census_fbs_threshold %>%
  group_by(census_type) %>% 
  summarise(above_fbs_threshold=round(sum(total_area),digits=0))
Table_3 <- Table_3a %>% 
  left_join(Table_3b, by = "census_type") %>% 
  bind_rows(c(census_type=14,colSums(Table_3a[,2]),colSums(Table_3b[,2]))) %>% 
  bind_rows(c(census_type=15,colSums(filter(Table_3a,census_type<9)[,2]),colSums(Table_3b[,2]))) %>% 
  mutate(percentage_above_threshold = round(100*above_fbs_threshold/census_population, digits=0))


###Table 6 - as Table 3 but for sheep

census_fbs_threshold$sheep[is.na(census_fbs_threshold$sheep)]=0
Table_6a <- census_data_process %>% 
  group_by(census_type) %>% 
  summarise(census_population=round(sum(sheep),digits=0))
Table_6b <- census_fbs_threshold %>%
  group_by(census_type) %>% 
  summarise(above_fbs_threshold=round(sum(sheep),digits=0))
Table_6 <- Table_6a %>% 
  left_join(Table_6b, by = "census_type") %>% 
  bind_rows(c(census_type=14,colSums(Table_6a[,2]),colSums(Table_6b[,2]))) %>% 
  bind_rows(c(census_type=15,colSums(filter(Table_6a,census_type<9)[,2]),colSums(Table_6b[,2]))) %>% 
  mutate(percentage_above_threshold = round(100*above_fbs_threshold/census_population, digits=0))
           

##Table 4 - as Table 2, but for workforce
Table_4a <- census_data_process %>% 
  group_by(census_type) %>% 
  summarise(census_population=sum(totlab))
Table_4b <- census_fbs_threshold %>% 
  group_by(census_type) %>% 
  summarise(above_fbs_threshold=sum(totlab, na.rm = T))
Table_4 <- Table_4a %>% 
  left_join(Table_4b, by = "census_type") %>% 
  bind_rows(c(census_type=14,colSums(Table_4a[,2]),colSums(Table_4b[,2]))) %>% 
  bind_rows(c(census_type=15,colSums(filter(Table_4a,census_type<9)[,2]),colSums(Table_4b[,2]))) %>% 
  mutate(percentage_above_threshold = round(100*above_fbs_threshold/census_population, digits=0))

##Table 5 - as Table 2, but for standard output
Table_5a <- census_data_process %>% 
  group_by(census_type) %>% 
  summarise(census_population = round(sum(sumso),digits=0))
Table_5b <- census_fbs_threshold %>% 
  group_by(census_type) %>% 
  summarise(above_fbs_threshold = round(sum(sumso),digits=0))
Table_5 <- Table_5a %>% 
  left_join(Table_5b, by= "census_type") %>% 
  bind_rows(c(census_type=14,colSums(Table_5a[,2]),colSums(Table_5b[,2]))) %>% 
  bind_rows(c(census_type=15,colSums(filter(Table_5a,census_type<9)[,2]),colSums(Table_5b[,2]))) %>% 
  mutate(percentage_above_threshold = round(100*above_fbs_threshold/census_population, digits=0))

## Apply farmtype formats (i.e., change "1" to "Cereals", "2" to "General cropping" etc.)
#Create a list of tables which need this done, then use lapply to call the apply_type_formats function on each item in the list
#The output of this is still a list, so use list2env to un-listify them
list_of_tables <- list(Table_1=Table_1, Table_2=Table_2, Table_3=Table_3, Table_6=Table_6, Table_4=Table_4, Table_5=Table_5, Table_A = Table_A, Table_A1=Table_A1)
list_of_tables <- lapply(list_of_tables, apply_type_formats)
list2env(list_of_tables, .GlobalEnv)

## Tidy up the tables for publishing
#Select columns and rename them. Also add a total holdings column to Table A.
Table_1_names <- c("FBS farm type classification", 
                   paste0(crop_year," Census farms with FBS thresholds"), 
                   paste0("FBS sample ",crop_year,"-",year+1), 
                   "% of census")
Table_1_out <- Table_1 %>% 
  select(farmtype, above_fbs_threshold, fbs_sample, percentage_of_census)
names(Table_1_out) <- Table_1_names

Table_2_5_names <- c("Farm type classification",
                   "Total census population",
                   paste0(crop_year," Census farms with FBS thresholds"),
                   "% of total census")
Table_2_out <- Table_2 %>% 
  select(farmtype, census_population, above_fbs_threshold, percentage_above_threshold)
names(Table_2_out) <- Table_2_5_names

Table_3_out <-Table_3 %>% 
  select(farmtype, census_population, above_fbs_threshold, percentage_above_threshold)
names(Table_3_out) <- Table_2_5_names

Table_6_out <- Table_6 %>% 
  select(farmtype, census_population, above_fbs_threshold, percentage_above_threshold)
names(Table_6_out) <- Table_2_5_names

Table_4_out <- Table_4 %>% 
  select(farmtype, census_population, above_fbs_threshold, percentage_above_threshold)
names(Table_4_out) <- Table_2_5_names

Table_5_out <- Table_5 %>% 
  select(farmtype, census_population, above_fbs_threshold, percentage_above_threshold)
names(Table_5_out) <- Table_2_5_names

Table_A_names <- c("Farm type classification", "Businesses with BRN", "Holdings with BRN", "Holdings without BRN", "Total holdings")
Table_A_out <- Table_A %>% 
  select(farmtype, businesses, holdings, holdings_no_BRN)
Table_A_out <- Table_A_out %>% 
  mutate(Total_holdings = rowSums(Table_A_out[,3:4]))
names(Table_A_out) <- Table_A_names

Table_A1_out <- Table_A1 %>% 
  select(farmtype, businesses, holdings, holdings_no_BRN)
Table_A1_out <- Table_A1_out %>% 
  mutate(Total_holdings = rowSums(Table_A1_out[,3:4]))
names(Table_A1_out) <- Table_A_names

Table_B_out <- Table_B %>% 
  mutate("Farm type classification"=row.names(Table_B)) %>% 
  mutate("Total businesses" = rowSums((Table_B[,1:5]))) %>% 
  select(c(6), everything())
row.names(Table_B_out) <- NULL

Table_B1_out <- Table_B1 %>% 
  mutate("Farm type classification"=row.names(Table_B1)) %>% 
  mutate("Total businesses" = rowSums((Table_B1[,1:5]))) %>% 
  select(c(6), everything())
row.names(Table_B1_out) <- NULL


##Create a Summary table for the very top of the document, pulling in key data from Tables 3-5.
##The as.data.frame commands suddenly became necessary in order to allow the "Summary_table[1]" subsetting in the next line.
##Not at all sure why it worked without them before.
Summary_table <- as.data.frame(Table_2_out) %>%
  mutate("Measure"="Holdings") %>% 
  rbind(mutate(as.data.frame(Table_3_out),"Measure"="Area (ha)")) %>%
  rbind(mutate(as.data.frame(Table_4_out),"Measure"="Workforce")) %>%
  rbind(mutate(as.data.frame(Table_5_out),"Measure"="Standard output (\u00A3)"))
Summary_table <- Summary_table[Summary_table[1]=="All farm types",] %>% 
  select(Measure, everything(), -"Farm type classification")
colnames(Summary_table) <- c("Measure", paste0("Total ",crop_year," census population"), paste0(crop_year," Census farms with FBS thresholds"), paste0(crop_year," census population represented by FBS (%)"))
rownames(Summary_table) <- NULL

##Also output tables in Excel format
output_tables <- list(Summary_table,Table_1_out, Table_2_out, Table_3_out, Table_4_out, Table_5_out, Table_A_out, Table_B_out)
write_xlsx(output_tables, paste0(output_file_title,".xlsx"))


write.csv(Table_6_out,"countingsheep.csv")
