#title: "Carbon audit QA Analysis"
#author: "David French"



###All farms in sample
CA_Curr <- CA %>% 
  filter(ys_year==Curr_year)
CA_Prev <- CA %>% 
  filter(ys_year==Prev_year)
BothYearsAllFarms <- rbind(CA_Curr,CA_Prev)
Fig3 <- BothYearsAllFarms %>% 
  group_by(Type,ys_year) %>% 
  summarize(count=n(),direct=sum(total_de),indirect=sum(total_ie),methane=sum(total_me),nitrous=sum(total_no))
Fig3a <- Fig3 %>% 
  gather('nitrous', 'methane','indirect','direct',key="Emission_type",value="Emissions")

p <- ggplot(Fig3a, aes(x=as.factor(ys_year),y=Emissions,fill=Emission_type)) +
  geom_bar(position="fill",stat="identity") +
  facet_wrap(~ Type,scales="free_x")
p + xlab("Farm type") + ylab("Fraction of emissions")
ggsave("Figure_emissions_both_years_all_farms.png",path="QA")

p <- ggplot(BothYearsAllFarms, aes(x=as.factor(ys_year), y=ghg_ha, fill=as.factor(ys_year))) +
  geom_boxplot()  +
  theme_bw() + 
  ylab("Gross emissions (co2eq.kg/ha)") +
  facet_wrap(~ Type,scales="free_x")    
p
ggsave("Figure_2_total_ghgha_both_years_all_farms.png",path="QA")

#Figure 1 main plot by kg and ghg scatter```
p <-ggplot(BothYearsAllFarms, aes(x=fo_kg_ha, y=ghg_ha, col=as.factor(ys_year))) +
  geom_point(size=2, alpha=1) + 
  theme(legend.title=element_blank())+  
  theme_bw() +
  scale_size(range = c(0, 5), name="Farm Type") +
  facet_wrap (~Type)
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq.kg/ha)")    
ggsave("Figure_1_total_ghg_scatter.png",path="QA")  


###Only consider farms present in both years


CA_Curr <- CA %>% 
  filter(ys_year==Curr_year)
CA_Prev <- CA %>% 
  filter(ys_year==Prev_year)

CA_in_both <- CA_Curr %>% 
  inner_join(CA_Prev,by="f_number") %>% 
  select(f_number)

CA_Curr <- CA_Curr %>% 
  inner_join(CA_in_both,by="f_number")
CA_Prev <- CA_Prev %>% 
  inner_join(CA_in_both,by="f_number")
##List of farms in matched sample which have changed farm type

MismatchedTypes <- CA_in_both %>% 
  left_join(CA_Curr,by="f_number") %>% 
  select(f_number,farmtype_Curr=farmtype) %>% 
  left_join(CA_Prev,by="f_number") %>% 
  select(f_number,farmtype_Curr,farmtype_Prev=farmtype) %>% 
  filter(farmtype_Curr!=farmtype_Prev)

MatchedTypes <- CA_in_both %>% 
  left_join(CA_Curr,by="f_number") %>% 
  select(f_number,farmtype_Curr=farmtype) %>% 
  left_join(CA_Prev,by="f_number") %>% 
  select(f_number,farmtype_Curr,farmtype_Prev=farmtype) %>% 
  filter(farmtype_Curr==farmtype_Prev) %>% 
  select(f_number)

CA_Curr <- CA_Curr %>% 
  inner_join(MatchedTypes,by="f_number")
CA_Prev <- CA_Prev %>% 
  inner_join(MatchedTypes,by="f_number")
BothYears <- rbind(CA_Curr,CA_Prev)

## Make graphs for matched sample
Fig3 <- BothYears %>% 
  group_by(Type,ys_year) %>% 
  summarize(count=n(),direct=sum(total_de),indirect=sum(total_ie),methane=sum(total_me),nitrous=sum(total_no))
Fig3a <- Fig3 %>% 
  gather('nitrous', 'methane','indirect','direct',key="Emission_type",value="Emissions")

p <- ggplot(Fig3a, aes(x=as.factor(ys_year),y=Emissions,fill=Emission_type)) +
  geom_bar(position="fill",stat="identity") +
  facet_wrap(~ Type,scales="free_x")
p + xlab("Farm type") + ylab("Fraction of emissions")
ggsave("Figure_emissions_both_years_matched_type.png",path="QA")

p <- ggplot(BothYears, aes(x=as.factor(ys_year), y=ghg_ha, fill=as.factor(ys_year))) +
  geom_boxplot()  +
  theme_bw() + 
  ylab("Gross emissions (co2eq.kg/ha)") +
  facet_wrap(~ Type,scales="free_x")    
p
ggsave("Figure_2_total_ghgha_both_years_matched_type.png",path="QA")



#Testing code to look at correlations between financial and environmental variables
aes_now <- function(...) {
  structure(list(...),  class = "uneval")
}
fin_env_plot <- function(x_var, y_var) {
  x_var <- sym(x_var)
  y_var <- sym(y_var)
  p <- ggplot(BothYearsAllFarms, aes(x=!!x_var, y=!!y_var)) +
    geom_point() + 
    geom_smooth(method="lm")+
    facet_wrap(~ Type, scales = "free")
  p
}

fin_env_plot("fa_cinc", "ghg_ha")
fin_env_plot("out_inp", "ghg_ha")
fin_env_plot("fbi", "ghg_kgout")
fin_env_plot("fbi", "ghg_ha")
fin_env_plot("out_inp", "nue")
fin_env_plot("fa_cinc", "farm_n_surplus")