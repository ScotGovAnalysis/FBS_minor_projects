
# title: "LFA Drystock"
# author: "Andrew Barnes"

#provides main scatter for report (fig a)
p <-ggplot(ALLLFA, aes(x=fo_kg_ha, y=ghg_ha, col=type)) +
  geom_point(size=2, alpha=0.6) +  
  theme_bw() + 
  theme(legend.title=element_blank())+
  scale_size(range = c(0.1, 5), name="Livestock Units")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")
ggsave("LFA_Drystock_kg_ghg_scatter.png",path="Figures") 

#clustering - used for appendix
par(mfrow=c(1,1))  
ALLLFA$s_ghgha <- scale(ALLLFA$ghg_ha) # standardize variables
ALLLFA$s_kgha <- scale(ALLLFA$fo_kg_ha) # standardize variables

df2 <- data.frame(ALLLFA$s_kgha, ALLLFA$s_ghgha)
plot(df2)
#note that this comes up with two solutions for LFA drystock: the numbers of clusters change can just simply 
#rerun the code block to renumber them
km <- kmeans(df2, centers=5) #this clusters on the mean distance -
kmeansRes<-factor(km$cluster)
km$centers
km$size
s.class(df2,fac=kmeansRes, add.plot=TRUE, col=c("blue","coral4","purple","red","black"))  
ggsave("ALLLFA_clusterplot.png",path="Figures")

ALLLFA$cluster <- km$cluster
table(ALLLFA$cluster)

#didnt use this but shows boxplots by emissions and by cluster
png("Figures/ALLLFA_clusterbyghg.png")
par(mfrow=c(1,4))
boxplot(tde_ha~cluster,data=ALLLFA, main="Direct/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  
boxplot(tie_ha~cluster,data=ALLLFA, main="Indirect/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")
boxplot(tch4_ha~cluster,data=ALLLFA, main="Methane/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")   
boxplot(tn20_ha~cluster,data=ALLLFA, main="NOx/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")    
dev.off()
dev.off()


png("Figures/ALLLFA_cluster_box.png")
par(mfrow=c(1,2))
boxplot(ghg_ha~cluster,data=ALLLFA, main="Gross emissions/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  

boxplot(fo_kg_ha~cluster,data=ALLLFA, main="Kg output/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="Kg/Ha")  
dev.off()
dev.set(dev.next())

#' Tables of management practices by cluster**
#' This is a little fiddly - the code produces main descriptives and then by each cluster.
#' The code then conducts a non-parametric ANOVA - Kruskal Wallis.  
#' Need to reorganise in excel Tab 2b and Tab 2b KW to show diff by cluster and sig.
#' @@ignore some of the warnings here - all seems to work


require(psych)
da <- ALLLFA %>% select(cluster, fa_aaua, op_ha, vc_ha, gm_ha, nm_ha,fbi_ha,slr_ha,sck_glu, sd_glu, sd_dc,lspec,feed_lu,homefeed_per,cspec,ifert_ha,arable_ha, fodder_ha,grass_ha,rough_grazing_ha, wood_aua_p,fwood_aua_p,nue, farm_n_surplus, dsecxx613,dseczz612,outputinputlesssubsratio, fo_kg_ha,netem_ha,ghg_ha, tde_ha, tie_ha, tch4_ha,tn20_ha)
t <- describe(da)
write.csv(t, file="CSV_Output/ALLLFA_cluster_table2a.csv")
tt <- describeBy(da, da$cluster)
##ignore warning message
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/ALLLFA_cluster_table2b.csv")

a <- kruskal.test(fa_aaua ~ cluster, data = ALLLFA) #area						
b <- kruskal.test(op_ha ~ cluster, data = ALLLFA) # financial						
c <- kruskal.test(vc_ha ~ cluster, data = ALLLFA)# financial						
d <- kruskal.test(gm_ha ~ cluster, data = ALLLFA)# financial						
e <- kruskal.test(nm_ha ~ cluster, data = ALLLFA)# financial						
f <- kruskal.test(fbi_ha ~ cluster, data = ALLLFA)# financial						
g <- kruskal.test(slr_ha ~ cluster, data = ALLLFA)# labourintensity						
h <- kruskal.test(sck_glu ~ cluster, data = ALLLFA)# Animal_Specialisation						
i <- kruskal.test(sd_dc ~ cluster, data = ALLLFA)# Animal_Specialisation						
j <- kruskal.test(sd_glu ~ cluster, data = ALLLFA)# Livestock Intensity						
k <- kruskal.test(lspec ~ cluster, data = ALLLFA)# Livestock Intensity						
kf <- kruskal.test(feed_lu  ~ cluster, data = ALLLFA)# Livestock Intensity						
kl <- kruskal.test(homefeed_per  ~ cluster, data = ALLLFA)# Livestock_Management						
l <- kruskal.test(cspec ~ cluster, data = ALLLFA)# Crop Intensity						
lf <- kruskal.test(ifert_ha ~ cluster, data = ALLLFA)# Crop Intensity						
m <- kruskal.test(arable_ha ~ cluster, data = ALLLFA)# Land Diversity						
n <- kruskal.test(fodder_ha ~ cluster, data = ALLLFA)# Land Diversity						
o <- kruskal.test(grass_ha ~ cluster, data = ALLLFA)# Land Diversity						
p <- kruskal.test(rough_grazing_ha ~ cluster, data = ALLLFA)# Land Diversity						
q <- kruskal.test(wood_aua_p ~ cluster, data = ALLLFA)# Land Diversity						
r <- kruskal.test(fwood_aua_p ~ cluster, data = ALLLFA)# Land Diversity						

s <- kruskal.test(nue ~ cluster, data = ALLLFA)# Nutrient Management						
t <- kruskal.test(farm_n_surplus ~ cluster, data = ALLLFA)# Nutrient Management						

u <- kruskal.test(dsecxx613 ~ cluster, data = ALLLFA)# Energy_Diversification						
v <- kruskal.test(dseczz612 ~ cluster, data = ALLLFA)# Energy_Diversification						

w <- kruskal.test(outputinputlesssubsratio ~ cluster, data = ALLLFA)# Productivity						
x <- kruskal.test(fo_kg_ha ~ cluster, data = ALLLFA)# Output Indicator						

y <- kruskal.test(netem_ha ~ cluster, data = ALLLFA)# GHG Indicator						

yg <- kruskal.test(ghg_ha ~ cluster, data = ALLLFA)# GHG Indicator						
y1 <- kruskal.test(tde_ha ~ cluster, data = ALLLFA)# GHG Indicator						
y2 <- kruskal.test(tie_ha ~ cluster, data = ALLLFA)# GHG Indicator						
y3 <- kruskal.test(tch4_ha ~ cluster, data = ALLLFA)# GHG Indicator						
y4 <- kruskal.test(tn20_ha ~ cluster, data = ALLLFA)# GHG Indicator						


df <- cbind(a,b,c,d,e,f,g,h,i,j,k,kf,kl,l,lf,m,n,o,p,q,r,s,t,u,v,w,x,y,yg,y1,y2,y3,y4)
write.csv(df, file="CSV_Output/ALLLFA_cluster_table2_KW.csv")

##write file with cluster solutions for further analysis
write.csv(ALLCS, file="CSV_Output/ALLLFA_All_Clustered.csv")

##additional exploring to explain clusters/outliers
da <- ALLLFA %>% select(cluster,n_cattle,n_sheep,n_sheep,n_milk, n_wholecrop,nue_p, nsurplus_p_ha )
tt <- describeBy(da, da$cluster)
tt
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/ALLLFA_cluster_otherindicators.csv")
table(ALLLFA$type, ALLLFA$cluster)

# a p value <0.05 means there is a significant difference between practices

#Main NUE Indicators - not used in report
ggplot(ALLCS, aes(x=fo_kg_ha, y=nue_p, size = fa_glu)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Livestock Units")
ggsave("ALLCS_nue_product.png",path="Figures")  

#by ghgs per ha and also sized by number of animals
ggplot(ALLCS, aes(x=ghg_ha, y=nue_p, size = fa_glu)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Livestock Units")
ggsave("ALLCS_nue_ghg.png",path="Figures") 

#tight but linear trend here
ggplot(ALLCS, aes(x=farm_n_surplus, y=ghg_ha, size = fa_glu)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Livestock Units")
ggsave("ALLCS_nue_ghg_nsurplus.png",path="Figures")