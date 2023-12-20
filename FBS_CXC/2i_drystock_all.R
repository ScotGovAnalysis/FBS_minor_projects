#title: "Drystock(include Lowland) Analysis"
#author: "Andrew Barnes"


p <-ggplot(ALLCS, aes(x=fo_kg_ha, y=ghg_ha, size=fa_glu, col=type)) +
  geom_point(alpha=0.6) +  
  theme(legend.title=element_blank())+
  scale_size(range = c(0.1, 5), name="Livestock Units")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")
ggsave("allcs_kg_ghg_scatter.png",path="Figures") 

par(mfrow=c(1,1))  
ALLCS$s_ghgha <- scale(ALLCS$ghg_ha) # standardize variables
ALLCS$s_kgha <- scale(ALLCS$fo_kg_ha) # standardize variables

df2 <- data.frame(ALLCS$s_kgha, ALLCS$s_ghgha)
plot(df2)
km <- kmeans(df2, centers=5) #this clusters on the mean distance - note the number of clusters change
kmeansRes<-factor(km$cluster)
km$centers
km$size
s.class(df2,fac=kmeansRes, add.plot=TRUE, col=c("blue","coral4","purple","red","black"))  
ggsave("ALLCS_clusterplot.png",path="Figures")

ALLCS$cluster <- km$cluster
table(ALLCS$cluster)

png("Figures/ALLCS_clusterbyghg.png")
par(mfrow=c(1,4))
boxplot(tde_ha~cluster,data=ALLCS, main="Direct/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  
boxplot(tie_ha~cluster,data=ALLCS, main="Indirect/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")
boxplot(tch4_ha~cluster,data=ALLCS, main="Methane/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")   
boxplot(tn20_ha~cluster,data=ALLCS, main="NOx/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")    
dev.off()
dev.off()


png("Figures/ALCS_cluster_box.png")
par(mfrow=c(1,2))
boxplot(ghg_ha~cluster,data=ALLCS, main="Gross emissions/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  

boxplot(fo_kg_ha~cluster,data=ALLCS, main="Kg output/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="Kg/Ha")  
dev.off()
dev.off()
dev.set(dev.next())

#' Tables of management practices by cluster**
#' This is a little fiddly - the code produces main descriptives and then by each cluster.
#' The code then conducts a non-parametric ANOVA - Kruskal Wallis.  
#' Need to reorganise in excel Tab 2b and Tab 2b KW to show diff by cluster and sig.
#' @@ignore some of the warnings here - all seems to work

require(psych)
da <- ALLCS %>% select(cluster, fa_aaua, op_ha, vc_ha, gm_ha, nm_ha,fbi_ha,slr_ha,sck_glu, sd_glu, sd_dc,lspec,feed_lu,homefeed_per,cspec,ifert_ha,arable_ha, fodder_ha,grass_ha,rough_grazing_ha, wood_aua_p,fwood_aua_p,nue, farm_n_surplus, dsecxx613,dseczz612,outputinputlesssubsratio, fo_kg_ha,netem_ha,ghg_ha, tde_ha, tie_ha, tch4_ha,tn20_ha)
t <- describe(da)
write.csv(t, file="CSV_Output/ALLCS_cluster_table2a.csv")
tt <- describeBy(da, da$cluster)
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="ALLCS_cluster_table2b.csv")

a <- kruskal.test(fa_aaua ~ cluster, data = ALLCS) #area
b <- kruskal.test(op_ha ~ cluster, data = ALLCS) # financial
c <- kruskal.test(vc_ha ~ cluster, data = ALLCS)# financial
d <- kruskal.test(gm_ha ~ cluster, data = ALLCS)# financial
e <- kruskal.test(nm_ha ~ cluster, data = ALLCS)# financial
f <- kruskal.test(fbi_ha ~ cluster, data = ALLCS)# financial
g <- kruskal.test(slr_ha ~ cluster, data = ALLCS)# labourintensity
h <- kruskal.test(sck_glu ~ cluster, data = ALLCS)# Animal_Specialisation
i <- kruskal.test(sd_dc ~ cluster, data = ALLCS)# Animal_Specialisation
j <- kruskal.test(sd_glu ~ cluster, data = ALLCS)# Livestock Intensity
k <- kruskal.test(lspec ~ cluster, data = ALLCS)# Livestock Intensity
kf <- kruskal.test(feed_lu  ~ cluster, data = ALLCS)# Livestock Intensity
kl <- kruskal.test(homefeed_per  ~ cluster, data = ALLCS)# Livestock_Management
l <- kruskal.test(cspec ~ cluster, data = ALLCS)# Crop Intensity
lf <- kruskal.test(ifert_ha ~ cluster, data = ALLCS)# Crop Intensity
m <- kruskal.test(arable_ha ~ cluster, data = ALLCS)# Land Diversity
n <- kruskal.test(fodder_ha ~ cluster, data = ALLCS)# Land Diversity
o <- kruskal.test(grass_ha ~ cluster, data = ALLCS)# Land Diversity
p <- kruskal.test(rough_grazing_ha ~ cluster, data = ALLCS)# Land Diversity
q <- kruskal.test(wood_aua_p ~ cluster, data = ALLCS)# Land Diversity
r <- kruskal.test(fwood_aua_p ~ cluster, data = ALLCS)# Land Diversity

s <- kruskal.test(nue ~ cluster, data = ALLCS)# Nutrient Management
t <- kruskal.test(farm_n_surplus ~ cluster, data = ALLCS)# Nutrient Management

u <- kruskal.test(dsecxx613 ~ cluster, data = ALLCS)# Energy_Diversification
v <- kruskal.test(dseczz612 ~ cluster, data = ALLCS)# Energy_Diversification

w <- kruskal.test(outputinputlesssubsratio ~ cluster, data = ALLCS)# Productivity
x <- kruskal.test(fo_kg_ha ~ cluster, data = ALLCS)# Output Indicator

y <- kruskal.test(netem_ha ~ cluster, data = ALLCS)# GHG Indicator

yg <- kruskal.test(ghg_ha ~ cluster, data = ALLCS)# GHG Indicator
y1 <- kruskal.test(tde_ha ~ cluster, data = ALLCS)# GHG Indicator
y2 <- kruskal.test(tie_ha ~ cluster, data = ALLCS)# GHG Indicator
y3 <- kruskal.test(tch4_ha ~ cluster, data = ALLCS)# GHG Indicator
y4 <- kruskal.test(tn20_ha ~ cluster, data = ALLCS)# GHG Indicator

df <- cbind(a,b,c,d,e,f,g,h,i,j,k,kf,kl,l,lf,m,n,o,p,q,r,s,t,u,v,w,x,y,yg,y1,y2,y3,y4)
write.csv(df, file="CSV_Output/ALLCS_cluster_table2_KW.csv")
write.csv(ALLCS, file="CSV_Output/AllCS_All_Clustered.csv")

da <- ALLCS %>% select(cluster,n_cattle,n_sheep,n_sheep,n_milk, n_wholecrop,nue_p, nsurplus_p_ha )
tt <- describeBy(da, da$cluster)
tt
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/ALLCS_cluster_otherindicators.csv")
table(ALLCS$type, ALLCS$cluster)
# a p value <0.05 means there is a significant difference between practices

#Main NUE Indicators
ggplot(ALLCS, aes(x=fo_kg_ha, y=nue_p, size = fa_glu)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Livestock Units")
ggsave("ALLCS_nue_product.png",path="Figures")  

ggplot(ALLCS, aes(x=ghg_ha, y=nue_p, size = fa_glu)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Livestock Units")
ggsave("ALLCS_nue_ghg.png",path="Figures") 

ggplot(ALLCS, aes(x=farm_n_surplus, y=ghg_ha, size = fa_glu)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Livestock Units")
ggsave("ALLCS_nue_ghg_nsurplus.png",path="Figures")










