# title: "MixedAnalysis"
# author: "Andrew Barnes"

M$crop_specialisation <- M$cspec

p <- ggplot(M, aes(x=fo_kg_ha, y=ghg_ha, size=fa_aaua, colour=crop_specialisation)) +
  geom_point(alpha=1) + scale_colour_gradient(low="darkslategray1", high="darkslategray")+
  theme_bw() + 
  scale_size(range = c(0.1, 5), name="Adj.Area (ha)")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")    
ggsave("m_total_ghg.png",path="Figures")  


p <- ggplot(M, aes(x=farm_n_surplus, y=ghg_ha, size = arable_ha)) +
  geom_point(color='darkslategrey', alpha=0.6) + 
  scale_size(range = c(0.1, 4), name="Arable/total area")
p + xlab("Product (kg/ha)") + ylab("Farm N surplus (kg)")
ggsave("M_nsurplus_ghg_scatter.png",path="Figures") 


summary(M$fo_kg_ha)
#this shows a large outlier here which we remove - if we identify the type and explore```
Mi <- subset(M, M$fo_kg_ha<15755)
par(mfrow=c(1,1))
Mi$s_opha <- scale(Mi$op_ha) # standardize variables
Mi$s_ghgha <- scale(Mi$ghg_ha) # standardize variables
Mi$s_kgha <- scale(Mi$fo_kg_ha) # standardize variables
df1 <- data.frame(Mi$s_opha, Mi$s_ghgha) # set the data frame (more could be added here)
df2 <- data.frame(Mi$s_kgha, Mi$ghg_ha)
plot(df2)
km <- kmeans(df2, centers=4) #this clusters on the mean distance - note the number of clusters change
kmeansRes<-factor(km$cluster)
km$centers
km$size
s.class(df2,fac=kmeansRes, add.plot=TRUE, col=c("blue","black","green","red"))  #should print out the clustergram
ggsave("M_clusterplot.png",path="Figures")
Mi$cluster <- km$cluster
table(Mi$cluster)

png("Figures/M_clusterbyghg.png")
par(mfrow=c(1,4))
boxplot(tde_ha~cluster,data=Mi, main="Direct/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  
boxplot(tie_ha~cluster,data=Mi, main="Indirect/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")
boxplot(tch4_ha~cluster,data=Mi, main="Methane/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")   
boxplot(tn20_ha~cluster,data=Mi, main="NOx/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")    
dev.off()
dev.off()





#group the boxplots by types```

png("Figures/M_cluster_box.png")
par(mfrow=c(1,2))
boxplot(ghg_ha~cluster,data=Mi, main="Gross emissions/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  

boxplot(fo_kg_ha~cluster,data=Mi, main="Kg Output/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="Kg/Ha")  
dev.off()

#' Tables of management practices by cluster**
#' This is a little fiddly - the code produces main descriptives and then by each cluster.
#' The code then conducts a non-parametric ANOVA - Kruskal Wallis.  
#' Need to reorganise in excel Tab 2b and Tab 2b KW to show diff by cluster and sig.
#' @@ignore some of the warnings here - all seems to work

require(psych)
da <- Mi %>% select(cluster, fa_aaua, op_ha, vc_ha, gm_ha, nm_ha,fbi_ha,slr_ha,sck_glu, sd_glu, sd_dc,lspec,feed_lu,homefeed_per,cspec,ifert_ha,arable_ha, fodder_ha,grass_ha,rough_grazing_ha, wood_aua_p,fwood_aua_p,nue, farm_n_surplus, dsecxx613,dseczz612,outputinputlesssubsratio, fo_kg_ha,netem_ha, ghg_ha, tde_ha, tie_ha, tch4_ha,tn20_ha)
t <- describe(da)
write.csv(t, file="CSV_Output/M_cluster_table2a.csv")
tt <- describeBy(da, da$cluster)
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/M_cluster_table2b.csv")


a <- kruskal.test(fa_aaua ~ cluster, data = Mi) #area

b <- kruskal.test(op_ha ~ cluster, data = Mi) # financial
c <- kruskal.test(vc_ha ~ cluster, data = Mi)# financial
d <- kruskal.test(gm_ha ~ cluster, data = Mi)# financial
e <- kruskal.test(nm_ha ~ cluster, data = Mi)# financial
f <- kruskal.test(fbi_ha ~ cluster, data = Mi)# financial

g <- kruskal.test(slr_ha ~ cluster, data = Mi)# labourintensity

h <- kruskal.test(sck_glu ~ cluster, data = Mi)# Animal_Specialisation
i <- kruskal.test(sd_dc ~ cluster, data = Mi)# Animal_Specialisation

j <- kruskal.test(sd_glu ~ cluster, data = Mi)# Livestock Intensity
k <- kruskal.test(lspec ~ cluster, data = Mi)# Livestock Intensity
kf <- kruskal.test(feed_lu  ~ cluster, data = Mi)# Livestock Intensity
kl <- kruskal.test(homefeed_per  ~ cluster, data = Mi)# Livestock_Management

l <- kruskal.test(cspec ~ cluster, data = Mi)# Crop Intensity
lf <- kruskal.test(ifert_ha ~ cluster, data = Mi)# Crop Intensity

m <- kruskal.test(arable_ha ~ cluster, data = Mi)# Land Diversity
n <- kruskal.test(fodder_ha ~ cluster, data = Mi)# Land Diversity
o <- kruskal.test(grass_ha ~ cluster, data = Mi)# Land Diversity
p <- kruskal.test(rough_grazing_ha ~ cluster, data = Mi)# Land Diversity
q <- kruskal.test(wood_aua_p ~ cluster, data = Mi)# Land Diversity
r <- kruskal.test(fwood_aua_p ~ cluster, data = Mi)# Land Diversity

s <- kruskal.test(nue ~ cluster, data = Mi)# Nutrient Management
t <- kruskal.test(farm_n_surplus ~ cluster, data = Mi)# Nutrient Management

u <- kruskal.test(dsecxx613 ~ cluster, data = Mi)# Energy_Diversification
v <- kruskal.test(dseczz612 ~ cluster, data = Mi)# Energy_Diversification

w <- kruskal.test(outputinputlesssubsratio ~ cluster, data = Mi)# Productivity
x <- kruskal.test(fo_kg_ha ~ cluster, data = Mi)# Output Indicator

y <- kruskal.test(netem_ha ~ cluster, data = Mi)# GHG Indicator
yg <- kruskal.test(ghg_ha ~ cluster, data = Mi)# GHG Indicator
y1 <- kruskal.test(tde_ha ~ cluster, data = Mi)# GHG Indicator
y2 <- kruskal.test(tie_ha ~ cluster, data = Mi)# GHG Indicator
y3 <- kruskal.test(tch4_ha ~ cluster, data = Mi)# GHG Indicator
y4 <- kruskal.test(tn20_ha ~ cluster, data = Mi)# GHG Indicator

df <- cbind(a,b,c,d,e,f,g,h,i,j,k,kf,kl,l,lf,m,n,o,p,q,r,s,t,u,v,w,x,y,yg,y1,y2,y3,y4)
write.csv(df, file="CSV_Output/M_cluster_KW.csv")

write.csv(Mi, file="CSV_Output/Mi_All_Clustered.csv")



