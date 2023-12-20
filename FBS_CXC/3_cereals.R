#title: "Cereal Analysis"
#author: "Andrew Barnes"

C$crop_specialisation <- C$cspec

p <- ggplot(C, aes(x=fo_kg_ha, y=ghg_ha, size=arable_ha, colour=crop_specialisation)) +
  geom_point(alpha=1) + scale_colour_gradient(low="darkslategray1", high="darkslategray")+
  theme_bw() + 
  scale_size(range = c(0.1, 5), name="Arable Area (Prop.)")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")
ggsave("cer_kg_ghg_scatter.png",path="Figures")  

p <- ggplot(C, aes(x=farm_n_surplus, y=ghg_ha, size = arable_ha)) +
  geom_point(color='darkslategrey', alpha=0.6) + 
  scale_size(range = c(0.1, 5), name="Arable/total area")
p + xlab("Product (kg/ha)") + ylab("Farm N surplus (kg)")
ggsave("cer_nsurplus_ghg_scatter.png",path="Figures") 


par(mfrow=c(1,1))
C$s_ghgha <- scale(C$ghg_ha) # standardize variables
C$s_kgha <- scale(C$fo_kg_ha) # standardize variables
df2 <- data.frame(C$s_kgha, C$s_ghgha)

plot(df2)

km <- kmeans(df2, centers=3) #this clusters on the mean distance - note the number of clusters change
kmeansRes<-factor(km$cluster)
km$centers
km$size
s.class(df2,fac=kmeansRes, add.plot=TRUE, col=c("blue","green","purple","red"))  #should print out the clustergram
ggsave("C_clusterplot_3.png",path="Figures")

C$cluster <- km$cluster
table(C$cluster)

png("Figures/C_clusterbyghg.png")
par(mfrow=c(1,4))
boxplot(tde_ha~cluster,data=C, main="Direct/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  
boxplot(tie_ha~cluster,data=C, main="Indirect/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")
boxplot(tch4_ha~cluster,data=C, main="Methane/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")   
boxplot(tn20_ha~cluster,data=C, main="NOx/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")    
dev.off()
dev.off()

png("Figures/C_cluster_box.png")
par(mfrow=c(1,2))
boxplot(ghg_ha~cluster,data=C, main="Gross emissions/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  

boxplot(fo_kg_ha~cluster,data=C, main="Kg Output/Ha",
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
da <- C %>% select(cluster, fa_aaua, op_ha, vc_ha, gm_ha, nm_ha,fbi_ha,slr_ha,sck_glu, sd_glu, sd_dc,lspec,feed_lu,homefeed_per,cspec,ifert_ha,arable_ha, fodder_ha,grass_ha,rough_grazing_ha, wood_aua_p,fwood_aua_p,nue, farm_n_surplus, dsecxx613,dseczz612,outputinputlesssubsratio, fo_kg_ha,netem_ha, ghg_ha, tde_ha, tie_ha, tch4_ha,tn20_ha)
t <- describe(da)
write.csv(t, file="CSV_Output/C_cluster_table2a.csv")
tt <- describeBy(da, da$cluster)
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/C_cluster_table2b.csv")


a <- kruskal.test(fa_aaua ~ cluster, data = C) #area

b <- kruskal.test(op_ha ~ cluster, data = C) # financial
c <- kruskal.test(vc_ha ~ cluster, data = C)# financial
d <- kruskal.test(gm_ha ~ cluster, data = C)# financial
e <- kruskal.test(nm_ha ~ cluster, data = C)# financial
f <- kruskal.test(fbi_ha ~ cluster, data = C)# financial

g <- kruskal.test(slr_ha ~ cluster, data = C)# labourintensity

h <- kruskal.test(sck_glu ~ cluster, data = C)# Animal_Specialisation
i <- kruskal.test(sd_dc ~ cluster, data = C)# Animal_Specialisation

j <- kruskal.test(sd_glu ~ cluster, data = C)# Livestock Intensity
k <- kruskal.test(lspec ~ cluster, data = C)# Livestock Intensity
kf <- kruskal.test(feed_lu  ~ cluster, data = C)# Livestock Intensity
kl <- kruskal.test(homefeed_per  ~ cluster, data = C)# Livestock_Management

l <- kruskal.test(cspec ~ cluster, data = C)# Crop Intensity
lf <- kruskal.test(ifert_ha ~ cluster, data = C)# Crop Intensity

m <- kruskal.test(arable_ha ~ cluster, data = C)# Land Diversity
n <- kruskal.test(fodder_ha ~ cluster, data = C)# Land Diversity
o <- kruskal.test(grass_ha ~ cluster, data = C)# Land Diversity
p <- kruskal.test(rough_grazing_ha ~ cluster, data = C)# Land Diversity
q <- kruskal.test(wood_aua_p ~ cluster, data = C)# Land Diversity
r <- kruskal.test(fwood_aua_p ~ cluster, data = C)# Land Diversity

s <- kruskal.test(nue ~ cluster, data = C)# Nutrient Management
t <- kruskal.test(farm_n_surplus ~ cluster, data = C)# Nutrient Management

u <- kruskal.test(dsecxx613 ~ cluster, data = C)# Energy_Diversification
v <- kruskal.test(dseczz612 ~ cluster, data = C)# Energy_Diversification

w <- kruskal.test(outputinputlesssubsratio ~ cluster, data = C)# Productivity
x <- kruskal.test(fo_kg_ha ~ cluster, data = C)# Output Indicator

y <- kruskal.test(netem_ha ~ cluster, data = C)# GHG Indicator
yg <- kruskal.test(ghg_ha ~ cluster, data = C)# GHG Indicator
y1 <- kruskal.test(tde_ha ~ cluster, data = C)# GHG Indicator
y2 <- kruskal.test(tie_ha ~ cluster, data = C)# GHG Indicator
y3 <- kruskal.test(tch4_ha ~ cluster, data = C)# GHG Indicator
y4 <- kruskal.test(tn20_ha ~ cluster, data = C)# GHG Indicator

df <- cbind(a,b,c,d,e,f,g,h,i,j,k,kf,kl,l,lf,m,n,o,p,q,r,s,t,u,v,w,x,y,yg,y1,y2,y3,y4)
write.csv(df, file="CSV_Output/cers_cluster_KW.csv")

##save the file for further analysis```
write.csv(C, file="CSV_Output/C_All_Clustered.csv")


#Main NUE Indicators

ggplot(C, aes(x=fo_kg_ha, y=nue_p, size = fa_aaua, color=fa_orgpc)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")
ggsave("C_nue_product.png",path="Figures")  

ggplot(C, aes(x=op_ha, y=nue_p, size = fa_aaua)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")
ggsave("C_nue_opprof.png",path="Figures") 

ggplot(C, aes(x=ghg_ha, y=nue_p, size = fa_aaua)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")

ggsave("C_nue_ghg.png",path="Figures") 

ggplot(C, aes(x=farm_n_surplus, y=ghg_ha, size = fa_aaua)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")
ggsave("C_nue_ghg_nsurplus.png",path="Figures")


write.csv(C, file="CSV_Output/C_All_Clustered.csv")








