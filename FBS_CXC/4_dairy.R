#title: "Dairy Analysis"
#author: "Andrew Barnes"

p <-ggplot(D, aes(x=fo_kg_ha, y=ghg_ha, size = ndc)) +
  geom_point(color='darkslategrey', alpha=1) + 
  theme_bw() + 
  scale_size(range = c(0.1, 5), name="Dairy Cows")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")
ggsave("d_kg_ghg_scatter.png",path="Figures") 

summary(D$ghg_ha)
summary(D$fo_kg_ha)

D$s_ghgha <- scale(D$ghg_ha) # standardize variables
D$s_kgha <- scale(D$fo_kg_ha) # standardize variables

df2 <- data.frame(D$s_kgha, D$s_ghgha)

plot(df2)

km <- kmeans(df2, centers=3) #this clusters on the mean distance - note the number of clusters change
kmeansRes<-factor(km$cluster)
km$centers
km$size
s.class(df2,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))  #should print out the clustergram

ggsave("D_clusterplot.png",path="Figures")

D$cluster <- km$cluster
D$Cluster <- as.factor(D$cluster)

##just playing to see how to show the clusters
p <-ggplot(D, aes(x=fo_kg_ha, y=ghg_ha, size=ndc, col = Cluster)) +
  geom_point(alpha=1) +
  theme_bw() + 
  scale_size(range = c(0.1, 5), name="Dairy Cows")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")
ggsave("d_kg_ghg_scatter_clust.png",path="Figures") 

table(D$cluster)

png("Figures/D_clusterbyghg.png")
par(mfrow=c(1,4))
boxplot(tde_ha~cluster,data=D, main="Direct/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  
boxplot(tie_ha~cluster,data=D, main="Indirect/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")
boxplot(tch4_ha~cluster,data=D, main="Methane/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")   
boxplot(tn20_ha~cluster,data=D, main="NOx/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")    
dev.off()
dev.off()

png("Figures/D_cluster_box.png")
par(mfrow=c(1,2))
boxplot(ghg_ha~cluster,data=D, main="Gross emissions/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  

boxplot(fo_kg_ha~cluster,data=D, main="Kg Output/Ha",
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
da <- D %>% select(cluster, fa_aaua, op_ha, vc_ha, gm_ha, nm_ha,fbi_ha,slr_ha,sck_glu, sd_glu, sd_dc,lspec,feed_lu,homefeed_per,cspec,ifert_ha,arable_ha, fodder_ha,grass_ha,rough_grazing_ha, wood_aua_p,fwood_aua_p,nue, farm_n_surplus, dsecxx613,dseczz612,outputinputlesssubsratio, fo_kg_ha,netem_ha, ghg_ha, tde_ha, tie_ha, tch4_ha,tn20_ha)
t <- describe(da)
write.csv(t, file="CSV_Output/D_cluster_table2a.csv")
tt <- describeBy(da, da$cluster)
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/D_cluster_table2b.csv")


a <- kruskal.test(fa_aaua ~ cluster, data = D) #area

b <- kruskal.test(op_ha ~ cluster, data = D) # financial
c <- kruskal.test(vc_ha ~ cluster, data = D)# financial
d <- kruskal.test(gm_ha ~ cluster, data = D)# financial
e <- kruskal.test(nm_ha ~ cluster, data = D)# financial
f <- kruskal.test(fbi_ha ~ cluster, data = D)# financial

g <- kruskal.test(slr_ha ~ cluster, data = D)# labourintensity

h <- kruskal.test(sck_glu ~ cluster, data = D)# Animal_Specialisation
i <- kruskal.test(sd_dc ~ cluster, data = D)# Animal_Specialisation

j <- kruskal.test(sd_glu ~ cluster, data = D)# Livestock Intensity
k <- kruskal.test(lspec ~ cluster, data = D)# Livestock Intensity
kf <- kruskal.test(feed_lu  ~ cluster, data = D)# Livestock Intensity
kl <- kruskal.test(homefeed_per  ~ cluster, data = D)# Livestock_Management

l <- kruskal.test(cspec ~ cluster, data = D)# Crop Intensity
lf <- kruskal.test(ifert_ha ~ cluster, data = D)# Crop Intensity

m <- kruskal.test(arable_ha ~ cluster, data = D)# Land Diversity
n <- kruskal.test(fodder_ha ~ cluster, data = D)# Land Diversity
o <- kruskal.test(grass_ha ~ cluster, data = D)# Land Diversity
p <- kruskal.test(rough_grazing_ha ~ cluster, data = D)# Land Diversity
q <- kruskal.test(wood_aua_p ~ cluster, data = D)# Land Diversity
r <- kruskal.test(fwood_aua_p ~ cluster, data = D)# Land Diversity

s <- kruskal.test(nue ~ cluster, data = D)# Nutrient Management
t <- kruskal.test(farm_n_surplus ~ cluster, data = D)# Nutrient Management

u <- kruskal.test(dsecxx613 ~ cluster, data = D)# Energy_Diversification
v <- kruskal.test(dseczz612 ~ cluster, data = D)# Energy_Diversification

w <- kruskal.test(outputinputlesssubsratio ~ cluster, data = D)# Productivity
x <- kruskal.test(fo_kg_ha ~ cluster, data = D)# Output Indicator

y <- kruskal.test(netem_ha ~ cluster, data = D)# GHG Indicator
yg <- kruskal.test(ghg_ha ~ cluster, data = D)# GHG Indicator
y1 <- kruskal.test(tde_ha ~ cluster, data = D)# GHG Indicator
y2 <- kruskal.test(tie_ha ~ cluster, data = D)# GHG Indicator
y3 <- kruskal.test(tch4_ha ~ cluster, data = D)# GHG Indicator
y4 <- kruskal.test(tn20_ha ~ cluster, data = D)# GHG Indicator

df <- cbind(a,b,c,d,e,f,g,h,i,j,k,kf,kl,l,lf,m,n,o,p,q,r,s,t,u,v,w,x,y,yg,y1,y2,y3,y4)
write.csv(df, file="CSV_Output/D_cluster_table2_KW.csv")

write.csv(D, file="CSV_Output/D_All_Clustered.csv")

# a p value <0.05 means there is a significant difference between practices


#Main NUE Indicators
ggplot(D, aes(x=fo_kg_ha, y=nue_p, size = ndc)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Dairy Cows")
ggsave("D_nue_product.png",path="Figures")  

ggplot(D, aes(x=op_ha, y=nue_p, size = ndc)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Number of Dairy Cows")
ggsave("D_nue_opprof.png",path="Figures") 


ggplot(D, aes(x=nue_p, y=ghg_ha, size = ndc)) +
  geom_point(alpha=0.5) + 
  stat_smooth(method=lm) +
  scale_size(range = c(0.1, 5), name="Number of Dairy Cows")
ggsave("D_nue_ghg.png",path="Figures") 

ggplot(D, aes(x=farm_n_surplus, y=ghg_ha, size = ndc)) +
  geom_point(alpha=0.5) + 
  stat_smooth(method=lm) +
  scale_size(range = c(0.1, 5), name="Number of Dairy Cows")
ggsave("D_nue_ghg_nsurplus.png",path="Figures")

write.csv(D, file="CSV_Output/Dairy_All_Clustered.csv")

