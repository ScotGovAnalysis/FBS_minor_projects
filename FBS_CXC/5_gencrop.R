#title: "General Cropping Analysis"
#author: "Andrew Barnes"

GC$crop_specialisation <- GC$cspec
p <- ggplot(GC, aes(x=fo_kg_ha, y=ghg_ha, size=arable_ha, colour=crop_specialisation)) +
  geom_point(alpha=1) + scale_colour_gradient(low="darkslategray1", high="darkslategray")+
  theme_bw() + 
  scale_size(range = c(0.1, 5), name="Arable Area (Prop.)")
p + xlab("Product (kg/ha)") + ylab("Gross emissions (co2eq kg/ha)")   
ggsave("GC_total_ghg.png",path="Figures") 


par(mfrow=c(1,1))
GC$s_opha <- scale(GC$op_ha) # standardize variables
GC$s_ghgha <- scale(GC$ghg_ha) # standardize variables
GC$s_kgha <- scale(GC$fo_kg_ha) # standardize variables
df1 <- data.frame(GC$s_opha, GC$s_ghgha) # set the data frame (more could be added here)
df2 <- data.frame(GC$s_kgha, GC$s_ghgha)
plot(df2)
km <- kmeans(df2, centers=3) #this clusters on the mean distance - note the number of clusters change
kmeansRes<-factor(km$cluster)
km$centers
km$size
s.class(df2,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))  #should print out the clustergram
ggsave("gc_cluster_3plot.png",path="Figures")

GC$cluster <- km$cluster
table(GC$cluster)

#Descriptives across Clusters

png("Figures/GC_sclusterbyghg.png")
par(mfrow=c(1,4))
boxplot(tde_ha~cluster,data=GC, main="Direct/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  
boxplot(tie_ha~cluster,data=GC, main="Indirect/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")
boxplot(tch4_ha~cluster,data=GC, main="Methane/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")   
boxplot(tn20_ha~cluster,data=GC, main="NOx/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")    
dev.off()
dev.off()

png("Figures/GC_cluster_box.png")
par(mfrow=c(1,2))
boxplot(ghg_ha~cluster,data=GC, main="Gross emissions/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="CO2e/Ha")  

boxplot(fo_kg_ha~cluster,data=GC, main="Kg Output/Ha",
        col=(c("darkslategrey")),
        xlab="Clusters", ylab="Op/Ha")  
dev.off()
dev.set(dev.next())

#' Tables of management practices by cluster**
#' This is a little fiddly - the code produces main descriptives and then by each cluster.
#' The code then conducts a non-parametric ANOVA - Kruskal Wallis.  
#' Need to reorganise in excel Tab 2b and Tab 2b KW to show diff by cluster and sig.
#' @@ignore some of the warnings here - all seems to work


require(psych)
da <- GC %>% select(cluster, fa_aaua, op_ha, vc_ha, gm_ha, nm_ha,fbi_ha,slr_ha,sck_glu, sd_glu, sd_dc,lspec,feed_lu,homefeed_per,cspec,ifert_ha,arable_ha, fodder_ha,grass_ha,rough_grazing_ha, wood_aua_p,fwood_aua_p,nue, farm_n_surplus, dsecxx613,dseczz612,outputinputlesssubsratio, fo_kg_ha,netem_ha, ghg_ha, tde_ha, tie_ha, tch4_ha,tn20_ha)
t <- describe(da)
write.csv(t, file="CSV_Output/GC_cluster_table2a.csv")
tt <- describeBy(da, da$cluster)
tt2<-do.call("rbind",tt)  
write.csv(tt2, file="CSV_Output/GC_cluster_table2b.csv")

# As numbers are small we run a non-parametric test of association


a <- kruskal.test(fa_aaua ~ cluster, data = GC) #area

b <- kruskal.test(op_ha ~ cluster, data = GC) # financial
c <- kruskal.test(vc_ha ~ cluster, data = GC)# financial
d <- kruskal.test(gm_ha ~ cluster, data = GC)# financial
e <- kruskal.test(nm_ha ~ cluster, data = GC)# financial
f <- kruskal.test(fbi_ha ~ cluster, data = GC)# financial

g <- kruskal.test(slr_ha ~ cluster, data = GC)# labourintensity

h <- kruskal.test(sck_glu ~ cluster, data = GC)# Animal_Specialisation
i <- kruskal.test(sd_dc ~ cluster, data = GC)# Animal_Specialisation

j <- kruskal.test(sd_glu ~ cluster, data = GC)# Livestock Intensity
k <- kruskal.test(lspec ~ cluster, data = GC)# Livestock Intensity
kf <- kruskal.test(feed_lu  ~ cluster, data = GC)# Livestock Intensity
kl <- kruskal.test(homefeed_per  ~ cluster, data = GC)# Livestock_Management

l <- kruskal.test(cspec ~ cluster, data = GC)# Crop Intensity
lf <- kruskal.test(ifert_ha ~ cluster, data = GC)# Crop Intensity

m <- kruskal.test(arable_ha ~ cluster, data = GC)# Land Diversity
n <- kruskal.test(fodder_ha ~ cluster, data = GC)# Land Diversity
o <- kruskal.test(grass_ha ~ cluster, data = GC)# Land Diversity
p <- kruskal.test(rough_grazing_ha ~ cluster, data = GC)# Land Diversity
q <- kruskal.test(wood_aua_p ~ cluster, data = GC)# Land Diversity
r <- kruskal.test(fwood_aua_p ~ cluster, data = GC)# Land Diversity

s <- kruskal.test(nue ~ cluster, data = GC)# Nutrient Management
t <- kruskal.test(farm_n_surplus ~ cluster, data = GC)# Nutrient Management

u <- kruskal.test(dsecxx613 ~ cluster, data = GC)# Energy_Diversification
v <- kruskal.test(dseczz612 ~ cluster, data = GC)# Energy_Diversification

w <- kruskal.test(outputinputlesssubsratio ~ cluster, data = GC)# Productivity
x <- kruskal.test(fo_kg_ha ~ cluster, data = GC)# Output Indicator

y <- kruskal.test(netem_ha ~ cluster, data = GC)# GHG Indicator
yg <- kruskal.test(ghg_ha ~ cluster, data = GC)# GHG Indicator
y1 <- kruskal.test(tde_ha ~ cluster, data = GC)# GHG Indicator
y2 <- kruskal.test(tie_ha ~ cluster, data = GC)# GHG Indicator
y3 <- kruskal.test(tch4_ha ~ cluster, data = GC)# GHG Indicator
y4 <- kruskal.test(tn20_ha ~ cluster, data = GC)# GHG Indicator

df <- cbind(a,b,c,d,e,f,g,h,i,j,k,kf,kl,l,lf,m,n,o,p,q,r,s,t,u,v,w,x,y,yg,y1,y2,y3,y4)
write.csv(df, file="CSV_Output/gc_cluster_KW.csv")

##save the file for further analysis```
write.csv(GC, file="CSV_Output/GC_All_Clustered.csv")

#Main NUE Indicators

ggplot(GC, aes(x=fo_kg_ha, y=nue_p, size = fa_aaua)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")
ggsave("GC_nue_product.png",path="Figures")  

ggplot(GC, aes(x=op_ha, y=nue_p, size = fa_aaua)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")
ggsave("GC_nue_opprof.png",path="Figures") 

ggplot(GC, aes(x=ghg_ha, y=nue_p, size = fa_aaua)) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(0.1, 5), name="Adjusted Area")
ggsave("GC_nue_ghg.png",path="Figures") 

write.csv(GC, file="CSV_Output/GC_All_Clustered.csv")

