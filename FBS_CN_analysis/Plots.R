#Separate script for creating charts
source("resas_theme.R")
# Defining a function which rounds in an expected way (rather than, eg,  rounding 53.2500 to 53.2).
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}


##Function for converting the tables created in CN_analysis.R into a format better for feeding into ggplot.
Create_plot_df <- function(Input_table, variable){
  Output_table <- Input_table %>% 
    gather(all_of(financial_years), key = "Year", value = "Value") %>% 
    spread(key= "Measure", Value) %>% 
    mutate(Quantity = paste(variable))
  colnames(Output_table) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")
  return(Output_table)
}
#Use the function to create dataframes for each Table
Table_1_plot <- Create_plot_df(Table_1, "t CO2-e per ha")
Table_2_plot <- Create_plot_df(Table_2, "kg CO2-e per kg output")
Table_3_plot <- Create_plot_df(Table_3, "Nitrogen surplus (kg)")
Table_4_plot <- Create_plot_df(Table_4, "Nitrogen use efficiency (%)")

##Combine the four plot tables into one
Master_plots <- Table_1_plot %>% 
  rbind(Table_2_plot, Table_3_plot, Table_4_plot)
#Create a factor with the four different quantities, in the order that they should be plotted.
#Without this they are arranged alphabetically.
Master_plots$facet_order = factor(Master_plots$Quantity, 
                                  levels = c("t CO2-e per ha", "kg CO2-e per kg output",
                                             "Nitrogen surplus (kg)", "Nitrogen use efficiency (%)"))

# Function for creating the four plots for each type - for use in the visual summary
# The input arguments are the type, quantity to be plotted, label for the y-axis and the rgb colour code.
# A default colour code (#23A845, one of the standard Resas greens) is included.
output_plot <- function(i, title_label, quantity_label, y_label, colour_code="#00833E"){
  output_plot <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)) +
    geom_col(aes(x=`Year`, y=Median), fill=colour_code) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    geom_hline(yintercept = 0,colour="grey90")+
    xlab(element_blank())+
    labs(title = paste0(title_label))+
    # facet_wrap(vars(facet_order), scales="free") +
    # scale_x_discrete(guide = guide_axis(n.dodge=3))+
    ylab(bquote(.(y_label)))
  return(output_plot)
}

# A slightly modified function for creating the plots for use in the html publication.
output_plot_pub <- function(i, title_label, quantity_label, y_label, colour_code="#00833E", NUE_flag=F){
  output_plot_pub <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)) +
    geom_col(aes(x=`Year`, y=Median), fill=colour_code) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    xlab(element_blank())+
    scale_y_continuous(expand=c(0,0))+
    # labs(title = paste0(title_label))+
    {if(NUE_flag==T)geom_text(aes(x=`Year`, y=Median, vjust=-0.5, hjust=-0.08, label=paste0(format(round2(Median,0), nsmall=0),"%")), colour="Black", size=12, fontface="bold")}+
    {if(NUE_flag==F)geom_text(aes(x=`Year`, y=Median, vjust=-0.5, hjust=-0.08, label=format(round2(Median,1), nsmall=1)), colour="Black", size=12, fontface="bold")}+
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 24))+
    ylab(bquote(.(y_label)))
  return(output_plot_pub)
}

#Use the modified function to create plots for publication
g_co2perha_pub <- output_plot_pub(9, "Absolute emissions", "t CO2-e per ha", "t CO"[2]~"-e / ha")
# plot(g_co2perha_pub)
g_co2perkg_pub <- output_plot_pub(9, "Emission intensity", "kg CO2-e per kg output", "kg CO"[2]~"-e / kg output")
# plot(g_co2perkg_pub)
g_Nsurplus_pub <- output_plot_pub(9, "Nitrogen balance", "Nitrogen surplus (kg)", "kg N surplus / ha", "#6EA022")
# plot(g_Nsurplus_pub)
g_NUE_pub <- output_plot_pub(9, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "NUE (%)", "#6EA022", NUE_flag = T)
# plot(g_NUE_pub)
#Save as SVG files
SVGWidth <- 9
SVGHeight <- SVGWidth * (5/9)
ggsave(file="co2perha_pub.svg", plot=g_co2perha_pub, width=SVGWidth, height=SVGHeight)
ggsave(file="co2perkg_pub.svg", plot=g_co2perkg_pub, width=SVGWidth, height=SVGHeight)
ggsave(file="Nsurplus_pub.svg", plot=g_Nsurplus_pub, width=SVGWidth, height=SVGHeight)
ggsave(file="NUE_pub.svg", plot=g_NUE_pub, width=SVGWidth, height=SVGHeight)

new_outputs <- c("Cereal", "General Cropping", "Dairy", "Less favoured area (LFA) livestock", "Mixed")
ggplot(filter(Master_plots, Quantity=="t CO2-e per ha", Year=="2021-22", `Farm type` %in% new_outputs))+
  geom_point(aes(y=`Farm type`, x=`Median`))+
  geom_errorbar(aes(y=`Farm type`, xmin=Q1, xmax = Q3))+
  ylab(element_blank())+
  xlab("Tonnes CO2-e per hectare")+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85"))+
  geom_vline(xintercept=0, colour="grey85")
