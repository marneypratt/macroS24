#this code will make stacked bar graphs for 
#relative abundance of functional feeding groups

ggplot(
data = ___, #put the data frame name here

#set the aesthetics
aes(x = ___,       #put the factor on the x-axis
    y = ___,       #proportion values = y
    fill = FFG, color = FFG)) + #functional feeding groups for fill & color
  
  #adds stacked bars
  geom_bar(position = "fill", stat = "identity") +
  

  #nicer labels (see axis.labels.R script)
  xlab("___") +
  ylab("Relative Abundance\n") +
  scale_fill_discrete(
    name = "Functional Feeding Groups",
    labels = c("collector filterers", 
               "collector gatherers",
               "predators", 
               "scrapers", 
               "shredders" )) +
  
  scale_color_discrete(
    name = "Functional Feeding Groups",
    labels = c("collector filterers", 
               "collector gatherers",
               "predators", 
               "scrapers", 
               "shredders" )) +
  
  #additional formatting
  coord_cartesian(ylim = c(0,1), expand = FALSE, clip = 'off') +
  theme_classic(base_size = 12)  +  #sets the font size
  theme(legend.position = "right") +  #controls legend/key

  #you can use the line below to split by factors
  #(season, year, location, or other factors)
  facet_grid(rows = vars(____), cols = vars(____)) 

#see the resources below for some additional options to make a nice plot 
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# https://ggplot2.tidyverse.org/index.html
# https://r-charts.com/part-whole/stacked-bar-chart-ggplot2/