#this code will make a violin plot 
#remove boxplots, points, or mean as desired
#make sure you have loaded the ggbeeswarm package to use geom_quasirandom()

ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___,       #factor on the x-axis
      y = ___,       #continuous variable = y
      fill = ___)) + #same factor as on x-axis
  
  #adds violin plot
  geom_violin(
    width = 0.5,     #spread
    alpha = 0.2) +   #transparency
  
  #adds boxplots
  geom_boxplot(width = 0.1,           #width
               alpha = 0.8,           #transparency
               color = "black",       #color of outline
               outlier.shape = NA) +  #removes outliers
  
  #adds quasirandom jittered points
  geom_quasirandom(aes(color = ___), #same factor as x and fill
                   shape = 21,       #shape as open circle
                   size = 1,         #size
                   alpha = 0.5,      #transparency
                   width = 0.25) +   #spread
  #adds mean
  stat_summary(fun = mean,      #graphs the mean
               geom = "point",  #single point for mean
               shape = 4,       #shape as X if 4
               size = 2,        #size
               color = "black", #color
               stroke = 1) +    #line thickness
  
  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "none")   #controls legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-violin-plot/
# https://ggplot2.tidyverse.org/index.html