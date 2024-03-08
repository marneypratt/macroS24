#this code will make a violin plot 
#remove boxplots, points, or mean as desired
#make sure you have loaded the ggbeeswarm package to use geom_quasirandom()

ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___,       #put 1st factor on x-axis
      y = ___,       #continuous variable = y
      fill = ___)) + #2nd factor as fill
  
  #adds violin plot
  geom_violin(
    position = position_dodge(width = 1), #match to dodge.width below
    width = 1,       #spread
    alpha = 0.2) +   #transparency
  
  #adds boxplots
  geom_boxplot(
    position = position_dodge(width = 1), #match to position_dodge(width = )
    width = 0.1,            #width
    alpha = 0.8,            #transparency
    color = "black",        #color of outline
    outlier.shape = NA,     #removes outliers 
    show.legend = FALSE) +  #removes extra key
  
  #adds quasirandom jittered points
  geom_quasirandom(aes(color = ___),      #same factor as fill
                   shape = 21,            #shape as open circle
                   size = 1,              #size of point
                   alpha = 0.5,           #transparency
                   width = 0.25,          #spread
                   dodge.width = 1,       #match with position_dodge(width = )
                   show.legend = FALSE) + #removes extra key
  
  #adds mean
  stat_summary(
    fun = mean,      #graphs the mean
    geom = "point",  #single point for mean
    shape = 4,       #shape as X if 4
    size = 2,        #size
    color = "black", #color 
    stroke = 1,      #line thickness
    position = position_dodge(width = 1), #match to dodge.width above
    show.legend = FALSE) +   #removes extra key
  
  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  labs(fill = "___") + #label for the fill key
  
  #remove repeated key
  guides(color = FALSE) +
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "top")   #controls legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-violin-plot/
# https://ggplot2.tidyverse.org/index.html