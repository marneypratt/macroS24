#this code will make a dot plot with median and all points included
#make sure you have loaded the ggbeeswarm package to use geom_quasirandom()


ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___,       #put 1st factor on x-axis
      y = ___,       #continuous variable = y
      group = ___, color = ___)) + #2nd factor as group & color
  
  #adds quasirandom jittered points
  geom_quasirandom(
    shape = 19,            #shape as circle
    size = 2,              #size of point
    alpha = 0.8,           #transparency
    width = 0.25,          #spread
    dodge.width = 1,       #match with position_dodge(width = )
    show.legend = TRUE) +  #creates key
  
  #adds median
  stat_summary(
    fun = median,       #graphs the median
    geom = "crossbar",  #crossbar shape
    width = 0.5,        #width of the bar
    linewidth = 0.75,   #thickness of bar
    color = "black",    #color 
    position = position_dodge(width = 1), #match to dodge.width above
    show.legend = FALSE) +   #removes extra key
  
  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  labs(color = "___") + #label for the color key
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "top")   #controls legend/key