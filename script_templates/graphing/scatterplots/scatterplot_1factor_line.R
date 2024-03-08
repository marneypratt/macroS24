# this code will make a scatter plot 
# if you do not want a line of best fit, then delete the geom_smooth() line

ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___,       #put the factor on the x-axis
      y = ___,       #continuous variable = y
      color = ___)) + #put the same factor as on x-axis 
  
  #add points
  geom_point(size = 3) +    #sets the size of the points
  
  #add best fit line
  geom_smooth(method = "lm", se = FALSE) + 

  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  labs(color = "___") + #label for the color key
  
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "top")   #controls legend/key

#see the resources below for some additional options to make a nice scatter plot 
# https://www.datanovia.com/en/lessons/ggplot-scatter-plot/
# https://ggplot2.tidyverse.org/index.html