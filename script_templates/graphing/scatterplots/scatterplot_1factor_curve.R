# this code will make a scatter plot with a curve
# 
ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___,       #put the factor on the x-axis
      y = ___,       #continuous variable = y
      color = ___)) + #put the same factor as on x-axis 
  
  #add points
  geom_point(size = 3) +    #sets the size of the points
  
  #add locally weighted smoothing curve
  geom_smooth(method = "loess",       
              se = FALSE, 
              formula = y ~ x) + 
  
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

#see the links below for more on smoothing curves using geom_smooth
# https://www.sharpsightlabs.com/blog/geom_smooth/ 
# https://blogs.bgsu.edu/math6820ywonkye/2018/10/25/loess/
# https://en.wikipedia.org/wiki/Local_regression