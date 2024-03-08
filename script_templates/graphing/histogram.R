#this code will make a histogram
#you can add faceting to split by factors
#use the standard deviation as a first guess for the binwidth
#then go up and down from there to find a useful binwidth

ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___)) + #x must be a continuous variable
  
  #plot a histogram
  geom_histogram(
    binwidth = ___,      #set the bin width 
    color = "white", 
    fil l= "#00BFC4") +
  
  #nicer labels (see axis.labels.R script)
  xlab("___") +
  ylab("Count") +
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "none")   #controls legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-histogram/
# https://ggplot2.tidyverse.org/index.html