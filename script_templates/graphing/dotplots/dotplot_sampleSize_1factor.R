#this code will make a boxplot + points with the sample size on the x-axis
#make sure you have loaded the ggbeeswarm package to use geom_quasirandom()

# first calculate the sample size
sample_size = ___  |>  #put the data frame name here
  group_by(___)  |>    #factor here
  summarize(num = n()) #don't change anything here

#ggplot with sample sizes added
___  |>    #put the same data frame name here as above
  
  #joins the dataframe with sample size information
  left_join(sample_size, by = "___")  |>  #same factor as group_by above
  
  #makes new column with sample size below each x-axis value
  mutate(
    myaxis = paste0(___, "\n", "n=", num)) |> #same factor as group_by above
  
  ggplot(
    
    #set the aesthetics
    aes(x = myaxis,    #put column created with sample size info
        y = ___,       #continuous variable = y
        color = ___)) + #same factor as group_by for color
    
  #adds quasirandom jittered points
  geom_quasirandom(
    shape = 19,            #shape as circle
    size = 2,              #size of point
    alpha = 0.8,           #transparency
    width = 0.25) +        #spread
  
  #adds median
  stat_summary(
    fun = median,       #graphs the median
    geom = "crossbar",  #crossbar shape
    width = 0.5,        #width of the bar
    linewidth = 0.75,   #thickness of bar
    color = "black") +  #color 
  
  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "none")   #controls legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-boxplot/
# https://ggplot2.tidyverse.org/index.html


sample_size = my.df  |>  #put the data frame name here
  group_by(location)  |>    #factor here
  summarize(num = n()) #don't change anything here

#ggplot with sample sizes added
my.df  |>    #put the same data frame name here as above
  
  #joins the dataframe with sample size information
  left_join(sample_size, by = "location")  |>  #same factor as group_by above
  
  #makes new column with sample size below each x-axis value
  mutate(
    myaxis = paste0(location, "\n", "n=", num)) |> #same factor as group_by above
  
  ggplot(
    
    #set the aesthetics
    aes(x = myaxis,    #put column created with sample size info
        y = log1p(density),       #continuous variable = y
        color = location)) + #same factor as group_by for color
  
  #adds quasirandom jittered points
  geom_quasirandom(
    shape = 19,            #shape as circle
    size = 2,              #size of point
    alpha = 0.8,           #transparency
    width = 0.25) +        #spread
  
  #adds median
  stat_summary(
    fun = median,       #graphs the median
    geom = "crossbar",  #crossbar shape
    width = 0.5,        #width of the bar
    size = 0.75,        #size
    color = "black") +  #color 
  
  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "none")   #controls legend/key