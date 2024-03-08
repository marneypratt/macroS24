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
        fill = ___, color = ___)) + #same factor as group_by for fill & color
    
  #adds boxplots
  geom_boxplot(
    width = 0.2,          #width of box
    alpha = 0.5,          #transparency
    color = "black",      #color of outline
    outlier.shape = NA) + #removes outliers
  
  #adds quasirandom jittered points
  geom_quasirandom(
    shape = 21,            #shape as open circle
    size = 1,              #size of point
    alpha = 0.5,           #transparency
    width = 0.25) +        #spread
  
  #adds mean
  stat_summary(
    fun = mean,      #graphs the mean
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
# https://www.datanovia.com/en/lessons/ggplot-boxplot/
# https://ggplot2.tidyverse.org/index.html