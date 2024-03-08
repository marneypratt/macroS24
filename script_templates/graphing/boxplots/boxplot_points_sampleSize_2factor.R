#this code will make a boplot with sample sizes
#remove points or mean as desired
#make sure you have loaded the ggbeeswarm package to use geom_quasirandom()


#Calculates the sample size of each group
n_fun <- function(x){
  return(data.frame(y = ___,      #match y in geom_text below
                    label = length(x)))
}

#graph mix plot with dots, boxes, and violins
ggplot(
  data = ___, #put the data frame name here
  
  #set the aesthetics
  aes(x = ___,       #put 1st factor on x-axis
      y = ___,       #continuous variable = y
      fill = ___, color = ___)) + #2nd factor as fill & color

  #adds boxplots
  geom_boxplot(
    position = position_dodge(width = 1), #match to position_dodge(width = )
    width = 0.3,            #width
    alpha = 0.8,            #transparency
    color = "black",        #color of outline
    outlier.shape = NA,     #removes outliers 
    show.legend = FALSE) +  #removes extra key
  
  #adds quasirandom jittered points
  geom_quasirandom(
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
  
  #calculates and adds the sample size
  stat_summary(
    fun.data = n_fun,        #calculates the sample size
    geom = "text",           #adds text based on value
    size = 6,                #size
    position = position_dodge(width = 1), #match to dodge.width above
    show.legend = FALSE) +   #removes extra key
  
  #adds a label for the sample size
  geom_text(x = 0.59,          #sets horizontal location
            y = __,            #match value used for y in n_fun above
            label = "N =",     #N= label for x-axis
            size = 6,          #size
            color = "black") + #color of text to black
  
  #nicer labels (see axis.labels.R script)
  ylab("___") +
  xlab("___") +
  labs(fill = "___") + #label for the fill key
  
  #additional formatting
  theme_classic(base_size = 16)  +  #sets the font size
  theme(legend.position = "top") +  #controls legend/key
  
  #remove repeated key
  guides(color = FALSE) +

  #the upper y-limit must fit all the data and the N values
  coord_cartesian(ylim = c(0, ___), expand = FALSE) 