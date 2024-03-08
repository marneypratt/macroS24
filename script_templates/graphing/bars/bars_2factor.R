
# this code will make a bar graph with means and standard errors



#first calculate the mean, sd, & count the observations in each group
df.sum <- ___ |>               #data frame name here
  group_by(___, ___) |>        #name of 2 factors here
  summarise(mean = mean(___),  #continuous variable here
            sd = sd(___),      #same continuous variable as above here
            n = n()) |>        #don't change anything here
  
  #calculate the standard error
  mutate(sem = sd/(sqrt(n)))


#make bar graph
ggplot(
  data=df.sum, 
  aes(x=___, y=mean, fill=___)) + #1st factor as x, 2nd factor as fill
  geom_bar(stat="identity", position="dodge", width = 0.5)+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), 
                position=position_dodge(width = 0.5), #this width must match width in geom_bar above
                width=0.2, size=0.5, show.legend = FALSE) +
  ylab("___") +
  xlab("___") +
  
  #adjust the second number in xlim to be 
  #0.5 greater than the number of groups in 1st factor
  coord_cartesian(xlim = c(0.5,5.5), expand=FALSE) + 
  
  theme_classic(base_size=16) +
  theme(legend.position = "top") #change the position of the key as needed 


#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-barplot/
# https://ggplot2.tidyverse.org/index.html