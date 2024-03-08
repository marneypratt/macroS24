#add this to your ggplot code to make a 
#log-scale axis on y-axes

  scale_y_log10() +     #changes y-axis to log-scale
  annotation_logticks(sides = "l") + # adds log-scale tick marks
