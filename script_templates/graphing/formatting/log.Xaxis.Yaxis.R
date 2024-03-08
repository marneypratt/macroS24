#add this to your ggplot code to make a 
#log-scale axis on both axes

  scale_y_log10() +     #changes y-axis to log-scale
  scale_x_log10() +     #changes x-axis to log-scale
  annotation_logticks() # adds log-scale tick marks
