# make sure the 'tidyverse' package is installed and loaded 
# the waterQ data file must be imported before you 
# can run the code below

my.df <- waterQ |> 
  
  #replace the blank below with the water quality variable(s) of interest
  dplyr::select(date, sampleID, season, year, location, 
                ___) |> 
  
  #filter out any missing values
  na.omit() |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")