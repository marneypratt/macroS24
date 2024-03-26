
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data files must be imported before you can run the code below

#calculate the density of all macros
macro.density <- macros  |>  
  
  # Summarize for each sampleID 
  # density of ALL macroinvertebrates
  group_by(sampleID)  |>  
  dplyr::summarise(density = sum(invDens, na.rm = TRUE)) 


#get sample info and env variables of interest
variables <- macros |>
  
  #join environmental variables
  left_join(env) |> 
  
  #select variables of interest
  #delete anything you don't need
  #add anything you do need in the blank with commas in between
  dplyr::select(date, sampleID, season, year, location, benthicArea,
         ___) |> 
  distinct()

#add sample info back to density data
my.df <- left_join(macro.density, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018") |> 
  
  #remove rows with any missing data
  na.omit()