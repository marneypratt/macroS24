
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below
  
macro.ffg <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  # Sum each FFG density for each sampleID 
  group_by(sampleID, FFG) |> 
  dplyr::summarise(ffgDens = sum(invDens, na.rm = TRUE)) |> 
  
  #adds back the zeros for FFGs that were not present in a sample
  #repeat all the grouping variables as above
  ungroup() |>
  complete(sampleID, FFG, 
           fill = list(ffgDens = 0)) 

#filter for the organisms without FFGs assigned
macro.ffg.na <- macro.ffg |> 
  filter(is.na(FFG) & ffgDens > 0)

#add back the organisms without FFGs assigned
macro.ffg <- macro.ffg |> 
  filter(!is.na(FFG)) |> 
  bind_rows(macro.ffg.na)

# select other variables you want present in your final dataset
variables <- macros |> 
  
  #join environmental variables
  left_join(env) |> 
  
  #select variables of interest
  #delete anything you don't need
  #add anything you do need in the blank with commas in between
  dplyr::select(date, sampleID, season, year, location, benthicArea,
                ___) |> 
  distinct()

#add in the variables just selected
#sampleID is the "key" used to match up the two data frames
my.df <- left_join(macro.ffg, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")