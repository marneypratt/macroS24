
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

macro.ffg <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
    # Sum FFG for each sampleID 
    group_by(sampleID, FFG) |> 
    dplyr::summarise (num.ffg = sum(number, na.rm = TRUE)) |> 
    
    #adds back the zeros for FFGs that were not present in a sample
    ungroup() |>
    complete(sampleID, FFG, fill = list(num.ffg = 0)) 
  
  
  ###sum the total number of all organisms for each sample
  macro.sample <- macros |>  #replace blank with the same data frame name from above
    group_by(sampleID) |> 
    dplyr::summarise (total.num = sum(number, na.rm = TRUE))
  
  # join the number of each FFG with total number of all organisms
  macro.ffg <- left_join(macro.ffg, macro.sample, by = "sampleID") |> 
  
  ###calculate the relative abundance 
  mutate(macro.ffg, relab=num.ffg/total.num) 
  
  #filter for the organisms without FFGs assigned
  macro.ffg.na <- macro.ffg |> 
    filter(is.na(FFG) & relab > 0)
  
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