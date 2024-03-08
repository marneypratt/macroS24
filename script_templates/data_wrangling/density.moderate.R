
# make sure the 'tidyverse' package is installed and loaded 
# macros, master.taxa, and env data files must be imported before you 
# can run the code below

#get total samples
samples <- macros |>
  select(sampleID) |> 
  distinct()

#calculate density for each sampleID
moderate.density <- macros |> 
  
  #join taxa & tolerance info
  left_join(master.taxa) |>
  
  #filter for moderately tolerant macros (4-7) 
  dplyr::filter(tolerance > 3, tolerance < 8) |> 
  
  #filter out the midges
  dplyr::filter(family != "Chironomidae") |> 
  
  # Summarize for each sampleID 
  # density of ALL macroinvertebrates
  group_by(sampleID)  |>  
  dplyr::summarise(density = sum(invDens, na.rm = TRUE)) |> 
  
  #add back in samples that had none present
  right_join(samples) |> 
  
  #fill in group combos where there were none present with zeros
  ungroup() |> 
  complete(sampleID,  
           fill = list(density = 0)) |> 
  
  mutate(tolerance = "moderate")
  
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
my.df <- left_join(moderate.density, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")