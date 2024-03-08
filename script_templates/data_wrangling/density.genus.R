
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data files must be imported before you can run the code below

#calculate density for each sampleID
density.df <- macros |> 
  
  #join taxa info
  left_join(master.taxa) |>
  
  # Summarize for each sampleID and each genus 
  group_by(sampleID, genus) |> 
  dplyr::summarise (density = sum(invDens, na.rm = TRUE)) |> 
  
  #fill in group combos where there were none present with zeros
  ungroup() |> 
  complete(sampleID, genus,
           fill = list(density = 0)) |> 
  
  #filter for genus of interest 
  #this needs to match what is in the macros file
  #replace this blank with the genus you want to keep
  filter(genus == "___")

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
my.df <- left_join(density.df, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")