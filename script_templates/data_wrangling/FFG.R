# make sure the 'tidyverse' package is installed and loaded to run the code below
# macros, master.taxa, & env data must all be imported before you can run the code below


##calculate the number of macros in each sampleID
macro.total <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #filter out midges
  filter(family != "Chironomidae") |> 
  
  #calculate the number of macros in each sampleID
  group_by(sampleID, benthicArea) |> 
  summarize(total.macros = sum(number, na.rm = TRUE))



##calculate the number of each FFG in each sampleID
macro.ffg <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #filter out midges
  filter(family != "Chironomidae") |> 
  
  # Summarize for each sampleID and each FFG 
  group_by(sampleID, FFG) |> 
  dplyr::summarise(number = sum(number, na.rm = TRUE)) |> 
  
  #fill in group combos where there were none present with zeros
  ungroup() |> 
  complete(sampleID, FFG,
           fill = list(number = 0)) 

#filter for the organisms without FFGs assigned
macro.ffg.na <- macro.ffg |> 
  filter(is.na(FFG) & number > 0)

#add back the organisms without FFGs assigned
macro.ffg <- macro.ffg |> 
  filter(!is.na(FFG)) |> 
  bind_rows(macro.ffg.na)


##join the two datasets
macro.joined  <- left_join(macro.total, macro.ffg) |> 
  
  #calc the relative abundance & density of each FFG
  mutate(relab = number/total.macros,
         density = number/benthicArea)


## select other variables you want present in your final dataset
variables <- macros |> 
  
  #join environmental variables
  left_join(env) |> 
  
  #select variables of interest
  #delete anything you don't need
  #add anything you do need in the blank with commas in between
  dplyr::select(date, sampleID, season, year, location, benthicArea,
                __) |> 
  distinct()

##add in the variables just selected
#sampleID is the "key" used to match up the two data frames
my.df <- left_join(macro.joined, variables)  |> 
  
  #filter out anything you don't want
  #the example below would filter for just scrapers
  dplyr::filter(FFG == "scr")  |> 
  
  #remove rows with any missing data
  na.omit()
