# make sure the 'tidyverse' package is installed and loaded to run the code below
# macros, master.taxa, & env data must all be imported before you can run the code below


##calculate the number of macros in each sampleID
macro.total <- macros |> 
  
  #calculate the number of macros in each sampleID
  group_by(sampleID, benthicArea) |> 
  summarize(total.macros = sum(number, na.rm = TRUE))



##calculate the number of each taxon of interest in each sampleID
macro.family <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  # Summarize for each sampleID and each family 
  # note you can exchange family with another taxonomic level of interest
  group_by(sampleID, family) |> 
  dplyr::summarise(number = sum(number, na.rm = TRUE)) |> 
  
  #fill in group combos where there were none present with zeros
  ungroup() |> 
  complete(sampleID, family,
           fill = list(number = 0)) |> 
  
  #filter for first family of interest
  #this needs to match what is in the macros file
  #replace this blank with the family you want to keep
  ## note you can exchange family with another taxonomic level of interest
  dplyr::filter(family == "___" | family == "___")  



##join the two datasets
macro.joined  <- left_join(macro.total, macro.family) |> 
  
  #calc the relative abundance & density
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
                ___) |> 
  distinct()

##add in the variables just selected
#sampleID is the "key" used to match up the two data frames
my.df <- left_join(macro.joined, variables) |> 
  
  #pivot wider to make a column for each taxon
  pivot_wider(names_from = family, 
              values_from = c(density, relab),
              values_fill = 0) |> 

  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018") |> 
  
  #remove rows with any missing data
  na.omit()