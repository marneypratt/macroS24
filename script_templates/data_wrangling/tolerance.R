# make sure the 'tidyverse' package is installed and loaded to run the code below
# macros, master.taxa, & env data must all be imported before you can run the code below


##calculate the number of macros in each sampleID
macro.total <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #filter out midges
  filter(family == "Chironomidae") |> 
  
  #calculate the number of macros in each sampleID
  group_by(sampleID, benthicArea) |> 
  summarize(total.macros = sum(number, na.rm = TRUE))

##Calculate number of each tolerance group
macros.tol <- macros  |> 
  
  #join taxonomic information 
  left_join(master.taxa)  |> 
  
  #filter out midges
  filter(family == "Chironomidae") |> 
  
  #remove NAs
  filter(!is.na(tolerance)) |> 

  #form new factor based on tolerance
  mutate(tolerance.factor = case_when(
    tolerance >= 0 & tolerance <= 3 ~ 'Sensitive',
    tolerance >= 4 & tolerance <= 6 ~ 'Moderate',
    tolerance >= 7 ~ 'Tolerant')) |> 

  # Summarize for each sampleID and each family 
  # note you can exchange family with another taxonomic level of interest
  group_by(sampleID, tolerance.factor) |> 
  dplyr::summarise(number = sum(number, na.rm = TRUE)) |> 
  
  #fill in group combos where there were none present with zeros
  ungroup() |> 
  complete(sampleID, tolerance.factor,
           fill = list(number = 0)) 

#set factor with levels in the right order
macros.tol$tolerance.factor <- factor(macros.tol$tolerance.factor, 
                                      levels = c('Sensitive', 
                                                 'Moderate',
                                                 'Tolerant'))
  

##join the two datasets
macro.joined  <- left_join(macro.total, macros.tol) |> 
  
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
  
  #filter out anything you don't want
  #the example below would filter for just the sensitive macros
  dplyr::filter(tolerance.factor == "Sensitive") |> 
  
  #remove rows with any missing data
  na.omit()
