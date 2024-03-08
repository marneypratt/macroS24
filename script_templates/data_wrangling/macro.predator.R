# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros, master.taxa, and env data must all be imported before you can run the code below

#this code will calculate the predator ratio 
# predator ratio = #predators /(#scrapers + #collector filterers + #collector gatherers + shredders)

macro.total <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID) |> 
  summarize(total.macros = sum(number, na.rm = TRUE))


macro.prd <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, FFG) |> 
  summarize(number = sum(number, na.rm = TRUE)) |> 
  
  #pivot wider to make column for each FFG
  pivot_wider(names_from = FFG, values_from = number, values_fill = 0) |> 
  
  #add in total of all macros
  left_join(macro.total, by = "sampleID") |> 
  
  #calculate the predator ratio
  mutate(prd.ratio = (prd/(total.macros-prd))) 


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
my.df <- left_join(macro.prd, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")