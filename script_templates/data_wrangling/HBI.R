
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

macro.tol <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |>

  # remove missing values
  dplyr::filter(!is.na(number), !is.na(tolerance)) |> 
  
  #tolerance value x the number present for each kind of organism in each sampleID
  group_by(sampleID, organism_aggr) |> 
  dplyr::summarise(num = sum(number), 
                   tol = mean(tolerance)) |> 
  mutate(x.t = num*tol)

# calculate the HBI for each sampleID
macro.HBI <- macro.tol |> 
  group_by(sampleID) |> 
  dplyr::summarise(x.t.sum = sum(x.t),
                   total.n = sum(num)) |> 
  mutate(HBI = x.t.sum/total.n)


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
my.df <- left_join(macro.HBI, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")