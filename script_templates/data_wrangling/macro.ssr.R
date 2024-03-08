# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

macro.total <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, benthicArea) |> 
  summarize(total.macros = sum(number, na.rm = TRUE))

#this code will calculate the substrate stability ratio (SSR)
# SSR = (#scrapers + #collector filterers)/(#collector gatherers + shredders)
macro.ssr <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, FFG) |> 
  summarize(number = sum(number, na.rm=TRUE)) |> 
  
  #pivot wider to make column for each FFG
  pivot_wider(names_from = FFG, values_from = number, values_fill=0) %>% 
  
  #calc number of macros that like rocks
  #calc number of macros that like sand
  #calc substrate stability ratio
  mutate(rock = sum(scr,cf, na.rm=TRUE),
         sand = sum(cg,sh, na.rm=TRUE),
    ssr = ((sum(scr,cf, na.rm=TRUE)+1)/(sum(cg,sh, na.rm=TRUE)+1))) |> 
  
  #add total of all macros
  left_join(macro.total) |> 
  
  #calc the relative abundance of macros that like rocks and sand
  mutate(relab.rock = rock/total.macros,
         relab.sand = sand/total.macros) |> 

  #calc the density of macros that like rocks and sand
  mutate(den.rock = rock/benthicArea,
         den.sand = sand/benthicArea)
  
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
my.df <- left_join(macro.ssr, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")