# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

macro.total <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, benthicArea) |> 
  summarize(total.macros = sum(number, na.rm = TRUE))

#this code will calculate various ratios
macro.ratios <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, FFG) |> 
  summarize(number = sum(number, na.rm=TRUE)) |> 
  
  #pivot wider to make column for each FFG
  pivot_wider(names_from = FFG, values_from = number, values_fill=0) |> 
  
  #add total of all macros
  left_join(macro.total) |> 
  
  #calc number of macros that like rocks (rock)
  #calc number of macros that like sand (sand)
  #calc total prey (prey)
  mutate(rock = sum(scr,cf, na.rm=TRUE),
         sand = sum(cg,sh, na.rm=TRUE),
    prey = (total.macros-prd)) |> 
  
  #calc ratios
  # the substrate stability ratio (SSR)
  # SSR = (#scrapers + #collector filterers)/(#collector gatherers + shredders)
  mutate(ssr = ((sum(scr,cf, na.rm=TRUE)+1)/(sum(cg,sh, na.rm=TRUE)+1)),
         predator.ratio = prd/prey) |> 
  
  #calc the relative abundance certain macros
  mutate(relab.rock = rock/total.macros,
         relab.sand = sand/total.macros,
         relab.prd = prd/total.macros,
         relab.prey = prey/total.macros) |> 

  #calc the density of certain macros
  mutate(den.rock = rock/benthicArea,
         den.sand = sand/benthicArea,
         den.prd = prd/benthicArea,
         den.prey = prey/benthicArea)
  
# select other variables you want present in your final dataset
variables <- macros |> 
  
  #join environmental variables
  left_join(env) |> 
  
  #select variables of interest
  #delete anything you don't need
  #add anything you do need in the blank with commas in between
  dplyr::select(date, sampleID, season, year, location, benthicArea,
                __) |> 
  distinct()

#add in the variables just selected
#sampleID is the "key" used to match up the two data frames
my.df <- left_join(macro.ratios, variables) |> 
  
  #filter out anything you don't want
  #the example below would filter out just the year 2018
  dplyr::filter(year != "2018")