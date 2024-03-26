
# make sure the `vegan` and 'tidyverse' packages are installed and loaded to run the code below

# macros and master.taxa data files must both be imported before you can run the code below

# remove missing values, and make sure each taxon is summarized within sampleID
macro.long <- macros |>  
  
  #join taxa info
  left_join(master.taxa) |>
  
  #diversity for families
  #for mix of taxa use organism_aggr instead of family
  #2018 is removed by default because of aggregated samples in the FALL
  dplyr::filter(!is.na(invDens), year !="2018") |> 
  dplyr::select(sampleID, family, invDens) |> 
  group_by(sampleID, family) |> 
  dplyr::summarise(density = sum(invDens))


# convert to wide format
macro.wide <- macro.long |> 
  pivot_wider(names_from = family, 
              values_from = density,
              values_fill = list(density = 0),
              values_fn = list(density = sum)) |>
  tibble::column_to_rownames("sampleID")


#Calculate diversity index values

#Shannon index (H)
shannon <- diversity(macro.wide)


#effective number of species
effective.sp <- exp(shannon)


#Richness
richness <- specnumber(macro.wide) 

#Max possible diversity
max.H <- log(richness)


#Pielou's Evenness J 
evenness <- shannon / max.H

#put all diversity values into a single data frame
macro.div <- data.frame(shannon, effective.sp, richness, max.H, evenness) |> 
  tibble::rownames_to_column("sampleID")


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
my.df <- left_join(macro.div, variables)



