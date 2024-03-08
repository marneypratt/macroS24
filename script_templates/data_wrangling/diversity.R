
# make sure the `vegan` and 'tidyverse' packages are installed and loaded to run the code below

# macros and master.taxa data files must both be imported before you can run the code below

# remove missing values, and make sure each taxon is summarized within sampleID
macro.long <- macros |>  
  
  #join taxa info
  left_join(master.taxa) |>
  
  #diversity for mix of taxa (organism_aggr)
  #change organism_aggr to family or other taxonomic unit if desired
  dplyr::filter(!is.na(invDens), year !="2018") |> 
  dplyr::select(sampleID, organism_aggr, invDens) |> 
  group_by(sampleID, organism_aggr) |> 
  dplyr::summarise(density = sum(invDens))


# convert to wide format
macro.wide <- macro.long |> 
  pivot_wider(names_from = organism_aggr, 
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



