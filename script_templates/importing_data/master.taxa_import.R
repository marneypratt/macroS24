#this will import the list of all taxa for the Mill River and NEON rivers

#make sure you have run library(tidyverse) or library(readr) before running this code

#can check taxonomy here as needed: https://data.neonscience.org/taxonomic-lists

master.taxa <- read_csv("data/master.taxa.csv") %>% 
  distinct()

