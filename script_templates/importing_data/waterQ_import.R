
# make sure the `readr` package is installed and loaded before you run the code below
# this code will import the water quality data for the Mill River
# this file includes all the Mill River data from 2018-2022
# see the file called waterQ_metadata.txt for details about all these variables and methods

# make sure the readr` package is loaded before importing these files

waterQ <- read_csv("data/waterQ.csv",
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
          
          location = col_factor(levels = c(
            "Downstream","Upstream")), 
          
          season = col_factor(levels = c("Summer","Fall")), 
          
          year = col_factor()
         )) 


