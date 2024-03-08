
# make sure the `readr` package is installed and loaded before you run the code below
# this code will import the environmental data for the Mill River 
# see the file called env_metadata.txt for details about all these variables and methods

# make sure the readr` package is loaded before importing these files

env <- read_csv("data/env.csv",
       col_types = cols(date = col_date(format = "%Y-%m-%d"),
        
        location = col_factor(levels = c(
          "Downstream","Upstream")), 
        
        season = col_factor(levels = c("Summer","Fall")), 
        
        year = col_factor()
       )) 

