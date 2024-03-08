
table.sum <- ___ |>  #put the name of your wrangled data here
  
  #summarize by date, season, year, & location (to get number of microhabs and area sampled per date)
  group_by(date, season, year, location) |> 
  summarize(
    area = sum(benthicArea),
    samples = n()
  ) |> 
  
  #change date to month/day
  mutate(date = format(as.Date(date), '%m/%d')) |> 
  
  #summarize by year, season, location 
  group_by(season, year, location) |> 
  summarize(
    dates = str_c(date, collapse = ", "),
    samples = sum(samples),
    area = sum(area),
  )  

#To get a formatted table, make sure that the {flextable} package is loaded
#Use additional code to create the summary table called "table.sum"
#If you want to format the table yourself, then don't use this code

#see https://davidgohel.github.io/flextable/ 
#for more information and formatting options

#create the formatted table
ft <- flextable(table.sum) |> 
  
  #create header labels
  set_header_labels(season = "Season",
                    year = "Year",
                    location = "Location",
                    dates = "Dates",
                    samples = "# of Samples",
                    area = "Total Area Sampled (m^2)") |> 
  
  #format superscript
  compose(j = "area", part="header",
          value = as_paragraph(
          "Total Area Sampled (m", 
          as_sup("2"),
          ")")) |> 
  
  #formatting
  theme_vanilla()  |>  
  fontsize(size = 10) |> 
  
  #set table properties
  set_table_properties(layout = "autofit", width = 1) |> 
  
  #center columns
  align(align = "center", part = "all" )

#print the table
#right click on the table, choose select all, 
#choose copy, then paste in your document
#finish formatting as needed in your document
ft