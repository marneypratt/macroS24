
#before you run the code below...

#make sure the 'flextable' and `tidyverse` packages are installed and loaded
#make sure your data are imported

#see https://davidgohel.github.io/flextable/ for more information and formatting options

#set the variable you want to summarize
x.var <- "___" #put the name of the column you want to summarize in the blank here

#calculate descriptive stats
df.sum <- ___ |> #put the name of the data frame here
  group_by(___) |> #put the grouping variable(s) here
  filter(!is.na(.data[[x.var]])) |> # remove missing values 
  
  #remove the lines below that you don't need 
  #(you won't usually need ALL these values)
  #change the digits as needed
  summarise(mean = round(mean(.data[[x.var]]), digits=2), 
            SD = signif(sd(.data[[x.var]]), digits=2),
            median = round(median(.data[[x.var]]), digits=2),
            IQR = round(IQR(.data[[x.var]]), digits=2),
            min = round(min(.data[[x.var]]), digits=2),
            max = round(max(.data[[x.var]]), digits=2),
            N = n())

#create the table
ft <- flextable(df.sum,
                cwidth = 0.75) |>  #can vary cell width as needed
  
  #bold the headings
  bold(part = "header") |> 
  
  #center columns
  align(align = "center", part = "all" )

#print the table
#right click on the table, choose select all, 
#choose copy, then paste in your document
#finish formatting as needed in your document
ft