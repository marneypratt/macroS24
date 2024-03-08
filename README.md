# Macroinvertebrate Data Analysis for Bio 131 Spring 2024

### by Marney Pratt

### Last updated on March 8, 2024

This webpage details how to download this project which includes all the files you need to analyze data for your Freshwater Macroinvertebrate Project for Bio 131.

It is assumed that you have access to R and Rstudio (installed on your computer or access to an RStudio cloud-based server) and that you know how to install any needed packages.


## Download this repository from GitHub using the usethis package

This series of instructions will copy all the files you need from here on GitHub to your computer or cloud-based RStudio. 

1. If you haven't already installed the usethis package (you can check your list of packages to see if it is already there), then install the usethis package by typing this code into the RStudio console and press Enter:

`install.packages("usethis")`

2. Once the package is installed, then you need to load usethis. This is similar to opening an app on your phone or computer. To load usethis, type this code into the RStudio console and press Enter:

`library(usethis)`

3. To download the repository, use this code:

`use_course("https://github.com/marneypratt/macroS24/archive/main.zip")`


4. When told "Downloading into..." "OK to proceed?" select the number for the option next to "I agree" (or whatever the affirmative response is) and note what directory it is putting the zipped file into. 

5. When asked "Shall we delete the ZIP file" select the number for the option that says "Definitely" (or whatever the affirmative response is)

A new session of RStudio will open with the unzipped folder containing all the files you need ready for you.


## Use Templates to Help with your Analysis

Start by opening the file "macro_analysis.qmd"

Explore the files in the "data" folder to figure out which files you need to import

Use the metadata text files in the "metadata" folder to help you know what is in each data file and what all the variable names mean.

Copy the code from the various script templates in the "script_templates" folder as needed and paste them in the "macro_analysis.qmd" file.  Replace the blanks in the code as needed.


When you are done with your analysis "render" your .qmd file. Render to HTML, open the HTML file in your browser, and then "print" to a PDF file.
