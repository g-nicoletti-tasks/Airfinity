library(tidyverse)

data <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))

glimpse(data)
 
## example areas of focus (there is no expectation to cover all of these, 
## you may choose to focus on one area or cover a simple visualisation on each. 
## Either is fine
##
##  o where confirmed cases are increasing / decreasing (geographically)
## 
##  o where case fatality rates have been highest or are currently 
##    increasing / decreasing
## 
##  o which countries have been most ‘successful’ at vaccinating their 
##    population, to date
## 
## where applicable, use Shiny ‘modules’ to construct the application
## any sort of visualisation is permitted (line graphs, bar charts)
## use of supplementary tables is fine, but the focus should be on 
## story telling through visualisations
##
## aim to spend no more than 2 hours
