library(tidyverse)
library(ggplot2)

data <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))

data <-
  data %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(grepl("^OWID_",iso_code) == FALSE)
  
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


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#
# task1: where confirmed cases are increasing / decreasing (geographically)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

task1 <- 
  data %>%
  dplyr::select(iso_code, location, date 
                , new_cases, new_cases_smoothed 
                , new_cases_per_million, new_cases_smoothed_per_million
                , reproduction_rate) %>%
  group_by(iso_code, location) %>% 
  arrange(date, .by_group = TRUE) %>%
  mutate(pct_change = (new_cases/lag(new_cases) - 1) * 100)

view(task1)  

# chart1: map of coutries, new_cases_smoothed_per_million - filter on dates
# task1 %>%
#   ggplot(aes(new_cases_smoothed_per_million)) +
  

# chart2: line chart, reproduction rate by date
task1 %>%
  dplyr::filter(location == "United Kingdom") %>%
  ggplot(aes(date, reproduction_rate)) +
  geom_line() +
  ylab("Reproduction rate") +
  labs(
    title = "How contagious is Covid-19?"
    , subtitle = "Avg no. of people who will contract Covid-19 from one person infected") +
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  theme_bw()




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#
# task2: where case fatality rates have been highest or are currently 
#        increasing / decreasing
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

task2 <- 
  data %>%
  dplyr::select(iso_code, location, date
                , total_deaths, total_deaths_per_million 
                , total_cases) %>%
  #mortality in patients at risk
  dplyr::mutate(case_fatality_rate = total_deaths / total_cases) %>% 
  dplyr::filter(total_cases > 0) %>%
  dplyr::group_by(iso_code) %>%
  dplyr::filter(date == max(date)) #%>%
  #view()

view(task2)

# chart3: scatterplot, mortality vs totald_deaths_per_million
task2 %>%
  ggplot(aes(case_fatality_rate, total_deaths_per_million)) +
  geom_jitter() +
  ylab("Deaths per million") +
  xlab("Case fatality rate") +
  labs(title = "How deadly is Covid-19?") +
  theme_bw()




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#
# task3: which countries have been most ‘successful’ at vaccinating their 
#        population, to date
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

task3 <- 
  data %>%
  dplyr::select(iso_code, location, date
                , people_vaccinated_per_hundred 
                , people_fully_vaccinated_per_hundred) %>%
  dplyr::filter(people_vaccinated_per_hundred >= 0 & people_fully_vaccinated_per_hundred >= 0) %>%
  dplyr::group_by(iso_code) %>%
  dplyr::filter(date == max(date)) #%>%
  # view()


# chart4: scatterplot, people_vaccinated_per_hundred vs people_fully_vaccinated_per_hundred
task3 %>%
  ggplot(aes(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)) +
  geom_jitter() +
  ylab("People vaccinated") +
  xlab("Fully vaccinated") +
  labs(title = "How deadly is Covid-19?") +
  theme_bw()


glimpse(data)  
  