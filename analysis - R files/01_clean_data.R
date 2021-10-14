library(tidyverse)
library(rvest)
library(lubridate)
library(glue)


path <- "C:\\Users\\User\\Desktop\\University\\Semester D\\Advanced Data Analysis in R\\FinalProject"

####################################  LOAD CASES TABLE  ####################################

url  <-
  'https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/United_States_medical_cases'

load_cases_table <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[2]/table') %>%
  html_table(fill = TRUE, header = FALSE)
load_cases_table  <-  load_cases_table[[1]] %>% 
  head(-5)
load_cases_table[load_cases_table==""]<-"0"


####################################  CLEAN MAIN TABLE  ####################################

states <- as.vector(load_cases_table[2, ])
states <-
  as.character(states[grep(pattern = "[A-Z][A-Z]", states)])
col_names <- c("Date", states)

covid_table <- load_cases_table %>%
  select(seq(1, 56)) %>%
  filter(across(X1, ~ !grepl('Date', .)))

colnames(covid_table) <- col_names

covid_table <- covid_table %>%
  mutate(Date = dmy(Date)) %>%
  mutate_at(vars(-"Date"),~sub(",","",.)) %>%
  mutate_if(is.character, as.numeric) 

# View(covid_table)


####################################  CREATE REGIONS TABLE  ####################################


regions <- as.vector(load_cases_table[1, ])
regions <-
  as.character(regions[grepl('West|Midwest|South|Northeast|Territories', regions)])

state_region <- data.frame(states, regions)
colnames(state_region) <- c('state', 'region')

# View(state_region)


####################################  CREATE INFORMATION TABLE  ####################################

daily_information <- load_cases_table %>%
  select(X57, X58, X60, X62, X64) %>%
  filter(across(X57, ~ !grepl('Date|VI', .)))

colnames(daily_information) <-
  c('Date',
    'Daily_Confirmed',
    'Daily_Death',
    'Daily_Recovered',
    'Total_Active')

daily_information <- daily_information %>%
  mutate(Date = dmy(Date))

daily_information <- daily_information %>%
  mutate_at(vars(-"Date"),~gsub(",","",.)) %>%
  mutate_if(is.character, as.numeric)

# View(daily_information)


####################################  CREATE DEATH TABLE  ####################################


load_death_table <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[3]/table') %>%
  html_table(fill = TRUE, header = FALSE)
load_death_table  <-  load_death_table[[1]] %>% 
  head(-5)

load_death_table[load_death_table==""]<-"0"

death_table <- load_death_table %>%
  select(seq(1, 56)) %>%
  filter(across(X1, ~ !grepl('Date', .)))

colnames(death_table) <- col_names

death_table <- death_table %>%
  mutate(Date = dmy(Date)) %>%
  mutate_at(vars(-"Date"),~sub(",","",.)) %>%
  mutate_if(is.character, as.numeric) 


# View(death_table)


####################################  CREATE STATES FULLNAME TABLE  ####################################

states_fullnames <- c('Alaska','Arizona','California','Colorado','Hawaii','Idaho','Montana','New Mexico','Nevada','Oregon','Utah','Washington','Wyoming','Iowa','Illinois','Indiana','Kansas','Michigan','Minnesota','Missouri','North Dakota','Nebraska','Ohio','Oklahoma','South Dakota','Wisconsin','Alabama','Arkansas','Florida','Georgia','Kentucky','Lousiana','Mississippi','North Carolina','South Carolina','Tennessee','Texas','Virginia','West Verginia','Connecticut','Washington DC','Delaware','Massachusetts','Maryland','Maine','New Hampshire','New Jersey','New York','Pennsylvania','Rhode Island','Vermont','Guam','Mariana Islands','Puerto Rico','Virgin Islands')
states_fullname <- data.frame(states, states_fullnames)
colnames(states_fullname) <- c('state', 'fullname')

# View(states_fullname)


####################################  SAVE TABLES  ####################################

path <-  "D:\\Omer - Main\\University\\Year 2\\Semester B\\Advanced Programming\\Project"

covid_table <- write_rds(covid_table, file = glue("{path}\\data\\covid_table.rds"))
state_region <- write_rds(state_region, file = glue("{path}\\data\\state_region.rds"))
daily_information <- write_rds(daily_information, file = glue("{path}\\data\\daily_information.rds"))
death_table <- write_rds(death_table, file = glue("{path}\\data\\death_table.rds"))
states_fullname <- write_rds(states_fullname, file = glue("{path}\\data\\states_fullname.rds"))

