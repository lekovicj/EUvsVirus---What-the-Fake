library(tidyverse)

#this cleans the scraped GDELT data and puts it togethe for plots and analysis

# clean articles ----------------------------------------------------------

files <- dir(path = "data/articleFiles/", full.names = T)

dataRAW <- lapply(X = files, FUN = read_csv) %>% 
    bind_rows() %>% 
    janitor::clean_names()

articles <- dataRAW %>% 
    arrange(date) %>% 
    mutate(dt = date, 
           date = lubridate::date(date), 
           week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%  
    mutate(source = urltools::domain(url)) %>% 
    distinct(date, source, .keep_all = T) #because the same source will publish same article more than once a day

articles %>% 
    write_csv(path = "data/articles.csv")

# clean time series -------------------------------------------------------

files <- dir(path = "data/intensityFiles/", full.names = T)

dataRAW <- lapply(X = files, FUN = read_csv) %>% 
    bind_rows() %>% 
    janitor::clean_names()

intensity <- dataRAW %>% 
    mutate(dt = date, 
           date = lubridate::date(date), 
           week = lubridate::floor_date(date, unit = "week", week_start = 1))

write_csv(intensity, path = "data/newsIntensity.csv")

dailyIntensity <- read_csv(file = "data/dailyIntensity.csv") %>% 
    janitor::clean_names()



