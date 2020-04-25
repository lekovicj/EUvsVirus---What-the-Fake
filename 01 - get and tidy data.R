library(tidyverse)

storyTopic <- "mask"

source("00 - theme and styling.R")


# load and filter factcheck data ------------------------------------------

factCheck <- readRDS("C:/Users/so run so vain/Projects/EUvsVirus - What the Fake/data/factCheck.rds")

factCheckTopic <- factCheck %>% 
    filter(str_detect(str_to_lower(paste(title, explanation)), pattern = str_to_lower(storyTopic))) 

factCheckTopic %>% 
    ggplot(aes(x = date, y = 0))+
    geom_point(col = wtfPalette$red)


# get google trends for topic ---------------------------------------------
library(trendyy)

gTrendsRelative <- trendy(search_terms = c(storyTopic, "coronavirus", paste(storyTopic, "coronavirus"), "covid", paste(storyTopic, "covid")), 
                from = "2020-01-01", 
                to = strftime(Sys.Date(), "%Y-%m-%d"))


get_interest(gTrendsRelative) %>% 
    ggplot(aes(x = date, y = as.numeric(hits), col = keyword, group = keyword))+
    geom_line()

get_interest_city(gTrendsRelative)

get_interest_country(gTrendsRelative)

get_related_queries(gTrendsRelative)

gTrendsSpecific <- trendy(search_terms = c( paste(storyTopic, "coronavirus"),  paste(storyTopic, "covid")), 
                  from = "2020-01-01", 
                  to = strftime(Sys.Date(), "%Y-%m-%d"))

get_interest(gTrendsSpecific) %>% 
    mutate(date = lubridate::date(date)) %>% 
    ggplot(aes(x = date, y = keyword, col = hits))+
    geom_point()+
    geom_point(data = factCheckTopic, 
               aes(x = date, y = "factChecks"), 
               col = wtfPalette$red)+
    scale_colour_gradient(low = wtfPalette$light, high = wtfPalette$green, limits = c(-10, 100))

get_interest_city(gTrendsSpecific)

get_interest_country(gTrendsSpecific)

get_related_queries(gTrendsSpecific)

# finally, look at how this story spreads in the news agenda --------------

source(file = "GDELT 01 - get daily events.R")

searchGDELT(query = storyTopic) # this takes a while


files <- dir(path = "data/articleFiles/", full.names = T)

dataRAW <- lapply(X = files, FUN = read_csv) %>% 
    bind_rows() %>% 
    janitor::clean_names()

GDELTarticles <- dataRAW %>% 
    arrange(date) %>% 
    mutate(dt = date, 
           date = lubridate::date(date), 
           week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%  
    mutate(source = urltools::domain(url)) %>% 
    distinct(date, source, .keep_all = T) #because the same source will publish same article more than once a day

GDELTarticles %>% 
    write_csv(path = "data/GDELTarticles.csv")

GDELTIntensity <- read_csv(file = "data/dailyIntensity.csv") %>% 
    janitor::clean_names()



# super early prototype ---------------------------------------------------

storyTimeline <- get_interest(gTrendsSpecific) %>% 
    mutate(date = lubridate::date(date)) %>% 
    filter(keyword == paste(storyTopic, "coronavirus")) %>% 
    select(date, value = hits) %>% 
    mutate(source = "gTrends") %>% 
    bind_rows(GDELTIntensity %>% 
                  select(date, value) %>% 
                  filter(date >= as.Date("2020-01-01")) %>% 
                  mutate(value = scales::rescale(x = value, to = c(0,100), from = c(0, max(value))), 
                         source = "GDELT")) %>% 
    bind_rows(factCheckTopic %>% 
                  select(date) %>% 
                  mutate(value = 100, 
                         source = "FactCheck") %>% 
                  distinct())


storyTimeline %>%  
    ggplot(aes(x = date, y = value, fill = source))+
    geom_area(data = . %>% filter(source != "FactCheck"))+
    geom_point(data = . %>% filter(source == "FactCheck"), col=  wtfPalette$red, size = 4, shape = 1)+
    facet_grid(vars(source), scales = "free_y")+
    scale_fill_manual(values = c(wtfPalette$red, wtfPalette$yellow, wtfPalette$green))+
    theme(axis.text.y = element_blank())

# fine tuning that timeline prototype -------------------------------------


storyTimeline <- get_interest(gTrendsSpecific) %>% 
    mutate(date = lubridate::date(date)) %>% 
    filter(keyword == paste(storyTopic, "coronavirus")) %>% 
    select(date, value = hits) %>% 
    mutate(value = value/100,
           source = "gTrends") %>% 
    bind_rows(GDELTIntensity %>% 
                  select(date, value) %>% 
                  filter(date >= as.Date("2020-01-01")) %>% 
                  mutate(value = scales::rescale(x = value, to = c(0,1), from = c(0, max(value))), 
                         source = "GDELT")) %>% 
    bind_rows(factCheckTopic %>% 
                  select(date) %>% 
                  mutate(value = 100, 
                         source = "FactCheck") %>% 
                  distinct())
