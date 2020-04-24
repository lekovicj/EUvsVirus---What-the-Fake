library(tidyverse)

storyTopic <- "bill gates"


# start by looking at where these appear on factCheck ---------------------
library(rvest)
library(xml2)

page <- 1

url <- function(page = 1, topic = storyTopic) {paste0("https://www.poynter.org/ifcn-covid-19-misinformation/page/",page,"/?search_terms=",str_replace_all(topic, " ", "+"),"&orderby=views&order=ASC")}

homePage <- read_html(url)

nav <- html_nodes(homePage, ".nav-links")

navLinks <- nav %>%
    html_nodes("a") %>%
    html_attr("href") %>% 
    enframe(value = "link")

maxpage <- navLinks %>% 
    arrange(desc(link)) %>% 
    filter(row_number() == 1)

tibble(name = "",
       titles = "",
       link = "",
       checkedBy = "",
       dateCountries = "",
       headerFullText = "", 
       fromURL = "") %>% write_csv(path = "data/pagesScraped.csv")
