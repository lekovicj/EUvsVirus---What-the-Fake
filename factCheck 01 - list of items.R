library(tidyverse)
library(xml2)
library(rvest)
library(selectr)

#https://www.poynter.org/ifcn-covid-19-misinformation/

#also: https://maldita.es/malditobulo/2020/04/02/coronavirus-bulos-pandemia-prevenir-virus/


url <- "https://www.poynter.org/ifcn-covid-19-misinformation/?orderby=views&order=ASC"

homePage <- read_html(url)

nav <- html_nodes(homePage, ".nav-links")

navLinks <- nav %>%
    html_nodes("a") %>%
    html_attr("href") %>% 
    enframe(value = "link")

maxPages <- navLinks$link[3] %>% 
    str_remove(pattern = "https://www.poynter.org/ifcn-covid-19-misinformation/page/") %>% 
    str_remove(pattern = "\\/\\?orderby=views&order=ASC#038;order=ASC") %>% 
    as.numeric()

minPage <- read_rds("data/factCheck/pagesScraped.rds") 

minPage <- minPage %>% 
    select(fromURL) %>% 
    mutate(minPage = str_remove(fromURL, pattern = "https://www.poynter.org/ifcn-covid-19-misinformation/page/") , 
           minPage = str_remove(minPage, pattern = "\\/\\?orderby=views&order=ASC#038;order=ASC"),
           minPage = as.numeric(minPage)) %>% 
    filter(minPage == max(minPage, na.rm = T)) %>% 
    distinct()

nextPage <- minPage$fromURL

minPage <- minPage$minPage


# then a big ol loop to get all the pages ---------------------------------

for(i in minPage:maxPages) {
    
    url <- nextPage
    
    homePage <- read_html(url)
    
    nav <- html_nodes(homePage, ".nav-links")
    
    items <-  html_nodes(homePage, ".post-container")
    
    headers <- items %>% 
        html_nodes(".entry-header")
    
    content <- items %>% 
        html_nodes(".entry-content__text")
    
    titles <- items %>% 
        html_nodes(".entry-title") 
    
    navLinks <- nav %>%
        html_nodes("a") %>%
        html_attr("href") %>% 
        enframe(value = "link")
    
    headers <- headers %>% 
        html_text() %>% 
        enframe(value = "headerFullText")
    
    titlesLinks <- titles %>% 
        html_nodes("a") %>% 
        html_attr("href") %>% 
        enframe(value = "link")
    
    titles <- titles %>% 
        html_text() %>% 
        enframe(value = "titles")
    
    content <- content %>% 
        html_text() %>% 
        enframe() %>% 
        mutate(name = name %% 2, 
               id = row_number() - row_number() %/% 2) %>% 
        pivot_wider(id_cols = id, names_from = name, values_from = value) %>% 
        rename(name = id, checkedBy = 2, dateCountries = 3)
    
    data <- titles %>% 
        left_join(titlesLinks) %>% 
        left_join(content) %>% 
        left_join(headers) %>% 
        mutate(fromURL = url)
    
    data %>% 
        bind_rows(read_rds(path = "data/factCheck/pagesScraped.rds")) %>% 
        distinct() %>% 
        write_rds(path = "data/factCheck/pagesScraped.rds")
    
    print(url)
    
    nextPage <- navLinks$link[nrow(navLinks)]
    
    Sys.sleep(runif(n = 1, min = 0.5, max = 5))
    
}

