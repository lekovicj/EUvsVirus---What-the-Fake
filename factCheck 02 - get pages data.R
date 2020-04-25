library(tidyverse)
library(xml2)
library(rvest)


dataRAW <- read_rds(path = "data/factCheck/pagesScraped.rds")

exclude <- read_rds(path = "data/factCheck/pageDetails.rds") %>% 
    select(link) %>% 
    distinct()

dataRAW <- dataRAW %>% 
    anti_join(exclude)

for(i in 1:nrow(dataRAW)) { 
    
    page <- read_html(dataRAW$link[i])
    
    details <- page %>% 
        html_nodes(".entry-content__text--smaller , .entry-content__button--smaller , .entry-content__text--explanation , .entry-title , strong , .entry-content__text--org") 
    
    
    details %>% 
        html_text() %>% 
        enframe(value = "text") %>% 
        left_join(details %>% 
                      html_attr("href") %>% 
                      enframe(value = "fullArticleUrl")) %>% 
        mutate(link = dataRAW$link[i]) %>% 
        bind_rows(read_rds("data/factCheck/pageDetails.rds")) %>% 
        distinct() %>% 
        write_rds(path = "data/factCheck/pageDetails.rds")
    
    
    print(paste(i,"###", dataRAW$link[i]))
    
    Sys.sleep(runif(n = 1, min = 0.3333, max = 3))
    
}
