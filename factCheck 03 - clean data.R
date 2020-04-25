library(tidyverse)

# clean pages data --------------------------------------------------------

pagesRAW <- read_rds(path = "data/factCheck/pagesScraped.rds")

pages <- pagesRAW %>% 
    select(-headerFullText, -fromURL) %>% 
    separate(col = titles, into = c("flag", "title"), sep = ":", extra = "merge") %>% 
    separate(col = dateCountries, into = c("date", "countries"), sep = "\\|", extra = "merge") 

pages <- pages %>% 
    mutate_all(.funs = str_trim) %>% 
    mutate(date = lubridate::ymd(date)) %>% 
    distinct()

# clean details data ------------------------------------------------------


detailsRAW <- read_rds(path = "data/factCheck/pageDetails.rds")

details <- detailsRAW %>% 
    select(-fullArticleUrl) %>% 
    pivot_wider(id_cols = link, 
                names_from = name, 
                values_from = text) %>% 
    rename(checkedBy = 2, 
           countries = 3, 
           flagTitle = 4, 
           explanation = 5, 
           sourceName = 6, 
           originatedFrom = 7) %>%
    separate(col = flagTitle, into = c("flag", "title"), sep = ":", extra = "merge") %>% 
    mutate(countries = str_remove(countries, "\\|"), 
           explanation = str_remove(explanation, "Explanation: "), 
           originatedFrom = str_remove(originatedFrom, "This false claim originated from: ")) %>% 
    mutate_all(.funs = str_squish) %>% 
    mutate(originatedFrom = str_to_lower(originatedFrom)) %>% 
    mutate(facebook = str_detect(originatedFrom, "facebook") | str_detect(originatedFrom, "fb"), 
           twitter = str_detect(originatedFrom, "twitter") | str_detect(originatedFrom, "@"), 
           whatsapp = str_detect(originatedFrom, "whatsapp"),
           youtube = str_detect(originatedFrom, "youtube"),
           social = str_detect(originatedFrom, "social")) %>% 
    left_join(detailsRAW %>% 
                  select(link, fullArticleUrl) %>% 
                  drop_na())

# clean full dataset ------------------------------------------------------

factCheck <- pages %>% 
    left_join(details %>% 
                  select(-checkedBy, -countries, -flag, -title, -sourceName))

factCheck <- factCheck %>% 
    mutate(week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>% 
    mutate(flag = str_to_lower(flag))

rm(details)
rm(detailsRAW) 
rm(pages) 
rm(pagesRAW)

write_rds(factCheck, path = "data/factCheck.rds")
write_csv(factCheck, path = "data/factCheck.csv")
