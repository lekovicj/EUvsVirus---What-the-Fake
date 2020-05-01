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
    mutate(facebook = str_detect(originatedFrom, "facebook") | str_detect(originatedFrom, "fb") | 
               str_detect(originatedFrom, "facebok") | str_detect(originatedFrom, "faacebook"), 
           twitter = str_detect(originatedFrom, "twitter") | str_detect(originatedFrom, "@") | str_detect(originatedFrom, "tweet") | str_detect(originatedFrom, "twtter"), 
           whatsapp = str_detect(originatedFrom, "whatsapp") | str_detect(originatedFrom, "whatapp"),
           youtube = str_detect(originatedFrom, "youtube"),
           instagram = str_detect(originatedFrom, "instagram"),
           social = str_detect(originatedFrom, "social"), 
           messaging = str_detect(originatedFrom, "text") | str_detect(originatedFrom, "message"), 
           website = str_detect(originatedFrom, "website") | str_detect(originatedFrom, "http") | str_detect(originatedFrom, "www")| str_detect(originatedFrom, "blog"),
           other = !facebook & !twitter & !whatsapp & !youtube & !social & !instagram & !messaging & !website) %>% 
    left_join(detailsRAW %>% 
                  select(link, fullArticleUrl) %>% 
                  drop_na())

# clean full dataset ------------------------------------------------------

factCheck <- pages %>% 
    left_join(details %>% 
                  select(-checkedBy, -countries, -flag, -title, -sourceName))

factCheck <- factCheck %>% 
    mutate(week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>% 
    mutate(flag = str_to_lower(flag)) %>% 
    distinct(link, .keep_all = T)

rm(details)
rm(detailsRAW) 
rm(pages) 
rm(pagesRAW)

write_rds(factCheck, path = "data/factCheck.rds")
write_csv(factCheck, path = "data/factCheck.csv")


# copy to shiny 

file.copy(from = "data/factCheck.rds", to = "whatthefake/data/factCheck.rds", 
          overwrite = T)
