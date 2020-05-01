library(tidyverse)

#from https://api.gdeltproject.org/api/v2/summary/summary?d=web&t=summary&k=covid+5g&ts=full&fsc=SP&svt=zoom&stc=yes&slm=city&sitwc=yes&siwtwc=yes&sta=list&sti=gallery&c=1

#this gets GDELT data on COVID appearing in online news, which allows us to do the following: 

searchGDELT <- function(query = "garlic"){
    
    search <- paste("(covid OR coronavirus)", query)
    
    lookupDates <- seq.Date(from = as.Date("2020-01-01"), to = Sys.Date()-1, by = 1)
    
    base <- "https://api.gdeltproject.org/api/v2/doc/doc"
    
    csv <- "?format=csv&"
    
    #html <- "?format=html&"
    
    dates <- paste0("startdatetime=", format(lookupDates[40], "%Y%m%d"),
                    "000000&enddatetime=", format(lookupDates[40], "%Y%m%d"), "235959")
    
    query <- paste0("&query=", str_replace_all(search, " ", "%20"))
    
    #timeSeries <- "&mode=timelinevol&timezoom=yes"
    
    articles <- "&mode=artlist&maxrecords=250&format=csv&sort=hybridrel"
    
    #cities <- "&mode=pointdata&format=csv" # and this doesn't filter on time, so... meh
    
    #sentiment <- "&mode=tonechart&format=csv" # generally looks crap
    
    
    download.file(paste0("https://api.gdeltproject.org/api/v2/doc/doc?format=csv&timespan=FULL", query, "&mode=timelinevol&timezoom=yes"), 
                  destfile = "data/dailyIntensity.csv", quiet = T)
    
    timeline <- read_csv("data/dailyIntensity.csv") 
    
    lookupDates <- timeline %>% 
        filter(Value > 0.001) %>% 
        filter(Value == max(Value) | Date == min(Date)) %>% 
        select(Date) 
    
    
    for(i in 1:length(lookupDates$Date)) {
        
        print(lookupDates$Date[i])
        
        dates <- paste0("startdatetime=", format(lookupDates$Date[i], "%Y%m%d"),
                        "000000&enddatetime=", format(lookupDates$Date[i], "%Y%m%d"), "235959")
        
        # download.file(url = paste0(base, csv,dates,query, timeSeries), 
        #               destfile = paste0("data/intensityFiles/intensity",format(lookupDates[i], "%Y%m%d"),".csv"), 
        #               method = "curl", quiet = T)
        # 
        # Sys.sleep(0.333333)
        
        # file.remove(dir(path = "data/articleFiles/", full.names = T))
        # 
        # download.file(url = paste0(base, csv,dates,query, articles), 
        #               destfile = paste0("data/articleFiles/articles",format(lookupDates$Date[i], "%Y%m%d"),".csv"), 
        #               method = "curl", quiet = T)
        # 
        # Sys.sleep(0.333333)
        
    }
    
    
}
