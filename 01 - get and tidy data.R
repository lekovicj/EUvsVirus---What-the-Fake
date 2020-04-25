library(tidyverse)

storyTopic <- "bleach"

source("00 - theme and styling.R")


# load and filter factcheck data ------------------------------------------

factCheck <- readRDS("C:/Users/so run so vain/Projects/EUvsVirus - What the Fake/data/factCheck.rds")

factCheckTopic <- factCheck %>% 
    filter(str_detect(str_to_lower(paste(title, explanation)), pattern = str_to_lower(storyTopic))) %>% 
    mutate(checkedBy = str_remove(checkedBy, "Fact-Checked by: "))

factCheckTopic %>% 
    ggplot(aes(x = date, y = 0))+
    geom_point(col = wtfPalette$red)


# get google trends for topic ---------------------------------------------
library(trendyy)

# gTrendsRelative <- trendy(search_terms = c(storyTopic, "coronavirus", paste(storyTopic, "coronavirus"), "covid", paste(storyTopic, "covid")), 
#                           from = "2020-01-01", 
#                           to = strftime(Sys.Date(), "%Y-%m-%d"))


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

#get_interest_city(gTrendsSpecific)

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

gTrendsIndex <- 0

gdeltIndex <- 1

factCheckIndex <- 2

gTrendsTimeline <- get_interest(gTrendsSpecific) %>% 
    mutate(date = lubridate::date(date)) %>% 
    filter(keyword == paste(storyTopic, "coronavirus")) %>% 
    select(date, value = hits) %>% 
    mutate(valueScaled = gTrendsIndex+scales::rescale(x = value, to = c(0.2,0.8), from = c(0, max(value))),
           source = "gTrends") 

#we're going to add labels to this guy, because that keeps the flow clean
gdeltTimeline <- GDELTIntensity %>% 
    select(date, value) %>% 
    filter(date >= as.Date("2020-01-01")) %>% 
    mutate(valueScaled = gdeltIndex+scales::rescale(x = value, to = c(0.2,0.8), from = c(0, max(value))), 
           source = "GDELT")

factCheckTimeline <- factCheckTopic %>% 
    group_by(date) %>% 
    summarise(value = n()) %>% 
    ungroup() %>% 
    mutate(valueScaled = factCheckIndex+scales::rescale(x = value, to = c(0,1), from = c(0, max(value))))

#


gdeltTimeline <- gdeltTimeline %>% 
    left_join(factCheckTopic %>% 
                  arrange(date) %>%
                  filter(row_number() == 1) %>% 
                  select(date, firstTitle = title, firstChecked = checkedBy)) %>% 
    left_join(factCheckTopic %>% 
                  arrange(date) %>%
                  filter(row_number() != 1) %>% 
                  inner_join(factCheckTimeline %>% 
                                 arrange(-value) %>% 
                                 filter(row_number() == 1) %>% 
                                 select(date)) %>% 
                  filter(row_number() == 1) %>% 
                  select(date, peakTitle = title, peakChecked = checkedBy)) %>% 
    mutate(labelLimit = factCheckIndex+1)
    
    


ggplot(data = gdeltTimeline, 
       aes(x = date))+
    
    # google trends
    geom_rect(aes(xmin = min(date)-10, xmax = max(date), 
                  ymin = gTrendsIndex+0.2, ymax = gTrendsIndex+0.8), fill = wtfPalette$green)+
    geom_ribbon(data = gTrendsTimeline,
                aes(ymin = gTrendsIndex+0.2, ymax = valueScaled), fill = wtfPalette$light)+
    geom_text(aes(x = min(date)-8, y = gTrendsIndex+0.5), 
              label = "Google\nsearches", 
              fontface = "bold",
              check_overlap = T, 
              hjust = 0, vjust = 0.5, 
              col = wtfPalette$light)+
    
    #GDELT data
    geom_rect(aes(xmin = min(date)-10, xmax = max(date), 
                  ymin = gdeltIndex+0.2, ymax = gdeltIndex+0.8), fill = wtfPalette$red)+
    geom_ribbon(data = gdeltTimeline,
                aes(ymin = gdeltIndex+0.2, ymax = valueScaled), fill = wtfPalette$light)+
    geom_text(aes(x = min(date)-8, y = gdeltIndex+0.5), 
              label = "News\ncoverage",
              fontface = "bold",
              check_overlap = T, 
              hjust = 0, vjust = 0.5, 
              col = wtfPalette$light)+
    #factCheck data
    geom_rect(aes(xmin = min(date)-10, xmax = max(date), 
                  ymin = factCheckIndex+0.2, ymax = factCheckIndex+0.8), fill = wtfPalette$yellow)+
    geom_line(aes(y = labelLimit), col = backgroundCol)+ # theoretically this should stop labels from overlapping with viz, but doesn't do shit
    geom_point(data = factCheckTimeline, 
               aes(y = factCheckIndex+0.5, size = valueScaled), 
               col = wtfPalette$light, shape = 16)+
    geom_point(data = factCheckTimeline, 
               aes(y = factCheckIndex+0.5, size = valueScaled/2), 
               col = wtfPalette$yellow, shape = 1)+
    geom_text(aes(x = min(date)-8, y = factCheckIndex+0.5), 
              label = "Fact\nchecks", 
              fontface = "bold",
              check_overlap = T, 
              hjust = 0, vjust = 0.5, 
              col = wtfPalette$light)+
    
    #label first fakenews
    # ggforce::geom_mark_circle(aes(y = factCheckIndex+0.5,
    #                               filter = !is.na(firstTitle),
    #                               label = paste0(firstChecked,"\n",strftime(date, format = "%d %b")), 
    #                               description = firstTitle),
    #                           col = wtfPalette$lightGrey, 
    #                           position = position_dodge(width = 5),
    #                           label.fontsize = c(15,8),
    #                           label.fill = wtfPalette$yellow,
    #                           label.colour = wtfPalette$light,
    #                           con.colour = wtfPalette$light )+
    
    #label peak day
    ggforce::geom_mark_circle(aes(y = factCheckIndex+0.5,
                                  filter = !is.na(peakTitle),
                                  label = paste0(peakChecked,"\n",strftime(date, format = "%d %b")), 
                                  description = peakTitle),
                              col = wtfPalette$lightGrey,
                              position = position_dodge(width = 5),
                              label.fontsize = c(15,8),
                              label.fill = wtfPalette$yellow,
                              label.colour = wtfPalette$light,
                              con.colour = wtfPalette$light )+
    
    
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b", position = "bottom")+
    scale_y_continuous(limits = c(0, 5))+
    scale_size(range = c(3, 10))+
    labs(title = "Fake news about", 
         subtitle = paste("Covid-19 and...", storyTopic), 
         caption = "\nSOURCES\nPoynter | The GDELT Project | Google Trends")+
    theme(#aspect.ratio = 1/1.5,
        axis.ticks.x = element_line(colour = foregroundCol, lineend = "round", size = 0.3),
        axis.text.y = element_blank(), 
        axis.text.x = element_text(colour = foregroundCol),
        plot.title = element_text(face = "plain", size = 23, hjust = 0.1, colour = wtfPalette$yellow), 
        plot.subtitle = element_text(face = "bold", size = 25, hjust = 0.1, colour = foregroundCol), 
        plot.caption = element_text(face = "plain", size = 10, hjust = 0.5, colour = foregroundCol))

ggsave(filename = paste0("plots/timeline ",storyTopic,".png"), 
       type = "cairo", dpi = "retina", 
       width = 30, height = 17, units = "cm", scale = 0.9)

# so next up - which countries is this biggest in -------------------------

topCountries <- get_interest_country(gTrendsSpecific) %>% 
    drop_na(hits) %>% 
    arrange(-hits) %>% 
    group_by(location) %>%
    filter(row_number() == 1) %>% 
    ungroup()

factCheckTopic %>% 
    distinct(countries) %>% 
    separate_rows(countries, sep = ",") %>% 
    mutate(countries = str_squish(countries)) %>% 
    group_by(countries) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    arrange(-count) %>% 
    rename(location = countries) %>% 
    right_join(topCountries) %>% 
    filter(hits > 50) %>% 
    mutate(location = reorder(location, hits, FUN = max)) %>% 
    print() %>% 
    ggplot(aes(x = hits, y = location, col = is.na(count)))+
    geom_segment(aes(xend = 0, yend = location), 
                 size = 5, lineend = "round")+
    scale_colour_manual(values = c(wtfPalette$green, wtfPalette$yellow), limits = c(TRUE, FALSE), 
                        labels = c("Not fact-checked\nin country", "Fact-checked\nin country"))+
    scale_x_continuous(labels = c("Low\nvolume"," ", "High\nvolume"), breaks = c(0, 50, 100), position = "top")+
    labs(title = "Countries by interest", 
         subtitle = paste("Searches for", storyTopic, "and\ncovid or coronavirus"), 
         caption = "\nSOURCES\nPoynter | Google Trends", 
         col = "")+
    theme(#aspect.ratio = 1/1.5,
        legend.position = "bottom",
        axis.text.x = element_text(colour = foregroundCol), 
        axis.text.y = element_text(colour = foregroundCol),
        plot.title = element_text(face = "plain", size = 23, hjust = 0.1, colour = wtfPalette$yellow), 
        plot.subtitle = element_text(face = "bold", size = 25, hjust = 0.1, colour = foregroundCol), 
        plot.caption = element_text(face = "plain", size = 10, hjust = 0.5, colour = foregroundCol))


ggsave(filename = paste0("plots/searchInterest ",storyTopic,".png"), 
       type = "cairo", dpi = "retina", 
       width = 17, height = 17, units = "cm", scale = 0.9)
