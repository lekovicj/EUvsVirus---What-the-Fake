#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(trendyy)
library(tidyverse)
library(ggfittext)

#setwd("C:\\Users\\so run so vain\\Projects\\EUvsVirus - What the Fake")

factCheck <- readRDS("data/factCheck.rds")

source("00 - theme and styling Shiny.R")
source(file = "GDELT 01 - get daily events.R")


shinyServer(function(input, output, session) {

    
    keyData <- eventReactive(input$shinyQ, {
        
        storyTopic <- req(input$shinyQ)
        
        factCheckTopic <- factCheck %>% 
            filter(str_detect(str_to_lower(paste(title, explanation)), pattern = str_to_lower(storyTopic))) %>% 
            mutate(checkedBy = str_remove(checkedBy, "Fact-Checked by: "))
        
        
        
        gTrendsSpecific <- trendy(search_terms = c( paste(storyTopic, "coronavirus")), 
                                  from = "2020-01-01", 
                                  to = strftime(Sys.Date(), "%Y-%m-%d"))
        
        searchGDELT(query = storyTopic) # this takes a while
        
        GDELTIntensity <- read_csv(file = "data/dailyIntensity.csv") %>% 
            janitor::clean_names()
        
        list(GDELTIntensity = GDELTIntensity, 
             gTrendsSpecific = gTrendsSpecific, 
             storyTopic = storyTopic, 
             factCheckTopic = factCheckTopic)
        
    })
    
    
    
    output$timeline <- renderPlot({
        
        storyTopic <- keyData()$storyTopic
        
        GDELTIntensity <- keyData()$GDELTIntensity
        
        gTrendsSpecific <- keyData()$gTrendsSpecific
        
        factCheckTopic <- keyData()$factCheckTopic
        
        
        # load and filter factcheck data ------------------------------------------
        
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
                      size = 5,
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
                      size = 5,
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
                      size = 5,
                      hjust = 0, vjust = 0.5, 
                      col = wtfPalette$light)+
            
            #label peak day
            geom_rect(data = . %>% filter(!is.na(peakTitle)), 
                      aes(xmin = date - 30, xmax = date+30, 
                          ymin = 3.1, ymax = 5), 
                      fill = wtfPalette$yellow)+
            ggfittext::geom_fit_text(data = . %>% filter(!is.na(peakTitle)), 
                                     aes(xmin = date - 30, xmax = date+30, 
                                         ymin = 4.2, ymax = 4.5, 
                                         label = strftime(date, format = "%d %b")), 
                                     col = wtfPalette$light,
                                     grow = T, 
                                     padding.x = unit(3, "mm"),
                                     place = "left", 
                                     fontface = "bold")+
            ggfittext::geom_fit_text(data = . %>% filter(!is.na(peakTitle)), 
                                     aes(xmin = date - 30, xmax = date+30, 
                                         ymin = 4.5, ymax = 5, 
                                         label = peakChecked), 
                                     col = wtfPalette$light,
                                     grow = T, 
                                     padding.x = unit(3, "mm"),
                                     padding.y = unit(3, "mm"),
                                     fontface = "bold",
                                     place = "left")+
            ggfittext::geom_fit_text(data = . %>% filter(!is.na(peakTitle)), 
                                     aes(xmin = date - 30, xmax = date+30, 
                                         ymin = 3.1, ymax = 4.2, 
                                         label = peakTitle), 
                                     col = wtfPalette$light,
                                     #grow = T, 
                                     padding.x = unit(3, "mm"),
                                     padding.y = unit(3, "mm"),
                                     fontface = "plain",
                                     place = "left",
                                     reflow = T)+
            geom_segment(data = . %>% filter(!is.na(peakTitle)), 
                         aes(x = date - 30, xend = date+30, 
                             y = 3.1, yend = 3.1), 
                         col = wtfPalette$light, 
                         size = 1)+
            geom_segment(data = . %>% filter(!is.na(peakTitle)), 
                         aes(x = date, xend = date, 
                             y = 2.5, yend = 3.1), 
                         col = wtfPalette$light, 
                         size = 1)+
            
            
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


        
    }) 
    
    output$platforms <- renderPlot({
        
        storyTopic <- keyData()$storyTopic
        
        GDELTIntensity <- keyData()$GDELTIntensity
        
        factCheckTopic <- keyData()$factCheckTopic
        
        
        factCheck %>% 
            select(title, date, facebook, twitter, whatsapp, youtube, social, instagram, messaging, website, other) %>% 
            gather(key = "platform", value = "onPlatform", -title, -date) %>% 
            filter(onPlatform) %>% 
            group_by(platform) %>% 
            summarise(countAll = n()) %>% 
            ungroup() %>% 
            mutate(pctAll = countAll/ sum(countAll)) %>% 
            left_join(factCheckTopic %>% 
                          select(title, date, facebook, twitter, whatsapp, youtube, social, instagram, messaging, website, other) %>% 
                          gather(key = "platform", value = "onPlatform", -title, -date) %>% 
                          filter(onPlatform) %>% 
                          group_by(platform) %>% 
                          summarise(count = n()) %>% 
                          ungroup() %>% 
                          mutate(pct = count/ sum(count))) %>% 
            mutate(pct = replace_na(pct, 0)) %>% 
            mutate(platform = reorder(platform, pct), 
                   higher = if_else(pct >= pctAll, 1, -1)) %>% 
            #print() %>% 
            ggplot(aes(y = platform))+
            geom_segment(aes(yend = platform, 
                             x = 0, xend = 1), 
                         size = 4, lineend = "round", 
                         col = wtfPalette$darkOffset)+
            
            geom_point(aes(x = pct), 
                       col = wtfPalette$yellow, 
                       size = 5)+
            geom_point(aes(x = pct), 
                       col = wtfPalette$dark, 
                       size = 1, shape = 16)+
            geom_segment(aes(yend = platform, 
                             x = pctAll, xend = pct), 
                         size = 1, lineend = "round", 
                         col = wtfPalette$dark)+
            geom_point(aes(x = pctAll), 
                       col = wtfPalette$light, 
                       size = 3, shape = 1)+
            geom_text(aes(x = pct+0.05*higher, label = scales::percent(pct, 1)), 
                      col = wtfPalette$yellow, 
                      fontface = "bold")+
            geom_text(aes(x = pctAll-0.04*higher, label = scales::percent(pctAll, 1)), 
                      col = wtfPalette$light, 
                      size = 3)+
            scale_x_continuous(labels = scales::percent, expand = expansion(c(0.05,0.1)))+
            labs(title = "Spread by platform", 
                 subtitle = paste("Proportion of", storyTopic, "\nstories fact-checked"), 
                 caption = "\nSOURCE\nPoynter", 
                 col = "")+
            theme(legend.position = "bottom",
                  axis.text.x = element_text(colour = foregroundCol), 
                  axis.text.y = element_text(colour = foregroundCol),
                  plot.title = element_text(face = "plain", size = 23, hjust = 0.1, colour = wtfPalette$yellow), 
                  plot.subtitle = element_text(face = "bold", size = 25, hjust = 0.1, colour = foregroundCol), 
                  plot.caption = element_text(face = "plain", size = 10, hjust = 0.5, colour = foregroundCol))
        })
    
    output$countries <- renderPlot({
        
        storyTopic <- keyData()$storyTopic
        
        gTrendsSpecific <- keyData()$gTrendsSpecific
        
        factCheckTopic <- keyData()$factCheckTopic
        
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
            top_n(n = 15, wt = hits) %>% 
            mutate(location = reorder(location, hits, FUN = max)) %>% 
            #print() %>% 
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
    })
    
    
    output$related <- renderPlot({
        
        storyTopic <- keyData()$storyTopic
        
        gTrendsSpecific <- keyData()$gTrendsSpecific
        
        
        top_related_searches <- get_related_queries(gTrendsSpecific) %>% 
            filter(related_queries == "top")
        
        top_related_searches %>%
            mutate(subject = as.numeric(subject)) %>% 
            top_n(n = 15, wt = subject) %>% 
            mutate(value =  reorder(value, subject)) %>% 
            ggplot(aes(y = value, x = subject))+ 
            geom_segment(aes(xend = 0, yend = value), 
                         size = 5, lineend = "round", 
                         col = wtfPalette$green)+
            scale_x_continuous(labels = c("Low\nvolume"," ", "High\nvolume"), breaks = c(0, 50, 100), position = "top")+
            labs(title = "Top related searches", 
                 subtitle = paste0("People who look for ", storyTopic, ",\nalso look for..."), 
                 caption = "\nSOURCE\nGoogle Trends", 
                 col = "")+
            theme(legend.position = "bottom",
                  axis.text.x = element_text(colour = foregroundCol), 
                  axis.text.y = element_text(colour = foregroundCol),
                  plot.title = element_text(face = "plain", size = 23, hjust = 0.1, colour = wtfPalette$yellow), 
                  plot.subtitle = element_text(face = "bold", size = 25, hjust = 0.1, colour = foregroundCol), 
                  plot.caption = element_text(face = "plain", size = 10, hjust = 0.5, colour = foregroundCol))
    })
    
    output$mostRecentStory <- renderText({
        
        factCheckTopic <- keyData()$factCheckTopic
        
        mostRecent <- factCheckTopic %>% 
            arrange(desc(date)) %>% 
            filter(row_number() == 1)
        
        title <- h3(mostRecent$title)
        
        checkedBy <- h4(mostRecent$checkedBy)
        date <- h4(strftime(mostRecent$date, "%d %b %y"))
        flag <- h4(str_to_upper(mostRecent$flag), class = "strong")
        
        explanation <- p(mostRecent$explanation, class = "blockquote")
        
        paste(title, checkedBy, flag, date, explanation)
        
        
        
    }, sep = "\n")

})
