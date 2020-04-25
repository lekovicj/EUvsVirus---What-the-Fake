library(tidyverse)
library(extrafont)


wtfPalette <- list( "dark" = "#262626",
                    "red" = "#FA6132",
                    "yellow"= "#FEA508", 
                    "green" = "#00BDBF",
                    "light" = "#FDFEFE", 
                    "lightGrey" = "#f7f7f7")

wtfFont <- fonttable() %>% 
    filter(str_detect(FullName, "Roboto")) %>% 
    filter(!Italic) %>% 
    distinct(FamilyName, .keep_all = T)


backgroundCol <- wtfPalette$dark

foregroundCol <- wtfPalette$light

#create theme for Creative Equals
wtfTheme <- theme_minimal(base_family = wtfFont$FamilyName[2])+
    theme(line = element_line(colour = foregroundCol), 
          text = element_text(colour = foregroundCol), 
          rect = element_rect(fill = backgroundCol, colour = NA), 
          title = element_text(colour = foregroundCol),
          plot.background = element_rect(fill = backgroundCol, colour = backgroundCol),
          panel.grid = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0, unit = "pt")),
          axis.text.y = element_text(margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "pt")),
          strip.switch.pad.grid = unit(1, "pt"), 
          strip.switch.pad.wrap = unit(1, "pt"),
          legend.position = "none",
          plot.margin = margin(3, 3, 3,3, unit = "pt"))

update_geom_defaults("text", list(colour = foregroundCol, family = wtfFont$FamilyName[2], lineheight = 0.8))
update_geom_defaults("line", list(colour = foregroundCol))
update_geom_defaults("area", list(fill = foregroundCol))
update_geom_defaults("col", list(fill = foregroundCol)) # is there a way to update all geom fills?
update_geom_defaults("point", list(colour = foregroundCol))

theme_set(wtfTheme)
