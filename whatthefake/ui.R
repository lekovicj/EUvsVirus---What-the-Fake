#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",

    # Application title
    #titlePanel("What the Fake!?"),
    
    img(src= "What the Fake!_small.png"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "shinyQ",
                        label = "Search for a topic"), 
            submitButton("Search", icon("search"))
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("shinyQ")
        )
    ),
    
    br(),
    
    h2("How has the topic changed over time?"),
    
    p("By looking at how many times the story has been fact-checked, how much volume it's seeing in news coverage."),
    
    plotOutput("timeline"), 
    
    br(),
    
    h2("Where do these fake news come from?"),
    
    p("These are the platforms fact-checkers noticed articles related to this story."),
    
    plotOutput("platforms"),
    
    br(),
    
    h2("Which countries are potentially at risk?"),
    
    p("These are the countries where people search for the topic, but might not have been fact-checked."),
    
    plotOutput("countries"),
    
    br(),
    
    h2("What other topics are people googling for?"),
    
    p("Other search terms that are likely to crop up."),
    
    plotOutput("related"),
    
))
