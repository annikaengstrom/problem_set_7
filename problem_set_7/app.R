#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(knitr)
library(tidyverse)
library(stringr)
library(kableExtra)
library(lubridate)
library(rebus)
library(shiny)
library(readr)


data <- read_rds("combined.rds")


# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Testing the effectiveness of poll weighting"),
  
  # Sidebar with a slider input for axis inputs 
  sidebarLayout(
    sidebarPanel(
      # Selecting variable for x axis
      radioButtons(inputId = "x", 
                  label = "Weighted or Unweighted Polled Republican Advantage",
                  choices = c("Unweighted" = "URA",
                              "Weighted" = "WRA"), 
                  selected = "URA"),
      
      # Selecting variable for z axis / point size
      selectInput(inputId = "z", 
                  label = "Present point size by:",
                  choices = c("Total number of votes" = "totalvotes",
                              "Total number of Republican votes" = "rep_votes", 
                              "Total number of Democratic votes" = "dem_votes",
                              "Total number of other votes" = "other_votes"), 
                  selected = "totalvotes")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)


# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    data %>% 
      ggplot(aes_string(x = input$x, y = data$ARA, size = input$z, color = data$win_party), xlim = c(-2.5, 2.5) ylim = c(-2.5, 2.5)) + 
      geom_point() +
      scale_color_manual(values = c("D" = "blue", "R" = "red", "UNDECIDED" = "green")) +
      xlab("Polled Republican Advantage") +
      ylab("Actual Results") +
      ggtitle("How accurate were the weighted versus unweighted polls in predicting the republican advantage in the last election?")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

