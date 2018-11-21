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
                  selected = "URA")
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
      ggplot(aes_string(x = input$x, y = data$ARA, color = data$win_party)) + 
      geom_point() +
      scale_x_continuous(name="Polled Republican Advantage", limits=c(-0.25, 0.25)) +
      scale_y_continuous(name="Actual Republican Advantage", limits=c(-0.25, 0.25)) +
      scale_color_manual(values = c("D" = "blue", "R" = "red", "UNDECIDED" = "green")) +
      ggtitle("How accurate were the weighted versus unweighted polls in predicting the republican advantage in the last election?")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

