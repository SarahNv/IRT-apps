
library(shiny)
library(tidyverse)
library(ggplot2)

# load dataset
irt_data <- readr::read_csv("raschappdata.csv")

#function
neg3_func <- function(difficulty, ability){
  exp(ability - difficulty) / (1 + (exp(ability - difficulty)))
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IRT: Rasch App"),
    p("This is an application designed to help you think about the relationship between latent 
      ability and item difficulty in the probability of endorsing an item correctly. 
      Select an item difficulty and an individual's latent ability in the sliding inputs 
      below to get started."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(fluid = FALSE,
                  sidebarPanel(
                    sliderInput("Select1", label = "Item Difficulty",
                                min = min(irt_data$difficulty), max=max(irt_data$difficulty), 
                                value = 0, step = 1),
                    
                    sliderInput("Select2", label = "Latent Ability",
                                min = min(irt_data$ability), max=max(irt_data$ability),
                                value = 0, step = 1)
                  ),
                  
                  mainPanel(
                    h3(textOutput("caption"), align = "left"),
                    plotOutput("plot"),
                    
                    uiOutput("prob")
                  
                  ),
    ),
    
)

                    
                
        


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Title main area
  # ----------------------------------
  output$caption <- renderText({
    paste("Item response function plot")
  })
  
  # Reactive elements
  point <- reactive({
    irt_data %>% filter(difficulty %in% input$Select1 & ability == input$Select2)
  })
  
  dataset <- reactive({
    irt_data %>% filter(difficulty %in% input$Select1)
  })
  
  output$plot <- renderPlot({
    dataset() %>% ggplot(aes(ability, prob)) +
      geom_smooth(method = "loess", span = 0.3, se = FALSE) + 
      scale_x_continuous(name = "Ability", limits = c(-4, 4), breaks = seq(-4,4,1)) +
      scale_y_continuous(name = "Probability", limits = c(0, 1), breaks = seq(0, 1, .25)) +
      geom_point(data = point(), aes(ability, prob), colour="red", shape = 17, size = 5)
      
    
  })
  
  output$prob <- renderUI ({
    p <- (round((exp(input$Select2 - input$Select1)) / (1 + (exp(input$Select2 - input$Select1))), digits = 2) * 100)
    tags$div(paste("The probability of a student answering a question with a difficulty level
                   of", input$Select1, "with a latent ability of", input$Select2, "is", p, "%", "as
                   noted by the red triangle in the graph above."))
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
