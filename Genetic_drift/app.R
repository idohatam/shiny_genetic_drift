#Load libraries
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plotly)

source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Sidebar with slide bars
  sidebarLayout(
    sidebarPanel(
      numericInput("ne", "Effective population size", 
                  min = 10, max = 15000, value = 100, step = 10),
      sliderInput("a1", "Proportion of allele A1", 
                  min = 0.1, max = 0.9, value = 0.5, step = 0.1),
      sliderInput("gen", "Number of generations", 
                  min = 10, max = 5000, value = 500, step = 10),
      sliderInput("sim", "Number of simulations", 
                  min = 1, max = 10, value = 5, step = 1)
    ),
    # Main page with plot output
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Run allele dynamics function with input from slide bars
  results <- reactive({
    allele_dynamics(input$ne, input$a1, input$gen, input$sim)
  })
  
  # Generate plot using ggplot2 and convert to plotly
  output$plot <- renderPlotly({
    gg <- ggplot(results(), aes(x = generation, y = A1, color = as.factor(simulation))) +
      geom_line(size = 1) +
      scale_color_brewer(palette = "Paired")
    ggplotly(gg)
  })
}

shinyApp(ui, server)
