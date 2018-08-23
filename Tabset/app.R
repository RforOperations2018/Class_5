# Class 1
# In Class Examples - Tabset

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(DT)

load_starwars <- starwars
load_starwars$films <- NULL
load_starwars$vehicles <- NULL
load_starwars$starships <- NULL
# Melt Data
load_meltwars <- melt(load_starwars, id = "name")
load_meltwars$name <- as.factor(load_meltwars$name)

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Star Wars Tabset"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        selectInput("char_select",
                    "Characters:",
                    choices = levels(load_meltwars$name),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("Luke Skywalker", "Darth Vader", "Jabba Desilijic Tiure", "Obi-Wan Kenobi", "R2-D2", "Dexter Jettster")),
        downloadButton("downloadData", "Download")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", 
                   plotlyOutput("plot")
                 ),
          tabPanel("Table",
                   DT::dataTableOutput("table")
                 )
        )
      )
   )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlotly({
    # Get reactive Data
    dat <- meltWarsInput()
    # Plot Selections
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # Star Wars Table Data
  starWarsInputs <- reactive({
    starwars <- load_starwars
    # Subset
    starwars <- subset(starwars, name %in% input$char_select)
    
    return(starwars)
  })
  # Reshaped Star Wars Data
  meltWarsInput <- reactive({
    meltwars <- load_meltwars
    # Subset
    meltwars <- subset(meltwars, name %in% input$char_select)
    
    return(meltwars)
  })
  # Render Output
  output$table <- DT::renderDataTable({
    starwars <- starWarsInputs()
    # Subset for Table
    subset(starwars, select = c(name, height, mass, birth_year, homeworld, species))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("star-wars-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(starWarsInputs(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)