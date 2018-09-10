# Class 5
# In Class Examples - Tabset

# Class 4
# In Class Examples - Inputs - Final

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

starwars.load <- starwars %>%
  mutate(films = as.character(films),
         vehicles = as.character(vehicles),
         starships = as.character(starships),
         name = as.factor(name))

diamonds.load <- diamonds

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Star Wars NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("worldSelect",
                                          "Homeworld:",
                                          choices = sort(unique(starwars.load$homeworld)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Naboo", "Tatooine")),
                              sliderInput("birthSelect",
                                          "Birth Year:",
                                          min = min(starwars.load$birth_year, na.rm = T),
                                          max = max(starwars.load$birth_year, na.rm = T),
                                          value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
                                          step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Star Wars Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output, session = session) {
  swInput <- reactive({
    starwars <- starwars.load %>%
      filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    
    if (length(input$worldSelect) > 0 ) {
      starwars <- subset(starwars, homeworld %in% input$worldSelect)
    }
    
    return(starwars)
  })
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  output$plot <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = mass, y = height, color = species, text = paste0("<b>", name, ":</b> ",
                                                                                  "<br>Homeworld: ", homeworld,
                                                                                  "<br>Mass: ", mass,
                                                                                  "<br>Height: ", height))) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    starwars <- swInput()
    
    subset(starwars, select = c(name, height, mass, birth_year, homeworld, species, films))
  })
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("star-wars-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput(), file)
    }
  )
  observeEvent(input$reset, {
    updateSelectInput(session, "worldSelect", selected = c("Naboo", "Tatooine"))
    updateSliderInput(session, "birthSelect", value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")