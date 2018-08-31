# Class 3
# Shiny Dashboard Example

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Clean Star Wars Data
starwars <- starwars
starwars$films <- NULL
starwars$vehicles <- NULL
starwars$starships <- NULL
meltwars <- melt(starwars, id = "name")
meltwars$name <- as.factor(meltwars$name)

pdf(NULL)

header <- dashboardHeader(title = "Star Wars Dashboard",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "5 escape pods deployed", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "Midichlorians")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Princess Leia",
                                         message = HTML("Help Me Obi-Wan Kenobi! <br> You're my only hope."),
                                         icon = icon("exclamation-circle"))
                          )
                        )

sidebar <- dashboardSidebar(
  sidebarMenu(
   id = "tabs",
   menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
   menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
   selectInput("char_select",
               "Characters:",
               choices = levels(meltwars$name),
               multiple = TRUE,
               selectize = TRUE,
               selected = c("Luke Skywalker", "Darth Vader", "Jabba Desilijic Tiure", "Obi-Wan Kenobi", "R2-D2", "Dexter Jettster"))
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Mass", plotlyOutput("plot_mass")),
                   tabPanel("Height", plotlyOutput("plot_height")))
            )
          ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
          )
))

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  output$plot_mass <- renderPlotly({
    dat <- subset(meltwars, name %in% input$char_select & variable == "mass")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  output$plot_height <- renderPlotly({
    dat <- subset(meltwars, name %in% input$char_select & variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  output$table <- DT::renderDataTable({
    subset(starwars, name %in% input$char_select, select = c(name, height, mass, birth_year, homeworld, species))
  })
  output$mass <- renderInfoBox({
    sw <- subset(starwars, name %in% input$char_select)
    num <- round(mean(sw$mass, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  output$height <- renderValueBox({
    sw <- subset(starwars, name %in% input$char_select)
    num <- round(mean(sw$height, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)