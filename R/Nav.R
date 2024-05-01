library(shiny)
library(leaflet)
library(maps)

#Load the dataset
#demand <- read.csv("EIAInterchangeClean.csv")s
#lightning <- read.csv("NCEICountiesClean.csv")
#wildfire <- read.csv("WildfiresClean.csv")

# UI
ui <- navbarPage(
  title = "STAT191",
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               # Dataset selection
               radioButtons("dataset", "Data Sets:",
                            choices = c("Merged", "Demand", "Lightning", "Wildfire"),
                            selected = "Merged")
             ),
             mainPanel(
               leafletOutput("map")
             )
           )),
  tabPanel("Summary Outputs",
           sidebarLayout(
             sidebarPanel(
             ),
             mainPanel(
               plotOutput("plot2")
             )
           ))
)

# Server
server <- function(input, output) {
  output$map <- renderLeaflet({
  
    us_states <- map("state", fill = TRUE, plot = FALSE)
    
    # Create leaflet map
    leaflet(data = us_states) %>%
      addTiles() %>%
      addPolygons(fillColor = "white", color = "black", weight = 1, fillOpacity = 0.5) %>%
      addMarkers(lng = -95, lat = 37, label = us_states$names) %>%
      addLegend(position = "bottomright", 
                colors = "white", 
                labels = "",
                title = "Demand")
  })
}

# Connection
shinyApp(ui = ui, server = server)