##----------------------------------------------------------------------------##
##                  Developed by: Omondi Robert                               ##
##----------------------------------------------------------------------------##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Data
species_data <- data.frame(
  Species = c("Common Zebra", "Elephant", "Buffalo", "Giraffe", "Eland",
              "Oryx", "Impala", "Hartebeest", "Grants Gazelle", "Wildebeest",
              "Lesser Kudu", "Warthog", "Hippo", "Ostrich", "Gerenuk",
              "Waterbuck", "Hirola", "Grevy's Zebra", "Crocodile"),
  Count = c(17964, 14964, 8051, 4314, 4635,
            3997, 2660, 2604, 2172, 823,
            698, 688, 516, 510, 376,
            203, 74, 55, 47)
)

# Define icons for each species
species_icons <- c(
  "Common Zebra" = "zebra",
  "Elephant" = "elephant",
  "Buffalo" = "buffalo",
  "Giraffe" = "giraffe",
  "Eland" = "antelope",
  "Oryx" = "antelope",
  "Impala" = "antelope",
  "Hartebeest" = "antelope",
  "Grants Gazelle" = "antelope",
  "Wildebeest" = "antelope",
  "Lesser Kudu" = "antelope",
  "Warthog" = "pig",
  "Hippo" = "hippo",
  "Ostrich" = "ostrich",
  "Gerenuk" = "antelope",
  "Waterbuck" = "antelope",
  "Hirola" = "antelope",
  "Grevy's Zebra" = "zebra",
  "Crocodile" = "crocodile"
)

# UI
ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "Tsavo Ecosystem Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Details", tabName = "details", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                # Introduction Text
                box(
                  title = "Welcome to the Tsavo Ecosystem Dashboard",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Welcome to the Tsavo Ecosystem Dashboard, designed to provide an in-depth look at wildlife populations within the Tsavo region. This dashboard is based on data from the National Wildlife Census 2021 and focuses on the diverse species inhabiting the Tsavo Ecosystem. The dashboard consists of several key components:"),
                  tags$ul(
                    tags$li(strong("Total Species Count:") , " Displays the total number of species observed in the ecosystem."),
                    tags$li(strong("Highest and Lowest Counts:") , " Highlights the species with the highest and lowest counts, respectively."),
                    tags$li(strong("Species Count Bar Plot:") , " A bar plot showing the count of each species for visual comparison."),
                    tags$li(strong("Species Count Pie Chart:") , " A pie chart representing the proportion of each species in the ecosystem."),
                    tags$li(strong("Species-Specific Value Boxes:") , " Provides a quick view of each species' count with associated icons for easy identification.")
                  ),
                  p(em(strong("Developed by Omondi Robert. Contact: 0791662830, robertdon388@gmail.com")))
                )
              ),
              fluidRow(
                # Value Boxes
                valueBoxOutput("totalSpecies"),
                valueBoxOutput("highestCount"),
                valueBoxOutput("lowestCount")
              ),
              fluidRow(
                # Bar Plot
                box(title = "Species Count Bar Plot", status = "primary", solidHeader = TRUE,
                    plotOutput("barPlot")),
                
                # Pie Chart
                box(title = "Species Count Pie Chart", status = "primary", solidHeader = TRUE,
                    plotOutput("pieChart"))
              ),
              fluidRow(
                # Value Boxes for Each Species
                uiOutput("speciesValueBoxes")
              )
      ),
      
      tabItem(tabName = "details",
              fluidRow(
                # Table
                box(title = "Species Data Table", status = "primary", solidHeader = TRUE,
                    tableOutput("speciesTable"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Value Boxes
  output$totalSpecies <- renderValueBox({
    total <- nrow(species_data)
    valueBox(
      value = total,
      subtitle = "Total Species",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$highestCount <- renderValueBox({
    highest <- species_data %>% filter(Count == max(Count)) %>% select(Species, Count)
    valueBox(
      value = highest$Count,
      subtitle = paste("Highest Count:", highest$Species),
      icon = icon("arrow-up"),
      color = "blue"
    )
  })
  
  output$lowestCount <- renderValueBox({
    lowest <- species_data %>% filter(Count == min(Count)) %>% select(Species, Count)
    valueBox(
      value = lowest$Count,
      subtitle = paste("Lowest Count:", lowest$Species),
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  # Bar Plot
  output$barPlot <- renderPlot({
    ggplot(species_data, aes(x = reorder(Species, -Count), y = Count, fill = Species)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Species", y = "Count", title = "Species Count Bar Plot")
  })
  
  # Pie Chart
  output$pieChart <- renderPlot({
    ggplot(species_data, aes(x = "", y = Count, fill = Species)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Species Count Pie Chart")
  })
  
  # Value Boxes for Each Species
  output$speciesValueBoxes <- renderUI({
    species_list <- lapply(1:nrow(species_data), function(i) {
      species <- species_data$Species[i]
      count <- species_data$Count[i]
      icon_name <- species_icons[species]
      
      valueBox(
        value = count,
        subtitle = species,
        icon = icon(icon_name),
        color = "green",
        width = 3
      )
    })
    
    do.call(fluidRow, species_list)
  })
  
  # Data Table
  output$speciesTable <- renderTable({
    species_data
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
