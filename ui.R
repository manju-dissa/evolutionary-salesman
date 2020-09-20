################################################################################
## Project: Travelling Salesman with an EA
## Script purpose: Server function for the shiny app
## Date: 18-Sep-2020
## Author: Manju Dissanayake
################################################################################

shinyUI(
    fluidPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", 
                href = "custom_styles.css"
            )
        ),
        title = "Traveling Salesman with an Evolutionary Algorithm, Shiny, and R",
        fluidRow(
            column(width = 8, plotOutput("map", height = "550px")),
            column(
                width = 4,
                selectInput(
                    "map_name",
                    "Choose a map and which cities to tour:",
                    c("World", "USA"),
                    "World"
                ),
                p(
                    "Type below to select individual cities or hit 
                    'Select Randomly' button."
                ),
                uiOutput("citiesUI"),
                checkboxInput("label_cities", "Label cities on map?", FALSE),
                fluidRow(
                    column(
                        width = 6,
                        actionButton(
                            "set_random_cities",
                            "set randomly",
                            icon = icon("refresh"),
                            width = '100%'
                        )
                    ),
                    column(
                        width = 6,
                        tags$button("SOLVE", id = "go_button", 
                            class = "btn btn-info btn-large action-button 
                                shiny-bound-input"
                        )
                    )
                )
            )
        )
    )
)
