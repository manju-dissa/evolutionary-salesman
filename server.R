################################################################################
## Project: Travelling Salesman with an EA
## Script purpose: Server function for the shiny app
## Date: 18-Sep-2020
## Author: Manju Dissanayake
################################################################################

library(doParallel)
library(dplyr)
library(foreach)
library(geosphere)
library(maps)
library(shiny)
library(tidyr)

shinyServer(function(input, output, session) {

    source("helpers.R", local = TRUE)

    # TODO Manju - Drive these through the UI
    ea_number_of_individuals <- 50
    ea_elitist_count <- 2
    ea_mutation_probability <- 75 #this is a %
    ea_max_iter <- 100
    ea_plot_after <- 5

    # Some globals
    dist_mat <- NULL
    ea_population <- NULL
    great_circles <- NULL

    vals = reactiveValues(iter = 0)

    if (!exists("all_cities")) all_cities = readRDS("data/cities.rds")
    if (!exists("usa_cities")) usa_cities = readRDS("data/usa_cities.rds")

    citiesForTourType = reactive({
        req(input$map_name)
        if (input$map_name == "World") {
            return(all_cities)
        } else {
            return(usa_cities)
        }
    })

    output$citiesUI <- renderUI({
        req(input$map_name)

        if (input$map_name == "World") {
            selectedCities <- generate_random_cities(n = 20, min_dist = 500,
                usa_only = FALSE
            )
        } else {
            selectedCities <- generate_random_cities(n = 20, min_dist = 50,
                usa_only = TRUE
            )
        }

        selectizeInput(
            inputId = "cities",
            label = NA,
            choices = citiesForTourType()$full.name,
            selected = selectedCities$full.name,
            multiple = TRUE,
            width = "100%",
            options = list(
                maxItems = 30,
                maxOptions = 100,
                placeholder = "Start typing to select some cities...",
                selectOnTab = TRUE,
                openOnFocus = FALSE,
                hideSelected = TRUE
            )
        )
    })

    observeEvent(input$cities, {
        if(length(input$cities) >= 2) {
            selectedCities <- citiesForTourType() %>%
                filter(
                    full.name %in% input$cities
                )
            selectedCities$n <- 1:length(input$cities)

            isolate({
                vals$cities <- selectedCities
                vals$tour <- 1:nrow(selectedCities)
                dist_mat <<- distm(vals$cities[, c("long", "lat")]) *
                    miles_per_meter
                dimnames(dist_mat) <- list(vals$cities$name, vals$cities$name)
                great_circles <<- calculate_great_circles(vals$cities)
                vals$tour_distance <- calculate_tour_distance(vals$tour)
            })
        }
    })

    observeEvent(input$set_random_cities, {
        req(input$map_name)

        if (input$map_name == "World") {
            randomCities <- generate_random_cities(n = 20, min_dist = 500,
                usa_only = FALSE
            )
        } else {
            randomCities <- generate_random_cities(n = 20, min_dist = 50,
                usa_only = TRUE
            )
        }

        updateSelectizeInput(
            session,
            "cities",
            selected = randomCities$full.name
        )
    })

    observe({
        isolate({
            if (vals$iter == 1) {
                print("Set up the EA")
                ea_population <<- data.frame(
                    ID = 1:ea_number_of_individuals,
                    rank = 0,
                    distance_travelled = 0,
                    cities_visited = NA
                ) %>%
                    rowwise() %>%
                    mutate(
                        cities_visited = list(sample(nrow(vals$cities)))
                    ) %>%
                    ungroup()
            }
            if (vals$iter >= 1) {
                ea_population <<- runEA(
                    ea_population,
                    ea_plot_after,
                    vals$iter,
                    ea_elitist_count,
                    ea_mutation_probability
                )
                vals$tour <-
                    ea_population$cities_visited[nrow(ea_population)][[1]]
                vals$tour_distance <-
                    ea_population$distance_travelled[nrow(ea_population)][[1]]
                vals$iter <- vals$iter + ea_plot_after
            }
        })

        if (vals$iter >= 1 & vals$iter < ea_max_iter) {
            invalidateLater(0, session)
        }
    })

    observeEvent(input$go_button, {
        vals$iter <- 1
    })

    output$map <- renderPlot({
        plot_tour(vals$cities, vals$tour, great_circles,
            map_name = tolower(input$map_name),
            label_cities = input$label_cities
        )

        pretty_dist = prettyNum(vals$tour_distance, big.mark = ",", digits = 0,
            scientific = FALSE
        )
        pretty_iter = prettyNum(vals$iter, big.mark = ",", digits = 0,
            scientific = FALSE
        )
        plot_title = paste0("Distance: ", pretty_dist, " miles\n",
            "Generations: ", pretty_iter
        )

        title(plot_title)
    }, height = 550)
})
