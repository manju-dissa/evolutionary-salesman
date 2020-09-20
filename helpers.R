################################################################################
## Project: Travelling Salesman with an EA
## Script purpose: Server function for the shiny app
## Date: 18-Sep-2020
## Author: Manju Dissanayake
################################################################################

miles_per_meter = 100 / 2.54 / 12 / 5280

generate_random_cities = function(n = 10, min_dist = 250, usa_only = FALSE) {
    if (usa_only) {
        candidates = usa_cities
    } else {
        candidates = all_cities
    }

    cities = candidates[sample(nrow(candidates), 1), ]
    candidates = subset(candidates, !(full.name %in% cities$full.name))

    i = 0
    while (nrow(cities) < n & i < nrow(all_cities)) {
        candidate = candidates[sample(nrow(candidates), 1), ]
        candidate_dist_matrix =
            distm(rbind(cities, candidate)[, c("long", "lat")]) * miles_per_meter

        if (min(candidate_dist_matrix[candidate_dist_matrix > 0]) > min_dist) {
            cities = rbind(cities, candidate)
            candidates = subset(
                candidates,
                !(candidates$full.name %in% cities$full.name)
            )
        }

        i = i + 1
    }

    cities = cities[order(cities$full.name), ]
    cities$n = 1:nrow(cities)

    return(cities)
}

plot_base_map = function(map_name = "world") {
    margins = c(3.5, 0, 3.5, 0)
    if (map_name == "world") {
        map(
            "world",
            col = "#f3f3f3",
            fill = TRUE,
            lwd = 0.2,
            mar = margins
        )
    } else if (map_name == "usa") {
        map(
            "usa",
            col = "#f3f3f3",
            border = FALSE,
            fill = TRUE,
            mar = margins
        )
        map(
            "state",
            add = TRUE,
            col = "#999999",
            fill = FALSE
        )
    }
}

plot_city_map = function(cities, map_name = "world", label_cities = TRUE) {
    plot_base_map(map_name)
    map.cities(
        cities,
        pch = 19,
        cex = 1.1,
        label = label_cities
    )
}

plot_tour = function(cities, tour, great_circles, map_name = "world",
        label_cities = TRUE) {
    plot_city_map(cities, map_name, label_cities = label_cities)

    if (length(tour) > 1) {
        closed_tour = c(tour, tour[1])
        keys = apply(
            embed(closed_tour, 2),
            1, function(row) paste(sort(row), collapse = "_")
        )
        invisible(sapply(great_circles[keys], lines, lwd = 0.8))
    }
}

calculate_great_circles = function(cities) {
    great_circles = list()
    if (nrow(cities) == 0) return(great_circles)

    pairs = combn(cities$n, 2)
    for (i in 1:ncol(pairs)) {
        key = paste(sort(pairs[, i]), collapse = "_")
        pair = subset(cities, n %in% pairs[, i])
        pts = gcIntermediate(
            c(pair$long[1], pair$lat[1]),
            c(pair$long[2], pair$lat[2]),
            n = 25,
            addStartEnd = TRUE,
            breakAtDateLine = TRUE,
            sp = TRUE
        )
        great_circles[[key]] = pts
    }

    return(great_circles)
}

calculate_tour_distance = function(tour) {
    sum(dist_mat[embed(c(tour, tour[1]), 2)])
}

runEA <- function(population, iterations, currentRun, elitist_count,
        mutation_probability) {

    registerDoParallel(3)

    for (i in 1:iterations) {
        message(paste("Generation: ", currentRun))
        population <- calculateFitnessOfPop(population)
        population <- calculateRanksOfPop(population)
        children <- generateChildren(population, elitist_count)
        children <- mutateChildren(children, mutation_probability)
        children <- children %>%
            mutate(cumulative_rank = NA)
        population <- population %>%
            tail(elitist_count) %>%
            rbind(children)
        currentRun <- currentRun + 1
    }
    population <- calculateFitnessOfPop(population)
    population <- calculateRanksOfPop(population)

    stopImplicitCluster()

    return(population)
}

calculateFitnessOfPop <- function(population) {
    popSize <- nrow(population)
    distances <- foreach (i = 1:popSize, .combine = c) %dopar% {
        calculate_tour_distance(population[i, ]$cities_visited[[1]])
    }
    population$ID = 1:popSize
    population$distance_travelled = distances
    return(population)
}

calculateRanksOfPop <- function(population) {
    population <- population %>%
        arrange(desc(distance_travelled))
    population$rank <- 1:nrow(population)
    population <- population %>%
        mutate(cumulative_rank = cumsum(rank))

    return(population)
}

generateChildren <- function(population, elitist_count) {

    totalFitness <- max(cumsum(1:nrow(population)))
    numberOfChildren <- nrow(population) - elitist_count
    childLength <- length(population[1,]$cities_visited[[1]])

    children <- foreach (i = 1:numberOfChildren, .combine = rbind) %dopar% {
        return(
            list(
                generateChild(totalFitness, childLength, population)
            )
        )
    }

    return(
        data.frame(
            ID = 1:numberOfChildren,
            rank = 0,
            distance_travelled = 0,
            cities_visited = children
        )
    )
}

generateChild <- function(totalFitness, childLength, population) {

    # Create two cut points (c1, c2)
    # Copy c1 - c2 from p2 to o1
    # copy bits from p1 to o1:
    #   copy if not in o1, else:
    #     get position on p2
    #     get corresponding bit from p1
    #     repeat until we get a free bit
    # Here's a worked example:
    # p1 <- c(3,4,8,2,7,1,6,5)
    # p2 <- c(4,2,5,1,6,8,3,7)
    # c1 <- 4 # inc this position
    # c2 <- 6 # inc this position, so 4,5,6

    p1CutOff <- sample(1:totalFitness, 1)
    p2CutOff <- sample(1:totalFitness, 1)

    p1 <- getParentCitiesVisitedFromPopUsingProb(population, p1CutOff)
    p2 <- getParentCitiesVisitedFromPopUsingProb(population, p2CutOff)

    # When creating cut points, you don't want to be too close to the edges
    cutPointStart <- 3
    cutPointEnd <- childLength - 3
    c1 <- sample(cutPointStart:cutPointEnd, 1)
    cutPointTwoStart <- c1 + 1
    cutPointTwoEnd <- childLength
    c2 <- sample(cutPointTwoStart:cutPointTwoEnd, 1)

    o1 <- vector(mode = "numeric", length = childLength)
    o1[c1:c2] <- p2[c1:c2]

    for (x in 1:childLength) {
        if (o1[x] == 0) {
            if (!p1[x] %in% o1) {
                o1[x] <- p1[x]
            } else {
                o1[x] <- getMappedGene(p1, p2, o1, p1[x])
            }
        }
    }
    return(o1)
}

getParentCitiesVisitedFromPopUsingProb <- function(population, probability) {
    cities_visited <- population %>%
        filter(cumulative_rank >= probability) %>%
        head(1) %>%
        select(cities_visited) %>%
        pull() %>%
        unlist()
    return(cities_visited)
}

getMappedGene <- function(p1, p2, o1, conflictGene) {
    loopStopper <- FALSE
    newGene <- conflictGene
    while (!loopStopper) {
        positionOnP2 <- which(p2 == newGene)
        newGene <- p1[positionOnP2]
        if (!(newGene %in% o1)) {
            loopStopper <- TRUE
        }
    }
    return(newGene)
}

mutateChildren <- function(children, mutation_probability) {
    for (i in 1:nrow(children)) {
        randomNumber <- sample(1:100, 1)
        if (randomNumber <= mutation_probability) {
            childLength <- length(children[i, ]$cities_visited[[1]])
            gene1ForSwap <- sample(1:childLength, 1)
            gene1Value <- children[i, ]$cities_visited[[1]][gene1ForSwap]
            gene2ForSwap <- sample(1:childLength, 1)
            gene2Value <- children[i, ]$cities_visited[[1]][gene2ForSwap]
            children[i, ]$cities_visited[[1]][gene1ForSwap] <- gene2Value
            children[i, ]$cities_visited[[1]][gene2ForSwap] <- gene1Value
        }
    }
    return(children)
}
