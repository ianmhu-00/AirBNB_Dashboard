# source("C:/Users/ianmh/OneDrive/Desktop/Shortcuts!/Programming/AirBNB/dashboard/app.R")

# setwd("C:/Users/ianmh/OneDrive/Desktop/Shortcuts!/Programming/AirBNB/dashboard")

rm(list=ls())
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(RColorBrewer)
require(leaflet)
require(sf)
# require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(DT)

load("clean/AirBNB_Data_Cleaned.RData")
neighborhoods <- st_read("raw/Neighborhoods_Data.geojson")
neighborhoods$centroid <- st_centroid(neighborhoods$geometry)
nyc_map <- neighborhoods[, names(neighborhoods) %in% c("neighborhood", "borough", "geometry")]

# Leaflet Interactive map
merge <- merge(nyc_map[, names(nyc_map) %in% c("neighborhood", "borough", "geometry")], 
               data %>% count(neighborhood), all = TRUE)
merge <- merge[]

merge$popup <- paste("<strong>", merge$borough, "</strong><br>",
                     merge$neighborhood, "<br>",
                     "Number of AirBNB Listings (2022): ",
                     prettyNum(merge$n, big.mark = ",")
)

# Leaflet Plot variables
lat_min <- -74.4
lat_max <- -73.55
long_min <- 40.45
long_max <- 40.95

NYCbins <- c(0,50,200,500,1000,3000,5000,8000)
NYCpal <- colorBin(palette = "YlGnBu", domain = merge$n, bins=NYCbins)

# Boroughs & Neighborhoods
neighborhood_table <- data %>%
  select(borough, neighborhood) %>%
  distinct() %>%
  arrange(borough, neighborhood) %>%
  group_by(borough) %>% 
  mutate(row_num = row_number()) %>%
  pivot_wider(names_from = borough, values_from = neighborhood) %>% 
  select(-row_num) %>% 
  mutate(across(everything(), ~replace_na(.x, "")))

dropdown_choices <- data.frame(Borough = c(unique(data$borough),
                                           rep("", nrow(neighborhood_table)-length(unique(data$borough)))),
                               neighborhood_table)

colnames(dropdown_choices)[names(dropdown_choices) == "Staten.Island"] <- "Staten Island"

# Comparing median rent
rent_data <- read.csv("raw/StreetEasy_MedianAskingRent.csv", stringsAsFactors=TRUE, header=TRUE)
rent_data <- rent_data[, c(colnames(rent_data)[1:3], "X2022.01", "X2022.02", "X2022.03")]
rent_data$asking_rent <- coalesce(rent_data[, "X2022.02"], rent_data[,"X2022.03"], rent_data[,"X2022.01"])
rent_data <- rent_data[rent_data$areaType=="neighborhood", ]
colnames(rent_data)[1] <- "neighborhood"

revenue_data <- data %>% 
  filter(room.type == "Entire home/apt") %>% 
  group_by(neighborhood) %>% 
  summarise(avg_price = round(mean(price, na.rm=TRUE), 2))

scatter_data <- merge(rent_data, revenue_data, by="neighborhood")
max_distance <- 500

# DASHBOARD
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "AirBNB Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Start Here!", tabName = "start_here", icon = icon("house-chimney")),
                        menuItem("Market Overview", tabName = "overview", icon = icon("airbnb")),
                        menuItem("Price by Room Types", tabName = "room_type", icon = icon("tree-city")),
                        menuItem("Revenue Estimator", tabName = "revenue_estimator", icon = icon("chart-line")),
                        menuItem("Data", tabName = "data", icon = icon("database"))
                      )
                    ),
                    dashboardBody(
                      tags$style(HTML("#nyc_map {
                        width = 100%;
                        height = 100%}")
                      ),
                      tabItems(
                        tabItem("start_here",
                                fluidPage(
                                  h1(HTML('<strong style="font-size: 48px;">Urban Retreats</strong><br>
          <span style="font-size: 24px;">Analyzing AirBNB Listings in NYC (Q1 2022)</span>')
                                  )
                                ),
                                fluidRow(
                                  column(5,
                                         # Welcome Box
                                         box(title = strong("Welcome!"), solidHeader = FALSE, status = "primary",
                                             p("Welcome to the NYC AirBNB dashboard! This tool provides an interactive overview of key data, insights, and visualizations. Explore the sections to get a more comprehensive understanding of trends and performance metrics."),
                                             h4("Objectives"),
                                             p("The primary objective of this dashboard is to determine the most popular areas for AirBNB rentals and the following:"),
                                             HTML("<ul><li>", "Listing count and prices across boroughs, neighborhoods, and room types",
                                                  "<li>", "Average ratings + reviews",
                                                  "<li>", "Market sizing and revenue forcasting", "</ul>"),
                                             h4("Data sources"),
                                             p(HTML("The data is sourced from Kaggle, <a href='https://www.kaggle.com/datasets/arianazmoudeh/airbnbopendata/data' target='_blank'>linked here</a>, dated from February 2022. Additionally, AirBNB offers publicly available quarterly data across a variety of cities and countries, <a href='https://insideairbnb.com/get-the-data/' target='_blank'>linked here</a>. Lastly, median asking rent data is taken from StreetEasy, <a href='https://streeteasy.com/blog/data-dashboard/?agg=Total&metric=Inventory&type=Sales&bedrooms=Any%20Bedrooms&property=Any%20Property%20Type&minDate=2010-01-01&maxDate=2025-02-01&area=Staten%20Island' target='_blank'>linked here</a>, dated Q1 2022.")),
                                             h4("Tabs"),
                                             p("Each tab analyzes different snapshots of real estate statistics and aggregated data. Take your time to explore the data and my findings!"),
                                             width=12),
                                         # Contact Box
                                         box(title = strong("Questions? Contact here:"), solidheader = FALSE, status = "warning",
                                             p(icon("linkedin"),
                                               HTML(" LinkedIn: <a href='https://be.linkedin.com/in/ianmhu' target='_blank'>Ian Hu</a></br>"),
                                               p(icon("envelope"),
                                                 HTML(" Email: ianmhu00@gmail.com")),
                                             ), width=12)
                                  ),
                                  # Leaflet Output
                                  box(title = strong("Interactive Map of NYC | Click on your neighborhood!"), solidHeader = FALSE, status = "primary",
                                      leafletOutput("nyc_map", height = "600px"), width = 7, height = "auto", res = 150)
                                )
                        ),
                        tabItem("overview",
                                fluidRow(
                                  box(title = "Exploring the Market", solidHeader = TRUE, status = "primary", width = 12,
                                      p("Navigate your way through the tabs to get a better understanding of popular AirBNB neighborhoods (by listing count), across different boroughs and neighborhoods. Select a specific borough to display intra-borough listing distributions or multiple boroughs to show inter-borough distributions! Select a specific neighborhood as a benchmark against the top 9 within your borough selection!"))),
                                fluidRow(column(
                                  6, box(width=12, plotOutput("borough_pie_chart"))
                                  ),
                                  column(
                                    6, box(width=12, plotOutput("borough_boxplot"))
                                  )
                                ),
                                fluidRow(
                                  column(
                                    6, box(status = "primary", width = 12,
                                           # Borough Checkbox
                                           checkboxGroupInput(inputId = "borough",
                                                              label = "Select Borough(s):",
                                                              choices = c("Manhattan", "Queens", "Brooklyn", "Bronx", "Staten Island"))
                                           )
                                    ),
                                  column(
                                    6, box(status = "primary", width = 12,
                                           # Select neighborhood
                                           uiOutput("neighborhood_selection"))
                                    )
                                )
                        ),
                        tabItem("room_type",
                                fluidRow(
                                  box(title = "Breakdown of Pricing Trends", solidHeader = TRUE, status = "primary", width = 12,
                                      p("Filter the data by borough or neighborhood and see how prices differ across room types (Entire Home/Apt., Private Room, Shared Room, or Hotel Room) to get a better understanding of sizing, pricing, construction, and availability trends."))),
                                fluidRow(column(9, box(solidheader=FALSE, status = "primary", width=12,
                                                       fluidRow(column(6, box(solidheader=FALSE, width=12,
                                                                              plotOutput("plot1", width="auto", height=250), style = "padding:0px")),
                                                                column(6, box(solidheader=FALSE, width=12,
                                                                              plotOutput("plot2", width="auto", height=250), style = "padding:0px"))),
                                                       fluidRow(column(6, box(solidheader=FALSE, width=12,
                                                                              plotOutput("plot3", width="auto", height=250), style = "padding:0px")),
                                                                column(6, box(solidheader=FALSE, width=12,
                                                                              plotOutput("plot4", width="auto", height=250), style = "padding:0px"))))),
                                         column(3, box(title = strong("New York City"), solidheader=FALSE, status = "primary", width=12,
                                                       selectizeInput("location_input", "Filter by:", choices = c("New York City", dropdown_choices), multiple=FALSE,
                                                                      width="240px")),
                                                   box(width=12,
                                                       uiOutput("room_type_stats"))))
                                ),
                        tabItem("revenue_estimator",
                                fluidRow(
                                  box(title = "Revenue Estimator", solidHeader = TRUE, status = "primary", width = 12,
                                      p("Input key variables (occupancy rate and operating cost deductions) to estimate potential revenue per neighborhood, filtered for exclusively entire home/apt listings. Click on the chart and find neighborhoods fitting a specific asking rent and gross revenue (within a $500 radius)."),
                                      p("Note that estimated revenue is calculated using nightly price, 30.4 (average number of days in a month) * occupancy rate, 1 - operating cost rate, and an assumed 3% service fee that hosts pay towards AirBNB. However, there may be additional costs to consider, such as cleanings per booking, property management fees, or regulations/permits, that may affect estimated revenue and this specific model."),
                                      p("Median asking rent (among all listings available) is gathered from StreetEasy (listings dated Q1 2022) and unfortunately does not contain neighborhood-level data for Staten Island, so the borough is excluded from the model below."))
                                ),
                                fluidRow(
                                  column(width=3, box(solidheader=FALSE, status="primary", width=12,
                                                      p(HTML('<strong style="font-size: 20px;">Occupancy Rate</strong><br>
          <span style="font-size: 12px;">Select a rate indicating listing occupancy, e.g., 50% = ~15 days')),
                                                      sliderInput("occupancy_rate", label="",
                                                                  min=0, max=100, post = "%",
                                                                  value=20),
                                                      p(HTML('<br><strong style="font-size: 20px;">Operating Cost Deduction</strong><br>
          <span style="font-size: 12px;">Select a rate indicating operating costs to be deducted from gross revenue (e.g., utilities, maintenance, or management fees)')),
                                                      sliderInput("op_costs", label="",
                                                                  min=0, max=50, post="%",
                                                                  value=20)),
                                      box(solidheader=FALSE, status="primary", width=12,
                                          p(HTML('<strong style="font-size: 20px;">Interactive Plot Info</strong><br>')),
                                          p("Click on the plot to see neighborhoods at specific rent prices and estimated revenues!"),
                                          verbatimTextOutput("rent_info"))),
                                  column(width=9,
                                  box(solidheader=FALSE, status="primary", width=12,
                                      plotOutput("scatter_rent", width="auto", height="600", click="plot_click")))
                                )
                        ),
                        tabItem("data",
                                fluidPage(
                                  h1(strong("Data")),
                                  downloadButton("download", "Download the data"),
                                  dataTableOutput("AirBNB_table"))
                        )
                      )
                    )
)

server <- function(input, output){
  output$nyc_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10) %>%
      setMaxBounds(
        lng1 = -74.25, lat1 = 40.25,  # SW corner of NYC
        lng2 = -73.75, lat2 = 41   # NE corner of NYC
      ) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      addPolygons(
        data = merge,
        color = "blue",
        smoothFactor = 0.3,
        weight = 1,
        opacity = 0.5,
        fillColor = ~NYCpal(n),
        fillOpacity = 0.7,
        popup = ~popup,
        highlightOptions = highlightOptions(color = "#E2068A", weight = 2,
                                            bringToFront = TRUE,
                                            fillOpacity = 0.9)
      ) %>% 
      addLegend("bottomright",opacity = 1,
                colors =c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
                title = "AirBNB Listings</br>(Q1 2022), NYC",
                labels= c("<50","50 - 199","200 - 499","500 - 999","1,000 - 2,999","3,000 - 4,999","5,000 - 8,000")
      )
  })
  output$neighborhood_selection <- renderUI({
    # If no borough is selected, show all neighborhoods
    neighborhoods <- if (is.null(input$borough) || length(input$borough) == 0) {
      unique(data$neighborhood)
    } else {
      unique(data$neighborhood[data$borough %in% input$borough])
    }
    
    selectInput("neighborhood", "Select Neighborhood:", 
                choices = c("All", neighborhoods), 
                selected = "All") # Set a default selected neighborhood
  })
  output$borough_pie_chart <- renderPlot({
    filtered_data <- data
    
    # Filter by borough(s) if selected
    if (!is.null(input$borough) && length(input$borough) > 0) {
      filtered_data <- filtered_data %>% 
        filter(borough %in% input$borough)
    }
    
    # Aggregate listings by neighborhood
    neighborhood_counts <- filtered_data %>% 
      count(borough, neighborhood, sort=TRUE) %>% 
      mutate(percent = round(n / sum(n), 3)) %>%
      arrange(desc(n))
    
    # If neither borough nor neighborhoods are selected
    if (is.null(input$borough) && (is.null(input$neighborhood) || input$neighborhood == "All")) {
      borough_counts <- data %>% 
        count(borough, sort=TRUE) %>%
        mutate(percent = round(n / sum(n), 3)) %>%
        arrange(desc(n))
      
      pie_chart <- data.frame(borough = borough_counts$borough,
                              labels = borough_counts$borough,
                              percent = borough_counts$percent,
                              values = borough_counts$n)
      
    } else {
      top_9 <- head(neighborhood_counts, 9)
      
      # If selected neighborhood is not in the top 9, add manually
      if (!is.null(input$neighborhood) && !(input$neighborhood %in% top_9$neighborhood)) {
        selected_neighborhood <- neighborhood_counts %>% filter(neighborhood == input$neighborhood)
        
        if (nrow(selected_neighborhood) > 0) {
          top_9 <- head(bind_rows(selected_neighborhood, top_9), 9)
        }
      }
      
      # Calculate "Other" category
      other_count <- neighborhood_counts %>% 
        filter(!neighborhood %in% top_9$neighborhood) %>% 
        summarise(n = sum(n), percent = sum(percent))
      
      if (nrow(other_count) > 0 && !is.na(other_count$n) && other_count$n > 0) {
        top_9 <- bind_rows(top_9, tibble(neighborhood = "Other", n=other_count$n, percent=other_count$percent))
      }
      
      pie_chart <- data.frame(borough = top_9$borough,
                              labels = top_9$neighborhood,
                              percent = top_9$percent,
                              values = top_9$n)
    }
    
    pie_chart <- pie_chart %>%
      arrange(desc(percent))

    
    # borough palettes
    borough_palettes <- list(
      Manhattan = colorRampPalette(brewer.pal(9, "Reds"))(18)[6:15],
      Queens = colorRampPalette(brewer.pal(9, "Purples"))(18)[6:15],
      Brooklyn = colorRampPalette(brewer.pal(9, "YlOrBr"))(18)[2:11],
      Bronx = colorRampPalette(brewer.pal(9, "Greens"))(18)[6:15],
      Staten_Island = colorRampPalette(brewer.pal(9, "Blues"))(18)[3:12]
    )
    
    if(is.null(input$borough) && input$neighborhood == "All") {
      pie_chart <- pie_chart %>% 
        mutate(color = case_when(
          borough == "Manhattan" ~ "#DA291C",
          borough == "Queens" ~ "#800080",
          borough == "Brooklyn" ~ "#FFC72C",
          borough == "Bronx" ~ "#00843D",
          borough == "Staten Island" ~ "#0078C6"
        ))
    } else {
      pie_chart <- pie_chart %>%
      mutate(color = case_when(
        borough == "Manhattan" ~ borough_palettes$Manhattan[as.numeric(factor(labels))],
        borough == "Queens" ~ borough_palettes$Queens[as.numeric(factor(labels))],
        borough == "Brooklyn" ~ borough_palettes$Brooklyn[as.numeric(factor(labels))],
        borough == "Bronx" ~ borough_palettes$Bronx[as.numeric(factor(labels))],
        borough == "Staten Island" ~ borough_palettes$Staten_Island[as.numeric(factor(labels))],
        borough == NA ~ '#cfcfc4'
      ))}
    
    pie_chart$labels <- factor(pie_chart$labels, levels=rev(as.character(pie_chart$labels)))
    
    ggplot(pie_chart, aes(x="", y=percent, fill = labels)) +
      geom_bar(stat="identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_col(aes(x = -0.7, y = 0)) +
      geom_text(aes(x = 1.9, 
                    label = paste0(round(percent*100, 1), "%")), 
                position = position_stack(vjust=0.5)) +
      labs(title = paste0("Distribution of Listings (n = ", prettyNum(sum(pie_chart$values), ","), ")"),
           subtitle = "Based on selected borough and/or neighborhood",
           fill = if(is.null(input$borough) && input$neighborhood == "All") "Borough" else "Neighborhood",
           x=NULL, y=NULL) +
      scale_fill_manual(values = rev(pie_chart$color)) +
      theme_minimal() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.subtitle = element_text(face = "italic"))
  })
  
  output$borough_boxplot <- renderPlot({
    filtered_data2 <- data[!is.na(data$price),]
    
    if(!is.null(input$borough) && length(input$borough) > 0){
      filtered_data2 <- filtered_data2 %>% 
        filter(borough %in% input$borough)
    }
    
    # if neither borough nor neighborhood is selected
    if(is.null(input$borough) && input$neighborhood == "All"){
      price_boxplot <- data.frame(
        area = filtered_data2$borough,
        price = filtered_data2$price,
        fill = filtered_data2$borough
      )
    } else {
      top_9_neighborhoods <- filtered_data2 %>% 
        count(borough, neighborhood, sort=TRUE) %>% 
        top_n(9, n)
      
      if(input$neighborhood != "All" && !input$neighborhood %in% top_9_neighborhoods$neighborhood){
        top_9_neighborhoods <- head(bind_rows(tibble(borough = input$borough,
                                                     neighborhood = input$neighborhood,
                                                     n = NA,
                                                     percent = NA), top_9_neighborhoods), 9)
      }
      
      filtered_data2 <- filtered_data2 %>% 
        filter(neighborhood %in% top_9_neighborhoods$neighborhood) # exclude "Other"
      
      filtered_data2$neighborhood <- ifelse(filtered_data2$neighborhood %in% top_9_neighborhoods$neighborhood, filtered_data2$neighborhood, "Other")
      price_boxplot <- data.frame(
        area = filtered_data2$neighborhood,
        price = filtered_data2$price,
        fill = filtered_data2$borough
      )
    }
    
    ggplot(price_boxplot, aes(x = area, y = price, fill = fill)) +
      geom_boxplot() +
      scale_fill_manual(values = c(
        "Manhattan" = "#DA291C",
        "Queens" = "#800080",
        "Brooklyn" = "#FFC72C",
        "Bronx" = "#00843D",
        "Staten Island" = "#0078C6"
      ), na.value = "gray") +  # Assigns gray for any unexpected values (e.g., NA)
      labs(
        title = if (is.null(input$borough) && input$neighborhood == "All") 
          "Nightly Prices Across Boroughs" 
        else 
          "Nightly Prices Across Neighborhoods",
        subtitle = "Based on selected borough(s) and neighborhood",
        fill = "Borough",
        x = NULL, 
        y = "Price ($)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = -20))
  })
  output$plot1 <- renderPlot({
    # if selection is borough or neighborhood
    if(input$location_input %in% c(unique(data$borough), unique(data$neighborhood))){
      if(input$location_input %in% unique(data$borough)){
        plot1_data <- data[data$borough == input$location_input,]
      } else if(input$location_input %in% unique(data$neighborhood)){ # if selection is neighborhood
        plot1_data <- data[data$neighborhood == input$location_input,]
      }
      market_sizing <- plot1_data %>% 
        group_by(room.type) %>% 
        summarise(mkt_size = sum((price * availability.365) / 1000000, na.rm=TRUE), .groups="drop") %>%
        complete(room.type, fill = list(mkt_size = 0))
      
      market_sizing <- market_sizing %>%
        mutate(room.type = factor(room.type, levels = c("Hotel room", "Shared room", "Private room", "Entire home/apt"))) %>% 
        arrange(room.type)
      
      ggplot(market_sizing, aes(x = room.type, y = mkt_size, fill = room.type)) +
        geom_col(color="black") +
        coord_flip() +
        scale_y_continuous(expand=c(0,0)) +
        scale_fill_manual(breaks=c("Entire home/apt",
                                   "Private room",
                                   "Shared room",
                                   "Hotel room"),
                          values=c("Entire home/apt" = "#F7A8A8",
                                   "Private room" = "#F9E4A1",
                                   "Shared room" = "#B7E5B7",
                                   "Hotel room" = "#A9D2E2")) +
        labs(title=paste(input$location_input, "Market Size"),
             subtitle = "Price × Availability.365 ($ Million)",) +
        guides(fill = "none") +
        theme_clean() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.subtitle = element_text(size=12),
              panel.border = element_blank())
    } else {
      plot1_data <- data # else select base case, all 5 boroughs
      market_sizing <- plot1_data %>% 
        group_by(borough, room.type) %>% 
        summarise(mkt_size = sum((price * availability.365) / 1000000, na.rm=TRUE))
      
      market_sizing <- market_sizing %>%
        mutate(room.type = factor(room.type, levels = c("Hotel room", "Shared room", "Private room", "Entire home/apt"))) %>% 
        arrange(room.type)
      
      ggplot(market_sizing, aes(x = borough, y = mkt_size, fill = room.type)) +
        geom_col(color="black") +
        coord_flip() +
        scale_x_discrete(drop = FALSE) +
        scale_y_continuous(breaks = seq(0,4000,500), expand=c(0,0)) +
        scale_fill_manual(breaks=c("Entire home/apt",
                                   "Private room",
                                   "Shared room",
                                   "Hotel room"),
                          values=c("Entire home/apt" = "#F7A8A8",
                                   "Private room" = "#F9E4A1",
                                   "Shared room" = "#B7E5B7",
                                   "Hotel room" = "#A9D2E2")) +
        labs(title="NYC Market Size",
             subtitle = "Price × Availability.365 ($ Million)",
             fill = "Room Type") +
        theme_clean() +
        theme(
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(angle=35),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(0.5, 'cm'),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              legend.position = "right",
              panel.border = element_blank())
    }
  })
  
  output$plot2 <- renderPlot({
    if(input$location_input %in% c(unique(data$borough), unique(data$neighborhood))){
      if(input$location_input %in% unique(data$borough)){
        plot2_data <- data[data$borough == input$location_input,]
      } else if(input$location_input %in% unique(data$neighborhood)){ # if selection is neighborhood
        plot2_data <- data[data$neighborhood == input$location_input,]
      }
      ggplot(plot2_data, aes(x=room.type, y=price, fill=room.type)) +
        geom_boxplot() +
        labs(title="Nightly Prices across Room Types", x='Room Type', y="Price") +
        scale_fill_manual(values=c("Hotel room" = "#A9D2E2",
                                   "Shared room" = "#B7E5B7",
                                   "Private room" = "#F9E4A1",
                                   "Entire home/apt" = "#F7A8A8")) +
        theme_clean() +
        guides(fill="none")
      
    } else {
      ggplot(data, aes(x=room.type, y=price, fill=room.type)) +
        geom_boxplot(outlier.shape = NA) +
        labs(title="Nightly Prices across Room Types", x='Room Type', y='Price') +
        scale_fill_manual(values=c("Hotel room" = "#A9D2E2",
                                   "Shared room" = "#B7E5B7",
                                   "Private room" = "#F9E4A1",
                                   "Entire home/apt" = "#F7A8A8")) +
        theme_clean() +
        guides(fill="none")
    }
  })
  
  output$plot3 <- renderPlot({
    if(input$location_input %in% c(unique(data$borough), unique(data$neighborhood))){
      if(input$location_input %in% unique(data$borough)){
        plot3_data <- data[data$borough == input$location_input,]
      } else if(input$location_input %in% unique(data$neighborhood)){ # if selection is neighborhood
        plot3_data <- data[data$neighborhood == input$location_input,]
      }
      ggplot(plot3_data, aes(x=Construction.year)) +
        geom_histogram(fill="#A1C6E7", binwidth=1, boundary=-0.5, color="black") +
        scale_x_continuous(breaks=seq(min(plot3_data$Construction.year, na.rm=TRUE),
                                      max(plot3_data$Construction.year, na.rm=TRUE),
                                      by=1)) +
        scale_y_continuous(expand=c(0,0)) +
        labs(title="Distribution of Construction Year", x="Year", y='Count') +
        theme_clean() +
        theme(axis.text.x = element_text(angle=90))
    } else {
    ggplot(data, aes(x=Construction.year)) +
      geom_histogram(fill="#A1C6E7", binwidth=1, boundary=-0.5, color='black') +
      scale_x_continuous(breaks=seq(min(data$Construction.year, na.rm=TRUE),
                                    max(data$Construction.year, na.rm=TRUE),
                                    by=1)) +
      scale_y_continuous(expand=c(0,0)) +
      labs(title="Distribution of Construction Year", x="Year", y='Count') +
      theme_clean() +
      theme(axis.text.x = element_text(angle=90))
    }
  })
  
  output$plot4 <- renderPlot({
    if(input$location_input %in% c(unique(data$borough), unique(data$neighborhood))){
      if(input$location_input %in% unique(data$borough)){
        plot4_data <- data[data$borough == input$location_input,]
      } else if(input$location_input %in% unique(data$neighborhood)){ # if selection is neighborhood
        plot4_data <- data[data$neighborhood == input$location_input,]
      }
      ggplot(plot4_data, aes(x=availability.365)) +
        geom_histogram(binwidth=30, boundary=0, fill="#A7DB8D", color="black") +
        scale_x_continuous(breaks=seq(0,365,30)) +
        scale_y_continuous(expand=c(0,0)) +
        labs(title="Listing Availability", subtitle="Over the next 365 days", x="Availability.365 (Bin Width = 30 days)", y="Count") +
        theme_clean()
    } else {
    ggplot(data, aes(x=availability.365)) +
      geom_histogram(binwidth=30, boundary=0, fill="#A7DB8D", color="black") +
      scale_x_continuous(limits=c(0,365),
                         breaks=seq(0,365,30)) +
      scale_y_continuous(expand=c(0,0)) +
      labs(title="Listing Availability", subtitle="Over the next 365 days", x="Days (Bin Width = 30 days)", y="Count") +
      theme_clean()
    }
  })
  
  output$room_type_stats <- renderUI({
    # Filtering by borough or neighborhood
    if(input$location_input %in% unique(data$borough)){
      stats_text <- data[data$borough == input$location_input,]
    } else if(input$location_input %in% unique(data$neighborhood)){
      stats_text <- data[data$neighborhood == input$location_input,]
    } else stats_text <- data
    
    num_listings <- prettyNum(nrow(stats_text), big.mark = ',')
    perc_listings <- paste0(round(nrow(stats_text) / nrow(data), 3) * 100, "%")
    total_listings <- prettyNum(nrow(data), big.mark = ",")
    
    # Summarizing stats by room type
    stats_by_room <- stats_text %>%
      group_by(room.type) %>%
      summarise(num_listings = n(),
                perc_listings = round(n() / nrow(stats_text), 3) * 100) %>%
      replace_na(list(num_listings = 0, perc_listings = 0.0))  # Replace NA with 0 and 0.0
    
    # Define custom room order
    room_order <- c("Entire home/apt", "Private room", "Shared room", "Hotel room")
    
    # Identify missing room types
    missing_rooms <- setdiff(room_order, stats_by_room$room.type)
    
    # Create a dataframe for missing rooms
    if(length(missing_rooms) > 0) {
      missing_rooms_df <- data.frame(
        room.type = missing_rooms,
        num_listings = 0,
        perc_listings = 0.0
      )
      stats_by_room <- bind_rows(stats_by_room, missing_rooms_df)
    }
    
    # Reorder room types based on the defined order
    stats_by_room <- stats_by_room %>%
      mutate(room.type = factor(room.type, levels = room_order)) %>%
      arrange(room.type)
    
    # Format number columns
    stats_by_room$num_listings <- prettyNum(stats_by_room$num_listings, big.mark = ",")
    stats_by_room$perc_listings <- paste0(stats_by_room$perc_listings, "%")
    
    # Create the output text for the UI
    room_type_output_text <- paste(
      "<b style='font-size: 32px;'>", num_listings, "</b><br/>",
      "<span style='font-size: 14px;'> out of ", total_listings, " listings (", perc_listings, ")</span><br/><br/>",
      "<b style='font-size: 20px;'>", stats_by_room$num_listings[1], " (", stats_by_room$perc_listings[1], ")</b><br/>", "entire homes/apartments<br/>",
      "<b style='font-size: 20px;'>", stats_by_room$num_listings[2], " (", stats_by_room$perc_listings[2], ")</b><br/>", "private rooms<br/>",
      "<b style='font-size: 20px;'>", stats_by_room$num_listings[3], " (", stats_by_room$perc_listings[3], ")</b><br/>", "shared rooms<br/>",
      "<b style='font-size: 20px;'>", stats_by_room$num_listings[4], " (", stats_by_room$perc_listings[4], ")</b><br/>", "hotel rooms<br/>",
      sep = ""
    )
    
    HTML(room_type_output_text)
  })
  output$scatter_rent <- renderPlot({
    scatter_data$monthly_gross <- scatter_data$avg_price * 30.4 * (input$occupancy_rate/100) * (1-(input$op_costs/100)) * 0.97
    
    xmax <- round(max(scatter_data$asking_rent, na.rm=TRUE) + 500, -3)
    ymax <- round(max(scatter_data$monthly_gross, na.rm=TRUE) + 500, -3)
      
    ggplot(scatter_data, aes(x=asking_rent, y=monthly_gross, color=Borough)) +
      geom_point(size=3) +
      geom_point(shape=1, size=3, color="black", ) +
      scale_color_manual(values=c("Manhattan" = "#DA291C",
                                 "Queens" = "#800080",
                                 "Brooklyn" = "#FFC72C",
                                 "Bronx" = "#00843D",
                                 "Staten Island" = "#0078C6")) +
      geom_abline(intercept=0, slope=1, color="red") +
      scale_x_continuous(limits=c(0,7500),
                         breaks=seq(0,7500,1000),
                         expand=c(0,0)) +
      scale_y_continuous(limits=c(0,7500),
                         breaks=seq(0,7500,1000),
                         expand=c(0,0)) +
      labs(title="Gross Revenue against Median Asking Rent", x="Asking Rent", y="Gross Revenue") +
      theme_clean() +
      theme(legend.position="bottom")
  })
  # Create reactive values to store plot click coordinates
  plot_click <- reactiveValues(x = NA, y = NA, neighborhood = NA, borough = NA)
  
  # Update reactive values when user clicks on the plot
  observeEvent(input$plot_click, {
    plot_click$x <- input$plot_click$x
    plot_click$y <- input$plot_click$y
    
    scatter_data$monthly_gross <- scatter_data$avg_price * 30.4 * (input$occupancy_rate/100) * (1-(input$op_costs/100)) * 0.97
    
    closest_point <- scatter_data %>%
      mutate(dist = sqrt((asking_rent - plot_click$x)^2 + (monthly_gross - plot_click$y)^2)) %>%
      filter(dist == min(dist, na.rm = TRUE), dist <= max_distance) %>%
      slice(1)
    
    if (nrow(closest_point) > 0) {  # If a point is found
      plot_click$neighborhood <- paste(closest_point$neighborhood, case_when(
        closest_point$Borough == "Manhattan" ~ "MH",
        closest_point$Borough == "Brooklyn" ~ "BK",
        closest_point$Borough == "Queens" ~ "QN",
        closest_point$Borough == "Bronx" ~ "BX",
      ), sep=", ")
      plot_click$rent <- closest_point$asking_rent
      plot_click$revenue <- closest_point$monthly_gross
    } else {  # If no exact match, reset values
      plot_click$neighborhood <- "No match within radius"
      plot_click$rent <- NA
      plot_click$revenue <- NA
    }
  })
  
  # Render text output using the reactive values
  output$rent_info <- renderText({
    req(plot_click$x, plot_click$y, plot_click$neighborhood)  # Ensure values exist before rendering
    paste0(plot_click$neighborhood,
           "\nAsking Rent = $", round(plot_click$x, 0),
           "\nEst. Revenue = $", round(plot_click$y, 0))
  })
  
  output$AirBNB_table <- renderDataTable({data},
                                         options = list(scrollX = TRUE)
  )
  output$download <- downloadHandler({data},
                                     filename = function(){"AirBNB_data.csv"},
                                     content = function(fname){
                                       write.csv(data(), fname)
                                     }
  )
}

shinyApp(ui, server)