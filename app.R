#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(dplyr)
library(readr)
library(rsconnect)
library(shiny)
library(ggplot2)
library(bslib)
library(readxl)
library(bsicons)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

joined_data1 <- read_excel("accident_person.xlsx")
joined_data2 <- read_excel("accident_person.xlsx")

count_brand <- c(4394,5144,3221,7113, 50, 1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Plots", fluid = TRUE,
      # Application title
      titlePanel(h1("Accident Frequencies", align = "center")),
      h4("Dataset Contains:"),
      
      # Sidebar with a slider input for number of bins
      layout_column_wrap(
        width = "200px",
        fill = FALSE,
        value_box(
          title = "Honda",
          value = scales::unit_format(unit = "cars")(count_brand[[1]]),
          showcase = bsicons::bs_icon("car-front")
        ),
        value_box(
          title = "Toyota",
          value = scales::unit_format(unit = "cars")(count_brand[[2]]),
          showcase = bsicons::bs_icon("car-front")
        ),
        value_box(
          title = "Nissan",
          value = scales::unit_format(unit = "cars")(count_brand[[3]]),
          showcase = bsicons::bs_icon("car-front"),
        ),
        value_box(
          title = "Cheverlot",
          value = scales::unit_format(unit = "cars")(count_brand[[4]]),
          showcase = bsicons::bs_icon("car-front")
        ),
        value_box(
          title = "States",
          value = scales::unit_format(unit = "")(count_brand[[5]]),
          showcase = bsicons::bs_icon("geo-fill")
        ),
        value_box(
          title = "Federal Districts",
          value = scales::unit_format(unit = "")(count_brand[[6]]),
          showcase = bsicons::bs_icon("star-fill")
        )
      ),
      card(
        card_header("Frequency Plot 1"),
        layout_sidebar(
          sidebar = sidebar(
            bg = "lightgrey",
            selectInput("route1", "Route Type:", c("All","Interstate"=1, "US Highway"=2, "State Highway"=3, "County Road"=4, "Local Street – Township"=5, "Local Street – Municipality"=6, "Local Street – Frontage Road"=7, "Other"=8, "Unknown"=9)),
            selectInput("area1", "Area Type:", c("All","Rural"=1, "Urban"=2, "Trafficway Not in State Inventory"=6, "Not Reported"=8, "Unknown"=9))
          ),
          plotOutput("plot1")
        )
      ),
      card(
        card_header("Frequency Plot 2"),
        layout_sidebar(
          sidebar = sidebar(
            bg = "lightgrey",
            selectInput("route2", "Route Type:", c("All","Interstate"=1, "US Highway"=2, "State Highway"=3, "County Road"=4, "Local Street – Township"=5, "Local Street – Municipality"=6, "Local Street – Frontage Road"=7, "Other"=8, "Unknown"=9)),
            selectInput("area2", "Area Type:", c("All","Rural"=1, "Urban"=2, "Trafficway Not in State Inventory"=6, "Not Reported"=8, "Unknown"=9))
          ),
          plotOutput("plot2")
        ))
      
    ),
    tabPanel("Map", fluid = TRUE,
            card(
              leafletOutput("map")
            )
             ),
    tabPanel("Data", fluid = TRUE,
      card(
        layout_sidebar(
          sidebar = sidebar(
            bg = "lightgrey",
            selectInput("state", "State:", state.name),
            selectInput("age", "Age Group:", c("10-19"=1, "20-29"=2, "30-39"=3, "40-49"=4, "50-59"=5, "60-69"=6, "70-79"=7, "80+"=8)),
            selectInput("brandcode", "Brand:", c("Honda"=0,"Toyota"=1, "Nissan"=2, "Chevrolet"=3))
          ),
          tableOutput("table1")
        )
      )
      
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filter1 <- reactive(input$route1)
  filter2 <- reactive(input$area1)
  
  filter3 <- reactive(input$route2)
  filter4 <- reactive(input$area2)
  
  filter5 <- reactive(input$age)
  filter6 <- reactive(input$brandcode)
  filter7 <- reactive(input$state)
  
  output$plot1 <- renderPlot({
    if (filter1() == "All" & filter2() == "All") {
      data1 <- joined_data1
      ggplot(joined_data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else if (filter1() == "All" & filter2() != "All") {
      data1 <- filter(joined_data1, RUR_URB == filter2())
      ggplot(data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else if (filter1() != "All" & filter2() == "All") {
      data1 <- filter(joined_data1, ROUTE == filter1())
      ggplot(data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else {
      data1 <- filter(joined_data1, RUR_URB == filter2() & ROUTE == filter1())
      ggplot(data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
  }, res = 96)
  
  output$plot2 <- renderPlot({
    if (filter3() == "All" & filter4() == "All") {
      data2 <- joined_data2
      ggplot(data2, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else if (filter3() == "All" & filter4() != "All") {
      data2 <- filter(joined_data2, RUR_URB == filter4())
      ggplot(data2, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else if (filter3() != "All" & filter4() == "All") {
      data2 <- filter(joined_data2, ROUTE == filter3())
      ggplot(data2, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else {
      data2 <- filter(joined_data2, RUR_URB == filter4() & ROUTE == filter3())
      ggplot(data2, aes(Age, colour = Brand)) + geom_freqpoly()
    }
  }, res = 96)
  
  output$table1 <- renderTable({
    data3 <- filter(joined_data1, AgeGroup == filter5() & BrandCode == filter6() & STATENAME == filter7())})
  
  output$map <- renderLeaflet({
    if (filter1() == "All" & filter2() == "All") {
      data1 <- joined_data1
      ggplot(joined_data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else if (filter1() == "All" & filter2() != "All") {
      data1 <- filter(joined_data1, RUR_URB == filter2())
      ggplot(data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else if (filter1() != "All" & filter2() == "All") {
      data1 <- filter(joined_data1, ROUTE == filter1())
      ggplot(data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    else {
      data1 <- filter(joined_data1, RUR_URB == filter2() & ROUTE == filter1())
      ggplot(data1, aes(Age, colour = Brand)) + geom_freqpoly()
    }
    leaflet(data1) %>%
      addTiles() %>%
      addCircleMarkers(
        ~LONGITUD, ~LATITUDE,
        radius = ~sqrt(FATALS) + 3,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5,
        label = ~paste("Fatalities:", FATALS)
      ) %>%
      setView(lng = -98.35, lat = 39.50, zoom = 4) 
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
