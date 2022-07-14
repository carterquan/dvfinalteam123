library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)

cardata=read.csv("ElectricCarData_Clean.csv")
oil = read.csv("oilprice.csv")
oil$date = as.Date(oil$date, format = "%m/%d/%Y")



ui <- dashboardPage(
  dashboardHeader(title = "Why Electric Cars"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Oil-Price",
      tabName = "page1",
      icon = icon("line-chart")
    ),
    menuItem("Market Trend", tabName = "page2", icon = icon("area-chart")),
    menuItem("Distribution Map", tabName = "page3", icon = icon("map-o")),
    menuItem("Vehicle Information", tabName = "page4", icon = icon("map-o"))
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "page1",
      sliderInput(
        "year",
        "Year:",
        min = 1983,
        max = 2022,
        value = c(1983, 2022),
        sep = ""
      ),
      plotOutput("plot1")
    ),
    tabItem(
      tabName = "page2",
      sliderInput(
        "year",
        "Year:",
        min = 2014,
        max = 2020,
        value = 1,
        step = 1,
        animate = animationOptions(interval = 2000, loop = FALSE)
      ),
      plotOutput("plot2")
    ),
    tabItem(tabName = "page3",
            h2("Electric Vehicle Charing Station Distribution"),
            leafletOutput("myMap", width = "100%")),
    tabItem(
      tabName = "page4",
      
      
      
      # Application title
      titlePanel("Electric Vehicles Information"),
      
      fluidRow(
        column(2,
               selectInput("Brand",
                           "Brand:",
                           c(
                             "All",
                             unique(as.character(cardata$Brand))
                           ))),
        column(2,
               selectInput("Model",
                           "Model:",
                           c(
                             "All",
                             unique(as.character(cardata$Model))
                           ))),
        column(2,
               selectInput(
                 "BodyStyle",
                 "BodyStyle:",
                 c("All",
                   unique(as.character(cardata$BodyStyle)))
               )),
        column(2,
               selectInput("Segment",
                           "Segment:",
                           c(
                             "All",
                             unique(as.character(cardata$Segment))
                           ))),
        column(2,
               selectInput("Seats",
                           "Seats:",
                           c(
                             "All",
                             unique(as.character(cardata$Seats))
                           )))
      ),
      # Create a new row for the table.
      DT::dataTableOutput("table")
      
      
      
    )
  ))
)


server <- function(input, output, session) {
  us_data = read_csv("ev_stations.csv")
  oil = read.csv("oilprice.csv")
  oil$date = as.Date(oil$date, format = "%m/%d/%Y")
  marketdata <- read_csv("evmarketshare.csv")
  marketdata <- pivot_longer(marketdata,
                             c("2021 q1","2021 q2","2021 q3","2021 q4","2022 q1","2022 q2"),
                             names_to = "time", values_to = "sales")
  marketdata$sales <- as.numeric(marketdata$sales)
  
  output$plot1 = renderPlot({
    minyear = input$year[1]
    maxyear = input$year[2]
    
    Q = oil %>%
      filter(as.numeric(format(oil$date, '%Y')) >= minyear &
               as.numeric(format(oil$date, '%Y')) <= maxyear) %>%
      ggplot(aes(x = date, y = price)) + geom_point() + geom_line() +
      ylim(0, 150) +
      labs(subtitle = "Oil-Price",
           x = "Time",
           y = "Price in ($)")
    
    return(Q)
    
    
  })
  
  output$plot2 = renderPlot({
    ggplot(marketdata, aes(x=time, y=sales, fill=automaker)) + 
      geom_col(position = "stack", color="black")+ 
      ggtitle("market trend")
    
  })
  
  output$myMap = renderLeaflet({
    view_data = us_data %>%
      group_by(lng = round(Longitude, 3), lat = round(Latitude, 3)) %>%
      summarise(N = n()) %>%
      mutate(
        latL = lat - 0.05,
        latH = lat + 0.05,
        lngL = lng - 0.05,
        lngH = lng + 0.05
      )
    
    m = view_data %>% leaflet() %>% addTiles() %>%
      
      setView(-98.58, 38.01, zoom = 4) %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addLayersControl(baseGroups = c("Toner", "OSM"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addRectangles(
        lng1 =  ~ lngL,
        lat1 =  ~ latL,
        lng2 =  ~ lngH,
        lat2 =  ~ latH,
        fillOpacity = ~ N / 10,
        opacity = 0,
        fillColor = "red",
        label = ~ N
      )
    
  })
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- cardata
    if (input$Brand != "All") {
      data <- data[cardata$Brand == input$Brand,]
    }
    if (input$Model != "All") {
      data <- data[cardata$Model == input$Model,]
    }
    if (input$BodyStyle != "All") {
      data <- data[cardata$BodyStyle == input$BodyStyle,]
    }
    if (input$Segment != "All") {
      data <- data[cardata$Segment == input$Segment,]
    }
    if (input$Seats != "All") {
      data <- data[cardata$Seats == input$Seats,]
    }
    data
  }))
  
  
  
}

shinyApp(ui = ui, server = server)
