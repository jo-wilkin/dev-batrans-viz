library(shiny)
library(vroom)
library(tidyverse)
library(janitor)
library(bslib)
library(showtext)
library(thematic)
library(plotly)

# MTC data is provided in XLS format from MTC website (https://mtc.ca.gov/tools-resources/data-tools/monthly-transportation-statistics)
# Golden Gate data is on the web only (https://www.goldengate.org/bridge/history-research/statistics-data/monthly-traffic-crossings/)
# For now, the two datasets are formatted and merged in Excel - the MTC data is not readable as a dataframe in its current format

# Read in transportation data
transport_data <- read.csv('monthly-transportation-tidy.csv', na.strings = "NULL", stringsAsFactors = FALSE) %>% clean_names()

# Select the column names from our transport data and set names to them
# Adds functionality if transport_data changes to easily add in new data columns to the app
transport_types <- select(transport_data, 2:5) %>% colnames() %>% setNames(c("Bay Bridge", "Other Bridges", "BART", "Golden Gate"))

# Re-read date column in data format
transport_data$date <- lubridate::my(transport_data$date)

# Plot code test - integrated into server function
# ggplot(transport_data, aes(x=date, y=golden_gate)) +
#   geom_line() + 
#   xlab("")

# Design theme for plots
transport_theme <- bs_theme(
  #bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198", 
  # Add custom base font from google
  base_font = font_google("Montserrat")
)

# Begin to build Shiny app

# UI Interface
ui <- fluidPage(
  # Add reference to stylesheet in www folder
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  # Add transport theme from above for base font
  theme = transport_theme,
  # Rename title panel - need to figure out removing H1 from the tab in browser
  titlePanel(h1("Monthly Transportation Statistics in the Bay Area")),
  # Initiate sidebar layout - note, needs siderbar and Main panel
  sidebarLayout(
    # Start with sidebar panel
    sidebarPanel(
      p("In March 2020, in response to the COVID-19 pandemic, the State of California ordered as residents to Shelter-In-Place.
        The aim of the order was to reduce the spread of the novel coronavirus encouraging all residents who could, to work at home."), 
      p("This visualisation helps to show the impact of this order on travel in the San Francisco Bay Area by looking at key transportation routes and their user statistics."),
      p("You can visualise monthly one-way toll crossing statistics for the Golden Gate Bridge, the Bay Bridge, other Bridges in the SF Bay Area (Antioch, Benicia-Martinez, Carquinez, Dumbarton, Richmond-San Rafael and San Mateo-Hayward bridges)
        and BART ridership statistics (average weekday BART station exits)."),
      p("Data is available from January 2019 until September 2021"),
      em("NB. The Golden Gate Bridge only has data until August 2021."),
      p(""),
      
      helpText("Choose a transit mode to plot their monthly statistics:"),
      
      # Input selector for the variables to display
      selectInput("var", 
                  label = "",
                  choices = transport_types,
                  selected = "bay_bridge"),
      
      p("You can also filter the data to a specific data range of interest"), 
      
      # Date Range Input
      #sliderInput("date_range", label = "Date Range Of Interest:", min = min(transport_data$date) , max = max(transport_data$date), c(min(transport_data$date), max(transport_data$date))),
      
      # Select minimum date
      #tags$div(selectInput("min_date", label = "From:", choices = transport_data$date, selected = min(transport_data$date)), style="display:inline-block"),
      
      # Select maximum date
      #tags$div(selectInput("max_date", label = "To:", choices = transport_data$date, selected = max(transport_data$date)), style="display:inline-block"),
      
      fluidRow(
        column(6, selectInput("min_date", label = "From:", choices = transport_data$date, selected = min(transport_data$date))),
        column(6, selectInput("max_date", label = "To:", choices = transport_data$date, selected = max(transport_data$date))),
      ),
    ),

    mainPanel(
      #plotOutput("graph", click = "plot_click"),
      #plotOutput("graph", hover = "plot_hover"),
      div(
        style = "position:relative",
        plotOutput("graph", 
                   hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
        uiOutput("hover_info")
      ),
      
      #fluidRow(
       # column(4, p("Number of users:")), 
        #column(4, textOutput("info"))),
      p("You can adjust the Y axis to investigate the data further."), 
      actionButton("StandardY", label = "Standardized Y Axis"),
      actionButton("AdjustedY", label = "Adjusted Y Axis"),
        )
      )
)
  
    
      

server <- function(input, output, session){
  
  selected <- reactive(transport_data %>% select(date, input$var) %>% rename(n = input$var))
  
  # Plot graph initially
  output$graph <- renderPlot({
    selected() %>% ggplot(aes(date, n)) +
      geom_line(color = "#0099f9", size = 1) +
      geom_point(color = "#0099f9", size = 3) +
      labs(title = "Number of users",
           subtitle = "Data from 2019 to 2021",
           caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
           y = "Number of Users",
           x = "Month / Year")  +
      #ylim(0, 9000000) +
      scale_y_continuous(
        labels = scales::unit_format(unit = "Million", scale = 1e-6), limits = c(0, 9000000)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date))) +
      theme(
        plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.caption = element_text(face = "italic", hjust = 0),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey",),
        axis.text.x=element_text(angle=60, hjust=1)
      )
  }, res = 96)
  
  observeEvent(input$StandardY, {
    output$graph <- renderPlot({
      selected() %>% ggplot(aes(date, n)) +
        geom_line(color = "#0099f9", size = 1) + 
        geom_point(color = "#0099f9", size = 3) +
        labs(title = "Number of users",
             subtitle = "Data from 2019 to 2021",
             caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
             y = "Number of Users",
             x = "Month / Year")  +
        #ylim(0, 9000000) +
        scale_y_continuous(
          labels = scales::unit_format(unit = "Million", scale = 1e-6), limits = c(0, 9000000)) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date))) +
        theme(
          plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic", hjust = 0),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "grey",),
          axis.text.x=element_text(angle=60, hjust=1)
        ) 
    }, res = 96)
  })
  
  observeEvent(input$AdjustedY, {
    output$graph <- renderPlot({
      selected() %>% ggplot(aes(date, n)) +
        geom_line(color = "#0099f9", size = 1) + 
        geom_point(color = "#0099f9", size = 3) +
        labs(title = "Number of users",
             subtitle = "Data from 2019 to 2021",
             caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
             y = "Number of Users",
             x = "Month / Year")  +
        #ylim(0, 9000000) +
        scale_y_continuous(
          labels = scales::unit_format(unit = "Million", scale = 1e-6)) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date))) +
        theme(
          plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic", hjust = 0),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "grey",),
          axis.text.x=element_text(angle=60, hjust=1)
        ) 
    }, res = 96)
  })
    
    # Experimenting with printing out the user numbers - want to supersede this with tooltip!
    # output$info <- renderText({
    #   paste0(round(input$plot_click$y))
  
    # output$info <- renderPrint({
    #   nearPoints(selected(), input$plot_hover)
      
    #output$info <- renderText({
    #  paste0((nearPoints(selected(), input$plot_click))[2])  
    #  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(selected(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", str_sub(toString(point$date), 1,7), "<br/>",
                    "<b>Number: </b>", point$n)))
    )
  })
}

thematic_shiny(font = "auto")

shinyApp(ui, server)