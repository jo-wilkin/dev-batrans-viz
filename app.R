library(shiny)
library(vroom)
library(tidyverse)
library(sf)
library(janitor)
library(bslib)
library(showtext)
library(thematic)
library(plotly)
library(leaflet)

# Spatial data - Should be extracted and put into a separate app!
# Cleaned and processed so all four can be joined together

# Downloaded from: https://geodata.lib.berkeley.edu/catalog/stanford-mh686mh0418
BART <- st_read("data/BART/mh686mh0418.shp") %>% select(c(1,4)) %>% mutate(name="bart") %>% as.data.frame()

# Downloaded from: https://geodata.lib.berkeley.edu/catalog/ark28722-s7ng6f
Bridges <- st_read("data/Bridges/s7ng6f.shp")

Bay_Bridge <- Bridges %>% filter(BRIDGE_NAM == "San Francisco Bay Bridge - West")  %>% select(c(3, 4)) %>% rename(route = BRIDGE_NAM) %>% mutate(name="bay_bridge")%>% as.data.frame()
  
Golden_Gate <- Bridges %>% filter(BRIDGE_NAM == "Golden Gate Bridge")  %>% select(c(3, 4)) %>% rename(route = BRIDGE_NAM) %>% mutate(name="golden_gate")%>% as.data.frame()

# Created through downloading ALL BRIDGES in SF Bay Area via OSM
# Extracting a single way for each of the named bridges (using name to filter!)
Other_Bridges <- st_read("data/Bridges/other_bridges_2.shp") %>% rename(route = name) %>% mutate(name="other_bridges") %>% as.data.frame()

# Join all together to use in app
transport_spd <- rbind(BART, Bay_Bridge, Golden_Gate, Other_Bridges) %>% st_as_sf()

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
  titlePanel("Monthly Transportation Statistics in the Bay Area"),
  
  # Insert white space between title and body
  fluidRow(column(12, p())),
  
  # Initiate sidebar layout - note, needs siderbar and Main panel
  sidebarLayout(
    # Start with sidebar panel
    sidebarPanel(
      
      # Short Text!
      strong("Understanding the impact of COVID-19 on travel in the SF Bay Area"),
      p(),
      p("Here you can investigate the monthly one-way toll crossing statistics for various transport routes in the SF Bay Area between January 2019 and September 2021 to understand the impact COVID-19 and the resulting Shelter-In-Place had on travel volumes."),
      
      # Help Text
      helpText("Choose a transportation mode to plot their monthly statistics:"),
      
      # Input selector for the variables to display
      selectInput("var", 
                  label = "",
                  choices = transport_types,
                  selected = "bay_bridge"),
      # Extra Text
      p("You can also filter the data to a specific data range of interest"),
      
      # Add to and from selectors
      fluidRow(
        column(6, selectInput("min_date", label = "From:", choices = transport_data$date, selected = min(transport_data$date))),
        column(6, selectInput("max_date", label = "To:", choices = transport_data$date, selected = max(transport_data$date))),
      ),
      # Additional text
      em("NB. The Golden Gate Bridge only has data until August 2021."),
      p(""),
      strong("Data Sources:"),
      p(),
      p("1. Transport data: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District."),
      p("2. Map data: UC Berkely GeoData Library, Openstreetmap.")
      
    ),
    
    # Set up main panel
    mainPanel(
     
      # Put map on top
      leafletOutput("transportmap"),
      
      # Add white space
      p(),
      
      # Div used to create tooltips
      div(
        style = "position:relative",
        plotOutput("graph", 
                   hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
        uiOutput("hover_info")
      ),
      
      # White space
      p(),
      # Text
      p("You can also adjust the Y axis to study the finer trends in each dataset.", align = 'center' ), 
      # Y axis buttons
      fluidRow(column(12, align = 'center', actionButton("StandardY", label = "Standardized Y Axis"),
                      actionButton("AdjustedY", label = "Adjusted Y Axis"))),
      
    )
  ),
  
)

# Server function
server <- function(input, output, session){
  
  # Selecting variable for graph
  selected <- reactive(transport_data %>% select(date, input$var) %>% rename(n = input$var))
  
  # Filtering spatial data to only rows with relevant geometry
  spatial_data <- reactive(transport_spd %>% filter(name==input$var))
  
   # Map Creation
  output$transportmap <- renderLeaflet({
    
    # Run leaflet
    leaflet() %>%
      # Set up stamen map
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      # Set view to SF Bay Area
      setView(lng = -122.4, lat = 37.8, zoom = 9) %>%
      # Add correct spatial data
      addPolylines(data = spatial_data())
   
  })
  
  
  
  # Graph creation - set up graph theme / aesthetic to use in reactive events
  graph_plot <- reactive(selected() %>% ggplot(aes(date, n)) +
                           geom_line(color = "#0099f9", size = 1) +
                           geom_point(color = "#0099f9", size = 3) +
                           labs(title = "Number of users",
                                subtitle = "Monthly Data",
                                #caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
                                y = "Number of Users",
                                x = "Month / Year")  +
                           theme(
                             plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
                             plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
                             plot.caption = element_text(face = "italic", hjust = 0),
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "grey",),
                             axis.text.x=element_text(angle=60, hjust=1)) +
                             scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date)))
                           )
  
  #Plot graph initially
  output$graph <- renderPlot({graph_plot() +
      scale_y_continuous(labels = scales::unit_format(unit = "Million", scale = 1e-6), limits = c(0, 9000000))
  }, res = 96)
  
  # React to button press
  observeEvent(input$StandardY, {
    output$graph <- renderPlot({graph_plot() +
        scale_y_continuous(labels = scales::unit_format(unit = "Million", scale = 1e-6), limits = c(0, 9000000))
      }, res = 96)
    })
  
  # React to button press
  observeEvent(input$AdjustedY, {
    output$graph <- renderPlot({graph_plot() +
        scale_y_continuous(labels = scales::unit_format(unit = "Million", scale = 1e-6))
      }, res = 96)
    })
  
  # Create tooltips
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

# Add theme for font
thematic_shiny(font = "auto")

shinyApp(ui, server)