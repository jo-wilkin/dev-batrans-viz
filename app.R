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
# Cleaning required transposing the data

# Read in transportation data
#transport_data <- read.csv('monthly-transportation.csv', na.strings = "NULL") %>% clean_names()
transport_data <- read.csv('monthly-transportation-tidy.csv', na.strings = "NULL", stringsAsFactors = FALSE) %>% clean_names()

transport_types <- select(transport_data, 2:5) %>% colnames() %>% setNames(c("Bay Bridge", "Other Bridges", "BART", "Golden Gate"))

transport_data$date <- lubridate::my(transport_data$date)

#td <- transport_data %>% select(date, golden_gate) %>% rename(n = golden_gate)

# ggplot(transport_data, aes(x=date, y=golden_gate)) +
#   geom_line() + 
#   xlab("")

# Design theme for plots

transport_theme <- bs_theme(
  #bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198", 
  base_font = font_google("Montserrat")
)

# Begin to build app

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  theme = transport_theme,
  titlePanel(h1("Monthly Transportation Statistics in the Bay Area")),
  sidebarLayout(
    sidebarPanel(
      p("In March 2020, in response to the COVID-19 pandemic, the State of California ordered as residents to Shelter-In-Place.
        The aim of the order was to reduce the spread of the novel coronavirus encouraging all residents who could, to work at home."), 
      p("This visualisation helps to show the impact of this order on travel in the San Francisco Bay Area by looking at key transportation routes and their user statistics."),
      p("You can visualise monthly one-way toll crossing statistics for the Golden Gate Bridge, the Bay Bridge, other Bridges in the SF Bay Area (Antioch, Benicia-Martinez, Carquinez, Dumbarton, Richmond-San Rafael and San Mateo-Hayward bridges)
        and BART ridership statistics (average weekday BART station exits)."),
      p("Data is available from January 2019 until September 2021 (NB. The Golden Gate Bridge only has data until August 2021)."), 
      helpText("Choose a transit mode to plot their monthly statistics:"),
      
      selectInput("var", 
                  label = "",
                  choices = transport_types,
                  selected = "bay_bridge"),
      
      p("You can also filter the data to a specific data range of interest"), 
      
      #sliderInput("range", 
       #           label = "Date Range Of Interest:",
        #          min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      plotOutput("graph"),
      )
  )
)

server <- function(input, output, session){
  
  selected <- reactive(transport_data %>% select(date, input$var) %>% rename(n = input$var))
  
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
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limit=c(as.Date("2019-01-01"),as.Date("2021-09-01"))) +
      theme(
        plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.caption = element_text(face = "italic", hjust = 0),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey",),
        axis.text.x=element_text(angle=60, hjust=1)
      ) 
  }, res = 96)
}

thematic_shiny(font = "auto")

shinyApp(ui, server)