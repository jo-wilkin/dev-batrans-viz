# Archive Code

# Add first row of text

fluidRow(
  column(4, p("In March 2020, in response to the COVID-19 pandemic, the State of California ordered all residents to Shelter-In-Place (SIP)."),
         p("The aim of the order was to reduce the spread of the novel coronavirus by encouraging all residents who could, to work at home.")),
  column(4, p("With many people working from home across the San Francisco Bay Area resulting in low levels of commuting, it was expected that car and public transport usage would drop."),
         p("This visualisation helps to show the impact of the SIP order on main travel routes in the San Francisco Bay Area by looking at key commuting corridors via bridges and the use of the main form of local public transportation, the BART.")), 
  column(4, p("Within this visualisation, you can look at monthly one-way toll crossing statistics for 1) the Golden Gate Bridge, 2) the Bay Bridge, 3) other Bridges in the SF Bay Area (Antioch, Benicia-Martinez, Carquinez, Dumbarton, Richmond-San Rafael and San Mateo-Hayward bridges)
        and 4) BART ridership statistics (average weekday BART station exits)."),
         p("Data is available from January 2019 until September 2021."))
),

p("In March 2020, in response to the COVID-19 pandemic, the State of California ordered all residents to Shelter-In-Place (SIP)."),
p("The aim of the order was to reduce the spread of the novel coronavirus by encouraging all residents who could, to work at home."),
p("With many people working from home across the San Francisco Bay Area resulting in low levels of commuting, it was expected that car and public transport usage would drop."),
p("This visualisation helps to show the impact of the SIP order on main travel routes in the San Francisco Bay Area by looking at key commuting corridors via bridges and the use of the main form of local public transportation, the BART."),
p("Within this visualisation, you can look at monthly one-way toll crossing statistics for 1) the Golden Gate Bridge, 2) the Bay Bridge, 3) other Bridges in the SF Bay Area (Antioch, Benicia-Martinez, Carquinez, Dumbarton, Richmond-San Rafael and San Mateo-Hayward bridges)
        and 4) BART ridership statistics (average weekday BART station exits)."),
p("Data is available from January 2019 until September 2021."),)),

Antioch, Benicia-Martinez, Carquinez, Dumbarton, Richmond-San Rafael and San Mateo-Hayward bridges

# Plot graph initially
# output$graph <- renderPlot({
#   selected() %>% ggplot(aes(date, n)) +
#     geom_line(color = "#0099f9", size = 1) +
#     geom_point(color = "#0099f9", size = 3) +
#     labs(title = "Number of users",
#          subtitle = "Data from 2019 to 2021",
#          caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
#          y = "Number of Users",
#          x = "Month / Year")  +
#     #ylim(0, 9000000) +
#     scale_y_continuous(
#       labels = scales::unit_format(unit = "Million", scale = 1e-6), limits = c(0, 9000000)) +
#     scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date))) +
#     theme(
#       plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
#       plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
#       plot.caption = element_text(face = "italic", hjust = 0),
#       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#       panel.background = element_blank(), axis.line = element_line(colour = "grey",),
#       axis.text.x=element_text(angle=60, hjust=1)
#     )
# }, res = 96)

# observeEvent(input$StandardY, {
#   output$graph <- renderPlot({
#     selected() %>% ggplot(aes(date, n)) +
#       geom_line(color = "#0099f9", size = 1) + 
#       geom_point(color = "#0099f9", size = 3) +
#       labs(title = "Number of users",
#            subtitle = "Data from 2019 to 2021",
#            caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
#            y = "Number of Users",
#            x = "Month / Year")  +
#       #ylim(0, 9000000) +
#       scale_y_continuous(
#         labels = scales::unit_format(unit = "Million", scale = 1e-6), limits = c(0, 9000000)) +
#       scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date))) +
#       theme(
#         plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
#         plot.caption = element_text(face = "italic", hjust = 0),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "grey",),
#         axis.text.x=element_text(angle=60, hjust=1)
#       ) 
#   }, res = 96)
# })

# observeEvent(input$AdjustedY, {
#   output$graph <- renderPlot({
#     selected() %>% ggplot(aes(date, n)) +
#       geom_line(color = "#0099f9", size = 1) + 
#       geom_point(color = "#0099f9", size = 3) +
#       labs(title = "Number of users",
#            subtitle = "Data from 2019 to 2021",
#            caption = "Data Sources: Metropolitan Transportation Commission and Golden Gate Bridge Highway & Transportation District",
#            y = "Number of Users",
#            x = "Month / Year")  +
#       #ylim(0, 9000000) +
#       scale_y_continuous(
#         labels = scales::unit_format(unit = "Million", scale = 1e-6)) +
#       scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limit=c(as.Date(input$min_date),as.Date(input$max_date))) +
#       theme(
#         plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
#         plot.caption = element_text(face = "italic", hjust = 0),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "grey",),
#         axis.text.x=element_text(angle=60, hjust=1)
#       ) 
#   }, res = 96)
# })

# Experimenting with printing out the user numbers - want to supersede this with tooltip!
# output$info <- renderText({
#   paste0(round(input$plot_click$y))

# output$info <- renderPrint({
#   nearPoints(selected(), input$plot_hover)

#output$info <- renderText({
#  paste0((nearPoints(selected(), input$plot_click))[2])  
#  })