
library(shiny)
library(leaflet)
library(dplyr)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Really shit Fit View-R"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            fileInput("file1", "Choose FIT File", accept = ".fit"),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel(
                "output.fileUploaded == true",
                h2("File uploaded"),
                h3("Summary of .fit:"),
                h4("Total distance:"),
                h4(textOutput("summaryDistance"), "km"),
                h4("Total Duration (seconds):"),
                h4(textOutput("summaryDuration")),
                h4("Average Speed:"),
                h4(textOutput("summaryAvgSpeed")),
                h4("Maximum Altitude:"),
                h4(textOutput("summaryMaxAlt")),
                h4("Minimum Altitude:"),
                h4(textOutput("summaryMinAlt")),
                h4("Overall Altitude change:"),
                h4(textOutput("summaryAltChange")),
                
                h3("Graphs of .fit"),
                h4("Altitude by timestamp"),
                plotOutput("graphAlt"),
                h4("Temperature by timestamp"),
                plotOutput("graphTemp"),
                h4("Cadence by timestamp"),
                plotOutput("graphCad"),
                h4("Speed by timestamp"),
                plotOutput("graphSpeed"),

                h3("Map of route"),
                leafletOutput(outputId = "mymap"),
                
                h3("Individual row details of .fit"),
                fluidRow(
                    uiOutput("details")
                ),
            ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rawData <- eventReactive(input$file1, {
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        ext <- tools::file_ext(inFile)
        validate(need(ext == "fit", "Incorrect file type. Please upload a fit file."))
        
        bind_rows(records(readFitFile(inFile$datapath, dropUnknown = TRUE, mergeMessages = TRUE)))
    })
    
    output$fileUploaded <- reactive({
        return(!is.null(rawData()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
    
    renderLineGraph <- function(dataInput, xn, yn) {
        renderPlot({
            df <- dataInput()
            p <- ggplot(data = df, aes_string(x = xn, y = yn)) +
                geom_point(size = 2,
                           colour = "deepskyblue4",
                           alpha = 0.5) +
                geom_line(colour = "gray30",
                          size = 0.5,
                          na.rm = FALSE)
            p
        })
    }
    
    
    pal <- reactive({
        colorNumeric(c("red", "blue","black"), 
                        domain = 0:max(mapdf()$position_id),
                        alpha = FALSE
                        )
    })
#   https://github.com/rstudio/shiny/issues/858  
#   https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
    
    output$mymap <- renderLeaflet({
        leaflet(mapdf()) %>% 
            setView(lng = medLon(),
                    lat = medLat(),
                    zoom = 14) %>% 
            addTiles() %>% 
            
            addCircleMarkers(data = mapdf(),
                             lat = mapdf()$position_lat,
                             lng = mapdf()$position_long,
                             radius = 3,
                             color = ~pal()(position_id),
                             fillOpacity = 0.8,
                             stroke = TRUE)
    })
    
    
##################################################################################################### 
 # Datasets for individual graphs - requires individual in case NA in only one field?  
    allSpeeds <- reactive({
        allSpeeds <- rawData() %>% 
            filter(!is.na(speed)) %>% 
            select(timestamp, speed)
    })

    allTemps <- reactive({
        allTemps <- rawData() %>% 
            filter(!is.na(temperature)) %>% 
            select(timestamp, temperature)
    })
    
    allCadence <- reactive({
        allCadence <- rawData() %>% 
            filter(!is.na(cadence)) %>% 
            select(timestamp, cadence)
    })
    
    allAltitude <- reactive({
        allAltitude <- rawData() %>% 
            filter(!is.na(altitude)) %>% 
            select(timestamp, altitude)
    })
    
    mapdf <- reactive({
        mapdf <- rawData() %>% 
            filter(!is.na(position_lat)) %>% 
            select(timestamp, position_lat, position_long) %>% 
            mutate(position_id = row_number()) %>% 
            mutate(maxPosition_id = max(row_number()))
    })

#####################################################################################################  
# Median coordinates for boundary of map    
    # Originally housed min and max for use with osmaps - found Leaflet tutorial easier and neater implementation:
   
    medLon <- reactive({
        medLon <- rawData() %>% 
            summarise(
                med_Lon = median(position_long, na.rm = TRUE)
            )
        paste0(medLon)
    })     
    
    medLat <- reactive({
        medLat <- rawData() %>% 
            summarise(
                med_Lat = median(position_lat, na.rm = TRUE)
            )
        paste0(medLat)
    }) 

#####################################################################################################     
# Summary values for info boxes
    
    summaryDistance <- reactive({
        summaryDistance <- rawData() %>% 
            summarise(
                maxDistance = max(distance, na.rm = TRUE)
            )
        paste0(summaryDistance)
    })
    
    summaryDuration <- reactive({
        summaryDuration <- rawData() %>% 
            summarise(
                sDuration = as.numeric(difftime(max(timestamp, na.rm = TRUE),
                                                min(timestamp, na.rm = TRUE),
                                                units = "secs")
                                       )
            )
        paste0(summaryDuration)
    })

    summaryAvgSpeed <- reactive({
        summaryAvgSpeed <- (as.numeric(summaryDistance()) / as.numeric(summaryDuration())) * 3600
        paste0(summaryAvgSpeed)
    })
    
    summaryMaxAlt <- reactive({
        summaryMaxAlt <- rawData() %>% 
            summarise(
                maxAlt = max(altitude, na.rm = TRUE)
            )
        paste0(summaryMaxAlt)
    })
    
    summaryMinAlt <- reactive({
        summaryMinAlt <- rawData() %>% 
            summarise(
                minAlt = min(altitude, na.rm = TRUE)
            )
        paste0(summaryMinAlt)
    })
    
    summaryAltChange <- reactive({
        summaryAltChange <- as.numeric(summaryMaxAlt()) - as.numeric(summaryMinAlt())
        paste0(summaryAltChange)
    })    
    
    
    
    output$summaryDistance <- renderText({
        summaryDistance()
    })
    output$summaryDuration <- renderText({
        summaryDuration()
    })
    output$summaryAvgSpeed <- renderText({
        summaryAvgSpeed()
    })
    output$summaryMaxAlt <- renderText({
        summaryMaxAlt()
    })
    output$summaryMinAlt <- renderText({
        summaryMinAlt()
    })
    output$summaryAltChange <- renderText({
        summaryAltChange()
    })
    
    
    output$contents1 <- renderTable({
        rawData()
    })
    
    output$contents2 <- DT::renderDataTable({
        DT::datatable(rawData())
    })

    
    output$graphAlt <- renderLineGraph(allAltitude, 'timestamp', 'altitude')
    output$graphTemp <- renderLineGraph(allTemps, 'timestamp', 'temperature')
    output$graphCad <- renderLineGraph(allCadence, 'timestamp', 'cadence')
    output$graphSpeed <- renderLineGraph(allSpeeds, 'timestamp', 'speed')
    
    pushDetails <- reactive({
        box(
            width = 12,
            title = span("Full export of fit details",
                         style = "color: white"),
            status = "primary",
            solidHeader = TRUE, 
            collapsible = TRUE, 
            collapsed = TRUE,
            #tableOutput("contents1")
            DT::dataTableOutput("contents2")
        )
    })
    output$details <- renderUI({
        pushDetails()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
