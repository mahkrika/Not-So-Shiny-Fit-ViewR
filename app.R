
library(shiny)
library(leaflet)
library(dplyr)
library(FITfileR)
library(ggplot2)
library(hms)
library(shinythemes)
library(shinyBS)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("yeti"),

    # Application title
    titlePanel("The Not-So-Shiny .Fit ViewR"),

    # Sidebar with input for .fit file
    sidebarLayout(
        sidebarPanel(
            
            fileInput("file1", "Choose FIT File", accept = ".fit"),
            
            h3(strong("About the No-So-Shiny Fit ViewR")),
                p("This little Shiny app is evidently not very pretty, but it is a work in progress
                  so will hopefully develop into something more aesthetically pleasing, and with more
                  features."),
                p("The tool came about because although I like the Suunto app for monitoring exercise
                  I am also a glutton for punishment and like to learn new data skills - in this instance
                  R. Therefore this is an attempt to port some of the Suunto (and any other exercise
                  monitoring watch/device) app functionality into an online tool."),
                p("It runs using the following packages"),
                HTML("<ul>
                                <li><a href='https://cran.r-project.org/web/packages/shiny/'>shiny</a></li>
                                <li><a href='https://cran.r-project.org/web/packages/shinythemes/'>shinythemes</a></li>
                                <li><a href='https://cran.r-project.org/web/packages/shinyBS/'>shinyBS</a></li>
                                <li><a href='https://cran.r-project.org/web/packages/dplyr/'>dplyr</a></li>
                                <li><a href='https://cran.r-project.org/web/packages/ggplot2/'>ggplot2</a></li>
                                <li><a href='https://cran.r-project.org/web/packages/leaflet/'>leaflet</a></li>
                                <li><a href='https://cran.r-project.org/web/packages/hms/index.html'>hms</a></li>
                                <li><a href='https://github.com/grimbough/FITfileR'>FITfileR</a></li>
                             </ul>"
                ),
            
            p("I want to bring particular attention to ", strong(em("FITfileR")), " which is a
              lovely package available from the link in the list. I could not for the life of me
              work out how to convert a .fit file into something usable in R. This package does all
              of the conversion of the .fit file into a dataframe within R which is then used for all
              subsequent operations. If you ever see this message, ", em("thank you for developing 
              this package!")),
            
            p("All of the code that underpins this Shiny app is available from Github below.")
            
        ),

        mainPanel(
            conditionalPanel(
                "output.fileUploaded == true",
                tabsetPanel(type = "tabs",
                            
                    tabPanel("Summary",
                        h1(em("File uploaded")),
                        h2(strong("Summary of .fit:")),
                        h3(strong("Total distance:")),
                        h3(textOutput("summaryDistance")),
                        h3(strong("Total Duration (hh:mm:ss):")),
                        h3(textOutput("summaryDurationHMS")),
                        h3(strong("Average Speed:")),
                        h3(textOutput("summaryAvgSpeed")),
                        h3(strong("Maximum Altitude:")),
                        h3(textOutput("summaryMaxAlt")),
                        h3(strong("Minimum Altitude:")),
                        h3(textOutput("summaryMinAlt")),
                        h3(strong("Overall Altitude change:")),
                        h3(textOutput("summaryAltChange"))
                    ),
                
                    tabPanel("Graphs",
                        h2(strong("Graphs of .fit")),
                
                        conditionalPanel(
                            "output.altValid == true",
                        h3(strong("Altitude by timestamp")),
                        plotOutput("graphAlt")
                        ),
                        
                        conditionalPanel(
                            "output.tempValid == true",
                        h3(strong("Temperature by timestamp")),
                        plotOutput("graphTemp")
                        ),
                        
                        conditionalPanel(
                            "output.cadValid == true",
                        h3(strong("Cadence by timestamp")),
                        plotOutput("graphCad")
                        ),
                        
                        conditionalPanel(
                            "output.speedValid == true",
                        h3(strong("Speed by timestamp")),
                        plotOutput("graphSpeed")
                        )
                    ),
                    
                    tabPanel("Map",
                        h2(strong("Map of route")),
                        leafletOutput(outputId = "mymap")
                    ),
                
                    tabPanel("Row level",
                        h2(strong("Individual row details of .fit")),
                
                        # Lovely little package for collapsible panel
                        bsCollapse(
                            bsCollapsePanel("Click here to expand full .fit table", "This is the full output of the fit ",
                                            "file and combines all data into one structure:",
                                            tableOutput("fullFitData"),
                                            style = "primary"
                            )
                        )
                    )
                )
            ),
        )
    )
)

# Define server functionality
server <- function(input, output) {

    #####################################################################################################      
    # Take the input file from the box and validate + load using the brilliant package FITfileR.
    #   Could not for the life of me work out how to load a .fit file. This package resolves the issue:
    #   Read in the .fit file and then load the records. Bind them as it outputs as multiple datasets.
    rawData <- eventReactive(input$file1, {
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        ext <- tools::file_ext(inFile)
        validate(need(ext == "fit", "Incorrect file type. Please upload a fit file."))
        
        bind_rows(records(readFitFile(inFile$datapath, dropUnknown = TRUE, mergeMessages = TRUE)))
    })
    
    #####################################################################################################      
    # Section to determine if a file has been loaded - for conditional display on main body.
    #   Not putting with other output code as dependent upon file input.
    output$fileUploaded <- reactive({
        return(!is.null(rawData()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
    
    # Lots more reactive conditionals for conditional panel in UI. Came to light as:
    # Pete doesn't record temperature - needs a catch to determine if data available...
        # Means nested conditional panels in the UI, but it does work, albeit with warnings for the missing data.
    output$tempValid <- reactive({
        return(!is.null(rawData()$temperature))
    })
    outputOptions(output, 'tempValid', suspendWhenHidden = FALSE)
    
    output$altValid <- reactive({
        return(!is.null(rawData()$altitude))
    })
    outputOptions(output, 'altValid', suspendWhenHidden = FALSE)
    
    output$cadValid <- reactive({
        return(!is.null(rawData()$cadence))
    })
    outputOptions(output, 'cadValid', suspendWhenHidden = FALSE)
    
    output$speedValid <- reactive({
        return(!is.null(rawData()$speed))
    })
    outputOptions(output, 'speedValid', suspendWhenHidden = FALSE)
    
    #####################################################################################################     
    # Function for consistent line graphs - can then duplicate all graphs from different inputs:
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
        }, 
        width = "auto",
        height = "auto")
    }
    
    #####################################################################################################     
# Mapping - again this is an output, however it was one of the most tricky parts, and one that I will return to.
    # Hence, it should be up the top:
    
# Fantastic basic tutorial on using Leaflet:
#   https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
    
# Really struggled to use the palette (pal) in color (addCircleMarkers). Much searching led to this answer:
#   https://github.com/rstudio/shiny/issues/858  (note the ~ is essential)
    
    # Custom palette to mark out the route to the number of position recordings. Starts at red marker and ends at black.
    
    pal <- reactive({
        colorNumeric(c("red", "blue","black"), 
                     domain = 0:max(mapdf()$position_id),
                     alpha = FALSE
        )
    })
    
    # Outputs as a map using Leaflet. Sets the centre of the map from variables.
        # Markers set from the full coordinates dataframe as declared later.
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
        if("speed" %in% colnames(rawData())){
        allSpeeds <- rawData() %>% 
            filter(!is.na(speed)) %>% 
            select(timestamp, speed)
        }
        else {return(NULL)}
    })

    allTemps <- reactive({
        if("temperature" %in% colnames(rawData())){
            allTemps <- rawData() %>% 
                filter(!is.na(temperature)) %>% 
                select(timestamp, temperature)           
        }
        else {return(NULL)}
    })
    
    allCadence <- reactive({
        if("cadence" %in% colnames(rawData())){
        allCadence <- rawData() %>% 
            filter(!is.na(cadence)) %>% 
            select(timestamp, cadence)
        }
        else {return(NULL)}
    })
    
    allAltitude <- reactive({
        if("altitude" %in% colnames(rawData())){
        allAltitude <- rawData() %>% 
            filter(!is.na(altitude)) %>% 
            select(timestamp, altitude)
        }
        else {return(NULL)}
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
                maxDistance = max(distance, na.rm = TRUE) / 1000 # for km
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
    
    summaryDurationHMS <- reactive({
        summaryDurationHMS <- rawData() %>% 
            summarise(
                sDuration = as.numeric(difftime(max(timestamp, na.rm = TRUE),
                                                min(timestamp, na.rm = TRUE),
                                                units = "secs")
                )
            )
        paste0(hms(as.numeric(summaryDurationHMS$sDuration), NULL, NULL))
    })

    summaryAvgSpeed <- reactive({
        summaryAvgSpeed <- round((as.numeric(summaryDistance()) / as.numeric(summaryDuration())) * 3600, digits = 2)
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
    
    
    #####################################################################################################      
# Outputs for summary details at the top of the page - all text.
        # use concatenate to pop in the km etc as it keeps it in the render for ease.
    output$summaryDistance <- renderText({
        c(summaryDistance(), "km")
    })
    output$summaryDuration <- renderText({
        summaryDuration()
    })
    output$summaryAvgSpeed <- renderText({
        c(summaryAvgSpeed(), "km/h")
    })
    output$summaryMaxAlt <- renderText({
        c(summaryMaxAlt(), "m")
    })
    output$summaryMinAlt <- renderText({
        c(summaryMinAlt(), "m")
    })
    output$summaryAltChange <- renderText({
        c(summaryAltChange(), "m")
    })
    
    output$summaryDurationHMS <- renderText({
        summaryDurationHMS()
    })
    
    #####################################################################################################      
# Output of graphs - make use of the function for graphing, for simplicity and consistency:

    output$graphAlt     <- renderLineGraph(allAltitude, 'timestamp', 'altitude')
    output$graphTemp    <- renderLineGraph(allTemps, 'timestamp', 'temperature')
    output$graphCad     <- renderLineGraph(allCadence, 'timestamp', 'cadence')
    output$graphSpeed   <- renderLineGraph(allSpeeds, 'timestamp', 'speed')
 
    #####################################################################################################   
# Still mulling over the best route for displaying the entire dataset - is it even necessary?!   

    
    output$fullFitData <- renderTable({
        rawData() %>% 
            mutate(timestampDTTM = format(timestamp, '%d-%m-%Y %H:%M:%S')) %>% 
            select(-timestamp) %>% 
            select(timestampDTTM, everything()) %>% 
            arrange(timestampDTTM)
    },
    striped = TRUE)
    
}
#####################################################################################################  
# Run the application 
shinyApp(ui = ui, server = server)
