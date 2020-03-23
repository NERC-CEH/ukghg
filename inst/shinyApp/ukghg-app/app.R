## UK-GHG Interface app
## R script to make a shiny app which provides acces to the ukghg package
##
## Peter Levy, CEH Edinburgh
## Bush Estate, Penicuik, EH26 0QB, U.K.
## plevy@ceh.ac.uk
## Tel: 0131 445 8556
## September 2019

#Load required packages
library(shiny)
library(lubridate)
library(ukghg)
library(ggplot2)

ls_sectors <- 1:11
sectorNames <- c("Energy production",
 "Domestic combustion",
 "Industrial combustion",
 "Industrial processes",
 "Offshore",
 "Solvents",
 "Road transport",
 "Other transport",
 "Waste",
 "Agriculture",
 "Natural")
names(ls_sectors) <- sectorNames

# some inital values for selections
df_init <- data.frame(
  startDate = "01/01/2014 00:00",
  endDate   = "01/12/2014 00:00",
  ghgName = "co2"
)

# Format the dates as POSIXct
df_init$startDate <- as.POSIXct(df_init$startDate, format = "%d/%m/%Y %H:%M", tz = "UTC")
df_init$endDate   <- as.POSIXct(df_init$endDate, format = "%d/%m/%Y %H:%M", tz = "UTC")

# Define UI for the app
ui <- navbarPage("UK-GHG Web Interface",
                 tabPanel("Run Model",
                            mainPanel(
                              fluidRow(
                                 helpText("This app provides access to the UK-GHG R package; this allows a user to produce a time series of maps of GHG fluxes for the UK and write these to netCDF files. Variation at a range of time scales can be included.")                                
                                ),
                              fluidRow(
                                column(width = 4,
                                       uiOutput("gas_filter")),
                                column(width = 4,
                                       uiOutput("proj_filter")),
                                column(width = 4,
                                       uiOutput("res_filter")),
                                column(width = 4,
                                       uiOutput("unitType_filter")),
                                column(width = 4,
                                       uiOutput("unitSIprefix_filter")),
                                column(width = 4,
                                       uiOutput("writeNetCDF_filter")),
                                uiOutput("date_info")
                              ),
                              fluidRow(
                                column(width = 6,
                                       h3("Select Start Date and Time"))
                              ),
                              fluidRow(
                                column(width = 6,
                                       uiOutput("start_date")),
                                column(width = 3,
                                       numericInput("shour", value = 00, label = "Hour", min = 0, max = 23, step = 1)),
                                column(width = 3,
                                       numericInput("smin", value = 00, label = "Minute", min = 0, max = 59, step = 1))
                              ),
                              fluidRow(
                                column(width = 6,
                                       h3("Select End Date and Time"))
                              ),
                              fluidRow(
                                column(width = 6,
                                       uiOutput("end_date")),
                                column(width = 3,
                                       numericInput("ehour", value = 00, label = "Hour", min = 0, max = 23, step = 1)),
                                column(width = 3,
                                       numericInput("emin", value = 00, label = "Minute", min = 0, max = 59, step = 1))
                            ),
                            fluidRow(
                              sliderInput("intslider", label = "Select the number of time steps between the start and end times:", min = 1, max = 366, value = 12, step = 1)
                            ),
                             fluidRow(                             
                               column(width = 6,
                                          h3("Time scales of variation"))
                             ),
                             fluidRow( 
                              column(width = 3,
                                checkboxInput("log_year", "Inter-annual", TRUE)),
                              column(width = 3,
                                checkboxInput("log_yday", "Seasonal", TRUE)),
                              column(width = 3,
                                checkboxInput("log_wday", "Day-of-week", FALSE)),
                              column(width = 3,
                                checkboxInput("log_hour", "Diurnal", FALSE)),
                              column(width = 3,
                                checkboxInput("log_includeBio", "Include Natural Biogenic Fluxes", TRUE))
                             ),
                            fluidRow(
                              actionButton("seejobsummary", "Review Run Settings")
                            ),
                            fluidRow(
                              tableOutput("job_table")
                              #DT::dataTableOutput("job_table")
                              #helpText("Show something here with synopsis of choices for processing, e.g. a table of the options.")
                            ),
                            fluidRow(
                              actionButton("sendjobbutton", "Run Model")
                            ),
                            uiOutput("submit_info"),
                            fluidRow(
                              uiOutput("sector_filter"),
                              actionButton("plotmap", "Plot Maps"),
                              actionButton("plottime", "Plot Time Series of Total Net Flux"),
                              actionButton("plottime_bySector", "Plot Time Series by Sector"),
                              downloadButton("netcdf_total", "Download netCDF file for total emission"),
                              downloadButton("netcdf_bySector", "Download netCDF files by sector")
                            ),
                            plotOutput("mapPlot"),
                            plotOutput("timePlot")
                          )
                 ),
                 tabPanel("Documentation",
                          p("UK-GHG is a spatio-temporal model of greenhouse gas fluxes from the UK."),
                          uiOutput("git_link"),
                          p("Other documenation available:"),
                          a("R package manual",target="_blank",href="ukghg.pdf"),
                          p("and slides from a presentation from Sept 2017:"),
                          a("Presentation",target="_blank",href="CMO_ukghg.pdf")
                 ),
                 tabPanel("Theory",
                          includeHTML("./www/ukghg_timeDisaggn.html")
                          
                 )

)

# Define server logic required for the app
server <- function(input, output) {
  #Create select input UI element with gas options
  output$sector_filter <- renderUI({
    selectInput("select_sector", label = h3("Select Sector"), choices = ls_sectors )
  })
    
  #Create select input UI element with gas options
  output$gas_filter <- renderUI({
    selectInput("select_gas", label = h3("Select Gas"), choices = as.list(c("ch4", "co2", "n2o")), selected = df_init$ghgName )
  })
    
  #Create select input UI element with gas options
  output$proj_filter <- renderUI({
    selectInput("select_proj", label = h3("Select Projection"), choices = as.list(c("OSGB", "LonLat - command line only")) )
  })
        
  #Create select input UI element with gas options
  output$res_filter <- renderUI({
    selectInput("select_res", label = h3("Select Resolution"), 
    choices = list(`OSGB (km)` = as.list(c("1", "20", "100")), 
                 `LonLat (deg)` = as.list(c("0.1"))), selected = "100" )
  })
      
  #Create select input UI element with gas options
  output$unitType_filter <- renderUI({
    selectInput("select_unitType", label = h3("Select Unit Molar or Mass Type"), choices = as.list(c("mol", "g")) )
  })
        
  #Create select input UI element with gas options
  output$unitSIprefix_filter <- renderUI({
    selectInput("select_unitSIprefix", label = h3("Select Unit SI Prefix"), 
        choices = as.list(c("kilo", "none", "milli", "micro", "nano", "pico")), selected = "micro" )
  })
          
  #Create select input UI element with gas options
  output$writeNetCDF_filter <- renderUI({
    selectInput("select_writeNetCDF", label = h3("Select NetCDF file output"), 
        choices = as.list(c(TRUE, FALSE)), selected = FALSE )
  })
  
  #Create a sentence of metadata about the site, station and gas selection made.
  output$date_info <- renderUI ({
    helpText(paste("You have selected gas ", 
    as.character(input$select_gas),
    "in units of ", as.character(input$select_unitSIprefix), 
    as.character(input$select_unitType), "/m2/s.
     Select your required start and end times below."))
  })
  
  #Create a reactive element with the earliest start date
  first_start_date <- reactive({
    min(as.Date(df_init$startDate))
  })
  
  #Create a reactive element with the latest end date
  last_end_date <- reactive({
    max(as.Date(df_init$endDate))
  })
  
  #Create a date input for the user to select start date
  output$start_date <- renderUI ({
    dateInput("sdate", value = first_start_date(), min = first_start_date(), max = last_end_date(), label = "Date")
  })
  
  #Create a date input for the user to select end date
  output$end_date <- renderUI ({
    dateInput("edate", value = last_end_date(), min = first_start_date(), max = last_end_date(), label = "Date")
  })
  
  # check boxes for time scales
  output$log_year <- renderText({ input$log_year })
  output$log_yday <- renderText({ input$log_yday })
  output$log_wday <- renderText({ input$log_wday })
  output$log_hour <- renderText({ input$log_hour })
  output$log_includeBio <- renderText({ input$log_includeBio })
  
  #Create a dataframe with all the information about the job
  # the dataframe is only created (or recreated) when the View Job Summary button is pressed
  job_df <- eventReactive(input$seejobsummary, {
    startDate <- paste(sprintf("%02d", day(input$sdate)), "/", sprintf("%02d", month(input$sdate)), "/", year(input$sdate), " ", sprintf("%02d", input$shour), ":", sprintf("%02d", input$smin), sep = "")    
    startDate <- as.POSIXct(strptime(startDate, "%d/%m/%Y %H:%M"), tz = "UTC")
    endDate   <- paste(sprintf("%02d", day(input$edate)), "/", sprintf("%02d", month(input$edate)), "/", year(input$edate), " ", sprintf("%02d", input$ehour), ":", sprintf("%02d", input$emin), sep = "")
    endDate   <- as.POSIXct(strptime(endDate, "%d/%m/%Y %H:%M"), tz = "UTC")
    nTimes <- input$intslider
    # create a sequence of timestamps
    datect <- seq(startDate, endDate, length = nTimes)

    data.frame(datech = as.character(datect),
               ghgName = input$select_gas, 
               proj = input$select_proj, 
               res = input$select_res, 
               unitType = input$select_unitType, 
               unitSIprefix = input$select_unitSIprefix, 
               writeNetCDF = input$select_writeNetCDF,
               log_year = input$log_year,
               log_yday = input$log_yday,
               log_wday = input$log_wday,
               log_hour = input$log_hour,
               log_includeBio = input$log_includeBio,
               datect = datect)
  })
  
  #Render the job info dataframe as a table
  output$job_table <- renderTable({
    job_df()
  })
  
  #When the submit processing job action button is pressed start the procedure by reading the job request file from dropbox
  observeEvent(input$sendjobbutton, {
    # calculate fluxes for these times
    #startDate <- as.POSIXct(strptime("01/06/2006", "%d/%m/%Y"), tz = "UTC")
    #endDate   <- as.POSIXct(strptime("02/06/2006", "%d/%m/%Y"), tz = "UTC")
    #nTimes <- 3
    # create a sequence of timestamps
    #datect <- seq(startDate, endDate, length = nTimes)
    #myFlux <- calcFlux("co2", output$datect, proj = "OSGB", 
    #                 res = "100", 
    #                 unitType = "mol",
    #                 unitSIprefix = "micro")
    myFlux <<- calcFlux(input$select_gas, job_df()$datect, proj = input$select_proj, 
      res = input$select_res, input$select_unitType, input$select_unitSIprefix, 
      includeBio = input$log_includeBio, 
      timeScales = c(input$log_year, input$log_yday, input$log_wday, input$log_hour),
      writeNetCDF = input$select_writeNetCDF)
    #plot(myFlux$ls_ghgBySectorByTime[[1]])
    #animate(myFlux$ls_ghgBySectorByTime[[1]])
  })
  
  #Set the link to the CBED page on EIDC
  git_url <- a("ukghg GitHub repository", href = "https://github.com/NERC-CEH/ukghg")
  
  #Create an output element for the url
  output$git_link <- renderUI({
    tagList("ukghg pages on GitHub:", git_url)
  })
   
  #When the submit job action button is pressed display a message which states how long the job might take and information on where to access the results.
  observeEvent(input$sendjobbutton, {
    output$submit_info <- renderUI ({
      helpText(paste("Your model run has completed. Plot and download the output via the options below."))
    })
  })
  
  # Plot results as maps
  observeEvent(input$plotmap, {
    output$mapPlot<-renderPlot(
      {
         names(myFlux$ls_ghgBySectorByTime[[as.numeric(input$select_sector)]]) <- job_df()$datect      
         plot(myFlux$ls_ghgBySectorByTime[[as.numeric(input$select_sector)]])
      })
    }) 
  
  # Plot results as time series
  observeEvent(input$plottime, {
    output$timePlot<-renderPlot(
      {
        ylab_text <- paste("Total Net Flux, Tg", input$select_gas, "/y")
        plot(job_df()$datect, myFlux$total, type = "b", xlab = "Date", ylab = ylab_text)
      })
    })
  
  # Plot results as time series
  observeEvent(input$plottime_bySector, {
    output$timePlot<-renderPlot(
      {
        v_F <- cellStats(myFlux$ls_ghgBySectorByTime[[as.numeric(input$select_sector)]], mean) # units are ug X m-2 s-1
        df <- data.frame(datect = job_df()$datect, Flux = v_F)
        p <- ggplot(df, aes(datect, Flux))
        p <- p + geom_line()
        p <- p + geom_point()
        p <- p + xlab("Date")
        ylab_text <- paste("Mean flux,", input$select_unitSIprefix, input$select_unitType, input$select_gas, "m2/s")
        p <- p + ylab(ylab_text)
        p <- p + ggtitle("Time series of mean flux")
        p
      })
    })

  # download netCDF file for total flux 
  output$netcdf_total <- downloadHandler(
    filename = function() {
      paste('ukghg-', Sys.Date(), '.nc', sep='')
    },
    content = function(file) {
    writeRaster(myFlux$s_ghgTotal, file)
    }
  )
  # download netCDF files by sector
  output$netcdf_bySector <- downloadHandler(
    filename = function() {
      paste('ukghg-', Sys.Date(), '.nc', sep='')
    },
    content = function(file) {
    writeRaster(myFlux$ls_ghgBySectorByTime[[as.numeric(input$select_sector)]], file)
    }
  )
}


# Run the application using local file links
# runApp()

# Run the application 
shinyApp(ui = ui, server = server)
