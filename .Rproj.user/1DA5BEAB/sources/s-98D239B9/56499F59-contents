# Define UI for Operation Healthy Air Dashboard
    # Header Items ------------------------------------------------------------
header <- dashboardHeader(
  title = tags$b(
    tags$style("font-color = #ffffff; font-family = Arial;"))
)

    # Sidebar Items -----------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Sensors", tabName = "sensors",
             icon = icon("th")),
    menuItem(text = "Sensor Health", tabName = "sensor_health",
             icon = icon("heartbeat")),
    menuItem(text = "Quality Assurance & Control", tabName = "cleaning",
             icon = icon("broom")),
    menuItem(text = "Calibrate", tabName = "calibration",
             icon = icon("check-square")),
    menuItem(text = "LifeTime", tabName = "lifetime",
             icon = icon("calendar-alt", lib = "font-awesome")),
    menuItem(text = "Weekly Averages", tabName = "weekly",
             icon = icon("calendar-week", class = "solid", lib = "font-awesome")),
    menuItem(text = "Wind", tabName = "wind",
             icon = icon("wind", lib = "font-awesome")),
    menuItem(text = "Data Download", tabName = "download",
             icon = icon("download", lib = "font-awesome")),
    menuItem(text = "Citations", tabName = "citations",
             icon = icon("book-reader", lib = "font-awesome"))
    ),
  radioButtons(inputId = "aqi_country", 
               label = "AQI Scale",
               choices = aqi_country_opts,
               inline = TRUE)
    
  )
    # Body --------------------------------------------------------------------
body <- dashboardBody(
  tags$link(rel = "stylesheet",
            type = "text/css",
            href = "style/styles.css"),
  tabItems(
    # SENSORS --------------------------------------------------------------
    tabItem(tabName = "sensors", 
            fluidRow(
              # TODO: Make Box wider.
              box(title = "Get Data",
                  # Label Input
                  textInput(
                    inputId = "sensor_label",
                    label = "Purple Air Sensor Labels",
                    value = "Sensor Label"
                  ),
                  # Load Sensor Timespan
                  dateRangeInput(
                    label = "Time Frame",
                    inputId = "sensor_timespan", 
                    # Default Start is Not Working
                    start = (as.Date(Sys.time()) - 7), 
                    min = "2000-01-01",
                    max = (as.Date(Sys.time())),
                    autoclose = TRUE
                  ),
                    # Load Button
                  actionButton(inputId = "sensor_load", 
                               label = "Load Sensor"),
                  collapsible = TRUE, 
                  footer = "max. 5 sensors loaded at a time."
                 ),
              box(title = "Loaded Sensors",
                  tableOutput("sensorStatusTable"),
                  br(),
                  actionButton(inputId = "clear_loaded",
                               label = "Clear Loaded Sensors"),
                  actionButton(inputId = "reset_sensors",
                               label = "Reset Sensor Raw")
                 )
              ),
            # Map
            fluidRow(
              leafletOutput(outputId = "sensor_worldMap")
              )
            ),
    # HEALTH -------------------------------------------------------------- 
    tabItem(tabName = "sensor_health",
            fluidPage(
              box(title = "Sensor",
                  width = 3,
                  selectizeInput(
                    "sensorHealthSelect", 
                    label = "Sensor",  
                    choices = "No Sensors Loaded",
                    multiple = FALSE)
                  ),
              tabBox(
                width = 8,
                tabPanel(
                  title = "Sensor Health Statistics",
                  formattable::formattableOutput("sensorHealthTable") %>%
                    withSpinner()
                  ),
                # TODO: Format Table.
                tabPanel(title = "Table Key",
                         tableOutput("sensorHealthTableKey")
                         )
                )
              )
            ),
  
    # CLEANING -----------------------------------------------------
    tabItem(tabName = "cleaning",
            fluidPage(
              box(collapsible = TRUE, 
                  selectizeInput(inputId = "sensorQASelect",
                                 label = "Sensor",  
                                 choices = "No Sensors Loaded",
                                 multiple = FALSE
                                 )
                  ),
              box(
                title = "Clean Data",
                collapsible = TRUE,
                p(
                  paste(
                    "Operation Healthy Air Assures Data Quality",
                    "using three processes:",
                    sep = " "
                    )
                ),
                tags$ol(
                  tags$li("Checking Data Validity"),
                  tags$li("Detecting and Replacing Likely Outliers"),
                  tags$li("Aggregating to Hourly Data.") 
                ),
                p(
                  paste(
                    "In this section you will can visualize how these",
                    "processes affect your data and apply them to your",
                    "sensors.", sep = " "
                    )
                  ),
                br(),
                h6(
                   paste(
                     "Pressing this button will clean all",
                     "of your currently loaded sensors",
                     "data using the inputs you've currently set below.",
                     "To get original raw data, you will have to reload",
                     "them in the Sensors tab.", sep = " "
                     )
                    ),
                 actionButton(inputId = "QA",
                              label = "Clean Data")
                )
            ),
            box(
              # Make box full width of screen.
              width = 12,
              tabsetPanel(
                tabPanel(title = "Data Validation",
                         br(),
                         selectizeInput(inputId = "validParameter", 
                                        label = "Measurement", 
                                        choices = list(
                                          "PM25 Channel A" = "pm25_A",
                                          "PM25 Channel B" = "pm25_B",
                                          "Both Channels" = "pm25_both",
                                          "Temperature" = "temperature",
                                          "Relative Humidity" = "humidity"
                                          )
                                        ),
                         p(
                           paste(
                             "Checking Data Validity:", "\n",
                             "The easiest data to clean, is data that you know is",
                             "impossible, and so can be invalidated. The best",
                             "example for our purposes is a Relative Humidity",
                             "measurement of 500%. Though our sensor might",
                             "report this measurement, we know it's impossible.",
                             "Purple Air supplies the specifications of their",
                             "sensor components, and so we know that the",
                             "equipment cannot detect certain values, though",
                             "we'll sometimes see them in our data.", 
                             "Purple Air Sensor Valid Measurement Ranges are",
                             "as follows:", sep = " "
                           )
                         ),
                         tags$ol(
                           tags$li("Particulate Matter 2.5: 0-1000 µg/m³"),
                           tags$li("Temperature: -40-185 °F"),
                           tags$li("Relative Humidity: 0-100%")
                         ),
                         plotOutput(outputId = "validPlot") %>% withSpinner()
                         ),
                
                tabPanel(
                  title = "Outlier Detection Settings",
                  column(3,
                    p(
                      paste(
                        "Detecting and Replacing Likely Outliers:", "\n",
                        "Operation Healthy Air detects and removes outliers",
                        "using the Hampel filter. The Hampel filter takes a",
                        "time series, such as Purple Air Sensor data and",
                        "runs a rolling window over the timeseries. Operation",
                        "Healthy Air uses a 23 observation window. Within the",
                        "23 observation window, the median is found, and the",
                        "standard deviation is calculated. All window",
                        "observations further than a specified number of",
                        "standard deviations, known here as the Threshold",
                        "Minimum are marked as outliers. Operation Healthy",
                        "Air then replaces outliers with the window median.",
                        "The process is repeated as the window 'rolls'",
                        "through the dataset.",
                        sep = " "
                        )
                      )
                    ),
                  column(8,
                    h2("Hampel Filter Toggles"),
                    numericInput(inputId = "HampelwindowSize",
                                 label = "Window Size",
                                 value = 23,
                                 min = 0,
                                 max = 100,
                                 step = 1),
                    numericInput(inputId = "HampelthresholdMin",
                                 label = "Standard Deviations",
                                 value = 8,
                                 min = 0,
                                 max = 20),
                    checkboxInput(inputId = "HampelimputeBoolean",
                                  label = "Replace Outliers",
                                  value = FALSE)
                    ), 
                 # TODO: Make gganimation of Hampel Filter Tagging Values
                 plotOutput(outputId = "outlierPlot") %>% withSpinner()
                 ),
                
                tabPanel(
                  title = "Aggregate Data",
                  br(),
                  p(
                    paste(
                      "Now we turn our dataset with two channels,",
                      "of Particulate Matter measured every",
                      "120-second measurements and aggregate the",
                      "values for each hour of data. We do this by",
                      "taking the average between the A and B", 
                      "channels, and then averaging the measurements",
                      "for every hour. Finally we apply a control",
                      "algorithm to the data. For the detail",
                      "oriented the algorithm criteria are:",
                      sep = " "
                      )
                    ),
                  tags$ol(
                    tags$li(
                      paste(
                        "Each hour must have at least 20",
                        "measurements from each channel.",
                        sep = " ")
                      ),
                    tags$li(
                      paste(
                        "The p-value of a two sample T-test between",
                        "channels A and B must be below 0.00001,",
                        "and the difference between the channels'",
                        "hourly averages is less than or equal to",
                        "10.", sep = " "
                        )
                    ),
                    tags$li(
                      paste(
                        "The difference between the channels'",
                        "hourly averages is less than or equal to",
                        "20, and the average of the two channels",
                        "is greater than or equal to 100."
                        )
                      )
                    ),
                  br(),
                  checkboxInput(
                    inputId = "QCalg",
                    label = "Apply Quality Control Algorithm"
                    ),
                  plotOutput(outputId = "QCalgPlot")
                  )
                )
              )
            ),
    
    # CALIBRATION -----------------------------------------------------------
    tabItem(tabName = "calibration",
            fluidRow(
              box(title = "Calibration",
                  collapsible = TRUE,
                  selectInput(inputId = "calibration_mod_select",
                              label = NULL,
                              choices = calibration_opts),
                  br(),
                  leafletOutput(outputId = "calibration_map") %>% withSpinner()
                  )
            ),
            fluidRow(
              box(
                # Make box full width of screen.
                width = 12,
                title = "Visualize Calibration",
                selectizeInput("calibration_sensor_select", 
                               label = "Sensor",  
                               choices = "No Sensors Loaded",
                               multiple = FALSE
                               ),
                actionButton(inputId = "calibrate",
                             label = "Calibrate Sensor"),
                plotOutput(outputId = "calibrationPlot") %>% withSpinner()
                )
              )
            ),
    # LIFETIME ------------------------------------------------------------
    tabItem(tabName = "lifetime",
            fluidRow(
              box(
                title = "Sensor",
                collapsible = TRUE,
                selectizeInput("lifetimeSelect", 
                               label = "Sensor",  
                               choices = "No Sensors Loaded",
                               multiple = FALSE
                                 ),
                ),
            ),
            fluidRow(
              box(
                  title = "AQI Calendar",
                  width = 6,
                  plotOutput(outputId = "calendarPlot") %>% withSpinner(),
                  numericInput(inputId = "calendar_minThresh",
                               label = "Minimum Measured Hours",
                               value = 20,
                               min = 0, max = 24, step = 1)
                  ),
                box(
                  title = "Hourly Measurements",
                  width = 6,
                  plotOutput(outputId = "hourlyavg_aqiColorPlot") %>%
                    withSpinner()
                  )
                )
            ),
    
    # WEEKLY --------------------------------------------------------------
    tabItem(tabName = "weekly",
            fluidPage(
              box(
                title = "Sensor",
                selectizeInput("weeklySelect", 
                               label = "Sensor",  
                               choices = "No Sensors Loaded",
                               multiple = FALSE
                ),
                plotOutput(outputId = "hourly_aqiStackedBarPlot") %>%
                  withSpinner()
              ),
              box(
                title = "Workweek vs. Weekend",
                checkboxGroupInput(inputId = "wrkwk_wknd_select",
                                   label = "Sensors",
                                   choices = "No Sensors Loaded"),
                plotOutput(outputId = "workweek_weekendPlot") %>%
                  withSpinner()
              )
            )),
    # WIND ----------------------------------------------------------------
    tabItem(
      tabName = "wind",
      fluidPage(
        box(
          title = "Sensor",
          selectizeInput("windSelect", 
                         label = "Sensor",  
                         choices = "No Sensors Loaded",
                         multiple = FALSE),
          selectizeInput("noaa_select",
                         label = "NOAA Site",
                         choices = "No Sensors Loaded",
                         multiple = FALSE),
          leafletOutput(outputId = "windMap")
          ),
        box(
          checkboxInput(inputId = "windNormalize",
                        label = "Normalize",
                        value = FALSE),
          uiOutput(outputId = "pollutionRose") %>% withSpinner(),
          )
        )
      ),
    # DOWNLOAD ----------------------------------------------------------------
    tabItem(
      tabName = "download",
      h1("Export your Dataset:"),
      selectizeInput("sensor_dwnld_select", 
                     label = "Sensor",  
                     choices = "No Sensors Loaded",
                     multiple = TRUE),
      inputPanel(
        # TODO: Add include wind data option.
        # TODO: Add include reference monitor data.
        # TODO: Add more flexible time scaling options.
        checkboxGroupInput("column_select",
                      label = "Include Columns",
                      choices = column_opts)
        ),
      tabsetPanel(
        tabPanel(title = "Individual Sensor Page",
                 selectInput("sensor_dwnld_select", 
                                label = "Sensor",  
                                choices = "No Sensors Loaded",
                                multiple = FALSE),
                 dataTableOutput("dwnld_sensor_sheets")),
        tabPanel(title = "Side-by-Side",
                 dataTableOutput("dwnld_pm_joined")),
        tabPanel(title = "MetaData",
                 dataTableOutput("dwnld_meta"))
      ),
      downloadButton(outputId = "dwnld")
      
      ),
    # CITATIONS ---------------------------------------------------------------
    tabItem(tabName = "citations",
            fluidPage(
              h2("Thank You!"),
              img(src = "images/UPS_Shield_S_19Dec16_RGB.png")
              )
            )
    )
  )

    
# Build -----------------------------------------------------------------------
dashboardPage(header = header,
              sidebar = sidebar,
              body = body)