server <- function(
  input,
  output,
  session
) {
  
  # Define Main Data Reactive Values Object
  data <- reactiveValues(
    pat_list_raw = NULL,
    pat_list = NULL,
    sensor_list = NULL,
    ingestion_meta_df = data.frame(Label = character(),
                                   Start = character(), 
                                   End = character(),
                                   QAQC = character(),
                                   Calibration = character(),
                                   stringsAsFactors = FALSE))
  
  # AQI Country
  aqi_country <- reactive({
    coded <- input$aqi_country %>% 
      countrycode::countrycode(origin = "country.name",
                               destination = "iso2c")
    return(coded)
  })
  # SENSORS TAB ----------------------------------------------------------------
  # Tracking Sensor Status
  
  # ALL SENSOR MAP
  output$sensor_worldMap <- 
    renderLeaflet({
      
      # Filter B Channels from PAS
      pas_filter(pas,
                 !stringr::str_detect(label, " B$") ) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(
          group = "sensors",
          lng = ~longitude,
          lat = ~latitude, 
          radius = 3,
          color = "purple",
          label = ~htmlEscape(label),
          # Add Clustering.
          # TODO: Change cluster colors so they don't look like AQI colors.
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = TRUE,
            removeOutsideVisibleBounds = TRUE,
            spiderLegPolylineOptions = list(
            weight = 3),
            disableClusteringAtZoom = 17
          )) %>% 
        addResetMapButton() %>% 
        # TODO: Search Feature Currently Slow, likely due to number of sensors. Try running again later.
        addSearchFeatures(targetGroups = "sensors",
                          searchFeaturesOptions(
                            propertyName = "label", casesensitive = FALSE, tooltipLimit = 3,
                            zoom=17, minLength = 3, moveToLocation = TRUE,
                            autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) 
    })
  
  # IMPORT NEW
  observeEvent(input$sensor_load, {
    
    if ( length(data$pat_list) == sensor_limit ) {
      # Modal Warning About Sensor Max
      showModal(
        modalDialog(
          title = "Sensor Max Reached",
          "To Load More Sensors You Must Remove a Currently Loaded Sensor")
      )
    } else if ( !(input$sensor_label %in% pas$label) ) {
      # Confirm label is valid.
      showModal(modalDialog(
        title = "Error",
        "Cannot find a sensor with the given name.",
        "Please check map for available sensors and spelling.",
        size = "m",
        easyClose = TRUE,
        fade = FALSE
      ))
      
    } else {
      
      # Load Pat.
      show_modal_spinner()
      
      pat <- pat_createNew(
        pas = pas,
        label = input$sensor_label,
        startdate = input[["sensor_timespan"]][[1]],
        enddate = input[["sensor_timespan"]][[2]]
      )
      
      data[["pat_list_raw"]][[input$sensor_label]] <- pat
      
      data[['pat_list']][[input$sensor_label]] <- pat
      
      # Update Sensor List
      sensor <- list()
      
      parameters <- c("pm25", "temperature", "humidity")
      
      for (i in 1:length(parameters)) {
        
        sensor[[parameters[[i]]]] <- pat_createAirSensor(
          pat = pat,
          parameter = parameters[[i]],
          qc_algorithm = "hourly_AB_00",
          min_count = 20,
          period = "1 hour")
      }  
      
      data[["sensor_list"]][[input$sensor_label]] <- sensor
      
      # Keep Track of Loaded Sensor Ingestion Parameters
      if ( any(data$ingestion_meta_df$Label == pat$meta$label) ) {
        
        # If sensor already loaded, replace row.
        data$ingestion_meta_df <- data$ingestion_meta_df %>%
            filter(Label != pat$meta$label) %>% 
            tibble::add_row(Label = pat$meta$label,
                            Start = format(input[["sensor_timespan"]][[1]],
                                           "%d-%b-%Y"),
                            End = format(input[["sensor_timespan"]][[2]],
                                         "%d-%b-%Y"),
                            QAQC = "None",
                            Calibration = "None")
        
      } else {
        # Else add row.
        data$ingestion_meta_df <- data$ingestion_meta_df %>% 
            tibble::add_row(Label = pat$meta$label,
                            Start = format(input[["sensor_timespan"]][[1]],
                                           "%d-%b-%Y"),
                            End = format(input[["sensor_timespan"]][[2]],
                                         "%d-%b-%Y"),
                            QAQC = "None",
                            Calibration = "None")
        
      }
      
      remove_modal_spinner()
      
    }
    
  }, priority = 10)
  
  # LOADED SENSORS
  
  # Record Sensor Ingestion Metadata for Sensor Loaded Table
  output$sensorStatusTable <- renderTable({

    req(data$pat_list,
        data$ingestion_meta_df)

    return(data$ingestion_meta_df)

  }, auto = TRUE, colnames = TRUE)

  
  # RESET BUTTON
  observeEvent(input$clear_loaded, {
    
    data$pat_list_raw <- NULL
    data$pat_list <- NULL
    data$sensor_list <- NULL
    data$ingestion_meta_df <- data.frame(
      Label = character(),
      Start = character(), 
      End = character(),
      QAQC = character(),
      Calibration = character(),
      stringsAsFactors = FALSE)
    
  })
  
  observeEvent(input$reset_loaded, {
    
    data$pat_list <- data$pat_list_raw
    
    # Update MetaData 
    data$ingestion_meta_df <- data$ingestion_meta_df %>%
      mutate(
        QAQC = rep("None", times = length(data$pat_list)), 
        Calibration = rep("None", times = length(data$pat_list))
      )
  })
  
  # HEALTH TAB ---------------------------------------------------------------- 
  
  ### Sensor Health Formattable ###
  ## Sensor Weekly Statistics
  # TODO: Add stacked bar to display daily missing values counts.
  # TODO: A lot of this may be redundant with other parts of the app. Reduce Redundancy to speed up.
  
  # Define the functions we want pat_dailySoH to run on our sensors.
  SoH_functions = c(
    "PurpleAirSoH_dailyPctReporting",
    "PurpleAirSoH_dailyPctValid",
    "PurpleAirSoH_dailyABFit"
    )
  
  # Columns to Use
  # TODO: Make this interactive?
  table_cols <- c(
    "sensor",
    "pct_usableHours_A",
    "pct_usableHours_B",
    "pm25_A_pctReporting",
    "pm25_B_pctReporting",
    "pm25_A_pctValid",
    "pm25_B_pctValid",
    "pm25_A_pm25_B_rsquared"
    )
  
  # Column Names to display
  table_colnames <-  c(
    "Sensor",
    "Pct. Usable A Hours",
    "Pct. Usable B Hours",
    "PM25 A Pct. Reporting",
    "PM25 B Pct. Reporting",
    "PM25 A Pct. Valid",
    "PM25 B Pct. Valid",
    "Channel AB R2"
  )
  
  # Update Sensor Selection
  observeEvent(input$sensor_load, {
    updateSelectizeInput(session, 
                         "sensorHealthSelect",
                         choices = names(data$pat_list))
    }, priority = 0)
  
  # Prepare Sensor Health Data
  pat_SoH <- eventReactive(input$sensorHealthSelect, {
    
    req(input$sensorHealthSelect,
        data$pat_list)
      
      SoH_pat <- data[["pat_list"]][[input$sensorHealthSelect]]
     
      # Get pat Sensor Label
      label <- SoH_pat %>%
        pat_extractMeta() %>%       
        select(label)
        
      # Rename label column to sensor for external ease.
      colnames(label) <- "sensor"
      
      # Calculate hours with at least 20 observations for A and for B.
      usableHours_summary <- SoH_pat %>%
        # Aggregate to hourly.
        pat_aggregate() %>%
        # Flag hours as usable or not for each channel.
        mutate(
          usable_A = ifelse(pm25_A_count >= 20, TRUE, FALSE),
          usable_B = ifelse(pm25_B_count >= 20, TRUE, FALSE)
        ) %>%
        summarize(
          start = min(datetime),
          end = max(datetime),
          range = (difftime(end, start, units = "hours") %>%
            as.numeric() %>%
            ceiling() + 1),
          usableHours_A = sum(usable_A),
          usableHours_B = sum(usable_B),
          # TODO: Using N here doesn't represent the percent of possible hours. 
          pct_usableHours_A = (sum(usable_A) / range) * 100,
          pct_usableHours_B = (sum(usable_B) / range) * 100,
        )
      
      # Generate daily State of Health df.
      pat_SoH <- SoH_pat %>%
        pat_dailySoH(SoH_functions = SoH_functions) %>%
        select(-c("pm25_A_pm25_B_slope", "pm25_A_pm25_B_intercept")) %>%
        summarize_if(is.numeric, mean) %>%
        # Bind to usable hours and label.
        bind_cols(usableHours_summary, label)
       
      return(pat_SoH)
  }, ignoreNULL = TRUE)

  # Render Table
  output$sensorHealthTable <- formattable::renderFormattable({
      
    req(input$sensorHealthSelect,
        pat_SoH)
      
      pat_SoH() %>%
          select(all_of(table_cols)) %>%
          #TODO: Have this be the bar that fills up. 
          mutate_at(vars(-sensor),
                    .funs = function(x) formattable::color_bar("#d685ff",
                                                               na.rm = TRUE)(x)) %>%
          formattable::formattable(booktabs = T,
                                   digits = 3,
                                   col.names = table_colnames)
  })
  
  # Table Key
  output$sensorHealthTableKey <- renderTable({
  
    col_descriptions <- c(
      "The user-created sensor label.",
      paste0(
        "The percentage of hours with at least 20 PM measurements by Channel ",
        "A out of a possible ", pat_SoH()$range, "."),
      paste0(
        "The percentage of hours with at least 20 PM measurements by Channel ",
        "B out of a possible ", pat_SoH()$range, "."),
      paste0(
        "The percentage of daily Particulate Matter 2.5 readings collected by ",
        "Channel A out of the total possible daily opportunities for data ",
        "collection. (Sensors channels generate readings every 120 seconds.)"),
      paste0(
        "The percentage of daily Particulate Matter 2.5 readings collected by ",
        "Channel B out of the total possible daily opportunities for data ",
        "collection. (Sensors channels generate readings every 120 seconds.)"),
      paste0(
        "The percent of Channel A PM2.5 readings that are within the sensor's ",
        "detection range (0 to 1000 ug/m3)."),
      paste0(
        "The percent of Channel B PM2.5 readings that are within the sensor's ",
        "detection range (0 to 1000 ug/m3)."),
      paste0("Essentially the correlation between channels A and B. Ranges from ",
      "0 being no predictable relationship between A and B and 1 being perfect ",
      "synchronicity between the channels.")
    )
    
    table_key <- data.frame(
      "Column Names" = table_colnames,
      "Description" = col_descriptions
    )
    
    return(table_key)
  
  })
  
  # QAQC TAB ------------------------------------------------------------------
  
  # Update Sensor Select Input
  observeEvent(input$sensor_load, {
    updateSelectizeInput(session, 
                         "sensorQASelect",
                         choices = names(data$pat_list))
  }, priority = 0)
  
  # Valid Measurement Plot
  output$validPlot <- renderPlot({
    
    req(input$sensorQASelect,
        data$pat_list)
    
    qa_pat <- data[["pat_list"]][[input$sensorQASelect]]
    
    plot <- pat_ValidationPlot(pat = qa_pat, param = input$validParameter)
    
    return(plot)
  })
  
  # Outlier Detection Plot
  output$outlierPlot <- renderPlot({
    
    req(input$sensorQASelect)
    
    qa_pat <- data[["pat_list"]][[input$sensorQASelect]]
    # TODO: Profile. Would like to be more responsive to inputs.
    plot <- pat_outlierPlot(pat = qa_pat, windowSize = input$HampelwindowSize, 
                            thresholdMin = input$HampelthresholdMin, 
                            replace = input$HampelimputeBoolean)
    return(plot)
  })
  
  # QC Algorithm Plot
  output$QCalgPlot <- renderPlot({
    
    req(input$sensorQASelect)
    
    qa_pat <- data[["pat_list"]][[input$sensorQASelect]]
    
    if( input$QCalg ) {
      alg <- "PurpleAirQC_hourly_AB_01"
    } else {
      alg <- "PurpleAirQC_hourly_AB_00"
    }
    
    data <- qa_pat %>%
      pat_aggregate() %>% 
      purrr::exec(.fn = alg, aggregationStats = .)
    # TODO: Add fade in fade out animation for when input$alg changes.
    plot <- data %>% 
      ggplot(aes(x = datetime, y = pm25)) +
      geom_point(alpha = 0.5, size = 5)
      
    return(plot)
  })
  
  # Press Clean Data Button
  observeEvent(input$QA, {
    
    # TODO: Add Text Box explaining process.
    req(input$sensorQASelect)
    
    show_modal_spinner()
    
    pat_list <- data$pat_list
    
    # TODO: Modularize this process and apply to one sensor at a time.
    pat_list_qcd <- pat_list %>% 
      # 1. Apply pat_qc to remove invalid readings.  
      # TODO: Add max_humidity as input.
      purrr::map(pat_qc, max_humidity = NULL) %>% 
      # 2. Count and replace outliers in dataset.
      # 3. Aggregate data to hourly.
      # TODO: Visualize Hampel Filter.
      purrr::map(pat_outliers,
                 windowSize = input$HampelwindowSize,
                 thresholdMin = input$HampelthresholdMin,
                 replace = input$HampelimputeBoolean,
                 showPlot = FALSE)
    
    data$pat_list <- pat_list_qcd
    
    if ( input$QCalg ) {
      alg <- "hourly_AB_01"
    } else {
      alg <- "hourly_AB_00"
    }
    
    # One element for every pat.
    for (i in 1:length(sensor_list)) {
      
      data[["sensor_list"]][[i]][["pm25"]] <- data[["pat_list"]][[i]] %>% 
        pat_createAirSensor(qc_algorithm = alg)
      
    }
    
    # Add to data.
    data$sensor_list <- sensor_list
    
    
    QAQC_summary <- paste(
      "windowSize", input$HampelwindowSize,
      "thresholdMin", input$HampelthresholdMin,
      "replace", input$HampelimputeBoolean,
      sep = ": "
    )
    # Update MetaData 
    data$ingestion_meta_df <- ingestion_meta_df %>%
        mutate(
          QAQC = rep(QAQC_summary, times = length(data$pat_list)) 
        )
    
    remove_modal_spinner()

  })
  
  # CALIBRATION TAB -----------------------------------------------------------
  
  # Load Selected Model
  calibration <-  reactive({
    proxy_site4file <- input$calibration_mod_select %>% 
      tolower() %>%
      stringr::str_replace_all(pattern = " ",
                               replacement = "-") %>%  
      stringr::str_c(".rds", sep = "")
  
      mod_path <- paste("data", "calibration-models",
                        proxy_site4file, sep = "/")
  
      mod <- readRDS(mod_path)
      
      return(mod)
  })
  
  # Update Sensor Selection
  observeEvent(input$sensor_load, {
    
    updateSelectizeInput(session, 
                         "calibration_sensor_select",
                         choices = names(data$pat_list))
  }, priority = 0)
  
  # Reset Sensor Selection

  # Calibration Map
  output$calibration_map <- renderLeaflet({
    
    req(
      calibration
      )
    
    ref_meta <- calibration()$meta
    
    ref_label <- paste(
      "Monitor Name:", str_to_title(ref_meta$location), "<br>",
      "Source Type:", str_to_title(ref_meta$sourceType), "<br>",
      "Source Name:", str_to_title(ref_meta$sourceName), "<br>"
    )
    
    # Calibration Map
    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = ref_meta, 
                 icon = list(
                   iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
                   iconSize = c(75, 75)
                   ),
                 lat  = ~latitude, lng = ~longitude,
                 label = HTML(ref_label))
    
    
    if ( !is.null(data$pat_list) ) { 
      
      pa_meta <- data$pat_list %>% 
        purrr::map(.f = pat_extractMeta) %>% 
        bind_rows(.id = "label")
      
      map <- map %>% 
      addCircleMarkers(data = pa_meta, color = "purple",
                       lat = ~latitude, lng = ~longitude, 
                       label = ~htmlEscape(label))
    }
    
    return(map)
    
  })
  
  # Calibration Visualization
  output$calibrationPlot <- renderPlot({
    
    req(input$calibration_sensor_select,
        data$pat_list,
        calibration)
    
    ref_meta <- calibration()$meta
    
    model <- calibration()$model
    
    pat_data <- data[["pat_list"]][[input$calibration_sensor_select]] %>%
      pat_extractData()
    
    # To calibrate a PAT, must calibrate each channel individually.
     
    pat_data[["pred_A"]] <- pat_data %>% 
      select(pm25_pa = pm25_A, temperature, humidity) %>% 
      modelr::add_predictions(model = model) %>% 
      pull(pred)
    
    pat_data[["pred_B"]] <- pat_data %>% 
      select(pm25_pa = pm25_B, temperature, humidity) %>% 
      modelr::add_predictions(model = model) %>% 
      pull(pred)
    
    # Get better contrasting colors
    to_swap <- ggthemr::swatch()[c(1, 9)]
    
    plot <- pat_data %>% 
      select(datetime, pm25_A, pm25_B, pred_A, pred_B) %>%  
      tidyr::pivot_longer(-datetime) %>% 
      mutate(
        channel = if_else(str_detect(name, "_A$"), "A", "B"),
        calibrated = str_remove(name, "_[AB]$"),
        calibrated = factor(
          calibrated,
          levels = c("pred",
                     "pm25"),
          labels = c("Calibrated",
                     "Uncalibrated")
        )
      ) %>% 
      ggplot(aes(x = datetime,
                 y = value,
                 color = calibrated)) +
      geom_line(position = "jitter",
                alpha = 0.5, size = 1.2) + 
      scale_color_manual(values = to_swap) +
      labs(
        color = "Calibrated",
        y = "µg/m3",
        title = "Calibration",
        caption = paste("FRM:", ref_meta)
      ) +
      facet_grid(channel~.) +
      theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_blank()
      ) +
      guides(
        color = guide_legend(override.aes = list(size = 5))
      )
    
    return(plot)
    
  })
  
  # Calibration Button
  observeEvent(input$calibrate, {
    
    req(input$calibration_mod_select,
        input$calibration_sensor_select)
    
    show_modal_spinner()
    
    pat <- data[["pat_list"]][[input$calibration_sensor_select]]
      
  # To calibrate a PAT, must calibrate each channel individually.
    calib_data <- pat %>%
      pat_extractData() 
    
    calib_data[["pm25_A"]] <- calib_data %>% 
      select(pm25_pa = pm25_A, temperature, humidity) %>% 
      modelr::add_predictions(model = calibration()$model) %>% 
      pull(pred)
    
    calib_data[["pm25_B"]] <- calib_data %>% 
      select(pm25_pa = pm25_B, temperature, humidity) %>% 
      modelr::add_predictions(model = calibration()$model) %>% 
      pull(pred)
    
    pat[["data"]] <- calib_data
    data[["pat_list"]][[input$calibration_sensor_select]] <- pat
    data[["sensor_list"]][[input$calibration_sensor_select]] <- pat_createAirSensor(pat = pat)
    
    calib_summary <- paste(
      "Proxy Calibration", "\n", 
      "Reference Site", input$calibration_mod_select,
      "Equation", model_equation(calibration()$model) 
    )
    
    # Update Metadata
    data$ingestion_meta_df <- data$ingestion_meta_df %>% 
      mutate(
        Calibration = replace(Calibration,
                              Label == input$calibration_mod_select,
                              calib_summary)
        )
    
    remove_modal_spinner()  
  })

  # LIFETIME TAB --------------------------------------------------------------
  # Update Sensor Selection
  observeEvent(input$sensor_load, {

    updateSelectizeInput(session,
                         "lifetimeSelect",
                         choices = names(data$pat_list))
  }, priority = 0)

  output$calendarPlot <- renderPlot({

    req(data$sensor_list,
        input$lifetimeSelect)

    aqi_info <- load_aqi_info(country = aqi_country())

    sensor_meta <- data[["sensor_list"]][[input$lifetimeSelect]][["pm25"]] %>%
      sensor_extractMeta()

    sensor_data <- data[["sensor_list"]][[input$lifetimeSelect]][["pm25"]] %>%
      sensor_extractData() %>%
      rename(date = 1, pm25 = 2)

    plot <- openair::calendarPlot(sensor_data,
                                  pollutant = "pm25",
                                  main = "Daily Average Particulate Matter 2.5",
                                  xlab = sensor_meta$label,
                                  ylab = "Particulate Matter 2.5",
                                  cols = aqi_info$colors,
                                  labels = aqi_info$names,
                                  breaks = aqi_info$breaks_24,
                                  data.thresh = input$calendar_minThresh)
    return(plot)
  })

  output$hourlyavg_aqiColorPlot <- renderPlot({
    # TODO: Make Month and Year Selectable
    req(data$sensor_list,
        input$lifetimeSelect)

    sensor <- data[["sensor_list"]][[input$lifetimeSelect]][["pm25"]]

    plot <- sensor_hourlyavg_aqicolorsPlot(
      sensor = sensor,
      aqi_country = aqi_country(),
      time_facet = "month",
      point_size = 1.5,
      point_alpha = 0.9
      )

    return(plot)
  })
  
  # WEEKLY TAB ----------------------------------------------------------------
  observeEvent(input$sensor_load, {

    updateSelectizeInput(session,
                         "weeklySelect",
                         choices = names(data$sensor_list))
    
    updateCheckboxGroupInput(session,
                             "wrkwk_wknd_select",
                             choices = names(data$sensor_list))
  }, priority = 0)

  output$workweek_weekendPlot <- renderPlot({

    req(data$sensor_list,
        input$wrkwk_wknd_select,
        aqi_country())

    sensor_list <- map(
      data[["sensor_list"]][[input$wrkwk_wknd_select]], "pm25"
      )

    plot <- workweek_weeknd_pmPlot(sensor_list = pm_data,
                                    aqi_country = aqi_country())
    return(plot)
  })

  output$hourly_aqiStackedBarPlot <- renderPlot({
    # TODO: Make day of week selectable or checkable.
    req(data$sensor_list,
        input$weeklySelect,
        aqi_country())

    sensor <- data[["sensor_list"]][[input$weeklySelect]][["pm25"]]

    plot <- day_of_week_aqiBar(
      sensor = sensor,
      aqi_country = aqi_country(),
      position = "fill")

    return(plot)
  })

  # WIND TAB ------------------------------------------------------------------
  observeEvent(input$sensor_load, {

    updateSelectizeInput(session,
                         "windSelect",
                         choices = names(data$pat_list))
  }, priority = 0)


  sensor <- reactive({

    req(data$sensor_list,
        input$windSelect)

    sensor <- data[["sensor_list"]][[input$windSelect]][["pm25"]]

    return(sensor)
  })

  noaa_meta <- reactive({

    req(sensor())
    
    # Extracting Sensor Label
    sensor_meta <- sensor()$meta
    # Getting NOAA Data for nearest sensor.
    noaa_meta <- worldmet::getMeta(lat = sensor_meta$latitude,
                                   lon = sensor_meta$longitude,
                                   plot = FALSE,
                                   n = 3)
    
    return(noaa_meta)
  })

  observe({

    req(noaa_meta())

    updateSelectizeInput(session,
                         inputId = "noaa_select",
                         choices = noaa_meta()$STATION)
  })

  wind_data <- reactive({

    req(
      data$sensor_list,
      sensor(),
      noaa_meta(),
      input$noaa_select
      )

    # Get NOAA Data
    sensor_start <- lubridate::year(min(sensor()$data$datetime))
    sensor_end <- lubridate::year(max(sensor()$data$datetime))

    # Define import timeframe.
    if ( sensor_start < sensor_end ) {
      years <- seq.int(from = sensor_start,
                       to = sensor_end,
                       by = 1)
    } else {
      years <- sensor_start
    }

    noaa_site <- noaa_meta() %>%
      filter(STATION == input$noaa_select)

    # Load NOAA site data for the timeframe.
    noaa_data <- worldmet::importNOAA(code = noaa_site$code,
                                      hourly = TRUE,
                                      precip = FALSE,
                                      PWC = FALSE,
                                      year = years)

    wind <- noaa_data %>%
      dplyr::select(date, wd, ws) %>%
      filter(!is.na(wd))

    # TODO: Confirm NOAA data loaded successfully.
    View(wind)
    return(wind)
  })

  output$windMap <- renderLeaflet({
    
    req(input$windSelect,
        sensor())
    
    noaa_label <- paste("Start:", noaa_meta()$BEGIN, "<br>",
                        "End:", noaa_meta()$END, "<br>",
                        "Elevation (m):", noaa_meta()[["ELEV(M)"]],
                        sep = " ")

    leaflet() %>%
      addTiles() %>%
      addMarkers(data = noaa_meta(),
                 icon = list(
                   iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
                   iconSize = c(75, 75)
                 ),
                 lat  = ~latitude, lng = ~longitude,
                 label = HTML(noaa_label)) %>%
      addCircleMarkers(data = sensor()$meta, color = "purple",
                       lat = ~latitude, lng = ~longitude,
                       label = ~htmlEscape(monitorID))

  })

  output$pollutionRose <- renderUI({

    # Check if wind_data loaded.
    if ( nrow(wind_data()) > 1 ) {

      # Getting aqi data
      aqi_info <- load_aqi_info(country = aqi_country())


      sensor_data <- sensor() %>%
        sensor_extractData() %>%
        rename(date = datetime, pm25 = 2)

      pollution_data <- dplyr::left_join(x = filter(wind_data(),
                                                    date >= min(sensor_data$date),
                                                    date <= max(sensor_data$date)),
                                         y = sensor_data,
                                         by = "date")
      output$pollutionRosePlot <- renderPlot({

        plot <- openair::pollutionRose(
          mydata = pollution_data,
          pollutant = "pm25",
          statistic = "prop.mean",
          normalise = input$windNormalize,
          cols = aqi_info$colors,
          breaks = aqi_info$breaks_24,
          type = "default"
          )

        return(plot)

      })
      
      plotOutput(outputId = "pollutionRosePlot")

    } else {
      
      showModal(modalDialog("Unable to load NOAA Wind Data.",
                            easyClose = TRUE))
    }

  })

  # DOWNLOAD TAB --------------------------------------------------------------
  
  # Excel Workbook has three pages:
  # 1. Metadata
  dwnld_meta <- reactive({
    # TODO: Include way to track changes made through QAQC and Calibration process.
    req(data$sensor_list,
        input$param_select)
    
    sensor_meta <- data[["sensor_list"]][[input$sensor_dwnld_select]] %>% 
      purrr::map("pm25") %>% 
      purrr::map(sensor_extractMeta) %>% 
      bind_rows(.id = "sensor") %>% 
      select(meta_columns)
    
    return(sensor_meta)
    
  })
  
  # 2. Individual Sensor Pages
  sensor_sheets <- reactive({
    
    req(data$sensor_list,
        input$sensor_dwnld_select,
        input$column_opts,
        aqi_country())
    
    aqi_info <- load_aqi_info(country = aqi_country())
    
    sensor_list <- data[["sensor_list"]][[input$sensor_dwnld_select]]
    
    sensor_sheets <- list()
    
    for (i in 1:length(sensor_list)) {
      
      label <- names(sensor_list)[[i]]
      
      sensor_data <- sensor_list[[i]] %>%
        purrr::map(sensor_extractData) %>% 
        purrr::reduce(full_join, by = "datetime") %>% 
        `colnames<-`(c("datetime", names(sensor_list[[i]]))) %>% 
        dplyr::mutate_if(is.numeric, round, digits = 2) %>% 
        mutate(
          date = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"),
          time = format(as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S"),
          weekday = weekdays(datetime),
          # TODO: Add TEMP option
          # Fahrenheit to Celsius
          temperature = (temperature - 32) * 5/9,
          aqi_category = cut(pm25,
                             breaks = aqi_info$breaks_24,
                             labels = aqi_info$names)
        ) %>%              
        select(column_opts)
      
      sensor_sheets[[label]] <- sensor_data
    }
    
      return(sensor_sheet)
    
  })
  
  # 3. Particulate Matter Joined
  pm_joined <- reactive({ 
    
    sensor_list <- data[["sensor_list"]][[input$sensor_dwnld_select]]
    
    # Create Sensor Objects for each Parameter
    pm_data <- sensor_list %>%
      purrr::map("pm25") %>% 
      purrr::map(.f = function(x) dplyr::rename(x$data, pm25 = 2))
    
    pm_joined <- pm_data %>% 
    purrr::reduce(full_join, by = "datetime") %>% 
    `colnames<-`(c("datetime", names(sensor_list))) %>% 
    dplyr::mutate_if(is.numeric, round, digits = 2)
  
    return(pm_joined)
  })
  
  # Display Tables
  output$dwnld_meta <- renderDataTable({
    return(dwnld_meta())
  })
  
  output$dwnld_sensor_sheets <- renderDataTable({
    
    sensor_sheets()[[input$sheet2show]]
    
    return()
  })
  
  output$dwnld_pm_joined <- renderDataTable({
    
  })
  
  # Download Button
  observeEvent(input$dwnld, {
    
  
    
    
  })
}