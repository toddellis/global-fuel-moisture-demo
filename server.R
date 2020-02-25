#
# Australia dead fine fuel moisture content and fire history
# 1980 - 2018
# Version 2.1
#

shinyServer(
  function(input, output) {
    
    ##########################
    ##### BETA DATA PREVIEW ##
    ##########################
  #  output$test_text <- renderPrint({
  #    ### NOTE: This is a temporary test area
  #    
  #  }) 
    
    ##########################
    ##### DYNAMIC MENU #######
    ##########################
    output$ui_response_var <- renderUI({
      if (is.null(input$input_file))
        return()
      
      switch(input$input_file,
             'fmc_prop' = pickerInput('response_var',
                                      '[ response ]',
                                      c('FMC 1%' = 'thresh1',
                                        'FMC 2%' = 'thresh2',
                                        'FMC 3%' = 'thresh3',
                                        'FMC 4%' = 'thresh4',
                                        'FMC 5%' = 'thresh5',
                                        'FMC 6%' = 'thresh6',
                                        'FMC 7%' = 'thresh7',
                                        'FMC 8%' = 'thresh8',
                                        'FMC 9%' = 'thresh9',
                                        'FMC 10%' = 'thresh10',
                                        'FMC 11%' = 'thresh11',
                                        'FMC 12%' = 'thresh12',
                                        'FMC 13%' = 'thresh13',
                                        'FMC 14%' = 'thresh14',
                                        'FMC 15%' = 'thresh15',
                                        'FMC 16%' = 'thresh16',
                                        'FMC 17%' = 'thresh17',
                                        'FMC 18%' = 'thresh18',
                                        'FMC 19%' = 'thresh19',
                                        'FMC 20%' = 'thresh20',
                                        'FMC 21%' = 'thresh21',
                                        'FMC 22%' = 'thresh22',
                                        'FMC 23%' = 'thresh23',
                                        'FMC 24%' = 'thresh24',
                                        'FMC 25%' = 'thresh25',
                                        'FMC 26%' = 'thresh26',
                                        'FMC 27%' = 'thresh27',
                                        'FMC 28%' = 'thresh28',
                                        'FMC 29%' = 'thresh29',
                                        'FMC 30%' = 'thresh30',
                                        'FMC 31%' = 'thresh31',
                                        'FMC 32%' = 'thresh32'),
                                      'thresh12'),
             'fmc_rt' = pickerInput('response_var',
                                    '[ response ]',
                                    c('Med. FMC / burned area' = 'baMedian',
                                      'Med. FMC / FRP' = 'frpMedian',
                                      'Min. FMC / burned area' = 'baMin',
                                      'Min. FMC / FRP' = 'frpMin'),
                                    'baMedian'))
    })
    
    output$ui_response_year <- renderUI({
      if (is.null(input$input_file))
        return()
      
      switch(input$input_file,
             'fmc_prop' = pickerInput('response_year',
                                      '[ year ]',
                                      c(1979:2018),
                                      2018),
             'fmc_rt' = pickerInput('response_year',
                                    '[ year ]',
                                    c('Summary (NULL)' = 2020, 
                                      1979:2018),
                                    'Summary (NULL)')
      )
      
    })
    
    ##########################
    ##### REACTIVE VALUES ####
    ##########################
    # Object to store selected points in
    data_of_click <- reactiveValues(clickedMarker = list())
    
    ## Set up data ahead of plotting
    foo_climbar <- reactive({
      if ( input$input_file == 'fmc_prop') 
        fmc_prop %>%
        filter(study_area == input$study_area) %>%
        dplyr::select(-study_area)
      else fmc_rt_prop  %>%
        filter(study_area == input$study_area) %>%
        dplyr::select(lat, long, year, response_var = !! input$response_var) %>%
        filter(!is.na(response_var)) 
      
    })
    
    ## Set up data for mapping
    foo <- reactive({
      foo <- if ( input$input_file == 'fmc_prop') 
        fmc_prop
      else if (input$response_year == 2020)
        fmc_rt_thresh
      else fmc_rt_prop
      
      foo <- foo %>%
        filter(study_area == input$study_area) %>%
        dplyr::select(lat, long, year, response_var = !! input$response_var) %>%
        filter(!is.na(response_var))
      
      foo
    })
    
    ## Set up response variable for plots
    response_var <- reactive({
      foo <- if(input$response_year == 2020) 
        foo() %>% dplyr::select(-year)
      else foo()
    })
    
    ## Set up independent variable for scatterplot
    interaction_var <- reactive({
      foo <- if(input$interaction_var == 'elev')
        elev
      else if (input$interaction_var %in% c('lat_int', 'long_int'))
        latlong
      else if (input$interaction_var %in% c('frp_n', 'frp_mean', 'frp_sum', 'burned_area'))
        modis
      else if (input$interaction_var %in% c('tp', 'tpds'))
        precip
      else if (input$interaction_var %in% c('mat', 'mcmt', 'mhmt', 'mdst'))
        temp
      else if (input$interaction_var %in% c('cvh', 'cvl')) 
        veg
      else if (input$interaction_var %in% c('baMedian', 'baMin', 'frpMedian', 'frpMin'))
        fmc_rt_thresh
      else yearx
      
      foo <- if(input$interaction_var %in% c('elev', 'lat_int', 'long_int', 'cvl', 'cvh', 'baMedian', 'baMin', 'frpMedian', 'frpMin'))
        foo %>% dplyr::select(lat, long, interaction_var = !! input$interaction_var)
      else if(input$interaction_var == 'year_int')
        foo %>% dplyr::select(year, interaction_var =  year_int)
      else foo %>% dplyr::select(lat, long, year, interaction_var = !! input$interaction_var)
      
      foo
    })
    
    ## Set up color grouping variable for scatterplot
    color_var <- reactive({
      foo <- if(input$color_var == 'none')
        tibble(lat = latlong$lat,
               long = latlong$long,
               none = NA)
      else if(input$color_var == 'ecoID')
        ecoID
      else if(input$color_var %in% c('slt', 'tvh', 'tvl'))
        veg 
      
      foo <- foo %>%
        dplyr::select(lat, long, color_var = !! input$color_var)
      
      foo
    })
    
    ## Further prepare data for mapping
    foo_map <- reactive( {
      foo <- foo() %>%
        filter(year == !! input$response_year)  %>%
        #rowid_to_column(., 'locationID') %>% # does not work with promises OR postgres
        mutate(locationID = row_number(),
               secondLocationID = paste0(as.character(locationID), '_selectedLayers')) %>%
        dplyr::select(locationID, secondLocationID,
                      long, lat, year, response_var) 
      
      foo <- if (input$response_year == 2020)
        foo %>% mutate(response_var_ln = log(response_var))
      else foo
    } )
    ## Extract coordinates for creating interactive spatial objects
    coordinates <-  reactive({
      foo_map() %>%
        SpatialPointsDataFrame(.[,c('long', 'lat')], .)
    })
    
    
    
    
    ##########################
    ##### OUTPUT MAP #########
    ##########################
    
    output$fmc_map <- renderLeaflet({
      
      ##### Rasterize the smallest dataset to visualize
      ### use log-transformed thresholds for summary data
      ## this is only associated with the color theme
      foo_map_raster <- if (input$response_year == 2020)
        foo_map() %>%
        dplyr::select(long, lat, response_var = response_var_ln) %>%
        rasterFromXYZ(., crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
      else foo_map() %>%
        dplyr::select(long, lat, response_var) %>%
        rasterFromXYZ(., crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
      
      ##### Set default view windows
      foo_map_view <- switch(input$study_area,
                             'nsw' = leaflet() %>% 
                               addProviderTiles(providers$Stamen.Terrain) %>% 
                               setView(lng = 146.5, lat = -35, zoom = 4),
                             'wna' = leaflet() %>% 
                               addProviderTiles(providers$Stamen.Terrain) %>%
                               setView(lng = -117, lat = 45.5, zoom = 5))
      
      ##### Render leaflet map
      foo_map_view %>%
        addCircles(data = foo_map(),
                   radius = 1000,
                   ~long,
                   ~lat,
                   fillColor = NA,
                   fillOpacity = 0, # 0
                   color = NA, # NA
                   opacity = 0, # 0
                   weight = 2, # ~sqrt(response_var),
                   stroke = T,
                   layerId = ~as.character(locationID)) %>%#,
        ### Add rasterized input data
        addRasterImage(foo_map_raster, 
                       colors = if(input$response_year == 2020) 
                         pal_climate 
                       else 
                         rev(pal_climate),
                       opacity = 0.45) %>%
        addMiniMap() %>%
        ### Add tools to select points
        ## Note: These can't be edited too much, unfortunately
        addDrawToolbar(
          targetGroup = 'Selected',
          polylineOptions = FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,
                                                                              color = 'white',
                                                                              weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,
                                                                                color = 'white',
                                                                                weight = 3)),
          circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,
                                                                            color = 'white',
                                                                            weight = 3)),
          editOptions = editToolbarOptions(edit = TRUE, 
                                           selectedPathOptions = selectedPathOptions()))
      
      
    })
    
    ##### Concatenate selected lat-long coordinates
    observeEvent(input$fmc_map_draw_new_feature, {
      found_in_bounds <- findLocations(shape = input$fmc_map_draw_new_feature,
                                       location_coordinates = coordinates(),
                                       location_id_colname = "locationID")
      for(id in found_in_bounds){
        if(id %in% data_of_click$clickedMarker){
          # don't add id
        } else {
          # add id
          data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
        }
      }
      
      selected <- subset(foo_map(), locationID %in% data_of_click$clickedMarker)
      
      ### Draw selected points differently
      proxy <- leafletProxy("fmc_map")
      proxy %>% 
        addCircles(data = selected,
                   radius = 1000,
                   lat = selected$lat,
                   lng = selected$long,
                   fillColor = "wheat",
                   fillOpacity = 1,
                   opacity = 0.3,
                   color = "darkred",
                   weight = 3,
                   stroke = T,
                   layerId = as.character(selected$secondLocationID),
                   highlightOptions = highlightOptions(color = "hotpink",
                                                       opacity = 1.0,
                                                       weight = 2,
                                                       bringToFront = TRUE))
   
    })
    
    ##### Extract selected lat-long coordinates reactively to reuse
    selectedLocations <- reactive({
      foo_map() %>%
        filter(locationID %in% data_of_click$clickedMarker) %>%
        dplyr::select(long, lat) 
    })
    
    
    ##### Remove de-selected points from concatenation
    observeEvent(input$fmc_map_draw_deleted_features,{
      # loop through list of one or more deleted features/ polygons
      for(feature in input$fmc_map_draw_deleted_features$features){
        
        # get ids for locations within the bounding shape
        bounded_layer_ids <- findLocations(shape = feature
                                           , location_coordinates = coordinates()
                                           , location_id_colname = "secondLocationID")
        
        
        # remove second layer representing selected locations
        proxy <- leafletProxy("fmc_map")
        proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
        
        first_layer_ids <- subset(foo_map(), secondLocationID %in% bounded_layer_ids)$locationID
        
        data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                   %in% first_layer_ids]
      }
    })
    
    
    ### We need 3 datasets for graphing:
    # 1. Climate bars: 
    #   a. year
    #   b. mean(response_val)
    # 2. Scatterplot: 
    #   a. response_val
    #   b. interaction_val
    #   c. ecoID
    # 3. Line plot:
    #   a. year
    #   b. response_threshold
    #   c. mean(response_val)
    
    ##### Pull selected-only data from database
    ## Basis dataset for all plots
    selectedLocations_sql <- eventReactive(input$fmc_map_draw_new_feature, {
      ## N.B. Artifact step from dropped PostgreSQL version
      selectedLocations() %>%
        distinct()
    })
    
    ## Extract selected data for scatterplot
    selectedLocations_sql_scatter <- eventReactive(input$update_plots, {
      foo <- selectedLocations_sql() %>%
        left_join(response_var()) %>%
        left_join(interaction_var()) %>%
        left_join(color_var())
      ## Add 1 for log-scale in scatterplot
      foo <- if(input$interaction_var %in% c('frp_n', 'frp_mean', 'frp_sum', 'burned_area'))
        foo %>% 
        mutate(interaction_var = interaction_var + 1)
      else foo
      
      foo
    })
    
    ## Extract selected data for climate bars & line plot
    selectedLocations_sql_climbar <- eventReactive(input$update_plots, {
      foo <- if(input$input_file == 'fmc_prop')
        ## Gathers all 32 thresholds for line plot
        selectedLocations_sql() %>%
        left_join(foo_climbar()) %>% 
        gather(fmc_threshold,
               response_var, 
               -c(lat, long, year)) %>%
        mutate(fmc_threshold = as.factor(parse_number(fmc_threshold)))
      ## Gathers proportions from locally-defined thresholds for all years
      else selectedLocations_sql() %>%
        left_join(foo_climbar())
      
      foo
        
    })
    
    ## Create grouped means for line and bar plots
    selectedLocations_plot_mean <- eventReactive(input$update_plots, {
      
      foo <- if(input$input_file == 'fmc_prop')
        selectedLocations_sql_climbar() %>%
        group_by(fmc_threshold, year) %>%
        summarise(response_var = mean(response_var)) %>%
        ungroup() 
      else selectedLocations_sql_climbar() %>%
        group_by(year) %>%
        summarise(response_var = mean(response_var)) %>%
        ungroup() 
      foo
    })
    
    ##########################
    ##### REACTIVE PLOTS #####
    ##########################
    
    ########################
    # Plot 1: Climate bars
    ########################
    
    ## Draw bar plot
    plot_bar <- eventReactive(input$update_plots, {
      
      tmp <- if(input$input_file == 'fmc_prop')
        selectedLocations_plot_mean() %>%
        filter(fmc_threshold == (parse_number(!! input$response_var)))
      else
        selectedLocations_plot_mean()
      
      p <- tmp %>%  # plot framework
        ggplot(aes(x = year, y = 1, fill = scale(response_var))) +
        geom_tile() +
        # customizing appearance
        scale_x_continuous(breaks = c(seq(1980, 2015, by = 5)),
                           position = 'top',
                           expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        guides(fill = guide_colorbar(barwidth = 1)) + 
        labs(title = '',
             caption = '') +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.line.y = element_blank(),
              axis.title = element_blank(),
              panel.grid.major = element_blank(),
              legend.title = element_blank(),
              legend.position = 'none',
              axis.text.x = element_text(vjust = 3),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 14, face = 'bold')) +
        # color gradient direction based on the response_var
        scale_fill_gradientn(colors = rev(pal_climate))
      
      p
    })
    
    ########################
    # Plot 2: Line 
    ########################
    # Draw line plot
    plot_line <- eventReactive(input$update_plots, {
      p <- selectedLocations_plot_mean() %>%
        ggplot(aes(x = year, y = response_var)) +
        theme_minimal() +
        geom_line(alpha = 0.25) +
        geom_line(stat = 'smooth', alpha = 0.95, se = F,
                  # customize k
                  method = 'gam', formula = y ~ s(x))  +
        scale_x_continuous(breaks = seq(1980, 2015, by = 5),
                           position = 'top', 
                           expand = c(0, 0)) + 
        scale_y_continuous(expand = c(0, 0),
                           position = 'left') +
        theme(axis.line.y = element_blank(),
              axis.title = element_blank(),
              panel.grid.major = element_blank(),
              legend.title = element_blank(),
              legend.position = 'none',
              axis.text.x = element_text(vjust = 3),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 14, face = 'bold')) +
        labs(title = '',
             caption = 'Change in dry season, 1980-2018')
      
      ## Highlight threshold % of choice
      p <- if(input$input_file == 'fmc_prop') 
        p + aes(group = fmc_threshold, 
                color = as.numeric(fmc_threshold)) +
        geom_smooth(data = . %>%
                      mutate(fmc_threshold = as.numeric(fmc_threshold)) %>%
                      filter(fmc_threshold == (parse_number(!! input$response_var))),
                    color = 'grey30', alpha = 0.3, size = 1.25,
                    se = T, level = 0.9, 
                    method = 'gam', formula = y ~ s(x)) +
        scale_color_gradientn(colors = rev(pal_climate))
      else p
      
      p
    
    })
    
    ########################
    # Plot 3A: Interaction
    ########################
    ## draw scatterplot
    plot_scatter <- eventReactive(input$update_plots, {
      p <- selectedLocations_sql_scatter() %>%
        ggplot(aes(x = interaction_var, y = response_var)) +
        theme_classic() 
      p <- if(input$color_var == 'none')
        p +
        geom_hex() +
        scale_fill_gradientn(colors = rev(pal_climate)) +
        theme(legend.position = 'none')
      else p +
        geom_point(alpha = 0.10) +
        scale_color_viridis_d() +
        theme(legend.position = 'top')
      
      p <- p +
        geom_smooth(se = T, alpha = 0.25, fill = 'hotpink', color = 'darkred', 
                    method = 'gam', formula = y ~ s(x, k = 9)) +
        labs(y = switch(input$response_var,
                        'thresh1'=  'Proportion of dry season <= 1%',
                        'thresh2'=  'Proportion of dry season <= FMC 2%',
                        'thresh3'=  'Proportion of dry season <= FMC 3%',
                        'thresh4'=  'Proportion of dry season <= FMC 4%',
                        'thresh5'=  'Proportion of dry season <= FMC 5%',
                        'thresh6'=  'Proportion of dry season <= FMC 6%',
                        'thresh7'=  'Proportion of dry season <= FMC 7%',
                        'thresh8'=  'Proportion of dry season <= FMC 8%',
                        'thresh9'=  'Proportion of dry season <= FMC 9%',
                        'thresh10'= 'Proportion of dry season <= FMC 10%',
                        'thresh11'= 'Proportion of dry season <= FMC 11%',
                        'thresh12'= 'Proportion of dry season <= FMC 12%',
                        'thresh13'= 'Proportion of dry season <= FMC 13%',
                        'thresh14'= 'Proportion of dry season <= FMC 14%',
                        'thresh15'= 'Proportion of dry season <= FMC 15%',
                        'thresh16'= 'Proportion of dry season <= FMC 16%',
                        'thresh17'= 'Proportion of dry season <= FMC 17%',
                        'thresh18'= 'Proportion of dry season <= FMC 18%',
                        'thresh19'= 'Proportion of dry season <= FMC 19%',
                        'thresh20'= 'Proportion of dry season <= FMC 20%',
                        'thresh21'= 'Proportion of dry season <= FMC 21%',
                        'thresh22'= 'Proportion of dry season <= FMC 22%',
                        'thresh23'= 'Proportion of dry season <= FMC 23%',
                        'thresh24'= 'Proportion of dry season <= FMC 24%',
                        'thresh25'= 'Proportion of dry season <= FMC 25%',
                        'thresh26'= 'Proportion of dry season <= FMC 26%',
                        'thresh27'= 'Proportion of dry season <= FMC 27%',
                        'thresh28'= 'Proportion of dry season <= FMC 28%',
                        'thresh29'= 'Proportion of dry season <= FMC 29%',
                        'thresh30'= 'Proportion of dry season <= FMC 30%',
                        'thresh31'= 'Proportion of dry season <= FMC 31%',
                        'thresh32'= 'Proportion of dry season <= FMC 32%',
                        'baMedian' = ifelse(input$response_year == 2020, 
                                            'Local median FMC-fire threshold (%)',
                                            'Proportion of dry season <= local FMC threshold'),
                        'frpMedian' = ifelse(input$response_year == 2020, 
                                             'Local median FMC-fire threshold (%)',
                                             'Proportion of dry season <= local FMC threshold'),
                        'baMin' = ifelse(input$response_year == 2020, 
                                         'Local minimum FMC-fire threshold (%)',
                                         'Proportion of dry season <= local FMC threshold'),
                        'frpMin' = ifelse(input$response_year == 2020, 
                                          'Local minimum FMC-fire threshold (%)',
                                          'Proportion of dry season <= local FMC threshold')),
             x = switch(input$interaction_var,
                        'elev' = 'Elevation',
                        'baMedian' = 'Median FMC-burned area threshold (%)',
                        'baMin' = 'Minimum FMC-burned area threshold (%)',
                        'frpMedian' = 'Median FMC-FRP threshold (%)',
                        'frpMin' = 'Minimum FMC-FRP threshold (%)',
                        'lat_int' = 'Latitude',
                        'long_int' = 'Longitude',
                        'frp_n' = 'MODIS FRP (count)',
                        'frp_mean' = 'MODIS FRP (mean)',
                        'frp_sum' = 'MODIS FRP (sum)',
                        'burned_area' = 'MODIS burned area',
                        'tp' = 'Precip.: Annual',
                        'tpds' = 'Precip.: Dry season',
                        'mat' = 'Temp.: Annual',
                        'mcmt' = 'Temp.: Coldest',
                        'mdst' = 'Temp.: Dry season',
                        'mhmt' = 'Temp.: Hottest',
                        'cvh' = 'Veg. cover (high)',
                        'cvl' = 'Veg. cover (low)',
                        'year_int' = 'Year')) 
      
      p <- if(input$response_year != 2020 && input$color_var != 'none')
        p + geom_point(data = . %>%
                         filter(year == !! input$response_year),
                       shape = 21, alpha = 0.35, size = 3,
                       color = 'white', fill = 'darkred')
      else p
      
      p <- if(input$color_var != 'none')
        p + 
        aes(color = color_var) + 
        geom_smooth(data = . %>% 
                      filter(!is.na(color_var)), 
                    se = T, alpha = 0.20, fill = 'hotpink',
                    method = 'gam', formula = y ~ s(x, k = 9)) +
        labs(color = switch(input$color_var,
                            'none' = '',
                            'ecoID' = 'Ecoregion',
                            'slt' = 'Soil type',
                            'tvh' = 'Veg. type (high)',
                            'tvl' = 'Veg. type (low)'))
      else p
      
      p <- if (!! input$interaction_var %in% c('frp_sum', 'frp_mean', 'frp_n', 'burned_area')) {
        p + 
          scale_x_log10()
      } else {
        p
      }
      
      p
    })
    
    ##########################
    ##### DRAW PLOTS #########
    ##########################
    
    output$climatePlots <- renderPlot(res = 90, {
      
      ## Concatenate all 3 reactive plots
      tmp <- align_plots(plot_bar(), plot_scatter(), 
                         align = 'v', axis = 'l')
      
      plot_grid(tmp[[1]],
                plot_grid(tmp[[2]], plot_line(),
                          rel_widths = c(1, 1.2)),
                ncol = 1)
      
    })
    
    
  })