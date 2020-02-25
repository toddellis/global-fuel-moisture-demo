#
# Australia dead fine fuel moisture content and fire history
# 1980 - 2018
# Version 2.1
#

shinyUI(
  tagList(
    navbarPage(
      # Set the theme
      theme = shinytheme('superhero'),
      # Set the title
      title = 'Global dead fine fuel moisture content, 1980-2018, v. 2.0',
      tabPanel("[ data explorer ]"),
      # Temporary data preview
      #fluidRow(
      #  verbatimTextOutput('test_text'),
      #),
      # Create row with map and control options
      fluidRow(
        # Interactive map element
        column(8,
               leafletOutput('fmc_map')
        ),
        column(4,
               # Sidebar with a slider input for number of bins 
               #  sidebarPanel(
               pickerInput('study_area',
                           '[ study area ]',
                           c('SE Australia' = 'nsw',
                             'W North America' = 'wna'),
                           'nsw'),
               pickerInput('input_file',
                           '[ data type ]',
                           c('FMC proportions' = 'fmc_prop',
                             'FMC thresholds' = 'fmc_rt'),
                           'fmc_rt'),
               uiOutput('ui_response_var'),
               uiOutput('ui_response_year'),
               pickerInput('interaction_var',
                           '[ plot interaction ]',
                           c('Elevation' = 'elev',
                             'Latitude' = 'lat_int',
                             'Longitude' = 'long_int',
                             'MODIS FRP (count)' = 'frp_n',
                             'MODIS FRP (mean)' = 'frp_mean',
                             'MODIS FRP (sum)' = 'frp_sum',
                             'MODIS burned area' = 'burned_area',
                             'Precip.: Annual' = 'tp',
                             'Precip.: Dry season' = 'tpds',
                             'Temp.: Annual' = 'mat',
                             'Temp.: Coldest' = 'mcmt',
                             'Temp.: Dry season' = 'mdst',
                             'Temp.: Hottest' = 'mhmt',
                             'Veg. cover (high)' = 'cvh',
                             'Veg. cover (low)' = 'cvl',
                             'Year' = 'year_int'),
                           'tpds'),
               pickerInput('color_var',
                           '[ plot color ]',
                           c('None' = 'none',
                             'Ecoregion' = 'ecoID',
                             'Soil type' = 'slt',
                             'Veg. type (high)' = 'tvh',
                             'Veg. type (low)' = 'tvl'),
                           'none'),
               actionButton('update_plots', 
                            'Update figures')
               
        )
        
      ),
      # Climate bars
      fluidRow(
        column(12,
               hr(),
               plotOutput('climatePlots', height = 720)
        )
      ),
      column(12,
             
             
             plotOutput("distPlot")
      )
      
    )
  )
)
