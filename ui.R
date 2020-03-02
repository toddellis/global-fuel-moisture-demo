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
      title = 'Global dead fine fuel moisture content, 1980-2018, v. 2.1',
      tabPanel("[ data explorer ]"),
      # Test data preview
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
               ## Select dataset: 
               ### a) 1-32 FMC threshold proportions
               ### b) locally-defined FMC thresholds & proportions
               pickerInput('input_file',
                           '[ input file ]',
                           c('FMC proportions (1-32%)' = 'fmc_prop',
                             'FMC proportions (local)' = 'fmc_rt_prop',
                             'FMC thresholds' = 'fmc_rt_thresh'),
                           'fmc_rt_thresh'),
               ## Select a study area
               pickerInput('study_area',
                           '[ study region ]',
                           c('SE Australia' = 'nsw',
                             'W North America' = 'wna'),
                           'nsw'),
               ## Dynamic response variable selector
               uiOutput('ui_response_var'),
               ## Dynamic response year selector
               uiOutput('ui_response_year'),
               hr(),
               ## Button to update figures
               actionButton('update_plots', 
                            'Generate figures from selection'),
               br(),br(),
               ## Button to remove selection
               ## TODO: Fix.
               #actionButton('reset_selection',
               #             'Reset selection'),
               ## Select an interaction variable for scatterplot
               pickerInput('interaction_var',
                           '[ scatterplot interaction variable ]',
                           c('Elevation' = 'elev',
                             'FMC threshold: Med. burned area' = 'baMedian',
                             'FMC threshold: Med. FRP' = 'frpMedian',
                             'FMC threshold: Min. burned area' = 'baMin',
                             'FMC threshold: Min. FRP' = 'frpMin',
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
               ## Select a color grouping variable for scatterplot
               pickerInput('color_var',
                           '[ scatterplot color variable ]',
                           c('None (hexplot)' = 'none',
                             'Ecoregion' = 'ecoID',
                             'Soil type' = 'slt',
                             'Veg. type (high)' = 'tvh',
                             'Veg. type (low)' = 'tvl'),
                           'none')
               
        )
        
      ),
      ## Section for figure outputs
      fluidRow(
        column(12,
               hr(),
               plotOutput('climatePlots', height = 720)
        )
      )
      
    )
  )
)
