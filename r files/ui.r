ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Soccer Analytics"),
    sidebarPanel(
      tabsetPanel(id='tabs',
        tabPanel("Transform",
                 fileInput("gameFile", "Game File", 
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                 fileInput("eventFile", "Event File", 
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                 sliderInput("loadTime", "Time", min=100, max=6300000, step=1000, value=c(100, 300000)),
                 actionButton("loadGame", "Transform"),
                 downloadButton("game", "Download")
                 ),
        tabPanel("Load",
                 fileInput("transformedFile", "Transformed File"),
                 actionButton("loadTransformedGame", "Load")
                 ),
        tabPanel("View",
                 sliderInput("timestamp", "Frame", min=1, max=1, step=1, value=1),
                 
                 actionButton("prevButton", "<<<"),
                 actionButton("nextButton", ">>>")
                 
                 ),
        tabPanel("CNM",
                 checkboxInput("sortedCheckbox", "Sorted", value = TRUE),
                 downloadButton("degreeCentrality", "Degree Centrality"),
                 downloadButton("betweennessCentrality", "Betweenness Centrality"),
                 downloadButton("closenessCentrality", "Closeness Centrality"),
                 downloadButton("efficiency", "Efficiency"),
                 fileInput("cnmFile", "CNM File"),
                 selectInput("selectedFilter", "Filter", c('None', 'Edge Detection', 'Opening', 'Closing',
                                                           'Erode', 'Dilate', 'Mean filter')),
                 actionButton("cnmLoad", "Load"),
                 downloadButton("filteredMeasures", "Download Filtered Measures")
                 ),
        tabPanel("Feature",
                 sliderInput("sizeTime", "Number of Frames", min=1, max=35, step=1, value=1),
                 checkboxInput("ballPossCheckbox", "Ball Possession", value = TRUE),
                 checkboxInput("attackingCheckbox", "Attacking", value = TRUE),
                 downloadButton("featureButton", "Extract Features")
                 
                 )
      )
    ),
    
    mainPanel(width=6,
      conditionalPanel(condition="input.tabs == 'Transform'",
                       
                       textOutput("transform_info")
      ),
      conditionalPanel(condition="input.tabs == 'View'",
                       textOutput("currentFrame"),
                       plotOutput("gameTS")
      ),
      conditionalPanel(condition="input.tabs == 'CNM' or input.tabs == 'Feature'",
                       plotOutput("plotGVR_TA", click="teamA_click"),
                       plotOutput("plotGVR_TB", click='teamB_click')
      
      )
      
      
    )
    
  )
  
)
