# Define UI
ui <- fluidPage(
  
  theme = shinytheme("flatly")
  , titlePanel("Covid Analysis")
  , sidebarLayout(
    
    sidebarPanel(
      
      # Select type of trend to plot
      selectInput(
        
        inputId = "type"
        , label = strong("Country")
        , choices = unique(sort(task1$location))
        , selected = "United Kingdom"
        
      )
      
      # Select date range to be plotted
      , dateRangeInput(
        
        "date"
        , strong("Date range")
        , start = min(task1$date)
        , end = max(task1$date)
        , min = min(task1$date)
        , max = max(task1$date)
        
      )
      
    )
    
    # Output: Description, lineplot, and reference
    , mainPanel(
      
      plotOutput(
        
        outputId = "lineplot"
        , height = "300px"
        
      )
      
      , textOutput(outputId = "desc")
      
    )
    
  )
  
)
