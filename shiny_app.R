library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(
  
  theme = shinytheme("lumen")
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

# Define server function
server <- function(input, output) {
  
  # Subset data
  r_rate <- 
    reactive({
      req(input$date)
      validate(
        
        need(
          
        !is.na(input$date[1]) & !is.na(input$date[2])
        , "Error: Please provide both a start and an end date.")
        
        )
      
      validate(
        
        need(
          
          input$date[1] < input$date[2]
          , "Error: Start date should be earlier than end date."
          
          )
        
        )
      
      task1 %>%
        filter(
          
          location == input$type
          , date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2])
          
          )
      })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- 
    renderPlot({
      color = "#434343"
      par(mar = c(4, 4, 1, 1))
      
      plot(
        
        x = r_rate()$date
        , y = r_rate()$reproduction_rate
        , type = "l"
        , xlab = "Date"
        , ylab = "Reproduction rate"
        , col = color
        , fg = color
        , col.lab = color
        , col.axis = color
        
        )
    # # Display only if smoother is checked
    # if(input$smoother){
    #   smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
    #   lines(smooth_curve, col = "#E6553A", lwd = 3)
    # }
      
      })
  
  # # Pull in description of trend
  # output$desc <- renderText({
  #   trend_text <- filter(trend_description, type == input$type) %>% pull(text)
  #   paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  # })
  }

# Create Shiny object
shinyApp(ui = ui, server = server)
