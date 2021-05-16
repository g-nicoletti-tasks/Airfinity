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
        
        # x = r_rate()$date
        # , y = r_rate()$reproduction_rate
        # , type = "l"
        # , xlab = "Date"
        # , ylab = "Reproduction rate"
        # , col = color
        # , fg = color
        # , col.lab = color
        # , col.axis = color
        # 
        
        task1 %>%
          dplyr::filter(location == input$type) %>%
          #dplyr::filter(date == input$date) %>%
          ggplot(aes(date, reproduction_rate)) +
          geom_line() +
          ylab("Reproduction rate") +
          labs(
            title = "How contagious is Covid-19?"
            , subtitle = "Avg no. of people who will contract Covid-19 from one person infected") +
          geom_hline(yintercept=1, linetype="dashed", color = "red") +
          theme_bw()
        
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
