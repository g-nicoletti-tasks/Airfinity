library(shiny)

# Define UI for application that draws plots
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Analysis"),

    # Sidebar with a selection windows and date range
    sidebarLayout(
        sidebarPanel(
            
            selectInput(
                inputId = "country"
                , label = strong("Select country")
                , choices = unique(sort(task1$location))
                , selected = "United Kingdom")
            
            , dateRangeInput(
                inputId = "date"
                , strong("Date range")
                , start = min(data$date)
                , end = max(data$date)
                , min = "2020-01-01"
                , max = Sys.Date())
            
            , selectInput(
                inputId = "type"
                , label = strong("Metric")
                , choices = c("Reproduction rate", "Case fatality rate", "Vaccination campaign")
                , selected = "United Kingdom")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plotSelector")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plotSelector <- renderPlot({
        
        if (input$type == "Reproduction rate")  {
            
            task1 %>%
                dplyr::filter(location == input$country) %>%
                dplyr::filter(date >= input$date[1] & date <= input$date[2]) %>%
                ggplot(aes(date, reproduction_rate)) +
                geom_line() +
                ylab("Reproduction rate") +
                labs(
                    title = "How contagious is Covid-19?"
                    , subtitle = "Avg no. of people who will contract Covid-19 from one person infected") +
                geom_hline(yintercept=1, linetype="dashed", color = "red") +
                theme_bw()
            
        }
        
        else if (input$type == "Case fatality rate") {
            
            highlight_df <- 
                task2 %>% 
                dplyr::filter(location == input$country)
            
            task2 %>%
                ggplot(aes(case_fatality_rate, total_deaths_per_million)) +
                geom_point(alpha = 0.3, size=3) +
                geom_point(data=highlight_df, 
                           aes(case_fatality_rate, total_deaths_per_million), 
                           color='red',
                           size=5) +
                ylab("Deaths per million") +
                xlab("Case fatality rate") +
                labs(title = "How deadly is Covid-19?") +
                theme_bw()
            
        }
        
        else if (input$type == "Vaccination campaign") {
            
            highlight_df <- 
                task3 %>% 
                dplyr::filter(location == input$country)
            
            task3 %>%
                ggplot(aes(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)) +
                geom_point(alpha = 0.3, size=3) +
                geom_point(data=highlight_df, 
                           aes(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred), 
                           color='red',
                           size=5) +
                ylab("People vaccinated") +
                xlab("Fully vaccinated") +
                labs(title = "How successful is the vaccine campaign?") +
                theme_bw()
            
        }
        
        #else {hist(-x)}
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
