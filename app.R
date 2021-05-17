library(shiny)
library(ggplot2)

# Define UI for application that draws plots
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Analysis"),

    # Sidebar with a selection windows and date range
    sidebarLayout(
        sidebarPanel(
            
            selectInput(
                inputId = "type"
                , label = strong("Metric")
                , choices = c("Outbreaks evolution", "Reproduction rate", "Case fatality rate", "Vaccination campaign")
                , selected = "Outbreaks evolution")
            
            , selectInput(
                inputId = "country"
                , label = strong("Select country")
                , choices = unique(sort(task1$location))
                , selected = "United Kingdom")
            
            , dateRangeInput(
                inputId = "date"
                , strong("Date range")
                , start = min(data$date)
                , end = "2021-05-12"
                , min = "2020-01-01"
                , max = Sys.Date())
            
            #, width = 2
        ),

        # Show a plot of the selected metric
        mainPanel(
           plotOutput("plotSelector", height = 900)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plotSelector <- renderPlot({
        
        if (input$type == "Outbreaks evolution")  {
            
            task1 %>%
                dplyr::filter(date == input$date[2]) %>%
                dplyr::arrange(desc(pct_change)) %>%
                dplyr::filter(pct_change >= 10 
                              | pct_change < -10 
                              | location == input$country) %>%
                drop_na() %>%
                ggplot(aes(x = reorder(location, pct_change)
                           , pct_change
                           , fill = factor(ifelse(location == input$country, "N", "H")))) +
                geom_col() +
                coord_flip() +
                scale_fill_manual(name = "location", values = c("grey50", "red")) +
                ylab("Percentage variation") +
                xlab("Country") +
                labs(
                    title = "Outbreaks and slumps"
                    , subtitle = "Daily variation of new cases per million (smoothed)."
                    , caption = str_glue("Only includes countries with daily variation >= 10 or <= -10.", "\n", "Daily variation on: ", {format(input$date[2], '%d %B %Y')})) +
                geom_hline(yintercept=0, linetype="dashed", color = "blue") +
                theme_bw() +
                theme(legend.position = "none")
            
        }
        
        else if (input$type == "Reproduction rate")  {
            
            task1 %>%
                dplyr::filter(location == input$country) %>%
                dplyr::filter(date >= input$date[1] & date <= input$date[2]) %>%
                ggplot(aes(date, reproduction_rate)) +
                geom_line() +
                ylab("Reproduction rate") +
                labs(
                    title = "How contagious is Covid-19?"
                    , subtitle = "Avg number of people who will contract Covid-19 from one person infected") +
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
                geom_text(data = subset(task2, case_fatality_rate > 0.05 
                                        | total_deaths_per_million > 2000 
                                        | location == input$country)
                          , aes(case_fatality_rate, total_deaths_per_million
                                , label = location)) +
                ylab("Deaths per million") +
                xlab("Case fatality rate") +
                labs(
                    title = "How deadly is Covid-19?"
                    , subtitle = "Selected country is highlighted in red."
                    , caption = str_glue("Snapshot date: ", {format(input$date[2], '%d %B %Y')})) +
                theme_bw()
            
        }
        
        else if (input$type == "Vaccination campaign") {
            
            highlight_df <- 
                task3 %>% 
                dplyr::filter(location == input$country)
            
            task3 %>%
                dplyr::filter(people_vaccinated_per_hundred <= 100 
                              | people_fully_vaccinated_per_hundred <= 100) %>%
                ggplot(aes(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)) +
                geom_point(alpha = 0.3, size=3) +
                geom_point(data=highlight_df, 
                           aes(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred), 
                           color='red',
                           size=5) +
                geom_text(data = subset(task3
                                        , (people_vaccinated_per_hundred > 60
                                        | people_fully_vaccinated_per_hundred > 60 
                                        | location == input$country)
                                        & (people_vaccinated_per_hundred <= 100 
                                           & people_fully_vaccinated_per_hundred <= 100))
                          , aes(people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred
                                , label = location)) +
                ylab("People vaccinated") +
                xlab("Fully vaccinated") +
                labs(
                    title = "How successful is the vaccine campaign?"
                    , subtitle = "Selected country is highlighted in red."
                    , caption = str_glue("Snapshot date: ", {format(input$date[2], '%d %B %Y')})) +
                theme_bw()
            
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
