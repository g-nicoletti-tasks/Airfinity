library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 tracker")
  , dashboardSidebar()
  , dashboardBody(
    box(plotOutput("R_rate_plot"), width = 8)
    , box(selectInput("location", "Country:",
                      unique(sort(task1$location))), width = 4)
  )
)

server <- function(input, output) {
  output$R_rate_plot <- renderPlot(
    plot(task1 %>%
           dplyr::filter(location == task1[[input$location]]) %>%
           ggplot(aes(date, reproduction_rate)) +
           geom_line() +
           ylab("Reproduction rate") +
           labs(
             title = "How contagious is Covid-19?"
             , subtitle = "Avg no. of people who will contract Covid-19 from one person infected") +
           geom_hline(yintercept=1, linetype="dashed", color = "red") +
           theme_bw()
    )
  )
}

shinyApp(ui, server)
