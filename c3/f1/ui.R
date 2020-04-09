library(shiny)
ui <- fluidPage(
    titlePanel("EPIDEMIA DE COVID-19"),
        mainPanel(
           plotOutput("log_plot", width="100vh"
                      , height = "700"
                      )
        )
    )
