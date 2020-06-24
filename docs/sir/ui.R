library(shiny)
library(shinythemes)
#library(shinyjs)
library(deSolve)

options(shiny.maxRequestSize = 30*1024^2) # ALLOW HANDLING LARGE FILES

ui <- fluidPage(theme=shinytheme("spacelab")
                , titlePanel(HTML("<h4>Modelo compartimentalizado</h4>"), windowTitle="covid-19")
                , plotOutput("modelo")
                , HTML("<br><br>")
                , fluidRow(
                    column(width=3, numericInput("beta", "beta:", 0.03, min = 0, max = 1))
                    , column(width=3, numericInput("gamma", "gamma:", 0.01, min = 0, max = 1))
                    , column(width=3, numericInput("delta", "delta:", 0.03, min = 0, max = 1))
                    )
                , fluidRow(
                    column(width=3, numericInput("c", "c:", 0.03, min = 0, max = 1))
                    , column(width=3, numericInput("letalidad", "letalidad:", 0.0025, min = 0, max = 1))
                    )
                , HTML("<br><p align='left'><a  href='mailto:gegp@ciencias.unam.mx' style='font-size:80%;'>Contact: gegp@ciencias.unam.mx</a></p>")
)
