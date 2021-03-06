# # # # ####################################################################################
# # # # ####################################################################################
# # # # # # author: Gabriel Ernesto García-Peña
# # # # # # beta version @
# # # # # # https://gegp01.shinyapps.io/conectividad_municipios
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(png)

options(shiny.maxRequestSize = 30*1024^2) # ALLOW HANDLING LARGE FILES
X<-readRDS("NODOS.rds")

ui <- fluidPage(theme=shinytheme("spacelab")
                , titlePanel("", windowTitle="C3 covid-19")
                , align="center"
                , tags$style('.container-fluid {
                             background-color:transparent; font-size:small;}')

            , sidebarLayout(
               sidebarPanel(width=5
                , selectInput("ENTIDAD", HTML("<b>Selecciona una Entidad</b>"), choices = NULL, width="100%")
                , selectInput("MUNICIPIO", HTML("<b>Selecciona un Municipio</b>"), choices = NULL, width="100%")
                , plotOutput("semaforo", width="200px", height="200px")
                )

             ,  mainPanel(width=6
                , htmlOutput("text.semaforo1")
                            )
                      )
              )
