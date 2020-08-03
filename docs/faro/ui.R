# # # ####################################################################################
# # # # # author: Gabriel Ernesto García-Peña
# # # # # beta version @
# # # # # https://gegp01.shinyapps.io/conectividad_municipios
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(visNetwork)
library(epicontacts)
library(adegenet)
library(png)

options(shiny.maxRequestSize = 30*1024^2) # ALLOW HANDLING LARGE FILES

ui <- fluidPage(theme=shinytheme("superhero")
                , titlePanel("", windowTitle="C3 covid-19")
                , align="center"
                , tags$style('.container-fluid {
                             background-color:transparent;}')
                , HTML("<h3>¿Te expones al COVID 19 cuando viajas?</h3>")
                , HTML("Selecciona el municipio a dónde vas:")
                , selectInput("ENTIDAD", HTML("<h4>Entidad</h4>"), choices = NULL, width="430px")
                , selectInput("MUNICIPIO", HTML("<h4>Municipio</h4>"), choices = NULL, width="430px")
                , textOutput('origen1')
                , plotOutput("semaforo", width="290px", height="290px")
                , htmlOutput("text.semaforo1")

)
