# # ####################################################################################
# # # # author: Gabriel Ernesto García-Peña
# # # # beta version @ https://gegp01.shinyapps.io/conectividad_municipios
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
                , selectInput("origen1", HTML("<h4>Selecciona un municipio para saber como avanza la epidemia</h4>"), choices = NULL, width="430px")
                
                , plotOutput("semaforo", width="290px", height="290px") #idth=1000/72, height=1000/72)
                , htmlOutput("text.semaforo1")
)
