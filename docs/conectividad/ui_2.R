# # author: Gabriel Ernesto García-Peña
# # beta version @
# # https://gegp01.shinyapps.io/conectividad_municipios
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(visNetwork)
library(epicontacts)
library(adegenet)
library(png)

options(shiny.maxRequestSize = 30*1024^2) # ALLOW HANDLING LARGE FILES

ui <- fluidPage(theme=shinytheme("spacelab")
                , titlePanel("", windowTitle="C3 covid-19")
                , align="center"
                , tags$style('.container-fluid {
                             background-color:#FFFFFF;}')  #f7f9f9
                , htmlOutput("text3")
                , selectInput("origen1","Selecciona un municipio para saber como avanza la epidemia", choices = NULL, width="430px")
                , htmlOutput("text.semaforo1")
                , plotOutput("semaforo", width="290px", height="290px") #idth=1000/72, height=1000/72)
#                , HTML("<h4>Toma precauciones para no infectarte o infectar a los demás</h4>")
                , htmlOutput("text.semaforo2")
                , HTML("<br><br>")
                , actionButton("red", "Dibuja la red de municipios")
                , actionButton("close", "Borra la red de municipios")
                , HTML("<br><br>")
                , visNetworkOutput('d3f')
                , htmlOutput("text1")
                , htmlOutput("text2")
                , dataTableOutput("links")
                )
