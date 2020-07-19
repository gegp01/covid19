
# author: Gabriel Ernesto García-Peña
# beta version @
# https://gegp01.shinyapps.io/conectividad_municipios
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(visNetwork)
library(epicontacts)
library(adegenet)

options(shiny.maxRequestSize = 30*1024^2) # ALLOW HANDLING LARGE FILES

ui <- fluidPage(
    tags$style('.container-fluid {
                             background-color:#FFFFFF;}')  #f7f9f9
    , htmlOutput("text3")
    , selectInput("origen1","", choices = NULL, width="90%")
    , visNetworkOutput('d3f')
    , htmlOutput("text1")
    , htmlOutput("text2")
    , dataTableOutput("links")
        )
