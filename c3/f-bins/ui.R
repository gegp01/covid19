# CONTACT AUTOR: gegp[AT]ciencias.unam.mx
# DESARROLLO EN EL SERVIDOR DEL C3-UNAM
#
library(shiny)
library(shinythemes)
source("https://gegp01.github.io/covid19/c3/f-bins.R") # EL ALGORITMO QUE HACE LOS CALCULOS ESTÁ AQUÍ.

options(shiny.maxRequestSize = 30*1024^2) # PERMITE MANIPULAR ARCHIVOS RELATIVAMENTE GRANDES.

# Define UI for application that draws a histogram
# UI 
ui<-(shinyUI(fluidPage(theme=shinytheme("spacelab"),
                       titlePanel("", windowTitle="deciles"),
                       
                       sidebarLayout(
                           sidebarPanel(width=3,
                                        fileInput('datafile', 'CARGAR ARCHIVOS EN FORMATO CSV',
                                                  accept=c('.csv'))
                                        , selectInput("z","variable:", choices = NULL)
                                        , selectInput("n","nivel de agregación:", choices = NULL)
                                        , downloadButton("downloadData", "bajar archivo csv")
                           ),
                           
                           mainPanel(
                               plotOutput("plot")
                               , tableOutput('bins')

                               
                           )
                       )))
)
