# SERVER
# author: Gabriel Ernesto García-Peña
server <- function(input, output, session) {
  
  net1<-readRDS("NET1.rds")
  net1$ID<-as.vector(paste(net1$from, net1$to, sep=""))
  X<-readRDS("NODOS.rds")
  
  observe({
    if (is.null(X)) return(NULL)
    var.opts<-c("NA", unique(net1$from.nom))
    updateSelectInput(session, "origen1", choices = var.opts, selected="Tlalpan-Distrito Federal-9012")
  })
  
  n1<-reactive({
    if (is.null(input$origen1)) return(NULL)
    na.exclude(net1[grep(input$origen1,as.vector(net1$from.nom)),])
  })
  
  n2<-reactive({
    if (is.null(input$origen1)) return(NULL)
    na.exclude(net1[grep(input$origen1,as.vector(net1$to.nom)),])
  })
  
  
  net<-reactive({
    data.frame(na.exclude(rbind(n1(), n2())))
  })
  
  nodos<-reactive({
    nms<-unique(c(as.vector(net()$from), as.vector(net()$to)))
    id<-X[match(nms, as.vector(X$MUN_OFICIA)),]
    
    positivos<-id$infectados_10milhab
    # COLOR
    myPal <- colorRampPalette(c("green", "red"))
    bg=transp(num2col(as.numeric(as.factor(positivos)), col.pal=myPal),.7)
    
    #focal<-as.vector(net1$from)[as.vector(net1$from.nom)==as.vector(input$origen1)]
    focal<-as.vector(net1$from)[grep(as.vector(input$origen1), as.vector(net1$from.nom))]
    
    # SHAPE
    shp<-as.vector(rep("circularImage", length(id$MUN_OFICIA)))
    shp2<-shp
    
    # SIZE
    sz<-150*(id$pobtot/max(id$pobtot))
#    sz1<-ifelse(id$MUN_OFICIA==focal, 333, sz)
    
    # LABEL
    lbl<-paste(id$NOM_MUN, id$NOM_ENT, id$MUN_OFICIA, sep="-")
    
    data.frame(id = id$MUN_OFICIA
               , title = paste(paste("<font face='sans' style='text-size:5px;'><b>", id$NOM_MUN, "-", id$NOM_ENT, "-", id$MUN_OFICIA,  "</b>", sep="")
                               , paste(as.integer(id$infectados_10milhab), " infectados/10mil hab")
                               , paste("Rt = ", round(id$Rt, 2))
                               # , paste("Rt hace 1 semana =", round(id$Rt.7, 2))
                               # , paste("Rt hace 2 semanas =", round(id$Rt.14, 2))
                               , "</font>"
                               , sep="<br>")
               , shape = shp2
               , size = sz
               , color = bg
               , image = "https://gegp01.github.io/covid19/images/house_white.png"
               , label = lbl
               , nom_mun = id$ID
               , positivos = positivos
    )
    
  })
  
  # VISNETWORK
  observeEvent(input$close, {
    output$d3f<-renderVisNetwork({NULL})
    output$text1<-renderUI({NULL})
    output$text2<-renderUI({NULL})
    output$links<- renderDataTable({NULL})
  
    })
  
  
  observeEvent(input$red, {

  output$d3f<-renderVisNetwork({
    visNetwork(nodos(), net()) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE, selected = nodos()$id[match(input$origen1, nodos()$nom_mun)])) %>%
      visNodes(shapeProperties = list(useBorderWithImage = TRUE), shadow=T) %>%
      visEdges(color = list(color = "rgba(70,130,180,0.6)", highlight = "orange")
#               , value="length"
#               , length = "length"
               , width=1
               , selectionWidth = 1.3
               , physics=F
               )
    })
  
  output$text1<-renderUI({
    HTML("<h4><p align='left'>Red de municipios conectados</h4>
             <p align='justify'>Cada casa representa un municipio conectado al municipio seleccionado (arriba) y se muestra el número de personas infectadas por cada 10 mil habitantes y el crecimiento de los infectados (Rt) observado;
              el color rojo representa la intensidad de la infección por SARS-CoV-2; y el tamaño de los círculos representa el número de habitantes en el municipio.</p></p>")
    })
  
  output$text2<-renderUI({
    HTML("<br><br><h4><p align='left'>Datos epidemiológicos de los municipios conectados</h4>
    <p align='left'>Rt, calculado con <a target='_blank' href='https://www.paho.org/en/documents/covid-19-modeling-exercise-how-calculate-rt-guide-epiestim'>el protocolo de la Organizacion Mundial de la Salud (PAHO)</a> representa el crecimiento diario de las infecciones, es el número de personas que inciaron síntomas en un día con respecto al día anterior. La epidemia crece si Rt es mayor que 1.
             </p></p><br>")
  })
  
  output$links<- renderDataTable({
    
    nms<-unique(c(as.vector(net()$from), as.vector(net()$to)))
    id<-X[match(nms, as.vector(X$MUN_OFICIA)),]
    
    positivos10mil<-round(id$infectados_10milhab, 1)
    municipio<-paste(id$NOM_MUN, id$NOM_ENT, id$MUN_OFICIA, sep="-")
    poblacion<-id$pobtot
    infectados<-id$infectados
    negativos<-id$negativos
    
    z<-data.frame(municipio
                  , infectados
                  , negativos
                  , round(id$Rt2, 2)
                  , positivos10mil
                  , poblacion
                  )
    names(z)<-c("municipio", "sospechosos infectados", "sospechosos negativos", "Rt"
                , "infectados / 10mil hab", "habitantes en 2010")
    z})
  })
  
  output$semaforo<-renderPlot({
#    focal<-as.vector(net1$from)[as.vector(net1$from.nom)==as.vector(input$origen1)]
    bg<-as.vector(nodos()$color)[grep(input$origen1, as.vector(nodos()$nom_mun))]
    pp <- readPNG("house_white.png")
    plot.new()
    par(mai=c(0.5,0.5,0.5,0.5))
    plot(NULL, xlim=c(0,1), ylim=c(0,1), axes=F, xlab="", ylab="")
    rasterImage(pp,0,0,1,1)
    points(0.5,0.5, pch=19, cex=35, col=bg)
    rasterImage(pp,0,0,1,1)
  })
  
  
  output$text0<-renderUI({
    HTML("<br><h4>Conectividad entre municipios</h4>
             <p align='justify'>De una forma u otra, la población humana está inter-conectada. La gente que vive en un municipio esta conectada con gente que vive en otros municipios mediante el movimiento de las personas y los productos.
             La conexión entre municipios facilita la dispersión del virus SARS-CoV-2, desde municipios con muchas personas infectadas a sitios en donde no hay. 
             Aqui puedes visualizar la intensidad actual de la epidemia entre los municipios.</p><br>")
  })
  
  
  output$text.semaforo1<-renderUI({
    nms<-unique(c(as.vector(net()$from), as.vector(net()$to)))
    id<-X[match(nms, as.vector(X$MUN_OFICIA)),]
    
    positivos<-id$infectados_10milhab
    I<-as.vector(X$infectados_10milhab)[as.vector(X$ID)==input$origen1]
    HTML(paste(
      "<h4>Tu exposición al covid-19 es "
      , ifelse(I<=mean(X$infectados_10milhab), "<font style='color:#75f80f;'>BAJA</font></b>", "<font style='color:red;'>ALTA</font>")
      ,  "en ", input$origen1, "</h4>"
      ))
  })
  
  output$text.semaforo2<-renderUI({
    nms<-unique(c(as.vector(net()$from), as.vector(net()$to)))
    id<-X[match(nms, as.vector(X$MUN_OFICIA)),]
    
    Rt<-id[match(input$origen1, as.vector(id$ID)), "Rt2"]
    
    positivos<-id$infectados_10milhab
      I<-as.vector(X$infectados_10milhab)[as.vector(X$ID)==input$origen1]
      HTML(paste(
            "<p align='justify'><br><b>Hoy ", input$origen1, " tiene ", round(I, 0)
            , " personas infectadas por cada 10 mil habitantes y las infecciones aumentan a un Rt = ", round(Rt, 2), "</b>. "
            , "La exposición es ALTA si el número de infectados por cada 10 mil habitantes es mayor que el promedio nacional ("
            , round(mean(X$infectados_10milhab), 1)
            , "). Las personas de ese municipio se conectan con personas de  "
            , dim(net())[1], " municipios, y el que tiene más infecciones es ", nodos()$nom_mun[nodos()$positivos==max(nodos()$positivos)]
            , " con "
            , round(max(nodos()$positivos), 0), " infectados por cada 10 mil habitantes.</p>"
            , sep=""

            )
           )
  })
  
  output$text3<-renderUI({
    HTML("<h3>¿Te expones al covid-19 cuando viajas?</h3><p align='justify'>Cuando nos movemos de un lugar a otro encontramos personas y materiales originarios de distintos lugares del país.
           Muchas personas no presentan síntomas del covid-19 y así pueden transmitir el virus SARS-CoV-2 de un lugar a otro, sin darse cuenta.<br><br>
           <h4 style='color:royalblue;'>Planea tus viajes, al viajar te expones al virus y también expones a los otros. Aquí puedes saber como avanza la epidemia en los municipios conectados al tuyo.</h4></p>")
  })
  
}
