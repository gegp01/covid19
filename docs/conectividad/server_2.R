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
      na.exclude(net1[grep(input$origen1,as.vector(net1$from.nom)),])
    })

    n2<-reactive({
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
        
        focal<-as.vector(net1$from)[grep(as.vector(input$origen1), as.vector(net1$from.nom))]
        
        # SHAPE
        shp<-as.vector(rep("circularImage", length(id$MUN_OFICIA)))
        shp2<-shp
        
        # SIZE
        sz<-150*(id$pobtot/max(id$pobtot))
        sz1<-ifelse(id$MUN_OFICIA==focal, 333, sz)
        
        # LABEL
        lbl<-paste(id$NOM_MUN, id$NOM_ENT, id$MUN_OFICIA, sep="-")

        data.frame(id = id$MUN_OFICIA
                   , title = paste(paste("<font face='sans' style='text-size:5px;'><b>", id$NOM_MUN, "-", id$NOM_ENT, "-", id$MUN_OFICIA,  "</b>", sep="")
                                   , paste(as.integer(id$infectados_10milhab), " infectados/10mil hab")
                                   , paste("Rt hace 1 semana =", round(id$Rt.7, 2))
                                   , paste("Rt hace 2 semanas =", round(id$Rt.14, 2))
                                   , "</font>"
                                   , sep="<br>")
                   , shape = shp2
                   , size = sz1
                   , color = bg
                   , image = "https://gegp01.github.io/covid19/images/house_white.png"
                   , label = lbl
                   )
        
            })

# VISNETWORK
     output$d3f<-renderVisNetwork({
      visNetwork(nodos(), net()) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
        visNodes(shapeProperties = list(useBorderWithImage = TRUE), shadow=T) %>%
        visEdges(color = list(color = "rgba(70,130,180,0.6)", highlight = "red")
                  , value="length"
                  , length = "length"
                  , physics=F
                )
       })
    
    output$semaforo<-renderPlot({
        focal<-as.vector(net1$from)[grep(as.vector(input$origen1), as.vector(net1$from.nom))]
        bg<-as.vector(nodos()$color)[grep(focal, as.vector(nodos()$id))]
        pp <- readPNG("house_white.png")
        plot.new()
        plot(NULL, xlim=c(0,1.5), ylim=c(-0.25,1.25), axes=F, xlab="", ylab="")
        rasterImage(pp,0,0,1,1)
        points(0.5,0.5, pch=19, cex=35, col="red")
        rasterImage(pp,0,0,1,1)
        text(1.15,0.5, paste("El municipio tiene:", X$infectados_10milhab[grep(input$origen1, X$ID)], "infectados"))
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
              , round(id$Rt.7, 2)
              , round(id$Rt.14, 2)
              , positivos10mil
              , poblacion
              )

            names(z)<-c("municipio", "sospechosos infectados", "sospechosos negativos", "Rt hace 1 semana", "Rt hace 2 semanas"
                        , "infectados / 10mil hab", "habitantes en 2010")
            z
            })
    

    output$text0<-renderUI({
        HTML("<br><h4>Conectividad entre municipios</h4>
             <p align='justify'>De una forma u otra, la población humana está inter-conectada. La gente que vive en un municipio esta conectada con gente que vive en otros municipios mediante el movimiento de las personas y los productos.
             La conexión entre municipios facilita la dispersión del virus SARS-CoV-2, desde municipios con muchas personas infectadas a sitios en donde no hay. 
             Aqui podemos visualizar la intensidad actual de la epidemia en los municipios conectado entre sí.</p><br>")
    })

    output$text1<-renderUI({
        HTML("<h4>Red de municipios conectados</h4>
             <p align='justify'>La casa representa el municipio seleccionado (arriba) y los círculos representan los municipios conectados (nodos). Cada nodo muestra el número de personas infectadas por cada 10 mil habitantes y el crecimiento de los infectados (Rt) observado hace una y dos semanas;
              el color rojo representa la intensidad de la infección por SARS-CoV-2; y el tamaño de los círculos representa el número de habitantes en el municipio.</p>")
    })
    
    
    output$text2<-renderUI({
        HTML("<br><h4>La epidemia en los municipios conectados</h4>
             <p align='justify'>En este cuadro se muestran algunos datos epidemiológicos de los municipios conectados. Rt refleja el crecimiento diario de las infecciones, es el número de personas que inciaron síntomas en un día con respecto al día anterior. La epidemia crece si Rt es mayor que 1.
             </p><br><br>")
    })
    
    output$text3<-renderUI({
      HTML("<h3>¿Te expones al covid-19 cuando viajas?</h3><p justify='align'>Vivimos en un mundo interconectado y cuando nos movemos de un lugar a otro, nos cruzamos con personas y materiales originarios de distintos lugares del país.
           Muchas personas no presentan síntomas del covid-19 y así pueden transmitir el virus SARS-CoV-2 de un lugar a otro, sin darse cuenta.<br><br>
           <h5 style='color:royalblue;'>Planea tus viajes, al viajar tú te expones al virus y también expones a los otros.<br>Aquí puedes saber como avanza la epidemia en los municipios conectados a tu municipio.</h5></p>")
    })
    
}
