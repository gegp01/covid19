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
    #myPal <- colorRampPalette(c("green", "red"))
    #bg=transp(num2col(as.numeric(as.factor(positivos)), col.pal=myPal),.7)
    bg<-ifelse(positivos <= mean(X$infectados_10milhab), "green", "red")
    
    # SHAPE
    shp<-as.vector(rep("circularImage", length(id$MUN_OFICIA)))
    shp2<-shp
    
    # SIZE
    sz<-150*(id$pobtot/max(id$pobtot))
    
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
               , pobtot = id$pobtot
    )
    
  })
  
  output$semaforo<-renderPlot({
    bg<-as.vector(nodos()$color)[grep(input$origen1, as.vector(nodos()$nom_mun))]
    pp <- readPNG("house_white.png")
    plot.new()
    par(mai=c(0.5,0.5,0.5,0.5), bg="transparent")
    plot(NULL, xlim=c(0,1), ylim=c(0,1), axes=F, xlab="", ylab="")
    #rasterImage(pp,0,0,1,1)
    points(0.5,0.5, pch=19, cex=30, col=bg)
    rasterImage(pp,0,0,1,1)

  })
  

  output$text.semaforo1<-renderUI({

    Rt<-X[match(input$origen1, as.vector(X$ID)), "Rt2"]
    I<-X$infectados[as.vector(X$ID)==input$origen1]
    I.100mil<-as.vector(X$infectados_10milhab)[as.vector(X$ID)==input$origen1]*10
    google.work<-X$workplaces_percent_change_from_baseline[as.vector(X$ID)==input$origen1]
    google.home<-X$residential_percent_change_from_baseline[as.vector(X$ID)==input$origen1]
    
    pobtot<-nodos()$pobtot[nodos()$nom_mun==input$origen1]
    semaforo<-nodos()$color[nodos()$nom_mun==input$origen1]
    
    HTML(paste(
      "<div style='text-align:justify;'><h3>La epidemia en el municipio</h3><h4>Hoy hay "
      , paste("<font style='color:", semaforo, "'>", sep="")
      , ifelse(semaforo=="green", "menos", "más"), "infectados</font> con SARS-CoV-2 que el promedio nacional"
      , "de", round(mean(X$infectados_10milhab), 1)*10, "infectados por cada 100 mil habitantes."
      , "<br><br>En", input$origen1
      , "se han reportado", round(I, 0)
      , "personas infectadas,"
      ,  round(I.100mil, 0), "contagiados por cada 100 mil habitantes,"
      , "y los contagios aumentan a un Rt",  ifelse(is.na(Rt), "incalculable.", paste("de ", round(Rt, 2), ".", sep=""))
      , "</h4><br><h3>Movilidad de personas</h3><h4>Google reporta que hoy las personas en esa entidad se mueven"
      , ifelse(google.work==0, "como antes", paste(google.work, "%"))
      , "al trabajo"
      , "y se quedan en casa"
      , ifelse(google.home==0, "como antes", paste(google.home, "%"))
      , "comparado con enero del 2020."
      , "<br><br>En 2010 el INEGI reportó", pobtot, "personas viviendo en el municipio, que se conectan con personas de"
      , dim(net())[1], " municipios. De los municipios conectados, el que tiene más infecciones es ", nodos()$nom_mun[nodos()$positivos==max(nodos()$positivos)]
      , " con "
      , round(max(nodos()$positivos), 0)*10
      , " infectados por cada 100 mil habitantes.</h4>"
      ,"</div>"
    ))
  })
  
}
