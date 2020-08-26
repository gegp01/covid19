# SERVER
# author: Gabriel Ernesto García-Peña
server <- function(input, output, session) {

   ID<-as.vector(paste(X$MUNICIPIO, X$ENTIDAD, sep="-"))

  observe({
    if (is.null(X)) return(NULL)
    var.opts.ENT<-sort(unique(as.vector(X$NOM_ENT))) ##### BUSCAR EN NODOS
    updateSelectInput(session, "ENTIDAD", choices = var.opts.ENT, selected="CIUDAD DE MÉXICO")
  })


  Q<-eventReactive(input$ENTIDAD, {
    sort(X$NOM_MUN[as.vector(X$NOM_ENT)==input$ENTIDAD])})
   
  observe({
    if (is.null(input$ENTIDAD)) return(NULL)
    updateSelectInput(session, "MUNICIPIO", choices = Q(), selected=NULL)
  })

  origen1<-reactive({
    paste(input$MUNICIPIO, input$ENTIDAD, sep="-")
  })

  output$origen1<-renderText({paste("", origen1())})

  # DATOS DE LA SELECCIÓN MUNICIPIO-ENTIDAD
   
  nodos<-reactive({

    id<-X[match(origen1(), as.vector(paste(X$NOM_MUN, X$NOM_ENT, sep="-"))),]
    
    positivos10milhab<-id$infectados15d_10milhab
    positivos<-id$infectados
    negativos<-id$negativos
    Rt<-id$Rt2
     
    # COLOR DEL SEMÁFORO 
    bg<-ifelse(positivos10milhab <= mean(X$infectados15d_10milhab), "#2ecc71", "red")

    # SHAPE
    shp<-as.vector(rep("circularImage", length(id$MUN_OFICIA)))
    shp2<-shp

    # SIZE
    sz<-100*(id$pobtot/max(id$pobtot))

    # LABEL
    lbl<-paste(id$NOM_MUN, id$NOM_ENT, id$MUN_OFICIA, sep="-")

    data.frame(id = id$MUN_OFICIA
               , title = paste(paste("<font face='sans' style='text-size:5px;'><b>", id$NOM_MUN, "-", id$NOM_ENT, "-", id$MUN_OFICIA,  "</b>", sep="")
                               , paste(as.integer(id$infectados_10milhab), " infectados/10mil hab")
                               , paste("Rt = ", round(id$Rt, 2))
                               , "</font>"
                               , sep="<br>")
               , shape = shp2
               , size = sz
               , color = bg
               , image = "https://gegp01.github.io/covid19/images/house_white.png"
               , label = lbl
               , nom_mun = as.vector(paste(input$MUNICIPIO, input$ENTIDAD, sep="-"))
               , positivos = positivos
               , negativos = negativos
               , pobtot = id$pobtot
               , positivos100milhab = positivos10milhab*10
               , Rt = Rt
    )

  })

  output$semaforo<-renderPlot({
    bg<-as.vector(nodos()$color)[grep(origen1(), as.vector(nodos()$nom_mun))]
    pp <- readPNG("house_white.png")
    plot.new()
    par(mai=c(0.5,0.5,0.5,0.5), bg="transparent")
    plot(NULL, xlim=c(0,1), ylim=c(0,1), axes=F, xlab="", ylab="")
    points(0.5,0.5, pch=19, cex=20, col=bg)
    rasterImage(pp,0,0,1,1)
  })

  output$text.semaforo1<-renderUI({
    Rt<-X[match(as.vector(origen1()), ID), "Rt2"]
    I<-X[match(as.vector(origen1()), as.vector(ID)), "infectados"]
    I.activos<-X[match(as.vector(origen1()), as.vector(ID)), "infectados15d_10milhab"]
    I.100mil<-as.vector(X$infectados_10milhab)[as.vector(ID)==origen1()]*10
    google.work<-X$workplaces_percent_change_from_baseline[as.vector(ID)==origen1()]
    google.home<-X$residential_percent_change_from_baseline[as.vector(ID)==origen1()]

    pobtot<-nodos()$pobtot 
    semaforo<-nodos()$color 
    HTML(paste(
      "<div style='text-align:justify;'>" 
      
      , "En", origen1()
      , "se han reportado"
      , paste("<font style='color:red'>", sep="")
      , nodos()$positivos
      , "personas infectadas</font>,", "<font color=#2ecc71>", nodos()$negativos, " casos negativos.</font>"
     
      , "<font style='color:", semaforo,";'></font> Hay", "<font style='color:", semaforo, "';>", ifelse(semaforo=="red", "más", "menos"),  "contagiados activos </font> en el municipio que el promedio nacional"
      , "de", round(mean(X$infectados15d_10milhab), 1)*10, "infectados por cada 100 mil habitantes."
      , "</font><br><br>"
      
      , "Al día de hoy se estiman "
      , paste("<font style='color:", semaforo, "'>", sep=""), round(I.activos*10,0)
      , "contagiados activos por cada 100mil habitantes en el municipio.</font> Estas son personas infectadas con coronavirus SARS-CoV-2 que comenzaron síntomas hace 15 dias (dividido entre los habitantes del municipio x 100 mil)."

      
      , "</br></br><b>Movilidad de personas</b></br><font color=#5dade2>G<font color=red>o<font color=#f4d03f>o</font></font>g<font color=#2ecc71>l<font color=red>e</font></font></font></font> reporta que actualmente las personas en", input$ENTIDAD , "se mueven"
      , ifelse(google.work==0, "como antes", paste(google.work, "%"))
      , "al trabajo"
      , "y se quedan en casa"
      , ifelse(google.home==0, "como antes", paste(google.home, "%"))
      , "comparado con enero del 2020."
      ,"</div>"
    ))
  })

}
