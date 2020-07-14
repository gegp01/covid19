server <- function(input, output, session) {
    
    net1<-readRDS("net1.rds")
    X<-readRDS("NODOS.rds")

    observe({
        if (is.null(X)) return(NULL)
        var.opts<-c("NA", unique(net1$from.nom))
        updateSelectInput(session, "origen1", choices = var.opts, selected=unique(net1$from.nom)[31])
    })
    
    
    n1<-reactive({
      na.exclude(net1[grep(input$origen1,as.vector(net1$from.nom)),])
    })
    
    n2<-reactive({
      na.exclude(net1[grep(input$origen1,as.vector(net1$to.nom)),])
    })
    
    
    net<-reactive({
      data.frame(na.exclude(rbind(n1(), n2()))
      )
    })
    
    nodos<-reactive({
        nms<-unique(c(as.vector(net()$from), as.vector(net()$to)))
        id<-X[match(nms, as.vector(X$MUN_OFICIA)),]

        positivos<-id$infectados_10milhab
        
        myPal <- colorRampPalette(c("olivedrab3", "red"))
        bg=transp(num2col(as.numeric(as.factor(positivos)), col.pal=myPal),.7)
        
        focal<-as.vector(net1$from)[grep(input$origen1, as.vector(net1$from.nom))]
        bg[match(id$MUN_OFICIA, focal)]<-"royalblue"
        
        shp<-rep("dot", length(id$MUN_OFICIA))
        shp[match(id$MUN_OFICIA, focal)]<-"image"
        
        sz<-100*(id$pobtot/max(id$pobtot))
        #sz<-1000*(id$pobtot/sum(id$pobtot))
        sz[match(id$MUN_OFICIA, focal)]<-333 #max(sz)
        

        data.frame(id = id$MUN_OFICIA
                   , title = paste(paste(as.integer(id$infectados_10milhab), " infectados/10mil hab")
                                   , paste("(", id$NOM_MUN, "-", id$NOM_ENT, "-", id$MUN_OFICIA,  ")", sep="")
                                   , paste(id$pobtot, "habitantes")
                                   , sep="\n")
                   , shape = shp
                   , size = sz
                   , color = bg
                   , image = "https://gegp01.github.io/covid19/images/house.png"
                   )
        
            })
        
    
    
#NETWORK

# VISNETWORK
     output$d3f<-renderVisNetwork({
      visNetwork(nodos(), net()) %>%
#        visPhysics(solver = "barnesHut") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(color = list(color = "rgba(70,130,180,0.7)", highlight = "red")
                 , value="pobtot"
                  , length = "length"
                  , physics=F
    )
       })
    
  
    
    output$links<- renderDataTable({
            z<-net()[,3:5]
            names(z)<-c("conectividad laboral", "nombre del origen", "nombre del destino")
            z
            })
    

    output$text0<-renderUI({
        HTML("<br><h4>Conectividad entre municipios</h4>
             <p align='justify'>De una forma u otra, la población humana está inter-conectada. La gente que vive en un municipio esta conectada con gente que vive en otros municipios mediante el movimiento de las personas y los productos. 
             Con esta app podemos visualizar la conectividad de un municipio y la intensidad actual de la epidemia en los municipios conectados.</p><br>")
    })

    output$text1<-renderUI({
        HTML("<br><h4>Red interactiva de municipios conectados al municipios seleccionado</h4>
             <p align='left'>La casa representa el municipio seleccionado (arriba) y los círculos (nodos) representan los municipios conectados. En cada nodo se muestra la clave oficial del municipio, y el número de infectados por cada 10mil habitantes.
              El  color rojo de los círculos denota la intensidad de la infección por SARS-CoV-2; y el tamaño de los círculos representa el número de habitantes en el municipio, en 2010.</p>")
    })
    
    
    output$text2<-renderUI({
        HTML("<br><h4>Municipios conectados al origen</h4>
             <p align='justify'>En este cuadro se muestran el factor de conectividad laboral y los nombres de los municipios origen y destino (compuesto de la clave del municipio, y los nombres del municipío y de la entidad).
             </p><br><br>")
    })
}
