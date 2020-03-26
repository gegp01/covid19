server <- shinyServer(function(input, output) {
    
    D<-read.csv("covid19-inegi2010.csv") # Datos inegi
    D$entidad<-as.vector(D$entidad)
    D$entidad[grep("Distrito Federal", D$entidad)]<-"Ciudad de México"
    
#    P<-st_read("Estados_WGS.shp")
    P<-st_read("dest2018gw.shp")
    P$infectados<-D$covid19.estado.infectados[match(P$NOM_ENT, D$entidad)]
    P$recuperados<-D$covid19.estados.recuperados[match(P$NOM_ENT, D$entidad)]
    P$muertos<-D$covid19.estado.muertos[match(P$NOM_ENT, D$entidad)]
#    P<-st_read("estadosMX.shp")
    
    text0<-paste(P$NOM_ENT, ":", P$muertos, "muertes", "|", P$infectados, "casos confirmados de covid-19")
    # text1<-paste("<p style='width: 90vh;'><b>Total de casos confirmados con COVID19: </b>", X$data[X$name=="Global.confirmed"]
    #              , "<br><b>Muertes: </b>", sum(dead.count$x)
    #              , "<br><a href='https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports' target='_blank'>Reporte de la WHO No: ", X$data[X$name=="WHO report"], "</a>"
    #              , "<br>Fecha: ", X$data[X$name=="Date"]
    #              , "<br></p>")
    
    # # Polygon values, ordered by wrld_simpl$NAME
    # infectados<-D$covid19.estado.infectados
    # recuperados<-D$covid19.estado.recuperados
    # muertos<-D$covid19.estado.muertos

    z<-P$infectados
    
    #z<-as.numeric(as.character(D$dead))/as.numeric(as.character(D$data)) # dead/infected. Note: Add epsilon here
    # z<-log(as.numeric(as.character(D$dead))+1)
    
    myPal <- colorRampPalette(c("royal blue", "red"))
    p<-transp(num2col(z, col.pal=myPal),.8)
    
    # D$latitude<-D$lat/10000
    # D$longitude<-(D$lon/10000)*-1
    # 
    output$map <-renderLeaflet({
#        leaflet() %>% addTiles() %>% addCircles(lng = D$longitude, lat = D$latitude, color=p)#, label=htmlEscape(text0), labelOptions(noHide=T))
#        leaflet() %>% setView(20, 10, zoom = 2) %>% addTiles() %>% addCircles(lng = D$longitude, lat = D$latitude, color=p)
                                                                              #, radius = as.numeric(as.character(D$data))*10, label=htmlEscape(text0), labelOptions(noHide=T), color=p)
        leaflet(P$geometry) %>% addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.5, fillColor = p, popup=htmlEscape(text0), popupOptions=popupOptions(closeButton = FALSE))        
    })
    
    # output$caption<-renderUI({
    #     HTML(text1)
    # })
    
})
