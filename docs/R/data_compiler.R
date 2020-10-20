# OBTENER DATOS OFICIALES
# COVID19_MX, BASE DE DATOS: NODOS.rds
source("https://gegp01.github.io/covid19/wget_covid19MX.R")
      saveRDS(covid, "~/COVID19_C3/html/datos/datos_oficiales.rds")
  
covid<-readRDS("~/COVID19_C3/html/datos/datos_oficiales.rds")

# saveRDS(d, "datos_oficiales.rds") # @ C3  
#covid<-readRDS("datos_oficiales.rds") # @ C3

###########3
# CARGAR DATOS DE REFERENCIA

setwd("~/COVID19_C3/html/red_laboral")
##########################################

  # LEER BASE DE DATOS CON LA ID OFICIAL DE LOS MUNICIPIOS Y AGREGAR LA POBLACION TOTAL (pobtot) DEL CENSO DE 2010
  
      # mun<-read.csv("municipios.csv"
      #               , colClasses=c(rep("character",13)))
      
#           iter<-read.csv("~/html/genetic/covid19_inegi2010/iter_00_cpv2010.csv") # dirección localhost
#         #iter<-readRDS("iter_00_cpv2010.rds") # desde el servidor
#       
#       mun$id<-paste(as.numeric(mun$CVE_ENT), as.numeric(mun$CVE_MUN))
#   
#       iter$id<-paste(iter$entidad, iter$mun)
#       iter$MUN_OFICIA<-mun$MUN_OFICIA[match(iter$id, mun$id)]
#         
#   # mun$pobtot<-iter$pobtot[match(id.mun, id.iter)]
#   # mun$id.iter<-id.iter[match(id.mun, id.iter)]
#   
#       pobtot.ag<-aggregate(iter$pobtot, list(iter$MUN_OFICIA), sum)
#       
#       pobtot<-pobtot.ag$x
#       names(pobtot)<-pobtot.ag$Group.1
#       
# 
#       mun$pobtot<-pobtot[match(mun$MUN_OFICIA, names(pobtot))]
# 
# # CORREGIR LOS NOMBRES
#       # ref<-read.csv("~/COVID19_C3/html/datos_covid19/catalogo_municipios.csv"
#       #               , colClasses=c(rep("character",3)))
# 
#       ref<-read.csv("catalogo_municipios.csv"
#                     , colClasses=c(rep("character",3)))
#       
#       ref.ent<-read.csv("catalogo_entidades.csv"
#                     , colClasses=c(rep("character",3)))
#       
      # ref$ENTIDAD_FEDERATIVA<-ref.ent$ENTIDAD_FEDERATIVA[match(ref$CLAVE_ENTIDAD, ref.ent$CLAVE_ENTIDAD)]
      # ref$MUN_OFICIA<-paste(ref$CLAVE_ENTIDAD, ref$CLAVE_MUNICIPIO, sep="")
      # 
      # ref$pobtot<-mun$pobtot[match(ref$MUN_OFICIA, mun$MUN_OFICIA)]
      # 
      # # y<-sample(ref$MUN_OFICIA, 1)
      # # ref$pobtot[ref$MUN_OFICIA==y]
      # # mun$pobtot[mun$MUN_OFICIA==y]
      # 
      # # AGREGAR COORDENADAS (CENTROIDES) DE LOS MUNICIPIOS
      # ref$latitud<-mun$ycoord[match(ref$MUN_OFICIA, mun$MUN_OFICIA)]
      # ref$longitud<-mun$xcoord[match(ref$MUN_OFICIA, mun$MUN_OFICIA)]
      # 
      # write.csv(ref, "datos_censo2010.csv", row.names=F)
#       
#       # head(cbind(mun$NOM_MUN, ref$MUNICIPIO[match(mun$MUN_OFICIA, ref$MUN_OFICIA)]))
#       mun$MUNICIPIO<-ref$MUNICIPIO[match(mun$MUN_OFICIA, ref$MUN_OFICIA)]
#       mun$ENTIDAD<-ref$ENTIDAD_FEDERATIVA[match(mun$MUN_OFICIA, ref$MUN_OFICIA)]
#       
#       require(Hmisc)
#       #mun$MUNICIPIO<-capitalize(tolower(mun$MUNICIPIO))  
#       mun$NOM_MUN<-mun$MUNICIPIO
#       
#       mun$NOM_ENT<-mun$ENTIDAD # CAPITALIZE? OR NOT? ASK THE EDITOR
      
  ##########################################
# INTEGRAR YCOMPILAR DATOS EPIDEMIOLÓGICOS


# Leer datos del censo nacional de poblacion y vivienda del INEGI

      mun<-read.csv("datos_censo2010.csv"
                  , colClasses=c(rep("character",8)))

      mun$pobtot<-as.numeric(mun$pobtot)

      covid$FECHA_ACTUALIZACION[covid$FECHA_ACTUALIZACION=="9999-99-99"]<-NA
      covid$FECHA_ACTUALIZACION<-as.Date(covid$FECHA_ACTUALIZACION)
  
      covid$FECHA_INGRESO[covid$FECHA_INGRESO=="9999-99-99"]<-NA
      covid$FECHA_INGRESO<-as.Date(covid$FECHA_INGRESO)
      covid$ingreso_julianos<-covid$FECHA_INGRESO-as.Date("2019-12-31")
      
      covid$FECHA_SINTOMAS[covid$FECHA_SINTOMAS=="9999-99-99"]<-NA
      covid$FECHA_SINTOMAS<-as.Date(covid$FECHA_SINTOMAS)
      covid$sintomas_julianos<-covid$FECHA_SINTOMAS-as.Date("2019-12-31")
      
      covid$FECHA_DEF[covid$FECHA_DEF=="9999-99-99"]<-NA
      covid$FECHA_DEF<-as.Date(covid$FECHA_DEF) 
      covid$defuncion_julianos<-covid$FECHA_DEF-as.Date("2020-01-01")
      
      covid$id<-paste(covid$ENTIDAD_RES, covid$MUNICIPIO_RES, sep="")
      #mun$id<-paste(mun$CVE_ENT, mun$CVE_MUN)
      #covid$municipio_oficial<-mun$MUN_OFICIA[match(covid$id, mun$id)] ####
      
      covid$municipio_oficial<-mun$MUN_OFICIA[match(covid$id, mun$MUN_OFICIA)] 
      
      covid$infectado<-ifelse(covid$RESULTADO==1, 1, 0)
      I<-aggregate(covid$infectado, list(covid$municipio_oficial), sum)
      
      ## def<-aggregate(covid$infectado, list(covid$municipio_oficial), sum)
      
      covid$ultimos15dias<-
        ifelse(max(covid$sintomas_julianos)-covid$sintomas_julianos<=15, 1, 0)

      covid$ultimos30dias<-
        ifelse(max(covid$sintomas_julianos)-covid$sintomas_julianos<=30, 1, 0)
      
      
      covid$infectados_15d<-covid$infectado*covid$ultimos15dias
      covid$infectados_30d<-covid$infectado*covid$ultimos30dias
            
#      covid$infectados_15d<-ifelse(max(covid$sintomas_julianos)-covid$sintomas_julianos<=30, covid$infectado, NA)
      
      
      I_ultimos15<-aggregate(covid$infectados_15d, list(covid$municipio_oficial), sum)
      I_ultimos30<-aggregate(covid$infectados_30d, list(covid$municipio_oficial), sum)
      
      
      covid$negativo<-ifelse(covid$RESULTADO==2, 1, 0)
      
      S<-aggregate(covid$negativo, list(covid$municipio_oficial), sum)
  
      covid$negativos_15d<-covid$negativo*covid$ultimos15dias
      covid$negativos_30d<-covid$negativo*covid$ultimos30dias  
      
      S_ultimos15<-aggregate(covid$negativos_15d, list(covid$municipio_oficial), sum)
      S_ultimos30<-aggregate(covid$negativos_30d, list(covid$municipio_oficial), sum)
      
      mun$infectados<-I$x[match(mun$MUN_OFICIA, I$Group.1)]
      mun$infectados[is.na(mun$infectados)]<-0
      
      mun$negativos<-S$x[match(mun$MUN_OFICIA, S$Group.1)]
      mun$negativos[is.na(mun$negativos)]<-0
      
      mun$infectados15d<-I_ultimos15$x[match(mun$MUN_OFICIA, I_ultimos15$Group.1)]
      mun$infectados15d[is.na(mun$infectados15d)]<-0
      
      mun$infectados30d<-I_ultimos30$x[match(mun$MUN_OFICIA, I_ultimos30$Group.1)]
      mun$infectados30d[is.na(mun$infectados30d)]<-0
  
      mun$negativos15d<-S_ultimos15$x[match(mun$MUN_OFICIA, S_ultimos15$Group.1)]
      mun$negativos15d[is.na(mun$negativos15d)]<-0
      
      mun$negativos30d<-S_ultimos30$x[match(mun$MUN_OFICIA, S_ultimos30$Group.1)]
      mun$negativos30d[is.na(mun$negativos30d)]<-0
      
    # estadísticas epidemiológicas
  #    mun$prevalencia<-mun$infectados/(mun$infectados+mun$negativos)
      mun$infectados_10milhab<-(mun$infectados/mun$pobtot)*10000
      mun$infectados15d_10milhab<-(mun$infectados15d/mun$pobtot)*10000
      mun$infectados30d_10milhab<-(mun$infectados30d/mun$pobtot)*10000
      
      deceso<-ifelse(is.na(covid$defuncion_julianos), 0, 1)
      covid$deceso_positivo<-ifelse(covid$RESULTADO==1, deceso, 0)
      
      death<-aggregate(covid$deceso_positivo, list(covid$municipio_oficial), sum)
      mun$deceso_positivo<-death$x[match(mun$MUN_OFICIA, death$Group.1)]
      mun$deceso_positivo_10milhab<-(mun$deceso_positivo/mun$pobtot)*10000
      
      
      
  # CALCULAR TASA DE CRECIMIENTO (INFECTADOS HOY/INFECTADOS AYER)
      
      x4<-table(covid$municipio_oficial[covid$infectado==1], covid$FECHA_SINTOMAS[covid$infectado==1])
      
  # require(EpiEstim)
  # data("Flu2009")
  # 
  # f1<-function(x) {
  #       y<-estimate_R(data.frame(x4[x,]), method="non_parametric_si"
  #                  , config = make_config(list(si_distr = Flu2009$si_distr)))
  #       mean(y[['R']][["Median(R)"]])
  #       }
  # x<-c(1:10) #c(1:nrow(x4))
  # Rt<-sapply(x, f1)
  
  # TODOS LOS MUNICIPIOS !!! warning !!!
  
  require(EpiEstim)
  t_start<-seq(2, length(colSums(x4))-10)
  t_end<-t_start + 10
  
  y<-estimate_R(colSums(x4), method="parametric_si"
                , config = make_config(list(
                  mean_si = 4.8
                  , std_si = 2.3
                  , t_start = t_start
                  , t_end = t_end))
  )
  
  f1<-function(x) {
    y<-estimate_R(data.frame(x4[x,]), method="parametric_si"
                  , config = make_config(list(
                    mean_si = 4.8
                    , std_si = 2.3
                    , t_start = t_start
                    , t_end = t_end
                    ))
                  )
    # data("Flu2009")
    # t_start<-seq(2, length(colSums(x4))-15) 
    # t_end<-t_start + 15
    
    # y<-estimate_R(data.frame(x4[x,]), method="non_parametric_si"
    #               , config = make_config(list(si_distr = Flu2009$si_distr
    #                                           , t_start = t_start
    #                                           , t_end = t_end)))
    # 
    mean(y[['R']][["Median(R)"]])
  }
  x<-c(1:nrow(x4))
  Rt<-sapply(x, f1)
  
  names(Rt)<-rownames(x4)
  
  
  # 
  #   
  # t_start<-seq(2, length(colSums(x4))-15) 
  # t_end<-t_start + 15
  # 
  # y<-estimate_R(data.frame(colSums(x4)), method="non_parametric_si"
  #               , config = make_config(list(si_distr = Flu2009$si_distr
  #                                           , t_start = t_start
  #                                           , t_end = t_end)))
  # 
  # mean(y[['R']][["Median(R)"]])
  # 
  # 
  # 
  #     ff<-function(x) {
  #       a<-c(NA, cumsum(x4[x,]))
  #       b<-c(cumsum(x4[x,]), NA)
  #       #c<-(b-a)/sum(na.omit(a)) # cambio diario
  #       b/a
  #     }
  #      
  #     x<-c(1:nrow(x4))
  # 
  #     Y<-sapply(x, ff)
  #     names(Y)<-rownames(x4)
  #     
  #     # Rt de hace dos semanas
  #     # Rt<-Y[ncol(x4)-14,1]
  #     # 
  #     # plot(Y[,1], type="l", main="")
  #     # lines(Y[,2], type="l", main="")
  # 
  #     fff<-function(x){
  #         Y[ncol(x4)-7,x]
  #     }
  #     
  #     ffff<-function(x){
  #       Y[ncol(x4)-14,x]
  #     }
  #     
  #     Rt.7<-sapply(x, fff)
  #     Rt.14<-sapply(x, ffff)
  #     
  #     mun$Rt.7<-Rt.7[match(mun$MUN_OFICIA, rownames(x4))]
  #     mun$Rt.14<-Rt.14[match(mun$MUN_OFICIA, rownames(x4))]
  
      mun$Rt<-Rt[match(mun$MUN_OFICIA, names(Rt))]
  
      mun$Rt2<-ifelse(mun$infectados+mun$negativos >= 20, mun$Rt, NA) # eliminamos los estimados de Rt con pocos casos
  
      mun$ID<-paste(mun$NOM_MUN, mun$NOM_ENT, mun$MUN_OFICIA, sep="-")
      
      mun$FECHA_ACTUALIZACION<-rep(covid$FECHA_ACTUALIZACION[1], nrow(mun))
      
      # DATOS DE GOOGLE MAPS  
      
    # wget https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv
    # G<-read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") # esto no es muy eficiente. Es mas facil desde el shell wget...

    G<-read.csv("~/COVID19_C3/html/datos/Global_Mobility_Report.csv")
    nmG<-paste("google_mobility_", Sys.Date(), ".rds", sep="")
    saveRDS(G, paste("~/COVID19_C3/html/datos/", nmG, sep=""))
    # google<-readRDS("~/COVID19_C3/html/datos/google_mobility.rds")
    google<-readRDS(paste("~/COVID19_C3/html/datos/", nmG , sep=""))

# Notas: Con el script de arriba se guarda un archivo con la fecha de lectura de Google. 
#    Cada vez que se corre el script, se actualiza SysDate(). Asi se acumula un registro de las versiones.
        
      gmx<-google[grep("Mex", google$country_region),]
      gmx$date<-as.Date(gmx$date)
      gmx$julianday<-gmx$date-as.Date("2019-12-31")
      gmx$entidad<-as.vector(gmx$sub_region_1)
      
      ################# Nota: hay que guardar "ent" en el servidor para el archivo data_compiler_c3.R
      ent<-read.csv("cve_entidad_gmx.csv", sep=",", header=F, colClasses=c(rep("character",3)))
      ent$CVE_ENT<-c("01", "02", "03", "04", "07", "08", "05", "06", "09", ent$V2[10:32])
      
      gmx$CVE_ENT<-ent$CVE_ENT[match(gmx$entidad, ent$V3)]
      gmx.hoy<-gmx[gmx$julianday==max(gmx$julianday),]
      
      gmx.semana.anterior<-gmx[gmx$julianday>=(max(gmx$julianday)-7),]
      
      z.mean<-aggregate(gmx.semana.anterior$workplaces_percent_change_from_baseline, list(gmx.semana.anterior$CVE_ENT), mean)
      z.max<-aggregate(gmx.semana.anterior$workplaces_percent_change_from_baseline, list(gmx.semana.anterior$CVE_ENT), max)
      
      gmx.hoy.sorted<-gmx.hoy[match(mun$CLAVE_ENTIDAD, gmx.hoy$CVE_ENT),]
      z.mean.sorted<-z.mean$x[match(mun$CLAVE_ENTIDAD, z.mean$Group.1)] # movilidad promedio en el trabajo la semana pasada
      z.max.sorted<-z.max$x[match(mun$CLAVE_ENTIDAD, z.max$Group.1)] # movilidad maxima en el trabajao la semana pasada
      table(gmx.hoy.sorted$CVE_ENT==mun$CLAVE_ENTIDAD) # checar que los datos en gmx.hoy.sorted estén en el orden de mu
      
      nodos<-data.frame(mun, gmx.hoy.sorted, z.max.sorted, z.mean.sorted)
      saveRDS(nodos, "~/COVID19_C3/html/datos/NODOS.rds")
      saveRDS(nodos, "~/COVID19_C3/html/red_laboral/NODOS.rds")

    # plot((mun$infectados+mun$negativos), mun$Rt, xlim=c(0, 5000))
    # abline(v=50, col="red")

# GRAFICA DE RT PARA CADA MUNICIPIO      
    # plot(spline(x4[1,]), ylim=c(0, max(x4)), type="l")
    #   f4<-function(x) {lines(spline(x4[x,]))}
    #   x<-c(2:nrow(x4))
    # sapply(x, f4)

    library(dplyr)
    library(dygraphs)
    library(xts)          # To make the convertion data-frame / xts format
    library(tidyverse)
    library(lubridate)
    
    # ref.ent<-read.csv("catalogo_entidades.csv"
    #                   , colClasses=c(rep("character",3)))
    # iter<-read.csv("~/html/genetic/covid19_inegi2010/iter_00_cpv2010.csv")
    
  
    covid$ENT_RES_NOM<-mun$ENTIDAD_FEDERATIVA[match(covid$ENTIDAD_RES, mun$CLAVE_ENTIDAD)]
    covid$MUN_RES_NOM<-mun$MUNICIPIO[match(covid$municipio_oficial, mun$MUN_OFICIA)]
    
    # covid$ENT_RES_NOM<-ref.ent$ENTIDAD_FEDERATIVA[match(covid$ENTIDAD_RES, as.numeric(ref.ent$CLAVE_ENTIDAD))]
    # covid$MUN_RES_NOM<-ref$MUNICIPIO[match(covid$municipio_oficial, ref$MUN_OFICIA)]
    
    
        L1<-split.data.frame(covid, covid$ENT_RES_NOM)
    
    f4<-function(x) {
#      Q<-table(L1[[x]]$municipio_oficial[L1[[x]]$infectado==1], L1[[x]]$FECHA_SINTOMAS[L1[[x]]$infectado==1])
      Q<-table(L1[[x]]$MUN_RES_NOM[L1[[x]]$infectado==1], L1[[x]]$FECHA_SINTOMAS[L1[[x]]$infectado==1])
#      p<-mun$pobtot[match(rownames(Q), mun$MUN_OFICIA)]
      nms<-L1[[x]]$municipio_oficial[match(rownames(Q), L1[[x]]$MUN_RES_NOM)]
      p<-mun$pobtot[match(nms, mun$MUN_OFICIA)]
#      p<-mun$pobtot[match(rownames(Q), mun$MUN_OFICIA)]
      (Q/p)*100000
    }
    
    x<-c(1:32)
    L2<-lapply(x, f4)
    names(L2)<-names(L1)
  
  # GENERAR FIGURAS EN BATCH
  require(plotly)
      
      f.fig<-function(x) {
        
        entidad<-names(L2)[x]   # SELECCIONAR UNA ENTIDAD POR NOMBRE
        
        X<-t(L2[[entidad]])
        Y<-xts(x=X, order.by = as.Date(rownames(X)))
        pal<-hsv(seq(0,1 - 1/12,length.out = ncol(X)), 1 , 1)
        
        # Finally the plot
        
        #par(font.main=1, mai=c(2,2,0,0))
        p <- dygraph(Y, ylab="Contagiados/100 mil hab </br> con síntomas en ese día"
                     , width="100%", height=400, main=paste("epidemia en los municipios de", entidad)) %>%
          dyOptions(labelsUTC = F, fillGraph=T, fillAlpha=0.05, drawGrid = FALSE, colors=pal, stepPlot = TRUE
                    , drawPoints = TRUE, pointSize = 1) %>%
          dySeries(strokeWidth = 1) %>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "both") %>%
          dyHighlight(highlightCircleSize = 2, highlightSeriesBackgroundAlpha = 0.1, hideOnMouseOut = FALSE)  %>%
          dyRoller(showRoller = FALSE, rollPeriod = 7)  %>% 
          dyCSS(css="~/COVID19_C3/html/css/spacelab/bootstrap.css") %>% 
          dyLegend(show="onmouseover", labelsSeparateLines = FALSE)
        
        # htmlwidgets::saveWidget(as_widget(, paste("epi-", entidad, ".html", sep=""), background="royalblue")
        htmlwidgets::saveWidget(as_widget(p), paste("epi-", entidad, ".html", sep=""), background="transparent")
        
  }
      
  x<-c(1:32)          
  setwd("~/COVID19_C3/html/datos/graficas/")
  lapply(x, f.fig)      
        
# ########################### UNA SOLA FIGURA
# # entidad<-names(L2)[1]   # SELECCIONAR UNA ENTIDAD POR NOMBRE
# # 
# X<-t(L2[[entidad]])
# Y<-xts(x=X, order.by = as.Date(rownames(X)))
# pal<-hsv(seq(0,1 - 1/12,length.out = ncol(X)), 0.5 , 1)
# 
# # Finally the plot
# 
# par(font.main=1)
# p <- dygraph(Y, ylab="<font size=2px>Contagiados que iniciaron síntomas en ese día</font>"
#              , width="auto", height=500, main=paste("<font size=2>epidemia en los municipios de", entidad, "</font>")) %>%
#   dyOptions(labelsUTC = F, fillGraph=F, fillAlpha=0.1, drawGrid = FALSE, colors=pal) %>%
#   dyRangeSelector() %>%
#   dyCrosshair(direction = "both") %>%
#   dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
#   dyRoller(rollPeriod = 10)  %>% dyCSS(css="~/COVID19_C3/html/css/spacelab/bootstrap.css") %>%
#   dyLegend(show="always", width=500)
# 
# p
# 
# 
# GRAFICA DE herd immunity
# LA FRACCION DE LA POBLACION QUE SE HA INFECTADO DE COVID.
  
  library(dplyr)
  library(dygraphs)
  library(xts)          # To make the convertion data-frame / xts format
  library(tidyverse)
  library(lubridate)
  
  # ref.ent<-read.csv("catalogo_entidades.csv"
  #                   , colClasses=c(rep("character",3)))
  # iter<-read.csv("~/html/genetic/covid19_inegi2010/iter_00_cpv2010.csv")
  
  
  covid$ENT_RES_NOM<-mun$ENTIDAD_FEDERATIVA[match(covid$ENTIDAD_RES, mun$CLAVE_ENTIDAD)]
  covid$MUN_RES_NOM<-mun$MUNICIPIO[match(covid$municipio_oficial, mun$MUN_OFICIA)]
  
  # covid$ENT_RES_NOM<-ref.ent$ENTIDAD_FEDERATIVA[match(covid$ENTIDAD_RES, as.numeric(ref.ent$CLAVE_ENTIDAD))]
  # covid$MUN_RES_NOM<-ref$MUNICIPIO[match(covid$municipio_oficial, ref$MUN_OFICIA)]
  
  
  L1<-split.data.frame(covid, covid$ENT_RES_NOM)
  
  f4<-function(x) {
    #      Q<-table(L1[[x]]$municipio_oficial[L1[[x]]$infectado==1], L1[[x]]$FECHA_SINTOMAS[L1[[x]]$infectado==1])
    Q<-table(L1[[x]]$MUN_RES_NOM[L1[[x]]$infectado==1], L1[[x]]$FECHA_SINTOMAS[L1[[x]]$infectado==1])
    #      p<-mun$pobtot[match(rownames(Q), mun$MUN_OFICIA)]
    
    fy<-function(y){
      q1<-cumsum(Q[y,])
    }
    
    y<-1:dim(Q)[1]
    W<-sapply(y, fy)
    W<-as.table(t(W))
    rownames(W)<-rownames(Q)
    
    nms<-L1[[x]]$municipio_oficial[match(rownames(Q), L1[[x]]$MUN_RES_NOM)]
    p<-mun$pobtot[match(nms, mun$MUN_OFICIA)]
    #      p<-mun$pobtot[match(rownames(Q), mun$MUN_OFICIA)]
    # (Q/p)*100000
    (W/p)*100
  }
  
  x<-c(1:32)
  L2<-lapply(x, f4)
  names(L2)<-names(L1)
  
  # GENERAR FIGURAS EN BATCH
  require(plotly)
  
  f.fig<-function(x) {
    
    entidad<-names(L2)[x]   # SELECCIONAR UNA ENTIDAD POR NOMBRE
    
    X<-t(L2[[entidad]])
    Y<-xts(x=X, order.by = as.Date(rownames(X)))
    pal<-hsv(seq(0,1 - 1/12,length.out = ncol(X)), 1 , 1)
    
    # Finally the plot
    
    #par(font.main=1, mai=c(2,2,0,0))
    p <- dygraph(Y, ylab="<h5>Porcentaje de habitantes del municipio <br> que se han infectado con COVID-19</h5>"
                 , width="100%", height=400, main=paste("epidemia en los municipios de", entidad)) %>%
      dyOptions(labelsUTC = F, fillGraph=T, fillAlpha=0.1, drawGrid = FALSE, colors=pal, stepPlot = TRUE
                , drawPoints = TRUE, pointSize = 1) %>%
      dySeries(strokeWidth = 1) %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "both") %>%
      dyHighlight(highlightCircleSize = 2, highlightSeriesBackgroundAlpha = 0.1, hideOnMouseOut = FALSE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7)  %>% 
      dyCSS(css="~/COVID19_C3/html/css/spacelab/bootstrap.css") %>% 
      dyLegend(show="onmouseover", labelsSeparateLines = FALSE)
    
    # htmlwidgets::saveWidget(as_widget(, paste("epi-", entidad, ".html", sep=""), background="royalblue")
    htmlwidgets::saveWidget(as_widget(p), paste("epi-", entidad, ".html", sep=""), background="transparent")
    
  }
  
  x<-c(1:32)          
  setwd("~/COVID19_C3/html/datos/graficas/herd/")
  lapply(x, f.fig)   
  

  
# ANALISIS DE EPSILON

  # SELECCIONAR DATOS EN LOS ÚLTIMOS 20 DÍAS
  D<-covid[covid$FECHA_SINTOMAS>=max(covid$FECHA_SINTOMAS)-20,]
  
  # identidad de los casos
  nms<-unique(D$ID_REGISTRO)
  
  # El número de sospechosos en ese municipio
  N<-length(nms)
  
  # El número de casos con covid en ese municipio
  Ncovid<-table(D$municipio_oficial[D$infectado==1])
  
  # El número de casos sospechosos de ese municipio 
  # X2<-table(D$ID_REGISTRO, D$municipio_oficial)
  # NH<-colSums(X2)
  NH<-table(D$municipio_oficial)
  
  Ncovid.sorted<-Ncovid[match(names(NH), names(Ncovid))]
  
  Z<-data.frame(cbind(NH, Ncovid.sorted))
#  Z<-data.frame(NH, Ncovid.sorted)
  Z$municipio_oficial<-as.vector(rownames(Z))
  names(Z)<-c("sospechosos", "confirmados", "municipio_oficial")
#  Z$Var1<-as.vector(Z$Var1)
#  names(Z)<-c("sospechosos", "municipio_oficial", "confirmados")
  
  
  P.covid.mun<-Z$confirmados/Z$sospechosos
  names(P.covid.mun)<-Z$municipio_oficial
  
  # Warning message:
  #   In Ops.factor(Z$confirmados, Z$sospechosos) :
  #   ‘/’ not meaningful for factors  
  
  
  P.covid<-sum(Ncovid)/N
  
  numerador<-(Z$sospechosos*(P.covid.mun-P.covid))
  denominador<-(Z$sospechosos*P.covid*(1-P.covid))^0.5
  
  
  epsilon<-numerador/denominador
  names(epsilon)<-rownames(Z)
  
  epsilon<-na.exclude(epsilon)
  
  # # BUSCAMOS CUALES SON LOS MUNICIPIOS CON EPSILON >= 1.7, Y LES ASIGNAMOS NOS NOMBRES CORRESPONDIENTES
  # mun.covid<-names(epsilon[epsilon>=1.7])
  # municipio<-ref$MUNICIPIO[match(mun.covid, ref$MUN_OFICIA)]
  # entidad<-ref$ENTIDAD_FEDERATIVA[match(mun.covid, ref$MUN_OFICIA)]
  # 
  # # Q ES LA LISTA DE MUNICIPIOS CON EPSILON >= 1.7
  # Q<-data.frame(municipio=as.vector(municipio), entidad=as.vector(entidad)
  #               , epsilon=round(epsilon[epsilon>=1.7], 2)
  #               , municipio_oficial=names(epsilon[epsilon>=1.7])
  # )


# hist(epsilon[epsilon<=-1.6], xlim=c(-20, 20), ylim=c(0, 900))
# hist(epsilon, add=T, col=rgb(1,0,0,0.5))
# hist(epsilon[epsilon>=1.6], add=T, col="lightblue")
        
  ##############################
# GENERAR POLIGONO PARA leaflet/ 
#
library(rgdal)
        
  mun<- readOGR(dsn = "~/COVID19_C3/municipios/", layer = "municipios_simpleX")
  D<-readRDS("~/COVID19_C3/html/datos/NODOS.rds")
 
  no2<-data.frame(MUN_OFICIA=D$MUN_OFICIA
                        , CVE_ENT=D$CLAVE_ENTIDAD
                        , CVE_MUN=D$CLAVE_MUNICIPIO
                        , ENTIDAD=D$ENTIDAD
                        , MUNICIPIO=D$MUNICIPIO
                        , infectados = D$infectados
                        , negativos = D$negativos
                        , inf15d= D$infectados15d
                        , pobtot = D$pobtot
                        , inf10mil = D$infectados_10milhab
                        , inf15d10mil=D$infectados15d_10milhab
                        , dead=D$deceso_positivo
                        , dead10mil=D$deceso_positivo_10milhab
                        , Rt = D$Rt
                        , wrkhy = D$workplaces_percent_change_from_baseline
                        , homhy = D$residential_percent_change_from_baseline
                        , wrkmnsem = D$z.mean.sorted # movilidad promedio en el trabajao la semana pasada
                        , wrkmxsem = D$z.max.sorted # movilidad maxima en el trabajao la semana pasada
                        , FECHA_DATOS = D$FECHA_ACTUALIZACION
                  
                    )
  
  no2$epsilon<-epsilon[match(no2$MUN_OFICIA, names(epsilon))]
  
  require(leaflet)
  Xpalette <- colorNumeric(palette=c('#FED976', '#BD0026'), domain=no2$Rt, na.color="transparent")
  no2$Rt_col<-Xpalette(no2$Rt)
  
  Xpalette <- colorNumeric(palette=c('#fadbd8', '#e74c3c'), domain=log(no2$inf15d10mil+1), na.color="transparent")
  no2$inf15d10mil_col<-Xpalette(log(no2$inf15d10mil+1))
  
  Xpalette <- colorNumeric(palette=c('#ebdef0', '#553f7c'), domain=log(no2$dead10mil+1), na.color="transparent")
  no2$dead10mil_col<-Xpalette(log(no2$dead10mil+1))
  
  Xpalette <- colorNumeric(palette=c('#ebdef0', '#553f7c'), domain=log(no2$dead+1), na.color="transparent")
  no2$dead_col<-Xpalette(log(no2$dead+1))

  Xpalette <- colorNumeric(palette=c('#FED976', '#BD0026'), domain=no2$epsilon, na.color="transparent")
  no2$epsilon_col<-Xpalette(no2$epsilon)
  
  Xpalette <- colorNumeric(palette=c('#85c1e9', '#db299e'), domain=no2$wrkmxsem, na.color="transparent")
  no2$wrkmxsem_col<-Xpalette(no2$wrkmxsem)
    
  mun@data = data.frame(mun@data, no2[match(mun$MUN_OFICIA, no2$MUN_OFICIA),])
  
  # HAY QUE BORRAR EL ARCHIVO EXISTENTE!!!! O BUSCAR UNA FORMA DE SALTARSE LA SEGURIDAD
  # sudo rm ~/COVID19_C3/html/unam-app/map/leaflet/datos/mun_nodos_2*
    
#  setwd("~/COVID19_C3/html/unam-app/map/leaflet/datos/")
  # setwd("~/COVID19_C3/html/datos/")
  # writeOGR(mun, ".", "mun_nodos", driver="ESRI Shapefile") #also you were missing the driver argument
  
  # setwd("~/COVID19_C3/html/unam-app/map/leaflet/datos/")
  # writeOGR(mun, ".", "mun_nodos_3", driver="ESRI Shapefile") #also you were missing the driver argument
  
  # AUTOMATIZAR. ESTA INSRTUCCION EN EL SERVIDOR DEBE GUARDAR EL ARVHICO DIRECTAMENTE EN /sr/shiny-server/
  setwd("~/COVID19_C3/html/datos/") # setwd("/srv/shyni-server/")     
  require(leafletR)
  toGeoJSON(mun)
  
  # In terminal:  ADD TEXT IN THE FIRST LINE OF MUNGEOJSON, TO ASSIGN A VARIABLE FOR LEAFLET JS
  # sed -i '1s/^/<added text> /' file
  # IN THIS CASE WE ADD THE NAME OF THE VARIABLE: sed -i '1s/^/ var XDATA =  /' mun.geojson
  # 
  # sed -i '$s/$/ \n?> /' file
  
# IN THIS CASE WE ADD A SEMICOLON AT THE END OF THE GEOJSON FILE: sed -i '$s/$/ ; /' mun.geojson
  
# ADD A semicolon at the the end of the file

######################################################################
# DATOS DE LA APP CORONAVIRUS UNAM

Q<-read.csv("~/COVID19_C3/html/unam-app/covidcontacts_27072020.csv")
  
names(Q)

q<-data.frame(lat, lon)
lat<-round(Q$latitude, 2)
lon<-round(Q$longitude, 2)
q<-na.exclude(data.frame(lat, lon))

X<-table(paste(q$lon, q$lat, sep=" H "))

x<-as.data.frame(do.call(rbind, strsplit(names(X), " H ")))

x$lon<-as.numeric(as.character(x$V1))
  x$lat<-as.numeric(as.character(x$V2))
  x$reportes<-as.character(X)

d<-data.frame(latitude=x$lat, longitude=x$lon, reportes=as.character(x$reportes))
coronavirusapp<-d
toGeoJSON(coronavirusapp)
toGeoJSON(d)



######################################################################
      
      # gráfica de Rt para cada municipio
  
  
  
  
  
      
      
      
      
      
      
      
      
      
      
      
      
      
##########################################    
# MACHINE LEARNING (CART)

    # MAKE A DUMMY VARIABLE ON WHETHER INFECTIONS INCREASED OR DECREASED IN A DAY, COMPARED WITH THE DAY BEFORE.
    
      # CAMBIO DIARIO EN INFECCIONES
      a<-c(NA, cumsum(x4))
      b<-c(cumsum(x4), NA)
      #c<-(b-a)/sum(na.omit(a)) # cambio diario
      c<-b/a # proporció de los infectados que se acumuló cada día
      
      
      fx<-function(x) {
        a<-c(NA, cumsum(x4[x,]))+1
        b<-c(cumsum(x4[x,]), NA)+1
        c<-b/a # proporción de los infectados que se acumuló cada día
      }
      
      x<-c(1:nrow(x4))    
      I<-lapply(x, fx)

      

######################            
# GRAFICA DE INFECTADOS ACUMULADOS EN CADA MUNICIPIO
# #################### NOTA: PUEBLA TIENE UNA CURVA DE ACUMULADOS MUY!! PRONUNCIADA!
#       plot(spline(cumsum(x4[1,])), type="l", ylim=c(0, 13000))
# 
#       fp<-function(x) {
#         lines(spline(cumsum(x4[x,])))
#       }
# 
#       x<-c(1:nrow(x4))    
#       sapply(x, fp)
# 
# ##########################################
# RED LABORAL, BASE DE DATOS NET1.rds
    X<-read.csv("flujos.csv")
    
    factor=as.numeric(as.character(X$FACTOR))
    longit=1-(factor/max(factor))
    
    net1<-data.frame(from=as.character(X$ORIGEN)
                     , to=as.character(X$DESTINO)
                     , length=longit
    )
    
    mun<-read.csv("municipios.csv")
    from<-as.vector(paste(mun$NOM_MUN,mun$NOM_ENT, mun$MUN_OFICIA,  sep="-")[match(as.character(net1$from), as.vector(mun$MUN_OFICIA))])
    to<-as.vector(paste(mun$NOM_MUN, mun$NOM_ENT, mun$MUN_OFICIA,  sep="-")[match(as.character(net1$to), as.vector(mun$MUN_OFICIA))])
    
    net1$from.nom<-from
    net1$to.nom<-to
    
    saveRDS(net1, "NET1.rds")

    

    
    # IMPLEMENTED FROM: https://hydroecology.net/downloading-extracting-and-reading-files-in-r/
    # create a temporary directory
    td = tempdir()
    # create the placeholder file
    tf = tempfile(tmpdir=td, fileext=".zip")
    # download into the placeholder file
    download.file("http://epidemiologia.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", tf)
    # download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", tf) # cambio la dirección! 31/jul/2020
    
    # get the name of the first file in the zip archive
    fname = unzip(tf, list=TRUE)$Name[1]
    # unzip the file to the temporary directory
    unzip(tf, files=fname, exdir=td, overwrite=TRUE)
    # fpath is the full path to the extracted file
    fpath = file.path(td, fname)
    
    # stringsAsFactors=TRUE will screw up conversion to numeric!
    d = read.csv(fpath, header=TRUE, row.names=NULL, 
                 stringsAsFactors=FALSE) #, colClasses=c(rep("character", 35)))

    
