# OBTENER DATOS OFICIALES
# COVID19_MX, BASE DE DATOS: NODOS.rds
source("https://gegp01.github.io/covid19/wget_covid19MX.R")
      saveRDS(covid, "datos_oficiales.rds") 
covid<-readRDS("datos_oficiales.rds")

# CARGAR DATOS DE REFERENCIA
# setwd("~/COVID19_C3/html/red_laboral")
##########################################   
  ##########################################
# INTEGRAR Y COMPILAR DATOS EPIDEMIOLÓGICOS


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
      
      covid$municipio_oficial<-mun$MUN_OFICIA[match(covid$id, mun$MUN_OFICIA)] 
      
      covid$infectado<-ifelse(covid$RESULTADO==1, 1, 0)
      I<-aggregate(covid$infectado, list(covid$municipio_oficial), sum)
  
      covid$ultimos15dias<-ifelse(max(covid$sintomas_julianos)-covid$sintomas_julianos<=15, 1, 0)

      covid$ultimos30dias<-ifelse(max(covid$sintomas_julianos)-covid$sintomas_julianos<=30, 1, 0)
      
      covid$infectados_15d<-covid$infectado*covid$ultimos15dias
      covid$infectados_30d<-covid$infectado*covid$ultimos30dias
 
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
      mun$infectados_10milhab<-(mun$infectados/mun$pobtot)*10000
      mun$infectados15d_10milhab<-(mun$infectados15d/mun$pobtot)*10000
      mun$infectados30d_10milhab<-(mun$infectados30d/mun$pobtot)*10000
      
      deceso<-ifelse(is.na(covid$defuncion_julianos), 0, 1)
      covid$deceso_positivo<-ifelse(covid$RESULTADO==1, deceso, 0)
      
      death<-aggregate(covid$deceso_positivo, list(covid$municipio_oficial), sum)
      mun$deceso_positivo<-death$x[match(mun$MUN_OFICIA, death$Group.1)]
      mun$deceso_positivo_10milhab<-(mun$deceso_positivo/mun$pobtot)*10000
      
  # CALCULAR TASA DE CRECIMIENTO (INFECTADOS HOY/INFECTADOS AYER)
  # los datos se organizan en una matriz de [municipios, fecha_síntomas]
      x4<-table(covid$municipio_oficial[covid$infectado==1], covid$FECHA_SINTOMAS[covid$infectado==1])
      
  # EpiEstim: PROTOCOLO RECOMENDADO POR LA ORGANIZACION MUNDIAL DE LA SALUD PARA ESTIMAR LA TASA DE CRECIMIENTO DE CONTAGIOS EN EL TIEMPO (Rt)
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

      # HACER UNA FUNCION PARA APLICAR EL PROTOCOLO A CADA MUNICIPIO
        f1<-function(x) {
          y<-estimate_R(data.frame(x4[x,]), method="parametric_si"
                        , config = make_config(list(
                          mean_si = 4.8
                          , std_si = 2.3
                          , t_start = t_start
                          , t_end = t_end
                          ))
                        )
              mean(y[['R']][["Median(R)"]])
        }

# APLICAR LA FUNCION f1 A CADA FILA DE LA MATRIZ x4 
      x<-c(1:nrow(x4))
      Rt<-sapply(x, f1)
      names(Rt)<-rownames(x4)
# Rt CONTIENE LOS VALORES DE Rt PARA CADA MUNICIPIO
  
      mun$Rt<-Rt[match(mun$MUN_OFICIA, names(Rt))]

# ELIMINAR VALORES DE Rt que fueron estimados con menos de 20 observaciones (sospechosos positivos + negativos)
      mun$Rt2<-ifelse(mun$infectados+mun$negativos >= 20, mun$Rt, NA) # eliminamos los estimados de Rt con pocos casos

# ASIGNAR UNA VARIABLE PARA IDENTIFICAR CASOS UÚNICOS DE MUNICIPIO Y LA ENTIDAD
      mun$ID<-paste(mun$NOM_MUN, mun$NOM_ENT, mun$MUN_OFICIA, sep="-")

# AGREGAR LA FECHA DE ACTUALIZACIÓN DE LOS DATOS
      mun$FECHA_ACTUALIZACION<-rep(covid$FECHA_ACTUALIZACION[1], nrow(mun))
      

# DATOS DE MOVILIDAD: GOOGLE      
    # wget https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv
    # G<-read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv") # esto no es muy eficiente. Es mas facil desde el shell wget...

#    G<-read.csv("~/COVID19_C3/html/datos/Global_Mobility_Report.csv")
    G<-read.csv("Global_Mobility_Report.csv") # en srv/shiny-server/

    nmG<-paste("google_mobility_", Sys.Date(), ".rds", sep="")
    saveRDS(G, paste("datos/", nmG, sep=""))
    # google<-readRDS("~/COVID19_C3/html/datos/google_mobility.rds")
    google<-readRDS(paste("datos/", nmG , sep=""))

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
      saveRDS(nodos, "datos/NODOS.rds")

# HACER ANALISIS CON EL CLASIFICADOR DE BAYES (NAIVE)

  # SELECCIONAR DATOS EN LOS ÚLTIMOS 20 DÍAS
  D<-covid[covid$FECHA_SINTOMAS>=max(covid$FECHA_SINTOMAS)-20,]
  
  # identidad de los casos
  nms<-unique(D$ID_REGISTRO)
  
  # El número de sospechosos en ese municipio
  N<-length(nms)
  
  # El número de casos con covid en ese municipio
  Ncovid<-table(D$municipio_oficial[D$infectado==1])
  
  # El número de casos sospechosos de ese municipio 
  NH<-table(D$municipio_oficial)
  
  Ncovid.sorted<-Ncovid[match(names(NH), names(Ncovid))]
  
  Z<-data.frame(cbind(NH, Ncovid.sorted))
  Z$municipio_oficial<-as.vector(rownames(Z))
  names(Z)<-c("sospechosos", "confirmados", "municipio_oficial")
  
  P.covid.mun<-Z$confirmados/Z$sospechosos
  names(P.covid.mun)<-Z$municipio_oficial
   
  P.covid<-sum(Ncovid)/N
  
  numerador<-(Z$sospechosos*(P.covid.mun-P.covid))
  denominador<-(Z$sospechosos*P.covid*(1-P.covid))^0.5
  
  epsilon<-numerador/denominador
  names(epsilon)<-rownames(Z)
  
  epsilon<-na.exclude(epsilon)
  
##############################
# GENERAR POLIGONO GEOJSON 
library(rgdal)
        
#  mun<- readOGR(dsn = "~/COVID19_C3/municipios/", layer = "municipios_simpleX")
  mun<- readOGR(dsn = "municipios/", layer = "municipios_simpleX")
  D<-readRDS("datos/NODOS.rds")
 
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
  
  #setwd("~/COVID19_C3/html/datos/") # setwd("/srv/shyni-server/")     
  require(leafletR)
  toGeoJSON(mun)
  
      # DESDE LA TERMINAL LINUX, HAY QUE AGREGAR UN TEXTO AL INICIO Y AL FINAL DEL ARCHIVO Mun.geojson, PARA QUE SEA PROCESABLE POR EL SISTEMA.
      # ESTO SE LOGRA CON: sed -i '1s/^/<added text> /' file
      # EN ESTE CASE: 
      # sed -i '1s/^/ var XDATA =  /' mun.geojson
      # PARA AGREGA REL TEXTO (;) AL FINAL DEL ARCHIVO: sed -i '$s/$/ \n?> /' file
      # EN ESTE CASO: sed -i '$s/$/ ; /' mun.geojson

######################################################################
# DATOS DE LA APP CORONAVIRUS UNAM
#     Q<-read.csv("~/COVID19_C3/html/unam-app/covidcontacts_27072020.csv") 
#     names(Q)

#     q<-data.frame(lat, lon)
#     lat<-round(Q$latitude, 2)
#     lon<-round(Q$longitude, 2)
#     q<-na.exclude(data.frame(lat, lon))

#     X<-table(paste(q$lon, q$lat, sep=" H "))

#     x<-as.data.frame(do.call(rbind, strsplit(names(X), " H ")))
#     x$lon<-as.numeric(as.character(x$V1))
#     x$lat<-as.numeric(as.character(x$V2))
#     x$reportes<-as.character(X)
#
# d<-data.frame(latitude=x$lat, longitude=x$lon, reportes=as.character(x$reportes))
# coronavirusapp<-d
# toGeoJSON(coronavirusapp)
# toGeoJSON(d)



######################################################################
      
