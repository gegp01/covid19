# LEER BASE DE DATOS CON LA ID OFICIAL DE LOS MUNICIPIOS Y AGREGAR LA POBLACION TOTAL (pobtot) DEL CENSO DE 2010
# DATOS DE MUNICIPIOS
    mun<-read.csv("municipios.csv")

# DATOS DEMOGRÁFICOS (INEGI-2010)
    iter<-readRDS("iter_00_cpv2010.rds") # desde el servidor
    
    mun$id<-paste(mun$CVE_ENT, mun$CVE_MUN)
    iter$id<-paste(iter$entidad, iter$mun)
    iter$MUN_OFICIA<-mun$MUN_OFICIA[match(iter$id, mun$id)]
      
    pobtot.ag<-aggregate(iter$pobtot, list(iter$MUN_OFICIA), sum)
    pobtot<-pobtot.ag$x
    names(pobtot)<-pobtot.ag$Group.1
    
    mun$pobtot<-pobtot[match(mun$MUN_OFICIA, names(pobtot))]

# CORREGIR LOS NOMBRES
      # ref<-read.csv("~/COVID19_C3/html/datos_covid19/catalogo_municipios.csv"
      #               , colClasses=c(rep("character",3)))
#
#      ref<-read.csv("catalogo_municipios.csv"
#                    , colClasses=c(rep("character",3)))
#
#      ref$MUN_OFICIA<-paste(ref$CLAVE_ENTIDAD, ref$CLAVE_MUNICIPIO, sep="")
#      
#      # head(cbind(mun$NOM_MUN, ref$MUNICIPIO[match(mun$MUN_OFICIA, ref$MUN_OFICIA)]))
#      mun$MUNICIPIO<-ref$MUNICIPIO[match(mun$MUN_OFICIA, ref$MUN_OFICIA)]
#      
#      require(Hmisc)
#      mun$MUNICIPIO<-capitalize(tolower(mun$MUNICIPIO))
#      mun$NOM_MUN<-mun$MUNICIPIO

##########################################
# COMPILAR BASE DE DATOS: NODOS.rds
# COVID19_MX, OBTENER DATOS OFICIALES DESDE EL SITIO OFICIAL    
    
    source("https://gegp01.github.io/covid19/wget_covid19MX.R")    
    saveRDS(d, "datos_oficiales.rds")
    covid<-readRDS("datos_oficiales.rds")
    
    covid$FECHA_ACTUALIZACION[covid$FECHA_ACTUALIZACION=="9999-99-99"]<-NA
    covid$FECHA_ACTUALIZACION<-as.Date(covid$FECHA_ACTUALIZACION)
    covid$sintomas_julianos<-covid$FECHA_ACTUALIZACION-as.Date("2019-12-31")

    covid$FECHA_INGRESO[covid$FECHA_INGRESO=="9999-99-99"]<-NA
    covid$FECHA_INGRESO<-as.Date(covid$FECHA_INGRESO)
    covid$sintomas_julianos<-covid$FECHA_INGRESO-as.Date("2019-12-31")
    
    covid$FECHA_SINTOMAS[covid$FECHA_SINTOMAS=="9999-99-99"]<-NA
    covid$FECHA_SINTOMAS<-as.Date(covid$FECHA_SINTOMAS)
    covid$sintomas_julianos<-covid$FECHA_SINTOMAS-as.Date("2019-12-31")
    
    covid$FECHA_DEF[covid$FECHA_DEF=="9999-99-99"]<-NA
    covid$FECHA_DEF<-as.Date(covid$FECHA_DEF) 
    covid$defuncion_julianos<-covid$FECHA_DEF-as.Date("2020-01-01")
    
    
    covid$id<-paste(covid$ENTIDAD_RES, covid$MUNICIPIO_RES)
    mun$id<-paste(mun$CVE_ENT, mun$CVE_MUN)
    
    covid$municipio_oficial<-mun$MUN_OFICIA[match(covid$id, mun$id)]

# INFECTADOS ACUMULADOS EN TOTAL
    covid$infectado<-ifelse(covid$RESULTADO==1, 1, 0)
    I<-aggregate(covid$infectado, list(covid$municipio_oficial), sum)
    
# INFECTADOS ACUMULADOS EN LOS ULTIMOS 15 DIAS
    covid$infectados_15d<-ifelse(covid$sintomas_julianos-max(covid$sintomas_julianos)<=14, covid$infectado, NA)
    I_ultimos15<-aggregate(covid$infectados_15d, list(covid$municipio_oficial), sum)
    
# NEGATIVOS ACUMULADOS EN TOTAL
    covid$negativo<-ifelse(covid$RESULTADO==2, 1, 0)
    S<-aggregate(covid$negativo, list(covid$municipio_oficial), sum)

# NEGATIVOS ACUMULADOS EN LOS ULTIMOS 15 DIAS
    covid$negativos_15d<-ifelse(covid$sintomas_julianos-max(covid$sintomas_julianos)<=14, covid$negativo, NA)
    S_ultimos15<-aggregate(covid$negativos_15d, list(covid$municipio_oficial), sum)
    
    mun$infectados<-I$x[match(mun$MUN_OFICIA, I$Group.1)]
    mun$infectados[is.na(mun$infectados)]<-0
    
    mun$negativos<-S$x[match(mun$MUN_OFICIA, S$Group.1)]
    mun$negativos[is.na(mun$negativos)]<-0
    
    mun$infectados15d<-I_ultimos15$x[match(mun$MUN_OFICIA, I_ultimos15$Group.1)]
    mun$infectados15d[is.na(mun$infectados15d)]<-0

    mun$negativos15d<-S_ultimos15$x[match(mun$MUN_OFICIA, S_ultimos15$Group.1)]
    mun$negativos15d[is.na(mun$negativos15d)]<-0
    
# ESTADISTICA EPIDEMIOLOGICAS
    mun$infectados_10milhab<-(mun$infectados/mun$pobtot)*10000
    mun$infectados15d_10milhab<-(mun$infectados15d/mun$pobtot)*10000
    
# CALCULAR TASA DE CRECIMIENTO (INFECTADOS HOY/INFECTADOS AYER)
    x4<-table(covid$municipio_oficial[covid$infectado==1], covid$FECHA_SINTOMAS[covid$infectado==1])
    
# PROTOCOLO RECOMENDADO POR LA ORGANIZACION MUNDIAL DE LA SALUD (PAHO)
require(EpiEstim)
data("Flu2009") # SE UTILIZA LA DISTRIBUCION DE LA INFLUENZA PARA EL MODELO

    f1<-function(x) {
      t_start<-seq(2, length(colSums(x4))-15) 
      t_end<-t_start + 15
      y<-estimate_R(data.frame(x4[x,]), method="non_parametric_si"
                    , config = make_config(list(si_distr = Flu2009$si_distr
                                                , t_start = t_start
                                                , t_end = t_end)))  
                        mean(y[['R']][["Median(R)"]])
                      }

      x<-c(1:nrow(x4))
      Rt<-sapply(x, f1)

      names(Rt)<-rownames(x4)
      mun$Rt<-Rt[match(mun$MUN_OFICIA, names(Rt))]
      mun$Rt2<-ifelse(mun$infectados+mun$negativos >= 50, mun$Rt, NA) # ELIMINAR RtS BASADOS EN POCOS DATOS (<50)

# GUARDAR DATOS ACTUALIZADOS EN DISCO DEL SERVIDOR
      mun$ID<-paste(mun$NOM_MUN, mun$NOM_ENT, mun$MUN_OFICIA, sep="-")

# LEER DATOS DE GOOGLE MOBILITY REPORT
      G<-read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
      saveRDS(G, "google_mobility.rds")
      google<-readRDS("google_mobility.rds")
      
      gmx<-google[grep("Mex", google$country_region),]
      gmx$date<-as.Date(gmx$date)
      gmx$julianday<-gmx$date-as.Date("2019-12-31")
      gmx$entidad<-as.vector(gmx$sub_region_1)
      
################# Nota: hay que guardar "ent" en el servidor para el archivo data_compiler_c3.R
      ent<-read.csv("cve_entidad_gmx.csv", sep=",", header=F, colClasses=c(rep("character",3)))
      
      gmx$CVE_ENT<-ent$V2[match(gmx$entidad, ent$V3)]
      gmx.hoy<-gmx[gmx$julianday==max(gmx$julianday),]
      
      gmx.hoy.sorted<-gmx.hoy[match(mun$CVE_ENT, gmx.hoy$CVE_ENT),]
      table(gmx.hoy.sorted$CVE_ENT==mun$CVE_ENT) # checar que los datos en gmx.hoy.sorted estén en el orden de mu
      
      nodos<-data.frame(mun, gmx.hoy.sorted)
      saveRDS(nodos, "NODOS.rds")

