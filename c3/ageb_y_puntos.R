
# SCRIPT PARA INCLUIR DATOS DE LOS POLIGONOS CON: ENTIDAD, MUNICIPIO, AGEB Y Ambito en una base de datos csv de puntos (en coordenadas)
# LIBRERIAS DE R NECESARIAS
require(sp)
require(rgdal)
require(raster)

# EN R ASIGNAR EL DIRECTORIO DE TRABAJO EN EL QUE ESTAN LOS SHAPEFILES DE AGEBS Y LOCALIDADES 
setwd("mi directorio")

#LEER DATOS DE LOCALIDADES DE CONABIO
loc<- readOGR(dsn = "urbrloc10gw/", layer = "urbrloc10gw")

# DETERMINAR LA PROYECCION ADECUADA (WGS84)
#projection(loc) = CRS("+proj=longlat +datum=WGS84") # NO SE USA SI LA PROYECCION ES ADECUADA

# LEER EL SHAPEFILE CON POLIGONOS EN EL DIRECTORIO: /ageb_mex/
ageb <- readOGR(dsn = "ageb_mex/", layer = "ageb_mex") 
#projection(ageb) = CRS("+proj=longlat +datum=WGS84") # ASIGNAR LA PROYECCIÓN GEODESICA WGS84 | NO SE USA SI LA PROYECCION ES ADECUADA

# CRUZAR LA INFORMACIÓN
loc_in_ageb <- over(loc,ageb)
loc$ageb <- loc_in_ageb # AGREGAR LA INFORMACION DEL POLIGONO A LOS PUNTOS

# ESCRIBIR EL ARCHIVO COMO CSV
write.csv(loc, "output.csv")

##################################################################
# EJERCICIO: CALCULAR EL NUMERO DE HABITANTES POR AGEB, MUNICIPIO Y ENTIDAD, EN EL AMBITO RURAL
#
# LOS DATOS PARA CALCULAR ESTO ESTAN EN LA VARIABLE POBTOT DEL SHAPEFILE ageb_mex.shp 
#
# 1. LEER DATOS EN CSV PARA TRABAJAR MAS RÁPÍDO CON DATOS EN CSV
loc<-read.csv("output.csv")

# 2. SELECCIONA SOLO CASOS RURALES
loc.rural<-loc[loc$POBTOT<=2499,] # FILTRAR POR NUMERO DE HABITANTES <2.500

# NOTA-VERIFICAR: El CLASIFICADOR "Ambito:{Rural, Urbano}" DEL SHAPEFILE DE AGEBs NO CONSIDERA A ALGUNAS POBLACIONES (<2500 habitantes) COMO RURALES.
# a<-loc[loc$ageb.Ambito=="Rural",] # FILTRAR POR NUMERO DE HABITANTES <2.500
# PRUEBA QUE ESTA RESTA ES DISTINTA DE CERO, HAY 2297 LOCALIDADES DE MAS EN loc.rural COMPARADA CON EL SUBCONJUNTO a
# dim(loc.rural)-dim(a)

# 3. SUMA POBTOT EN CADA AGEB, MUNICIPIO Y ENTIDAD.
pobl.rural.entidad<-aggregate(loc.rural$POBTOT, list(loc.rural$NOM_ENT), sum)
colnames(pobl.rural.entidad)<-c("NOM_ENT", "pobtot.rural")

pobl.rural.municipio<-aggregate(loc.rural$POBTOT, list(loc.rural$CVE_MUN), sum)
colnames(pobl.rural.municipio)<-c("CVE_MUN", "pobtot.rural")

pobl.rural.ageb<-aggregate(loc.rural$POBTOT, list(loc.rural$ageb.CVE_AGEB), sum)
colnames(pobl.rural.ageb)<-c("CVE_AGEB", "pobtot.rural")

# 4. ESCRIBIR LOS ARCHIVOS CSV CON LOS DATOS AGREGADOS
write.csv(pobl.rural.ageb, "pobl-rural-ageb.csv")
write.csv(pobl.rural.entidad, "pobl-rural-entidad.csv")
write.csv(pobl.rural.municipio, "pobl-rural-municipio.csv")


