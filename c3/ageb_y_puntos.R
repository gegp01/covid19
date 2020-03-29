
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
#projection(loc) = CRS("+proj=longlat +datum=WGS84")

# LEER EL SHAPEFILE CON POLIGONOS EN EL DIRECTORIO: /ageb_mex/
ageb <- readOGR(dsn = "ageb_mex/", layer = "ageb_mex")
#projection(ageb) = CRS("+proj=longlat +datum=WGS84") # ASIGNAR LA PROYECCIÓN GEODESICA WGS84

# CRUZAR LA INFORMACIÓN
loc_in_ageb <- over(loc,ageb)
loc$ageb <- loc_in_ageb # AGREGAR LA INFORMACION DEL POLIGONO A LOS PUNTOS

# ESCRIBIR EL ARCHIVO COMO CSV
write.csv(loc, "output.csv")



