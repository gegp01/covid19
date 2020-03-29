
# SCRIPT PARA INCLUIR DATOS DE LOS POLIGONOS CON: ENTIDAD, MUNICIPIO, AGEB Y Ambito en una base de datos csv de puntos (en coordenadas)
# LIBRERIAS DE R NECESARIAS
require(sp)
require(rgdal)
require(raster)

# LEER DATOS
it<-read.csv("iter_00_cpv2010.csv")
it2<-it[is.na(it$longitud)==F,] # ELIMINAR LAS FILAS SIN COORDENADAS

# TRANSFORMAR LAS COORDENADAS DEL ARCHIVO CSV
it2$lon<-(it2$longitud/10000)*-1
it2$lat<-(it2$latitud/10000)

# ASIGNAR COORDENADAS
coordinates(it2) <- ~lon + lat

# DETERMINAR LA PROYECCION ADECUADA (WGS84)
projection(it2) = CRS("+proj=longlat +datum=WGS84")

# LEER EL SHAPEFILE CON POLIGONOS EN EL DIRECTORIO: /ageb_mex/
ageb <- readOGR(dsn = "ageb_mex/", layer = "ageb_mex")
projection(ageb) = CRS("+proj=longlat +datum=WGS84") # ASIGNAR LA PROYECCIÓN GEODESICA WGS84

# CRUZAR LA INFORMACIÓN
it2_in_ageb <- over(it2,ageb)
it2$ageb <- it2_in_ageb # AGREGAR LA INFORMACION DEL POLIGONO A LOS PUNTOS

# ESCRIBIR EL ARCHIVO COMO CSV
write.csv(it2, "output.csv")



