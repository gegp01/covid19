
mun<-read.csv("https://gegp01.github.io/covid19/c3/municipios_cve_latlon.csv") # diccionario de variables

f.bins<-function(X, z, n){
  X.aggregate<-aggregate(X[z], list(X[,n]), sum)
  X.bins<-within(X.aggregate, decil <- as.integer(cut(X.aggregate[,z], quantile(X.aggregate[,z], probs=0:10/10), include.lowest=TRUE)))
  
  # estadomapa<-X.bins[,1]
  cve_ent<-X$ENTIDAD_RES[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  cve_mun<-X$MUNICIPIO_RES[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  estadomapa<-as.vector(mun$nom_ent)[match(cve_ent, mun$CLAVE_ENTIDAD)]
  
  # latitud<-mun$lat[match(cve_ent, mun$CLAVE_ENTIDAD)]
  # longitud<-mun$lon[match(cve_ent, mun$CLAVE_ENTIDAD)]

  # old
  # latitud<-X$latitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  # longitud<-X$longitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  # altitud<-X$altitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  # altitud<-ifelse(is.numeric(altitud)==T, altitud, 9999)
  
  # estadomapa<-X$nom_ent[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  #  ordenvalido<-as.vector(X.bins$Group.1)
  
  valor.min<-aggregate(X.bins[,z], list(X.bins$decil), min)
  valor.max<-aggregate(X.bins[,z], list(X.bins$decil), max)
  valor.min<-valor.min[match(X.bins$decil, valor.min$Group.1),2]
  valor.max<-valor.max[match(X.bins$decil, valor.max$Group.1),2]
  
  data.frame(estadomapa
             , valor.min
             , valor.max 
             , valores=X.bins[,2]
             , decil=X.bins[,3])
}
  
