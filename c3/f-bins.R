# DESCIPCIÓN: FUNCION PARA CALCULAR LOS DECIELS DE UNA VARIABLE (z) EN UNA BASE DE DATOS CSV (X) CON UN NIVEL DE AGREGACIÓN (n)
# AUTOR: gegp[AT]ciencias.unam.mx
#
# X base de datos iter
# z nombre de la variable en X
# i nivel: entidad (ent), municipio (mun)

f.bins<-function(X, z, n){
  X.aggregate<-aggregate(X[z], list(X[,n]), sum)
  X.bins<-within(X.aggregate, decil <- as.integer(cut(X.aggregate[,z], quantile(X.aggregate[,z], probs=0:10/10), include.lowest=TRUE)))

  estadomapa<-X.bins
  latitud<-X$latitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  longitud<-X$longitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  altitud<-X$altitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  estadomapa<-X$nom_ent[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  ordenvalido<-as.vector(X.bins$Group.1)

  valor.min<-aggregate(X.bins[,z], list(X.bins$decil), min)
  valor.max<-aggregate(X.bins[,z], list(X.bins$decil), max)
  valor.min<-valor.min[match(X.bins$decil, valor.min$Group.1),2]
  valor.max<-valor.max[match(X.bins$decil, valor.max$Group.1),2]
  
  especievalida<-paste(z, "(", round(valor.min, 2), "% - ", round(valor.max,2), "%)", sep="")
  
  clasevalida<-rep("INEGI 2010", nrow(X.bins))
  familiavalida<-rep("Indicadores de Población", nrow(X.bins))
  diacolecta<-rep(1, nrow(X.bins))
  mescolecta<-rep(1, nrow(X.bins))
  aniocolecta<-rep(1, nrow(X.bins))
  idejemplar<-rep("COVID-19", nrow(X.bins))
  generovalido<-rep(z, nrow(X.bins))
  paismapa<-rep("México", nrow(X.bins))
  altitudmapa<-altitud
  reinovalido<-rep("Municipio", nrow(X.bins))
  phylumdivisionvalido<-rep("Demográfico", nrow(X.bins))
  categoriainfraespecievalida<-rep("libre", nrow(X.bins))
  proyecto<-rep("EPI-SPECIES", nrow(X.bins))
  
  

  data.frame(estadomapa
             , ordenvalido
             , clasevalida
             , familiavalida
             , diacolecta
             , mescolecta
             , aniocolecta
             , idejemplar
             , generovalido
             , paismapa
             , altitudmapa
             , reinovalido
             , phylumdivisionvalido
             , categoriainfraespecievalida
             , proyecto
             , especievalida
             , latitud
             , longitud
             , valores=X.bins[,2]
             , decil=X.bins[,3])
}
# 
