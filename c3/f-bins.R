# DESCIPCIÓN: FUNCION PARA CALCULAR LOS DECIELS DE UNA VARIABLE (z) EN UNA BASE DE DATOS CSV (X) CON UN NIVEL DE AGREGACIÓN (n)
# AUTOR: gegp[AT]ciencias.unam.mx
#
# X base de datos iter
# z nombre de la variable en X
# i nivel: entidad (ent), municipio (mun)

d<-read.csv("https://gegp01.github.io/covid19/c3/diccionario.csv") # diccionario de variables

f.bins<-function(X, z, n){
  X.aggregate<-aggregate(X[z], list(X[,n]), mean)
  X.bins<-within(X.aggregate, decil <- as.integer(cut(X.aggregate[,z], quantile(X.aggregate[,z], probs=0:10/10), include.lowest=TRUE)))
  
#  estadomapa<-X.bins[,1]
  estadomapa<-X$nom_ent[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  latitud<-X$latitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  longitud<-X$longitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  altitud<-X$altitud[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
  altitud<-ifelse(is.numeric(altitud)==T, altitud, 9999)
  
  estadomapa<-X$nom_ent[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
#  ordenvalido<-as.vector(X.bins$Group.1)
  
  valor.min<-aggregate(X.bins[,z], list(X.bins$decil), min)
  valor.max<-aggregate(X.bins[,z], list(X.bins$decil), max)
  valor.min<-valor.min[match(X.bins$decil, valor.min$Group.1),2]
  valor.max<-valor.max[match(X.bins$decil, valor.max$Group.1),2]
  
#  especievalida<-paste(z, " (", round(valor.min, 4)*100, "%-", round(valor.max,4)*100, "%)", sep="")
  especievalida<-paste(round(valor.min, 3)*100, "%:", round(valor.max,3)*100, "%", sep="")
  
  clasevalida<-rep("Variables para todas las localidades habitadas", nrow(X.bins))
  
  nms.d<-tolower(d$variable.1)
  d$variable.1[match(nms.X, nms.d)]
  familiavalida<-rep(d$familia[match(z, nms.d)], nrow(X.bins))
#  familiavalida<-rep("Población", nrow(X.bins))
  
  
  diacolecta<-rep(1, nrow(X.bins))
  ordenvalido<-rep("Indicadores de Poblacion y Vivienda", nrow(X.bins))
  mescolecta<-rep(1, nrow(X.bins))
  aniocolecta<-rep(2010, nrow(X.bins))
  idejemplar<-rep("COVID-19", nrow(X.bins))
#  generovalido<-rep(z, nrow(X.bins))
  generovalido<-X.bins[,1]
  paismapa<-rep("México", nrow(X.bins))
  altitudmapa<-altitud
  reinovalido<-rep("Demográficos", nrow(X.bins))
  phylumdivisionvalido<-rep("Censo de Poblacion y Vivienda 2010", nrow(X.bins))
#  categoriainfraespecievalida<-rep(z, nrow(X.bins))
  categoriainfraespecievalida<-rep(NA, nrow(X.bins))
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

## 
