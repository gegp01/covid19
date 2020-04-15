
f.bins<-function(X, z, n){
  X.aggregate<-aggregate(X[z], list(X[,n]), mean)
  X.bins<-within(X.aggregate, decil <- as.integer(cut(X.aggregate[,z], quantile(X.aggregate[,z], probs=0:8/8), include.lowest=TRUE)))
  
  estadomapa<-X.bins[,1]
 # estadomapa<-X$nom_ent[match(as.vector(X.bins$Group.1), as.vector(X[,n]))]
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
  
  
  
