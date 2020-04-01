# DESCIPCIÓN: FUNCION PARA CALCULAR LOS DECIELS DE UNA VARIABLE (z) EN UNA BASE DE DATOS CSV (X) CON UN NIVEL DE AGREGACIÓN (n)
# AUTOR: gegp[AT]ciencias.unam.mx
#
# X base de datos iter
# z nombre de la variable en X
# i nivel: entidad (ent), municipio (mun)

f.bins<-function(X, z, n){
  X.aggregate<-aggregate(X[z], list(X[,n]), sum)
  X.bins<-within(X.aggregate, decil <- as.integer(cut(X.aggregate[,z], quantile(X.aggregate[,z], probs=0:10/10), include.lowest=TRUE)))
  
  valor.min<-aggregate(X.bins[,z], list(X.bins$decil), min)
  valor.max<-aggregate(X.bins[,z], list(X.bins$decil), max)
  
  valor.min<-valor.min[match(X.bins$decil, valor.min$Group.1),2]
  valor.max<-valor.max[match(X.bins$decil, valor.max$Group.1),2]
  
  tag<-paste("(", valor.min, "-", valor.max, ")", sep="")
  X.bins$tag<-tag
  colnames(X.bins)<-c(n, paste("variable:", z, sep=""), "label", "tag")
  print(X.bins)
}
