require(httr)
require(rjson)

json_body = '{"iterations": 1, "target_taxons":[{"taxon_rank":"species","value":"COVID-19 CONFIRMADO"}],"idtime":1600718392290,"apriori":false,"mapa_prob":false,"min_cells":1,"fosil":false,"lim_inf_validation":"2020-06-01","lim_sup_validation":"2020-05-30","lim_inf":"2020-05-01","lim_sup":"2020-05-31","date":false,"idtabla":"","grid_resolution":"mun","region":1,"get_grid_species":false,"with_data_score_cell":true,"with_data_freq":true,"with_data_freq_cell":true,"with_data_score_decil":true,"excluded_cells":[],"target_name":"targetGroup","covariables":[{"name":"GpoBio1","biotic":true,"merge_vars":[{"rank":"kingdom","value":"Movilidad","type":0,"level":"species"}],"group_item":1}],"decil_selected":[10]}'

# VERBO (url)
url.epipuma = 'http://covid19.c3.unam.mx/api/dev/niche/countsTaxonsGroup'

# query
out<-POST(url.epipuma, content_type_json(), body = json_body)


# NOMBRES DE MUNICIPIOS PARA CADA CELDA
require(rjson)
require(httr)

# json_body
source("https://gegp01.github.io/covid19/R/grid_epipuma.R")

# url: VERBO
url.nms = 'http://covid19.c3.unam.mx/api/dev//niche/especie/getColumnsGrid'

# query
ent.mun<-POST(url.nms, content_type_json(), body = json_body)



content(out)$cell_summary[[1]]$gridid

content(ent.mun)$data[1]

X<-content(out)$cell_summary
Y<-content(ent.mun)$data


f.score<-function(x) {data.frame(X[[x]]$score)}

x = 1:length(X)
score = unlist(sapply(x, f.score))

f.p_score<-function(x) {data.frame(X[[x]]$positive_score)}

x = 1:length(X)
positive_score = unlist(sapply(x, f.p_score))


f.n_score<-function(x) {data.frame(X[[x]]$negative_score)}

x = 1:length(X)
negative_score = unlist(sapply(x, f.n_score))

modelo<-data.frame(score, positive_score, negative_score)

# 
f1<-function(x) {
				X[[x]]$gridid
			}

x = 1:length(X)
grid.id = sapply(x, f1)

modelo$grid.id<-grid.id

# 
f2<-function(x) {
				as.vector(X[[x]]$grupo_riesgo)
			}

x = 1:length(X)
riesgo = sapply(x, f2)

modelo$riesgo<-riesgo





# AGREGAR LOS DATOS DEL GRIDID - MUNICIPIO

Y<-content(ent.mun)$data

f3<-function(x) {
				Y[[x]]$gridid_munkm
			}

x = 1:length(Y)
id2 = sapply(x, f3)


f.nom_mun<-function(x) {as.character(Y[[x]]$NOM_MUN)}

f.nom_ent<-function(x) {as.character(Y[[x]]$NOM_ENT)}

f.cve_mun<-function(x) {as.character(Y[[x]]$CVE_MUN)}

f.cve_ent<-function(x) {as.character(Y[[x]]$CVE_ENT)}


x = 1:length(Y)

CVE_ENT<-sapply(x, f.cve_ent)
CVE_MUN<-sapply(x, f.cve_mun)

NOM_ENT<-sapply(x, f.nom_ent)
NOM_MUN<-sapply(x, f.nom_mun)


d<-data.frame(NOM_MUN, NOM_ENT, CVE_MUN, CVE_ENT, MUN_OFICIA=paste(CVE_ENT, CVE_MUN, sep=""))


cell<-d[match(grid.id, id2),]


model0 = data.frame(cell, modelo)

# ADD POLIGON
require(rgdal)
mun<- readOGR(dsn = "~/COVID19_C3/municipios/", layer = "municipios_simpleX")

# MAKE COLORS
  require(leaflet)
 # Xpalette <- colorNumeric(palette=c('#FED976', '#BD0026'), domain=model0$score, na.color="transparent")

# Xpalette <- colorNumeric(palette=c('transparent', '#BD0026'), domain=model0$score, na.color="transparent")

Xpalette <- colorNumeric(palette=c('green', 'yellow', 'red'), domain=model0$score, na.color="transparent")

  model0$score_col<-Xpalette(model0$score)

# ADD DATA    
  mun@data = data.frame(mun@data, model0[match(mun$MUN_OFICIA, model0$MUN_OFICIA),])
  

# rREMOVE OLD FILES IN SERVER
#  system("rm epipuma.geojson")

  # AUTOMATIZAR. ESTA INSRTUCCION EN EL SERVIDOR DEBE GUARDAR EL ARVHICO DIRECTAMENTE EN /sr/shiny-server/
  # setwd("~/COVID19_C3/html/datos/") 
  #setwd("../")  

 require(leafletR)

 epipuma<-mun
 toGeoJSON(epipuma)

  # EDITAR EL ARCHIVO DESDE EL SISTEMA (Linux)
  system("sed -i '1s/^/ var modelo =  /' epipuma.geojson")
  system("sed -i '$s/$/ ; /' epipuma.geojson")
