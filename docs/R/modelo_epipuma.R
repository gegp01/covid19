require(httr)
require(rjson)

json_body = '{"iterations": 1, "target_taxons":[{"taxon_rank":"species","value":"COVID-19 CONFIRMADO"}],"idtime":1600718392290,"apriori":false,"mapa_prob":false,"min_cells":1,"fosil":false,"date":false,"idtabla":"","grid_resolution":"mun","region":1,"get_grid_species":false,"with_data_score_cell":true,"with_data_freq":true,"with_data_freq_cell":true,"with_data_score_decil":true,"excluded_cells":[],"target_name":"targetGroup","covariables":[{"name":"GpoBio1","biotic":true,"merge_vars":[{"rank":"kingdom","value":"Movilidad","type":0,"level":"species"}],"group_item":1}],"decil_selected":[10]}'

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



f0<-function(x) {	
	data.frame(X[[x]])
	}

x = 1:length(X)
L = sapply(x, f0)

modelo<-as.data.frame(t(L))[,c("score", "positive_score", "negative_score")]


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


f4<-function(x) {
				Y[[x]][c("NOM_MUN", "CVE_MUN", "NOM_ENT", "CVE_ENT")]
			}

x = 1:length(Y)
d = sapply(x, f4)

f5<-function(x){
	as.vector(d[,match(id1[1], id2)])
}

D = sapply(x, f5)


cell<-as.data.frame(t(D))

MUN_OFICIA = as.character(paste(cell$CVE_ENT, cell$CVE_MUN, sep=""))

model0 = data.frame(cell, modelo, MUN_OFICIA)
