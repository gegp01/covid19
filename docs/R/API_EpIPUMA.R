# DESCRIPCION: Instrucciones en R para conectarse a EpI-PUMA y generar un modelo descrito en json_modelo.
#
# En este ejemplo, el modelo es P(municipios con más contagios que los demás ~ variables de conectividad)
# La salida del modelo se asocia al indice de grid de EpiPUMA y a poligonos de cada municipio.
# Al final de las instrucciones se genera un mapa epipuma.geojson, que se despliega en epipuma.html
#
# Autor: Gabriel E. García Peña
# Institución: Centro de Ciencias de la Complejidad (C3), Universidad Nacional Autónoma de México.
#
###############3
# LIBRERIAS NECESARIAS

require(httr)
require(rjson)
require(rgdal)
require(leaflet)

# DETERMINAR EL MODELO EN mod_json

# asignar tiempo de entrenamiento y validación


hoy = Sys.Date()

validacion_end = as.character(hoy)
validacion_start = as.character(hoy - 30)

entrenamiento_end = as.character(hoy-30)
entrenamiento_start = as.character(hoy-60)


x<-list(iterations = 1
		, target_taxons = c(list(taxon_rank = "species", value = "COVID-19 CONFIRMADO"))
		, idtime = 1600718392290
		, apriori = FALSE
		, mapa_prob = FALSE
		, min_cells = 1
		, fosil = FALSE
		, lim_inf_validation = validacion_start
		, lim_sup_validation = validacion_end
		, lim_inf = entrenamiento_start
		, lim_sup = entrenamiento_end
		, date = FALSE
		, idtabla = ""
		, grid_resolution = "mun"
		, region = 1		
		, get_grid_species = FALSE
		, with_data_score_cell = TRUE
		, with_data_freq = TRUE
		, with_data_freq_cell = TRUE
		, with_data_score_decil = TRUE
		, excluded_cells = NULL 				# agregar []
		, target_name = "targetGroup"
		, covariables = list(name = "GpoBio1" 	# agregar []
							, biotic = TRUE
							, merge_vars = list(rank ="kingdom" # agregar []
												, value = "Movilidad"
												, type = 0
												, level = "species")

							, group_item = 1)
		, decil_selected = list(10)
		)

X<-rjson::toJSON(x)
#apply(do.call(rbind, strsplit(c(x, mod_json), "")), 1, function(x){length(unique(x[!x %in% "_"])) == 1})
# CAMBIAR A MANO!!! LOS CORCHETES PARA LAS LISTAS INTERNAS [{}], Y PARA: excluded_cells = []
X
w = "{\"iterations\":1,\"target_taxons\":[{\"taxon_rank\":\"species\",\"value\":\"COVID-19 CONFIRMADO\"}],\"idtime\":1600718392290,\"apriori\":false,\"mapa_prob\":false,\"min_cells\":1,\"fosil\":false,\"lim_inf_validation\":\"2020-11-15\",\"lim_sup_validation\":\"2020-12-15\",\"lim_inf\":\"2020-09-16\",\"lim_sup\":\"2020-10-16\",\"date\":false,\"idtabla\":\"\",\"grid_resolution\":\"mun\",\"region\":1,\"get_grid_species\":false,\"with_data_score_cell\":true,\"with_data_freq\":true,\"with_data_freq_cell\":true,\"with_data_score_decil\":true,\"excluded_cells\":[],\"target_name\":\"targetGroup\",\"covariables\":[{\"name\":\"GpoBio1\",\"biotic\":true,\"merge_vars\":[{\"rank\":\"kingdom\",\"value\":\"Movilidad\",\"type\":0,\"level\":\"species\"}],\"group_item\":1}],\"decil_selected\":[10]}"

# mod_json = '{"iterations": 1, "target_taxons":[{"taxon_rank":"species","value":"COVID-19 CONFIRMADO"}],"idtime":1600718392290,"apriori":false,"mapa_prob":false,"min_cells":1,"fosil":false,"lim_inf_validation":"2020-06-01","lim_sup_validation":"2020-05-30","lim_inf":"2020-05-01","lim_sup":"2020-05-31","date":false,"idtabla":"","grid_resolution":"mun","region":1,"get_grid_species":false,"with_data_score_cell":true,"with_data_freq":true,"with_data_freq_cell":true,"with_data_score_decil":true,"excluded_cells":[],"target_name":"targetGroup","covariables":[{"name":"GpoBio1","biotic":true,"merge_vars":[{"rank":"kingdom","value":"Movilidad","type":0,"level":"species"}],"group_item":1}],"decil_selected":[10]}'


# Definir el verbo (la direccion url para conectarse al backend de EpI-PUMA)
	url.epipuma = 'http://covid19.c3.unam.mx/api/dev/niche/countsTaxonsGroup'

# Hacer la petición a EpI-PUMA
	out<-POST(url.epipuma, content_type_json(), body = json_body)

# INDEXAR LOS DATOS DEL MUNICIPIO CON RESPECTO A SU NUMERO DE CELDA
# NOMBRES DE MUNICIPIOS PARA CADA CELDA

# Definir el json body
	source("https://gegp01.github.io/covid19/R/grid_epipuma.R")

# Definir el verbo (url)
	url.nms = 'http://covid19.c3.unam.mx/api/dev//niche/especie/getColumnsGrid'

# Pedir a EpI-PUMA que devuelva el indice de las celdas en la cuadrícula (grid, el conjunto de poligonos municipales).
	ent.mun<-POST(url.nms, content_type_json(), body = json_body)

# Leer los contenidos de las peticiones a EpI-PUMA (out y ent.mun) 
#content(out)$cell_summary[[1]]$gridid
#content(ent.mun)$data[1]
	X<-content(out)$cell_summary
#	Y<-content(ent.mun)$data

# Leer los datos de score
	f.score<-function(x) {data.frame(X[[x]]$score)}
	x = 1:length(X)
	score = unlist(sapply(x, f.score))

# Leer los datos de score positivo: La suma de los scores > 0 en el conjunto de predictores (i.e. variables de movilidad)
	f.p_score<-function(x) {data.frame(X[[x]]$positive_score)}
	x = 1:length(X)
	positive_score = unlist(sapply(x, f.p_score))

# Leer los datos de score negativo: La suma de los scores < 0 en el conjunto de predictores (i.e. variables de movilidad)
	f.n_score<-function(x) {data.frame(X[[x]]$negative_score)}
	x = 1:length(X)
	negative_score = unlist(sapply(x, f.n_score))

# Leer el identificador de la celda en la cuadricula, que a los que corresponden los datos del modelo.
	f1<-function(x) {X[[x]]$gridid}
	x = 1:length(X)
	grid.id = sapply(x, f1)

# Leer el grupo de riesgo
	f2<-function(x) {as.vector(X[[x]]$grupo_riesgo)}
	x = 1:length(X)
	riesgo = sapply(x, f2)

# Juntar los datos del modelo en un data.frame
	modelo<-data.frame(score, positive_score, negative_score, grid.id, riesgo)


# ASIGNAR DATOS DEL GRIDID A CADA MUNICIPIO
# Leer datos de indentificación de cada celda en la cuadricula (i.e. conjunto de poligonos de los municipios)
	Y<-content(ent.mun)$data

# Funciones para extraer los datos de Y
	f.munkm<-function(x) {Y[[x]]$gridid_munkm}
	f.nom_mun<-function(x) {as.character(Y[[x]]$NOM_MUN)}
	f.nom_ent<-function(x) {as.character(Y[[x]]$NOM_ENT)}
	f.cve_mun<-function(x) {as.character(Y[[x]]$CVE_MUN)}
	f.cve_ent<-function(x) {as.character(Y[[x]]$CVE_ENT)}

# Extrae los datos
	x = 1:length(Y)
	id2 = sapply(x, f.munkm)
	CVE_ENT<-sapply(x, f.cve_ent)
	CVE_MUN<-sapply(x, f.cve_mun)
	NOM_ENT<-sapply(x, f.nom_ent)
	NOM_MUN<-sapply(x, f.nom_mun)

# juntar todos los datos de Y en el data.frame d
	d<-data.frame(NOM_MUN, NOM_ENT, CVE_MUN, CVE_ENT, MUN_OFICIA=paste(CVE_ENT, CVE_MUN, sep=""))

# organizar d en correspondencia con el indice de las celdas en el modelo (grid.id)
	cell<-d[match(grid.id, id2),]

# Juntar los datos en el data.frame modelo
	model0 = data.frame(cell, modelo)

# ARREGLAR LOS NOMBRES DEL MUNICIPIO Y DE LA ENTIDAD FEDERATIVA UTILIZANDO EL ARCHIVO DE REFERENCIA ref
	ref<-read.csv("https://gegp01.github.io/clasificador_Bayes/R/datos_censo2010.csv"
		      , colClasses=c(rep("character",8)))

	ref$pobtot<-as.numeric(ref$pobtot)
	model0$MUNICIPIO = ref$MUNICIPIO[match(model0$MUN_OFICIA, ref$MUN_OFICIA)]
	model0$ENTIDAD = ref$ENTIDAD_FEDERATIVA[match(model0$MUN_OFICIA, ref$MUN_OFICIA)]

# Agregar datos del Censo de Población y Vivienda (INEGI 2010) 
	model0$pob2010 = ref$pobtot[match(model0$MUN_OFICIA, ref$MUN_OFICIA)]

# Eliminar las columnas NOM_ENT y NOM_MUN del data.frame model0. Son redundantes con las nuevas variables ENTIDAD y MUNICIPIO
	model0 = model0[, -1*(1:2)]

# ASIGNAR POLIGONOS PARA GENERAR UN MAPA
# libreria y polígonos
	require(rgdal)
	mun<- readOGR(dsn = "~/COVID19_C3/municipios/", layer = "municipios_simpleX")

# ASIGNAR COLORES DEL SEMÁFORO
  require(leaflet)
 	# Xpalette <- colorNumeric(palette=c('#FED976', '#BD0026'), domain=model0$score, na.color="transparent")
	# Xpalette <- colorNumeric(palette=c('transparent', '#BD0026'), domain=model0$score, na.color="transparent")
	#Xpalette <- colorNumeric(palette=c('green', 'yellow', 'red'), domain=model0$score, na.color="transparent")

# Indice numérico para la variable "riesgo"
	model0$riesgo = as.vector(model0$riesgo)
	model0$riesgo.num = model0$riesgo
	model0$riesgo.num[model0$riesgo.num == "Muy alto"]<-5
	model0$riesgo.num[model0$riesgo.num == "Alto"]<-4
	model0$riesgo.num[model0$riesgo.num == "Mediano"]<-3
	model0$riesgo.num[model0$riesgo.num == "Bajo"]<-2
	model0$riesgo.num[model0$riesgo.num == "Muy bajo"]<-1

	model0$riesgo.num<-as.numeric(model0$riesgo.num)

# GENERAR CATEGORIAS DE COLOR CON BASE LAS 5 CATEGORIAS DE RIESGO
	Xpalette <- colorNumeric(palette=c('green', 'yellow', 'red'), domain=c(1:5), na.color="transparent")
# Asignar los colores 
	model0$semaforo<-Xpalette(model0$riesgo.num)

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
