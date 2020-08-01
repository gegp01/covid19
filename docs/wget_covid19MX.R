# IMPLEMENTED FROM: https://hydroecology.net/downloading-extracting-and-reading-files-in-r/
# create a temporary directory
td = tempdir()
# create the placeholder file
tf = tempfile(tmpdir=td, fileext=".zip")
# download into the placeholder file
download.file("http://epidemiologia.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", tf)
# download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", tf) # cambio la dirección! 31/jul/2020

# get the name of the first file in the zip archive
fname = unzip(tf, list=TRUE)$Name[1]
# unzip the file to the temporary directory
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
# fpath is the full path to the extracted file
fpath = file.path(td, fname)

# stringsAsFactors=TRUE will screw up conversion to numeric!
d = read.csv(fpath, header=TRUE, row.names=NULL, 
             stringsAsFactors=FALSE)
