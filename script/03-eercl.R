
# ponderadores ir icl

# encuestra estructural 2021

# limpiando información previa
source("script/02-parametros.R")

# objetos a trabajar
bbddeercl21 <- leer_excel(ruta_bbddeercl21)

# encuesta estructural
columnas_seleccionadas <- c("pw", "rol", 
                            "categoria", "tamano", "sexo", "grupo",
                            "nt", 
                            "ho", "he", "ht", 
                            "ro", "re", "cl", "ri", "rt")

columnas_nuevas <- c("cadena_corta", "cadena_larga")

# depurando base: renombres, creacion de cadenas, seleccion de variables
bbddeercl21 <- renombre_eercl(bbddeercl21)
bbddeercl21 <- crear_cadenas(bbddeercl21)
bbddeercl21 <- seleccion_columnas(bbddeercl21)

saveRDS(bbddeercl21, "insumos/bbddeercl21.rds")