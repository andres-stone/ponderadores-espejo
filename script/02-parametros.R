
# ponderadores ir icl

# parametros
source("script/01-funciones.R")

# asignando nombres
nombres <- c("insumos", "output", "script", "ig")

# asignando nombres a carpeta
invisible(lapply(nombres, crear_carpetas))

# rutas

# encuesta estructural 2021
ruta_bbddeercl21 <- "insumos/bbdd_eercl2021_interna_excel.xlsx"

# encuesta mensual 2023
fecha <- "2024-04-04_con_var"
ruta_microdatos  <- glue("insumos/microdatos_{fecha}.xlsx")

# insumos calculo
ruta_eercl   <- "insumos/bbddeercl21.rds"
ruta_emrcl   <- "insumos/microdatos.rds"
ponderadores <- "output/ponderadores.xlsx"

# 
valor_aceptacion <- .002

# rol dador
meses_con_datos <- 7
meses_finales   <- c(10,11,12)
n_dadores       <- 4