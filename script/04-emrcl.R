
# ponderadores ir icl

# encuestra mensual 2023

# limpiando información previa
source("script/02-parametros.R")

# Lectura de microdatos (optimizar)
microdatos <- leer_excel(ruta_microdatos)

microdatos <- microdatos %>%
  mutate(grupo = as.numeric(substr(grupo,2,2))) %>% 
  select(mes, rol, categoria, tamano, sexo, grupo,
         nt, ho, he, ro, re, cl)

microdatos <- crear_cadenas(microdatos)
saveRDS(microdatos, "insumos/microdatos.rds")