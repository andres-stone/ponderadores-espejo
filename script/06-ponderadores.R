
# ponderadores ir icl

# calculo ponderadores

# limpiando información previa
source("script/02-parametros.R")

ponderadores <- leer_excel(ponderadores)

# total
total <- ponderadores %>% 
  group_by(id_ponderador) %>% 
  summarise(W = sum(ponderador, na.rm = T))

# segun actividad
categoria <- ponderadores %>%
  group_by(categoria) %>%
  summarise(
    w_ir  = sum(ponderador[id_ponderador == "ir"], na.rm = TRUE),
    w_icl = sum(ponderador[id_ponderador == "icl"], na.rm = TRUE),
    w_horo = sum(ponderador[id_ponderador == "horo"], na.rm = TRUE),
    w_htcl = sum(ponderador[id_ponderador == "htcl"], na.rm = TRUE),
    w_htrore = sum(ponderador[id_ponderador == "htrore"], na.rm = TRUE)
  )

# segun sexo
sexo <- ponderadores %>%
  group_by(sexo) %>%
  summarise(
    w_ir  = sum(ponderador[id_ponderador == "ir"], na.rm = TRUE),
    w_icl = sum(ponderador[id_ponderador == "icl"], na.rm = TRUE),
    w_horo = sum(ponderador[id_ponderador == "horo"], na.rm = TRUE),
    w_htcl = sum(ponderador[id_ponderador == "htcl"], na.rm = TRUE),
    w_htrore = sum(ponderador[id_ponderador == "htrore"], na.rm = TRUE)
  )

# segun grupo
grupo <- ponderadores %>% 
  group_by(grupo) %>%
  summarise(
    w_ir  = sum(ponderador[id_ponderador == "ir"], na.rm = TRUE),
    w_icl = sum(ponderador[id_ponderador == "icl"], na.rm = TRUE),
    w_horo = sum(ponderador[id_ponderador == "horo"], na.rm = TRUE),
    w_htcl = sum(ponderador[id_ponderador == "htcl"], na.rm = TRUE),
    w_htrore = sum(ponderador[id_ponderador == "htrore"], na.rm = TRUE)
  )

# segun tamaño
tamano <- ponderadores %>% 
  group_by(tamano) %>%
  summarise(
    w_ir  = sum(ponderador[id_ponderador == "ir"], na.rm = TRUE),
    w_icl = sum(ponderador[id_ponderador == "icl"], na.rm = TRUE),
    w_horo = sum(ponderador[id_ponderador == "horo"], na.rm = TRUE),
    w_htcl = sum(ponderador[id_ponderador == "htcl"], na.rm = TRUE),
    w_htrore = sum(ponderador[id_ponderador == "htrore"], na.rm = TRUE)
  )

write_xlsx(ponderadores, "output/wt.xlsx")
write_xlsx(categoria,    "output/wt_categoria.xlsx")
write_xlsx(sexo,         "output/wt_sexo.xlsx")
write_xlsx(grupo,        "output/wt_grupo.xlsx")
write_xlsx(tamano,       "output/wt_tamano.xlsx")
