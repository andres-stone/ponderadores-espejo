
# ponderadores ir icl

# comparacion IG

# limpiando información previa
source("script/02-parametros.R")

# ponderadores ad
ponderadores_ad <- leer_excel("output/wt.xlsx")
categoria_ad    <- leer_excel("output/wt_categoria.xlsx")
sexo_ad         <- leer_excel("output/wt_sexo.xlsx")
grupo_ad        <- leer_excel("output/wt_grupo.xlsx")
tamano_ad       <- leer_excel("output/wt_tamano.xlsx")

# ponderadoes ig
ponderadores_ig <- leer_excel("ig/tbl_wt.xlsx")
categoria_ig    <- leer_excel("ig/tbl_wt_categoria.xlsx")
sexo_ig         <- leer_excel("ig/tbl_wt_sexo.xlsx")
grupo_ig        <- leer_excel("ig/tbl_wt_grupo.xlsx")
tamano_ig       <- leer_excel("ig/tbl_wt_tamano.xlsx")

ponderadores_ig <- ponderadores_ig %>% 
  mutate(cadena_corta = paste0(tamano, categoria, sexo, grupo))

# comparando general
ponderadores_ad %>% summarise(n_distinct(cadena_corta))
ponderadores_ig %>% summarise(n_distinct(cadena_corta))

ponderadores_ad <- ponderadores_ad %>% 
  select(cadena_corta, id_ponderador, ponderador) %>% 
  rename(ponderador_a = ponderador)

ponderadores_ig <- ponderadores_ig %>% 
  select(cadena_corta, id_ponderador, ponderador) %>% 
  rename(ponderador_i = ponderador)

ponderadores <- ponderadores_ad %>% 
  inner_join(ponderadores_ig, by = c("cadena_corta", "id_ponderador"))

ponderadores %>% summarise(n_distinct(cadena_corta))

write_xlsx(ponderadores, "output/diferencias_ponderadores.xlsx")
