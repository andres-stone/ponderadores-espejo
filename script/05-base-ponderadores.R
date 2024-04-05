
# ponderadores ir icl

# calculo base ponderadores

# limpiando información previa
source("script/02-parametros.R")

# objetos a trabajar
eercl <- leer_rds(ruta_eercl)
emrcl <- leer_rds(ruta_emrcl)

# 1. información de la encuesta estructural 2021
# nt > 0 -> ro,ho,cl,rt mayores que 0
eercl <- depurando_nt(eercl)

# 2. participacion de cadenas en RO y CL
participacion_cadenas <- participacion_eercl(eercl)

n1 <- participacion_cadenas %>% 
  distinct(cadena_corta) %>% nrow()

# principales estadisticos en participacion RO y CL
# estadisticos_eercl1 <- estadisticos_principales(participacion_cadenas, 
#                                                 "W_RO", "W_CL")

participacion_cadenas <- participacion_minima(participacion_cadenas, 
                                              valor_aceptacion)

n2 <- participacion_cadenas %>% distinct(cadena_corta) %>% nrow()

print(glue("quedan fuera {n1 - n2} cadenas con participacion
           menor a {valor_aceptacion}% en remuneración ordinaria 
           y costos labores"))

# 3. informacion en la mensual 2023
# nt > 0 -> ro,ho,cl,rt mayores que 0
emrcl <- depurando_nt(emrcl)

# cadenas eercl
cadena_corta_eercl <- cadena_unica(participacion_cadenas, cadena_corta)
cadena_corta_emrcl <- cadena_unica(emrcl, cadena_corta)

# cadenas que cruzan entre 
cruces_cadena <- cadenas_ambas(cadena_corta_eercl, 
                               cadena_corta_emrcl)

# 4. rol dador 
# dadores con meses minimo con informacion
a <- meses_minimo(emrcl, cadena_larga, mes)

# dadores con informacion en los ultimos meses
# agregar condicion
b <- meses_ultimo(emrcl)

# dadores que cumplen ambas combinaciones
c <- dadores_iniciales(a, b)
# c <- dadores_finales(a, b)

# cadenas que pondera
base_ponderadores <- eercl[cadena_corta %in% c$cadena_corta]

# calculo ponderadores
base_ponderadores <- variables_expandidas(base_ponderadores)
base_ponderadores <- variables_ponderadores(base_ponderadores)
base_ponderadores <- variables_seleccion(base_ponderadores)

write_xlsx(base_ponderadores %>% summarise(n_distinct(cadena_corta)),
           "output/cadenas.xlsx")

# ponderadores finales
ponderadores <- variables_longer(base_ponderadores)
ponderadores_finales <- agrupar_ponderadores(ponderadores)

# guardando ponderadores
write_xlsx(ponderadores_finales, "output/ponderadores.xlsx")

