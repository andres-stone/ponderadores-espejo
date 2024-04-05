
# ponderadores ir icl

# funciones

source("script/00-librerias.R")

# crear carpetas
crear_carpetas <- function(carpetas) {
  
  if (!dir.exists(carpetas)) {
    dir.create(carpetas)
    message(glue("Directorio de trabajo {carpetas} creado correctamente."))
  } else {
    message(glue("El directorio de trabajo {carpetas} ya existe."))
  }
  
}

# funcion para leer archivos excel
leer_excel <- function(direccion) {
  
  data <- as.data.table(read_excel(direccion)) %>% 
    clean_names()
  
  return(data)
  
}

# funcion para leer archivos rds
leer_rds <- function(direccion) {
  
  data <- as.data.table(readRDS(direccion)) %>% 
    clean_names()
  
  return(data)
  
}

# crear cadenas (cortas y largas)
crear_cadenas <- function(datos) {
  datos <- datos %>% 
    mutate(cadena_corta = paste0(tamano, categoria, sexo, grupo),
           cadena_larga = paste0(rol, tamano, categoria, sexo, grupo)
    )
}

# renombre en estructural
renombre_eercl <- function(datos) {
  datos <- datos %>% 
    rename(categoria = sector)
}

# seleccion de columnas
seleccion_columnas <- function(datos) {
  datos <- datos %>% 
    select(all_of(c(columnas_seleccionadas, columnas_nuevas)))
}

# variables mensual
variables_claves <- function(datos) {
  
  datos <- datos %>%
    mutate(
      nt = v1,
      ro = v2 + v3 + v4 + v5 + v6 + v8 + v11 + v12,
      re = v7,
      rt = v2 + v3 + v4 + v5 + v6 + v7 + v8 + v11 + v12, 
      cl = v2 + v3 + v4 + v5 + v6 +
        v7 + v8 + v9 + v11 + v12 + v13 + v14 + v15 + v17,
      ri = v2 + v3 + v4 + v5 + v6 + v7 + v8 + v10 + v11 + v12,
      ho = v22,
      he = v23,
      ht = v22 + v23,
      i0 = sub(" .*", "", periodo_referencia),
      sexo  = as.numeric(substr(sexo, 2, 2)),
      # sexo  = if_else(sexo == 2, 1,
      #                 sexo == 3, 2, NA),
      grupo = as.numeric(substr(grupo, 2, 2))
    ) 
  
  return(datos)
  
}

# inner join mensual
variables_aux <- function(datos){
  datos <- datos %>% 
    inner_join(meses, by = "i0")
}

# quitando nt = 0
depurando_nt <- function(datos){
  datos <- datos %>% 
    filter(nt > 0 & ro > 0 & ho > 0 & cl > 0)# & ht > 0 & rt > 0
}

# cadenas
cadena_unica <- function(datos, columna) {
  
  cadena <- datos %>% 
    select({{ columna }}) %>% 
    distinct()
  
  return(cadena)
  
}

# cruce cadenas
cadenas_ambas <- function(datos1, datos2) {
  datos1 <- datos1 %>% 
    inner_join(datos2, by = "cadena_corta")
}

# participacion en RO y CL
participacion_eercl <- function(datos) {
  datos <- datos %>%
    mutate(ropw = ro * pw,
           clpw = cl * pw,) %>% 
    group_by(cadena_corta) %>% 
    summarise(RO = sum(ropw, na.rm = T),
              CL = sum(clpw, na.rm = T)) %>% 
    mutate(
      W_RO = 100 * (RO / sum(RO, na.rm = T)),
      W_CL = 100 * (CL / sum(CL, na.rm = T))
    )
}

# estadisticos principales 
estadisticos_principales <- function(datos, columna1, columna2) {
  # Convertir las columnas a numéricas
  datos[[columna1]] <- as.numeric(as.character(datos[[columna1]]))
  datos[[columna2]] <- as.numeric(as.character(datos[[columna2]]))
  
  # Calcular estadísticas
  estadisticos <- list(
    media_WRO = mean(datos[[columna1]], na.rm = TRUE),
    mediana_WRO = median(datos[[columna1]], na.rm = TRUE),
    sd_WRO = sd(datos[[columna1]], na.rm = TRUE),
    min_WRO = min(datos[[columna1]], na.rm = TRUE),
    max_WRO = max(datos[[columna1]], na.rm = TRUE),
    media_WCL = mean(datos[[columna2]], na.rm = TRUE),
    mediana_WCL = median(datos[[columna2]], na.rm = TRUE),
    sd_WCL = sd(datos[[columna2]], na.rm = TRUE),
    min_WCL = min(datos[[columna2]], na.rm = TRUE),
    max_WCL = max(datos[[columna2]], na.rm = TRUE)
  )
  
  # Crear data frame
  estadisticos_df <- data.frame(
    estadistico = names(estadisticos),
    valor = unlist(estadisticos)
  )
  
  return(estadisticos_df)
}

# valor minimo en participacion
participacion_minima <- function(datos, valor_requerido) {
  datos <- datos %>% 
    filter(W_RO >= valor_requerido & W_CL >= valor_requerido)
}

# cadenas largas con n meses de datos
meses_minimo <- function(datos, columna1, columna2) {
  datos <- datos %>%
    group_by({{columna1}}) %>% 
    summarise(meses = n_distinct({{columna2}})) %>% 
    filter(meses >= meses_con_datos) %>% 
    select(-meses)
}

# cadenas largas con datos en los ultimos meses
# meses_ultimo <- function(datos) {
#   datos <- datos %>% 
#     filter(mes %in% meses_finales)
# }
meses_ultimo <- function(datos, meses_finales = 10:12, meses_datos = 3) {
  
  datos <- datos %>% 
    filter(mes %in% meses_finales) %>% 
    group_by(cadena_larga) %>% 
    summarise(meses = n_distinct(mes)) %>% 
    filter(meses >= meses_datos) %>% 
    select(-meses)
  
}


# dadores finales
# dadores_finales <- function(datos1, datos2) {
#   datos <- datos1 %>% 
#     inner_join(datos2, by = "cadena_larga") %>% 
#     group_by(cadena_corta) %>% 
#     summarise(dadores = n_distinct(cadena_larga)) %>% 
#     filter(dadores >= n_dadores) %>% 
#     select(cadena_corta)
#   
#   return(datos)
# }
dadores_iniciales <- function(datos1, datos2, n_dadores = 4) {
  datos <- datos1 %>% 
    inner_join(datos2, by = "cadena_larga") %>% 
    select(cadena_larga) %>% 
    mutate(
      cadena_corta = substr(cadena_larga,
                            nchar(cadena_larga)-3,
                            nchar(cadena_larga)
                            )
    ) %>% 
    group_by(cadena_corta) %>% 
    summarise(dadores = n_distinct(cadena_larga)) %>% 
    filter(dadores >= n_dadores)
}

# dadores_finales <- function(datos1, datos2) {
#   datos <- datos1 %>% 
#     inner_join(datos2, by = "cadena_larga") %>% 
#     group_by(cadena_corta) %>% 
#     summarise(dadores = n_distinct(cadena_larga)) %>% 
#     filter(dadores >= n_dadores) %>% 
#     select(cadena_corta)
#   
#   return(datos)
# }


# variables expandidas
variables_expandidas <- function(datos) {
  datos <- datos %>% 
    mutate(
      RO = as.numeric(ro * pw),
      HO = as.numeric(ho * pw),
      HE = as.numeric(he * pw),
      HT = as.numeric(ht * pw),
      RE = as.numeric(re * pw),
      CL = as.numeric(cl * pw),
      RI = as.numeric(ri * pw),
      RT = as.numeric(rt * pw)
    )
}

# variables ponderadores
variables_ponderadores <- function(datos) {
  datos <- datos %>% 
    mutate(
      ponderador_ir   = ((RO) / sum(RO)),
      ponderador_icl  = ((CL) / sum(CL)),
      ponderador_horo = ((RO/HO) / sum( (RO/HO) )),
      ponderador_htrore = ((HT/RT) / sum( (HT/RT) )),
      ponderador_htcl   = (HT) / (sum(CL))
    )
}

# variables seleccion
variables_seleccion <- function(datos){
  datos <- datos %>% 
    select(categoria, tamano, grupo, sexo, cadena_corta,
           starts_with("ponderador_"))
}

# base final
variables_longer <- function(datos) {
  datos <- datos %>% 
    pivot_longer(
      cols = starts_with("ponderador_"),
      names_to = "id_ponderador",
      values_to = "ponderador"
    ) %>%
    mutate(id_ponderador = gsub("ponderador_", "", id_ponderador))
}

# agrupando
agrupar_ponderadores <- function(df) {
  df <- df %>% 
    group_by(cadena_corta, tamano, categoria, sexo, grupo,
             id_ponderador) %>% 
    summarise(ponderador = sum(ponderador, na.rm = T))
}

