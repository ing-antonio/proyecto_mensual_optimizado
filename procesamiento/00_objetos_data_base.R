
# INSTALA PAQUETES
pacman::p_load(here,showtext,sysfonts,dplyr,lubridate,DBI,RPostgreSQL,
               janitor,gt,stringr,purrr,webshot2,RSQLite,ggplot2,ggtext,patchwork,
               scales,yaml,glue,readr,highcharter,htmlwidgets,reticulate,readxl,
               googledrive,sf,leaflet,paletteer,leaflegend,webshot,telegram)

# CARGA DE PAQUETES
library(DBI)
library(RSQLite)

# CARGA DE FUENTES
font_add_google(c("Bebas Neue","Poppins","Montserrat","Overpass","Roboto","Lexend","Manrope","Cabin"))
showtext_auto()

# CARGA SCRIPTS
source(here('auxiliares', "funciones_mensual.R"))
source(here('auxiliares', "funciones.R") )
config_file <- here("auxiliares/config.yml")
config <- yaml::read_yaml(config_file)

# UBICACION SCRIPT ACTUAL
here()

bandera = 1

conn <- dbConnect(
  "PostgreSQL", 
  dbname = config$db_name, 
  host = config$db_host,
  port = config$db_port, 
  user = config$db_username, 
  password = config$db_password
)
dbSendQuery(conn, "SET client_encoding = 'UTF8';")

#################### SCRIPTS 1, 2 Y 3 ####################

carpetas <- dbGetQuery(conn, paste0(
  "SELECT DATE(fecha_inicio) AS fecha_inicio, categoria_delito
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE fecha_inicio >= '2023-01-01'
  AND categoria_delito != 'Delito de bajo impacto'"
  )) %>% as_tibble()

victimas <- dbGetQuery(conn,
  "SELECT fechainicio FROM dashboard_seguridad.victimas
  WHERE fechainicio >= '2023-01-01'"
  ) %>% clean_names()

df_alcaldia <- dbGetQuery(conn, paste0(
  "SELECT DATE(fecha_inicio) AS fecha_inicio, alcaldia_hecho
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE fecha_inicio >= '2023-01-01'
  AND categoria_delito != 'Delito de bajo impacto'"
  )) %>% as_tibble()

if (bandera == 1) {
  fecha_corte4 <- floor_date(Sys.Date(), unit = "month")
  anio_corte4 <- year(fecha_corte4) 
} else {
  fecha_corte4 <- ceiling_date(Sys.Date(), unit = "month")
  anio_corte4 <- year(fecha_corte4)
}

#################### SCRIPT 4 ####################

anios_objetivo <- c(2019,2023,2024,2025)

filtros_fecha4 <- paste0(
  "(fecha_inicio >= '", anios_objetivo, "-01-01' AND fecha_inicio < '", anios_objetivo, "-", sprintf("%02d", month(fecha_corte4)), "-01')"
)

condicion_fecha <- paste(filtros_fecha4, collapse = " OR ")

query_carpetas4 <- glue::glue("
    SELECT
      CASE 
        WHEN categoria_delito = 'Homicidio doloso' THEN 'Homicidio doloso'
        ELSE categoria_delito
      END AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fecha_inicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE
      categoria_delito != 'Delito de bajo impacto'
      AND (
        {condicion_fecha}
      )
    GROUP BY categoria_delito_modificada, anio
    
    UNION ALL
    
    SELECT
      'Robo a pasajero a bordo del metrobús con violencia' AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fecha_inicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE
      delito IN ('Robo a pasajero a bordo de Metrobús con violencia')
      AND (
        {condicion_fecha}
      )
    GROUP BY anio")

datos_carpetas4 <- dbGetQuery(conn, query_carpetas4)

filtros_fecha4 <- paste0(
  "(fechainicio >= '", anios_objetivo, "-01-01' AND fechainicio < '", anios_objetivo, "-", sprintf("%02d", month(fecha_corte4)), "-01')"
)

condicion_fecha4 <- paste(filtros_fecha4, collapse = " OR ")

query_victimas4 <- glue::glue(
  "SELECT 'Homicidio doloso (víctimas)' AS categoria_delito_modificada,
  EXTRACT(YEAR FROM fechainicio) AS anio, COUNT(*) AS total_carpetas
  FROM dashboard_seguridad.victimas
  WHERE {condicion_fecha4} GROUP BY anio")

datos_victimas4 <- dbGetQuery(conn, query_victimas4)

#################### SCRIPT 5 ####################

if (bandera == 1) {
  fecha_ultimo_mes5 <- floor_date(Sys.Date(), "month") - months(1) 
} else {
  fecha_ultimo_mes5 <- floor_date(Sys.Date(), "month")
}

# OBTENER EL MES ANTERIOR COMPLETO
mes_objetivo5 <- month(fecha_ultimo_mes5)
anio_objetivo5 <- year(fecha_ultimo_mes5)
nombre_mes5 <- format(fecha_ultimo_mes5, "%B") |> tools::toTitleCase()

# FILTROS PARA SOLO ESE MES EN CADA ANIO
filtros_fecha5 <- paste0(
  "(fecha_inicio >= '", anios_objetivo, "-", sprintf("%02d", mes_objetivo5), "-01' AND ",
  "fecha_inicio < '", anios_objetivo, "-", sprintf("%02d", mes_objetivo5 + 1), "-01')"
)

condicion_fecha5 <- paste(filtros_fecha5, collapse = " OR ")

# OBJETO QUERY_CARPETAS5 A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
query_carpetas5 <- glue::glue("
    SELECT
      CASE 
        WHEN categoria_delito = 'Homicidio doloso' THEN 'Homicidio doloso'
        ELSE categoria_delito
      END AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fecha_inicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE
      categoria_delito != 'Delito de bajo impacto'
      AND (
        {condicion_fecha5}
      )
    GROUP BY categoria_delito_modificada, anio
    
    UNION ALL
    
    SELECT
      'Robo a pasajero a bordo del metrobús con violencia' AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fecha_inicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE
      delito IN ('Robo a pasajero a bordo de Metrobús con violencia')
      AND (
        {condicion_fecha5}
      )
    GROUP BY anio")

datos_carpetas5 <- dbGetQuery(conn, query_carpetas5)

filtros_fecha5 <- paste0(
  "(fechainicio >= '", anios_objetivo, "-", sprintf("%02d", mes_objetivo5), "-01' AND ",
  "fechainicio < '", anios_objetivo, "-", sprintf("%02d", mes_objetivo5 + 1), "-01')"
)

condicion_fecha5 <- paste(filtros_fecha5, collapse = " OR ")

query_victimas5 <- glue::glue(
  "SELECT 'Homicidio doloso (víctimas)' AS categoria_delito_modificada,
  EXTRACT(YEAR FROM fechainicio) AS anio,
  COUNT(*) AS total_carpetas FROM dashboard_seguridad.victimas
  WHERE {condicion_fecha5} GROUP BY anio")

datos_victimas5 <- dbGetQuery(conn, query_victimas5)

datos5 <- bind_rows(datos_carpetas5,datos_victimas5)

#################### SCRIPT 6 ####################

if (bandera == 1) {
  fecha_inicio6 <- as.Date("2019-01-01")
  fecha_corte6 <- floor_date(Sys.Date(), unit = "month") 
  ultimo_mes_completo6 <- month(fecha_corte6- days(1))
} else {
  fecha_inicio6 <- as.Date("2019-01-01")
  fecha_corte6 <- floor_date(Sys.Date(), unit = "month") 
  ultimo_mes_completo6 <- month(fecha_corte6)
}

consultaCarpetas6 <- dbGetQuery(
  conn, paste0(
  "SELECT  date(fecha_inicio) AS fecha_inicio, categoria_delito 
     FROM dashboard_seguridad.carpetas_directas_cc_cdmx
     WHERE 
       DATE(fecha_inicio) >= '", fecha_inicio6, "' 
       AND EXTRACT(MONTH FROM fecha_inicio) <= '", ultimo_mes_completo6 ,"'
       AND categoria_delito != 'Delito de bajo impacto'"
  )
) %>% as_tibble()

consulta_victimas6 <- dbGetQuery(
  conn, paste0(
    "SELECT date(fechainicio) AS fecha_inicio, cve_delito AS categoria_delito
     FROM dashboard_seguridad.victimas
     WHERE 
       DATE(fechainicio) >= '", fecha_inicio6, "' 
       AND EXTRACT(MONTH FROM fechainicio) <= '", ultimo_mes_completo6, "'"
  )
) %>% as_tibble()

consulta_total6 <- bind_rows(consultaCarpetas6, consulta_victimas6)

#################### SCRIPT 7 ####################

if (bandera == 1) {
  fecha_corte7 <- floor_date(Sys.Date(), unit = "month") 
  ultimo_mes_completo7 <- month(fecha_corte7- days(1))
  
} else {
  fecha_corte7 <- floor_date(Sys.Date(), unit = "month") 
  ultimo_mes_completo7 <- month(fecha_corte7)
}

consultaCarpetas7 <- dbGetQuery(
  conn, paste0(
  "SELECT date(fecha_inicio) AS fecha_inicio, categoria_delito 
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE EXTRACT(MONTH FROM fecha_inicio) = '", ultimo_mes_completo7, "'
  AND EXTRACT(YEAR FROM fecha_inicio) >= 2019
  AND categoria_delito != 'Delito de bajo impacto'"
  )) %>% as_tibble()

consulta_victimas7 <- dbGetQuery(
  conn, paste0(
  "SELECT date(fechainicio) AS fecha_inicio, cve_delito AS categoria_delito
  FROM dashboard_seguridad.victimas
  WHERE EXTRACT(MONTH FROM fechainicio) = '", ultimo_mes_completo7, "'
  AND EXTRACT(YEAR FROM fechainicio) >= 2019"
  )) %>% as_tibble()

consulta_total7 <- bind_rows(consultaCarpetas7, consulta_victimas7)

#################### SCRIPT 8 ####################

meses_espaniol <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

if (bandera == 1) {
  dias_a_restar <- day(today()) %>% as.numeric()
  fecha_corte_actual <- today() - dias_a_restar
  
  fecha_corte <- today() - dias_a_restar
  
  # OBTENER NUMERO DEL MES (1-12)
  num_mes_actual <- month(fecha_corte_actual)
  num_mes_anterior <- ifelse(num_mes_actual == 1, 12, num_mes_actual - 1)
  
  # OBTENER NOMBRE DEL MES EN ESPANIOL
  mes_actual_espaniol <- str_to_title(meses_espaniol[num_mes_actual])
  mes_anterior_espaniol <- str_to_title(meses_espaniol[num_mes_anterior])
  
  dias_a_restar <- day(today()) %>% as.numeric()
  fecha_corte <- today()-dias_a_restar
  fecha_inicio <- fecha_corte-60
} else {
  dias_a_restar <- day(today()-1) %>% as.numeric()
  fecha_corte_actual <- today() -1
  
  # fecha_corte = as.Date(Sys.time())
  fecha_corte <- today() -1
  
  # OBTENER NUMERO DEL MES (1-12)
  num_mes_actual <- month(fecha_corte_actual)
  num_mes_anterior <- ifelse(num_mes_actual == 1, 12, num_mes_actual - 1)
  
  # OBTENER NOMBRE DEL MES EN ESPANIOL
  mes_actual_espaniol <- str_to_title(meses_espaniol[num_mes_actual])
  mes_anterior_espaniol <- str_to_title(meses_espaniol[num_mes_anterior])
  
  dias_a_restar <- day(today()) %>% as.numeric()
  #fecha_corte <- today()-dias_a_restar
  fecha_inicio <- floor_date(Sys.Date() %m-% months(1), unit = "month")
}

# CALCULAR EL MES ACTUAL Y EL MES PASADO (RESPETANDO MESES CRUZANDO ANIOS)
if (bandera == 1) {
  semana_actual <- as.Date(cut(Sys.Date()-dias_a_restar, "month"))
  semana_pasada <- as.Date(cut(Sys.Date()-dias_a_restar, "month")) %m-% months(1)
  
} else {
  semana_actual <- as.Date(cut(Sys.Date(), "month"))
  semana_pasada <- as.Date(cut(Sys.Date(), "month")) %m-% months(1)
}

# OBJETO SEMANAL A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
semanal <- dbGetQuery(
  conn, paste0(
  "SELECT alcaldia_hecho, date(fecha_inicio) fecha_inicio, categoria_delito
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE DATE(fecha_inicio) >= '",semana_pasada,"' and DATE(fecha_inicio) <= '",fecha_corte,"'  and categoria_delito != 'Delito de bajo impacto'"
  )) %>% as_tibble()

# OBJETO SEMANAL_CUADRANTE A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
semanal_cuadrante <- dbGetQuery(conn, paste0(
  "SELECT alcaldia_hecho, nombre_sec, \"Nomenclatu\", date(fecha_inicio) fecha_inicio, categoria_delito, colonia_hecho
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE DATE(fecha_inicio) >= '",semana_pasada,"' and DATE(fecha_inicio) <='",fecha_corte,"'  and categoria_delito != 'Delito de bajo impacto'"
  )) %>% as_tibble() %>% 
  mutate(nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec)) %>% 
  mutate(alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho))

# OBJETO SEMANAL_SECTOR A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
semanal_sector <- dbGetQuery(conn, paste0(
  "SELECT alcaldia_hecho, nombre_sec,  date(fecha_inicio) fecha_inicio, categoria_delito
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE DATE(fecha_inicio) >= '2024-12-01' and DATE(fecha_inicio) <=  '",fecha_corte,"' and categoria_delito != 'Delito de bajo impacto'"
  )) %>% as_tibble() %>% 
  mutate(nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec)) %>% 
  mutate(alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho))

#################### SCRIPT 9 ####################

if (bandera == 1){
  dias_a_restar9 <- day(today()) %>% as.numeric()
  fecha_corte_actual9 <- today() - dias_a_restar9
  fecha_corte9 <- today() - dias_a_restar9
} else {
  dias_a_restar9 <- day(today()) %>% as.numeric()
  fecha_corte_actual9 <- today()
  fecha_corte9 <- today()
}

# OBTENER NUMERO DEL MES (1-12)
num_mes_actual9 <- month(fecha_corte_actual9)
num_mes_anterior9 <- ifelse(num_mes_actual9 == 1, 12, num_mes_actual9 - 1)

# OBTENER NOMBRE DEL MES EN ESPANIOL
mes_actual_espaniol9 <- str_to_title(meses_espaniol[num_mes_actual9])
mes_anterior_espaniol9 <- str_to_title(meses_espaniol[num_mes_anterior9])

# SOBJETO SEMANAL9 A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
semanal9 <- dbGetQuery(conn, paste0(
  "SELECT date(fecha_inicio) fecha_inicio, alcaldia_hecho, nombre_sec, colonia_hecho, \"Nomenclatu\", categoria_delito
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE DATE(fecha_inicio) >= '2025-01-01' and DATE(fecha_inicio) <= '",fecha_corte_actual9,"'")
  ) %>% as_tibble()

dbDisconnect(conn)

py_require("pathlib")
py_require("pandas")
py_require("python-pptx")
py_require("datetime")
py_require("hooking")
py_require("scipy")
py_require("matplotlib")
py_require("sympy")
