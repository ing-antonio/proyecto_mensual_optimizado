
# INSTALA PAQUETES
pacman::p_load(here,showtext,sysfonts,dplyr,lubridate,DBI,RPostgreSQL,
  janitor,gt,stringr,purrr,webshot2,RSQLite,ggplot2,ggtext,patchwork,
  scales,yaml,glue,readr,highcharter,htmlwidgets,reticulate,readxl,
  googledrive,sf,leaflet,paletteer,leaflegend,webshot,telegram)

# CARGA PAQUETES
library(DBI)

# CARGA FUENTES
font_add_google(c("Bebas Neue","Poppins","Montserrat","Overpass","Roboto","Lexend","Manrope","Cabin"))
showtext_auto()

# CARGA SCRIPTS
source(here('auxiliares', "funciones_mensual.R"))
source(here('auxiliares', "funciones.R"))
config_file <- here("auxiliares/config.yml")
config <- yaml::read_yaml(config_file)
ruta_cuenta_servicio <- here("auxiliares/servicio_de_cuenta.json")

# UBICACION SCRIPT ACTUAL
here()

#################### DESCARGA RDATA ####################

drive_auth(path = ruta_cuenta_servicio)
id_carpeta <- "1EUQAKFWZKOEKG3zKxHmcWfstIgsDuaS5"

# LISTAR ARCHIVOS RDATA ALOJADOS EN CARPETA DRIVE
archivos <- drive_ls(as_id(id_carpeta), pattern = "\\.RData$")

if (nrow(archivos) == 0) {
  stop("NO SE ENCONTRARON ARCHIVOS RDATA EN LA CARPETA DRIVE.")
}

# OBTENER FECHA MODIFICACION Y CONVERTIRLA A FORMATO DATETIME
archivos$fecha_modificacion <- sapply(archivos$drive_resource, function(x) x$modifiedTime)
archivos$fecha_modificacion <- ymd_hms(archivos$fecha_modificacion)

# SELECCIONAR ARCHIVO MAS RECIENTE
indice_mas_reciente <- which.max(archivos$fecha_modificacion)
archivo_mas_reciente <- archivos[indice_mas_reciente, ]

# RUTA LOCAL DONDE SE GUARDARA ARCHIVO
ruta_guardado <- file.path(here("datos/datos_originales"), archivo_mas_reciente$name)

# DESCARGAR ARCHIVO MAS RECIENTE Y SOBRESCRIBIENDOLO SI EXISTE
drive_download(
  file = as_id(archivo_mas_reciente$id),
  path = ruta_guardado,
  overwrite = TRUE
)

# GUARDAR RUTA
writeLines(ruta_guardado, here("datos", "datos_originales", "ultimo_archivo_descargado.txt"))

temp_env <- new.env()
load(ruta_guardado, envir = temp_env)
df_diario <- get("df_diario", envir = temp_env)

#################### CARPETAS ####################

bandera = 1
primer_dia_mes_actual <- floor_date(Sys.Date(), unit = "month") # 2025-10-01
fecha_corte <- primer_dia_mes_actual # 2025-10-01
primer_dia_mes_proximo <- ceiling_date(Sys.Date(), unit = "month") # 2025-11-01
anio_actual <- anio_corte <- year(primer_dia_mes_actual) # 2025
anio_corte <- anio_actual # 2025
anios_objetivo <- c(2019,2023,2024,2025)

filtros_fecha_carpetas <- paste0(
"(fecha_inicio >= '", anios_objetivo, "-01-01' AND fecha_inicio < '", 
anios_objetivo, "-", sprintf("%02d", month(fecha_corte)), "-01')")

condicion_fecha_carpetas <- paste(filtros_fecha_carpetas, collapse = " OR ")

filtros_fecha_victimas <- paste0(
"(fechainicio >= '", anios_objetivo, "-01-01' AND fechainicio < '", 
  anios_objetivo, "-", sprintf("%02d", month(fecha_corte)), "-01')")

condicion_fecha_victimas <- paste(filtros_fecha_victimas, collapse = " OR ")

meses_espaniol <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

#################### SCRIPTS 1 y 2 ####################

carpetas <- df_diario %>%
  filter(categoria_delito != 'Delito de bajo impacto', fecha_inicio >= '2023-01-01') %>%
  select(fecha_inicio, categoria_delito) %>%
  as_tibble()

#################### SCRIPT 3 ####################

df_alcaldia <- df_diario %>%
  filter(categoria_delito != 'Delito de bajo impacto', fecha_inicio >= '2023-01-01') %>%
  select(fecha_inicio, alcaldia_hecho) %>%
  as_tibble()

#################### SCRIPT 4 ####################

# RANDOS DE FECHAS A REPLICAR (INICIO INCLUSIVE, FIN EXCLUSIVO)
periodos <- list(
  c(ymd("2019-01-01"), ymd("2019-10-01")),
  c(ymd("2023-01-01"), ymd("2023-10-01")),
  c(ymd("2024-01-01"), ymd("2024-10-01")),
  c(ymd("2025-01-01"), ymd("2025-10-01"))
)

# FUNCION AUXILIAR PARA VERIFICAR SI FECHA ESTA EN ALGUNO DE LOS PERIODOS
en_periodo <- function(fecha) {
  fecha <- as_date(fecha)
  any(sapply(periodos, function(p) fecha >= p[1] & fecha < p[2]))
}

# PRIMERA PARTE: SELECCION POR CATEGORIA DELITO (CON AJUSTE DE NOMBRE PARA HOMICIDIO)
parte1 <- df_diario %>%
  filter(categoria_delito != "Delito de bajo impacto") %>%
  filter(en_periodo(fecha_inicio)) %>%
  mutate(categoria_delito_modificada = if_else(categoria_delito == "Homicidio doloso",
  "Homicidio doloso",categoria_delito),anio = year(fecha_inicio)) %>%
  group_by(categoria_delito_modificada, anio) %>%
  summarise(total_carpetas = n(), .groups = "drop")

# SEGUNDA PARTE: FILAS REFORZADAS PARA 'ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA'
# NOTA: EN SQL SE USA WHERE DELITO IN ('ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA')
# EN R  SE REPLICA IGUALANDO LA COLUMNA 'DELITO'
categoria_fija <- "Robo a pasajero a bordo del metrobús con violencia"
delito_match_string <- "Robo a pasajero a bordo de Metrobús con violencia" # SEGUN SQL

parte2 <- df_diario %>%
  filter(delito %in% c(delito_match_string)) %>%
  filter(en_periodo(fecha_inicio)) %>%
  mutate(
    categoria_delito_modificada = categoria_fija,
    anio = year(fecha_inicio)
  ) %>%
  group_by(categoria_delito_modificada, anio) %>%
  summarise(total_carpetas = n(), .groups = "drop")

# UNION ALL: CONCATENAR (NO ELIMINANDO DUPLICADOS)
datos_carpetas4 <- bind_rows(parte1, parte2) %>% arrange(categoria_delito_modificada, anio)

condicion_fecha <- paste(filtros_fecha_carpetas, collapse = " OR ")

#################### SCRIPT 5 ####################

anios_objetivo <- c(2019,2023,2024,2025)
fecha_ultimo_mes5 <- floor_date(Sys.Date(), "month") - months(1)

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

carpetas5 <- df_diario %>% mutate(fecha_inicio = as.Date(fecha_inicio))

# DEFINIR ANIOS/MES OBJETIVO
years_target <- c(2019, 2023, 2024, 2025)
month_target <- 9

parte1 <- carpetas5 %>%
  filter(
    categoria_delito != "Delito de bajo impacto",
    month(fecha_inicio) == month_target,
    year(fecha_inicio) %in% years_target
  ) %>%
  mutate(
    categoria_delito_modificada = if_else(
      categoria_delito == "Homicidio doloso",
      "Homicidio doloso",
      categoria_delito
    ),
    anio = year(fecha_inicio)
  ) %>%
  group_by(categoria_delito_modificada, anio) %>%
  summarise(total_carpetas = n(), .groups = "drop")

parte2 <- carpetas5 %>%
  filter(
    delito %in% c("Robo a pasajero a bordo de Metrobús con violencia"),
    month(fecha_inicio) == month_target,
    year(fecha_inicio) %in% years_target
  ) %>%
  mutate(
    categoria_delito_modificada = "Robo a pasajero a bordo del metrobús con violencia",
    anio = year(fecha_inicio)
  ) %>%
  group_by(categoria_delito_modificada, anio) %>%
  summarise(total_carpetas = n(), .groups = "drop")

# UNION ALL (BIND_ROWS CONSERVA FILAS DEPLICADAS COMO EN UNION ALL)
datos_carpetas5 <- bind_rows(parte1, parte2) %>%
  arrange(categoria_delito_modificada, anio)

#################### SCRIPT 6 ####################

fecha_inicio6 <- as.Date("2019-01-01")
fecha_corte6 <- floor_date(Sys.Date(), unit = "month") 
ultimo_mes_completo6 <- month(fecha_corte6- days(1))

consultaCarpetas6 <- df_diario %>%
  filter(
    fecha_inicio >= as.Date({fecha_inicio6}),
    month(fecha_inicio) <= {ultimo_mes_completo6},
    categoria_delito != "Delito de bajo impacto"
  ) %>%
  mutate(fecha_inicio = as.Date(fecha_inicio)) %>%
  select(fecha_inicio, categoria_delito) %>%
  as_tibble()

#################### SCRIPT 7 ####################

fecha_corte <- floor_date(Sys.Date(), unit = "month") 
ultimo_mes_completo <- month(fecha_corte- days(1))

consultaCarpetas7 <- df_diario %>%
  filter(
    month(fecha_inicio) == {ultimo_mes_completo},
    year(fecha_inicio) >= 2019,
    categoria_delito != "Delito de bajo impacto"
  ) %>%
  mutate(fecha_inicio = as.Date(fecha_inicio)) %>%
  select(fecha_inicio, categoria_delito) %>%
  as_tibble()

#################### SCRIPT 8 ####################

dias_a_restar <- day(today()) %>% as.numeric()
fecha_corte <- today() - dias_a_restar

# OBTENER NUMERO DEL MES (1-12)
num_mes_actual <- month(fecha_corte)
num_mes_anterior <- ifelse(num_mes_actual == 1, 12, num_mes_actual - 1)

# OBTENER NOMBRE DEL MES EN ESPANIOL
mes_actual_espaniol <- str_to_title(meses_espaniol[num_mes_actual])
mes_anterior_espaniol <- str_to_title(meses_espaniol[num_mes_anterior])

dias_a_restar <- day(today()) %>% as.numeric()
fecha_corte <- today()-dias_a_restar
fecha_inicio <- fecha_corte-60

semana_actual <- as.Date(cut(Sys.Date()-dias_a_restar, "month"))
semana_pasada <- as.Date(cut(Sys.Date()-dias_a_restar, "month")) %m-% months(1)

semanal <- df_diario %>%
  filter(
    fecha_inicio >= as.Date({semana_pasada}),
    fecha_inicio <= as.Date({fecha_corte}),
    categoria_delito != "Delito de bajo impacto"
  ) %>%
  mutate(fecha_inicio = as.Date(fecha_inicio)) %>%
  select(alcaldia_hecho, fecha_inicio, categoria_delito) %>%
  as_tibble()

# OBJETO SEMANAL_CUADRANTE A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
semanal_cuadrante <- df_diario %>%
  filter(
    fecha_inicio >= as.Date({semana_pasada}),
    fecha_inicio <= as.Date({fecha_corte}),
    categoria_delito != "Delito de bajo impacto"
  ) %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio),
    nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec),
    alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)
  ) %>%
  select(alcaldia_hecho, nombre_sec, Nomenclatu, fecha_inicio, categoria_delito, colonia_hecho) %>%
  as_tibble()

# OBJETO SEMANAL_SECTOR A PARTIR DE TABLA CARPETAS_DIRECTAS_CC_CDMX
semanal_sector <- df_diario %>%
  filter(
    fecha_inicio >= as.Date("2024-12-01"),
    fecha_inicio <= as.Date({fecha_corte}),
    categoria_delito != "Delito de bajo impacto"
  ) %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio),
    nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec),
    alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)
  ) %>%
  select(alcaldia_hecho, nombre_sec, fecha_inicio, categoria_delito) %>%
  as_tibble()

#################### SCRIPT 9 ####################

dias_a_restar <- day(today()) %>% as.numeric()
fecha_corte <- today() - dias_a_restar
fecha_corte <- today() - dias_a_restar

# OBTENER NUMERO DEL MES (1-12)
num_mes_actual9 <- month(fecha_corte)
num_mes_anterior9 <- ifelse(num_mes_actual9 == 1, 12, num_mes_actual9 - 1)

# OBTENER NOMBRE DEL MES EN ESPANIOL
mes_actual_espaniol9 <- str_to_title(meses_espaniol[num_mes_actual9])
mes_anterior_espaniol9 <- str_to_title(meses_espaniol[num_mes_anterior9])

semanal9 <- df_diario %>%
  filter(
    fecha_inicio >= as.Date("2025-01-01"),
    fecha_inicio <= as.Date("2025-09-30")
  ) %>%
  mutate(fecha_inicio = as.Date(fecha_inicio)) %>%
  select(fecha_inicio, alcaldia_hecho, nombre_sec, colonia_hecho, Nomenclatu, categoria_delito) %>%
  as_tibble()

#################### MAPA ####################

# CARGA CAPAS
alcaldias <- st_read( here('capas',"limite_de_las_alcaldias.shp") )
colonias <- st_read(here('capas',"cuadrante_colonia.shp") )

# REPROYECCION DE CAPA
colonias <- colonias %>%  
  st_transform(4326) %>% 
  select(alcaldia=Deleg, colonia=X_NOMUT, Sector, Cuadrante=Nomenclatu, geometry)

sf_use_s2(FALSE)

fecha_corte<- today()-1
fecha_aux<- fecha_corte - 6

# OBTENER LUNES DE SEMANA ACTUAL
lunes_semana <- floor_date(fecha_corte, unit = "week", week_start = 1)

# SI HOY ES LUNES, TOMAR LUNES DE SEMANA PASADA
if (wday(fecha_corte, week_start = 1) == 1) {
  lunes_semana <- lunes_semana - weeks(1)
}

# VERIFICAR RESULTADO
dia_de_la_semana <- wday(lunes_semana, label = TRUE, abbr = FALSE, week_start = 1)

list(fecha = lunes_semana, dia = dia_de_la_semana)

# CONVERTIR A FORMATO FECHA
corte <- lunes_semana

fgj_aux <- df_diario %>%
  filter(
    fecha_inicio >= as.Date("2025-10-20"),
    fecha_inicio <= as.Date("2025-10-26"),
    !is.na(latitud),
    !is.na(longitud),
    latitud != "Inf",
    longitud != "Inf",
    categoria_delito != "Delito de bajo impacto"
  ) %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio),
    lat = as.numeric(latitud),
    lon = as.numeric(longitud),
    alcaldia_hechos = alcaldia_hecho
  ) %>%
  select(fecha_inicio, categoria_delito, delito, alcaldia_hechos, lat, lon) %>%
  as_tibble() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_intersection(colonias) %>% 
  mutate(tooltip=paste0("<b>","Categoria delito: ","</b>", str_to_sentence(categoria_delito),"<br>",
                        "<b>","Delito: ","</b>", str_to_sentence(delito),"<br>",
                        "<b>","Alcaldía: ","</b>", str_to_sentence(alcaldia),"<br>",
                        "<b>","Sector: ","</b>", str_to_sentence(Sector),"<br>",
                        "<b>","Colonia: ","</b>", str_to_sentence(colonia),"<br>",
                        "<b>","Cuadrante: ","</b>", Cuadrante,"<br>",
                        "<b>","Fecha inicio: ","</b>", fecha_inicio,"<br>"
  )) %>% 
  mutate(categoria_delito = toupper(categoria_delito),
         delito = toupper(delito),
         categoria_delito=case_when(
           categoria_delito=="ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA" ~ delito,
           categoria_delito=="ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA" ~ delito,
           categoria_delito=="ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA" ~ delito,
           categoria_delito=="ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA" ~ delito,
           categoria_delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ delito,
           categoria_delito=="ROBO A REPARTIDOR CON Y SIN VIOLENCIA" ~ delito,
           T ~ categoria_delito
         )) %>% 
  mutate(categoria_delito=case_when(
    (grepl("VEHICULO", delito ) & grepl("CON VIOLENCIA|C/V", delito ))  ~ "ROBO DE VEHÍCULO CON VIOLENCIA",
    (grepl("VEHICULO", delito ) & grepl("SIN VIOLENCIA|S/V", delito ))  ~ "ROBO DE VEHÍCULO SIN VIOLENCIA",
    (grepl("MOTOCICLETA", delito ) & grepl("CON VIOLENCIA", delito ))  ~ "ROBO DE MOTOCICLETA CON VIOLENCIA",
    (grepl("MOTOCICLETA", delito ) & grepl("SIN VIOLENCIA", delito ))  ~ "ROBO DE MOTOCICLETA SIN VIOLENCIA",
    (grepl("PESERO", categoria_delito ) & grepl("SIN VIOLENCIA", categoria_delito ))  ~ "ROBO A PASAJERO A BORDO DE MICROBUS",
    (grepl("PESERO", categoria_delito ) & grepl("CON VIOLENCIA", categoria_delito ))  ~ "ROBO A PASAJERO A BORDO DE MICROBUS",
    (grepl("TRANSEUNTE", categoria_delito ) & grepl("CON VIOLENCIA", categoria_delito ))  ~ "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA",
    (grepl("TRANSEUNTE", categoria_delito ) & grepl("SIN VIOLENCIA", categoria_delito ))  ~ "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA",
    categoria_delito=="ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA"  ~ "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA",
    categoria_delito=="ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA"  ~ "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA",
    T ~ categoria_delito
  )) %>% 
  select(categoria_delito, geometry, tooltip, Cuadrante, colonia, alcaldia, Sector) %>% 
  filter(categoria_delito != 'ROBO DE MERCANCIA A TRANSPORTISTA C/V') %>% 
  filter(categoria_delito != 'ROBO DE MAQUINARIA SIN VIOLENCIA')  %>% 
  mutate(categoria_delito = ifelse(categoria_delito == 'ROBO A TRANSEÚNTE EN VÍA PÚBLICA CON VIOLENCIA', 'ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA', categoria_delito)) %>% 
  mutate(categoria_delito = ifelse(categoria_delito == 'ROBO A TRANSEÚNTE EN VÍA PÚBLICA SIN VIOLENCIA', 'ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA', categoria_delito)) %>% 
  mutate(categoria_delito = ifelse(categoria_delito == 'ROBO A PASAJERO A BORDO DE MICROBÚS CON Y SIN VIOLENCIA', 'ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA', categoria_delito))

#################### VICTIMAS ####################

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
victimas <- dbGetQuery(conn,"
  SELECT fechainicio FROM dashboard_seguridad.victimas
  WHERE fechainicio >= '2023-01-01'") %>% clean_names()

#################### SCRIPT 4 ####################

query_victimas4 <- glue::glue("
  SELECT 'Homicidio doloso (víctimas)' AS categoria_delito_modificada,
  EXTRACT(YEAR FROM fechainicio) AS anio, COUNT(*) AS total_carpetas
  FROM dashboard_seguridad.victimas
  WHERE {condicion_fecha_victimas} GROUP BY anio")

datos_victimas4 <- dbGetQuery(conn, query_victimas4)

#################### SCRIPT 5 ####################

fecha_ultimo_mes5 <- floor_date(Sys.Date(), "month") - months(1) 

mes_objetivo5 <- month(fecha_ultimo_mes5)
anio_objetivo5 <- year(fecha_ultimo_mes5)
nombre_mes5 <- format(fecha_ultimo_mes5, "%B") |> tools::toTitleCase()

filtros_fecha5 <- paste0(
  "(fechainicio >= '", anios_objetivo, "-", sprintf("%02d", mes_objetivo5), "-01' AND ",
  "fechainicio < '", anios_objetivo, "-", sprintf("%02d", mes_objetivo5 + 1), "-01')")

condicion_fecha5 <- paste(filtros_fecha5, collapse = " OR ")

query_victimas5 <- glue::glue("
  SELECT 'Homicidio doloso (víctimas)' AS categoria_delito_modificada,
  EXTRACT(YEAR FROM fechainicio) AS anio,
  COUNT(*) AS total_carpetas FROM dashboard_seguridad.victimas
  WHERE {condicion_fecha5} GROUP BY anio")

datos_victimas5 <- dbGetQuery(conn, query_victimas5)
datos5 <- bind_rows(datos_carpetas5,datos_victimas5)

#################### SCRIPT 6 ####################

consulta_victimas6 <- dbGetQuery(
  conn, paste0(
    "SELECT date(fechainicio) AS fecha_inicio, cve_delito AS categoria_delito
     FROM dashboard_seguridad.victimas
     WHERE 
       DATE(fechainicio) >= '", fecha_inicio6, "' 
       AND EXTRACT(MONTH FROM fechainicio) <= '", ultimo_mes_completo6, "'")) %>% as_tibble()

consulta_total6 <- bind_rows(consultaCarpetas6, consulta_victimas6)

#################### SCRIPT 7 ####################

consulta_victimas7 <- dbGetQuery(conn, paste0("
  SELECT date(fechainicio) AS fecha_inicio, cve_delito AS categoria_delito
  FROM dashboard_seguridad.victimas
  WHERE EXTRACT(MONTH FROM fechainicio) = '", ultimo_mes_completo, "'
  AND EXTRACT(YEAR FROM fechainicio) >= 2019")) %>% as_tibble()

consulta_total7 <- bind_rows(consultaCarpetas7, consulta_victimas7)

dbDisconnect(conn)

py_require("pathlib")
py_require("pandas")
py_require("python-pptx")
py_require("datetime")
py_require("hooking")
py_require("scipy")
py_require("matplotlib")
py_require("sympy")

