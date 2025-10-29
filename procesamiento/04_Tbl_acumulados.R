
# IMAGEN TBL_ACUMULADOS.PNG
# IMAGEN ROBO_ESPACIO_PUBLICO.PNG
# IMAGENES TBLACUM_...PNG
# IMAGENES VARACUM_...PNG

# COMBINAR CARPETAS Y VICTIMAS
datos <- bind_rows(datos_carpetas4,datos_victimas4)
  
# GUARDAR RDATA
save(datos, file = here("datos", "datos_crudos.RData"))

# PIVOTEAR Y CALCULAR VARIACIONES
tabla <- datos %>%
  pivot_wider(
    names_from = anio,
    values_from = total_carpetas,
    names_prefix = "total_"
  ) %>%
  mutate(
    total_2019 = replace_na(total_2019, 0),
    total_2023 = replace_na(total_2023, 0),
    total_2024 = replace_na(total_2024, 0),
    total_2025 = replace_na(total_2025, 0),
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  ) %>%
  arrange(desc(total_2025))

# SACAR TOTALES DE ALTO IMPACTO Y ROBO CON Y SIN VIOLENCIA
# FILA ALTO IMPACTO
fila_total <- tabla %>%
  filter(categoria_delito_modificada != "Homicidio doloso (víctimas)" & categoria_delito_modificada != "Robo a pasajero a bordo del metrobús con violencia") %>%  # EXCLUYE VICTIMAS Y ROBO METROBUS
  summarise(
    categoria_delito_modificada = "Alto Impacto",
    total_2019 = sum(total_2019, na.rm = TRUE),
    total_2023 = sum(total_2023, na.rm = TRUE),
    total_2024 = sum(total_2024, na.rm = TRUE),
    total_2025 = sum(total_2025, na.rm = TRUE)
  ) %>%
  mutate(
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  )

# FILA ROBO DE VEHICULO CON Y SIN VIOLENCIA
fila_robo <- tabla %>%
  filter(categoria_delito_modificada == "Robo de vehículo con violencia" | categoria_delito_modificada == "Robo de vehículo sin violencia") %>% 
  summarise(
    categoria_delito_modificada = "Robo de vehículo con y sin violencia",
    total_2019 = sum(total_2019, na.rm = TRUE),
    total_2023 = sum(total_2023, na.rm = TRUE),
    total_2024 = sum(total_2024, na.rm = TRUE),
    total_2025 = sum(total_2025, na.rm = TRUE)
  ) %>%
  mutate(
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  )

# UNIR TABLAS
tabla_formateada <- bind_rows(fila_total,fila_robo, tabla)

# CREAR VECTOR DE ORIGEN
orden_delitos <- c(
  "Alto Impacto" = 1,
  "Homicidio doloso" = 2,
  "Homicidio doloso (víctimas)" = 3,
  "Lesiones dolosas por disparo de arma de fuego" = 4,
  "Robo a transeunte en vía pública con y sin violencia" = 5,
  "Robo a transeunte en vía pública con violencia" = 6,
  "Robo a transeunte en vía pública sin violencia" = 7,
  "Robo de vehículo con y sin violencia" = 8,
  "Robo de vehículo con violencia" = 9,
  "Robo de vehículo sin violencia" = 10,
  "Secuestro" = 11,
  "Violación" = 12,
  "Robo a negocio con violencia" = 13,
  "Robo a pasajero a bordo del metro con y sin violencia" = 14,
  "Robo a repartidor con y sin violencia" = 15,
  "Robo a pasajero a bordo de microbus con y sin violencia" = 16,
  "Robo a cuentahabiente saliendo del cajero con violencia" = 17,
  "Robo a pasajero a bordo de taxi con violencia" = 18,
  "Robo a casa habitación con violencia" = 19,
  "Robo a transportista con y sin violencia" = 20,
  "Robo a transeúnte en vía pública con y sin violencia" = 21,
  "Robo a pasajero a bordo de microbús con y sin violencia" = 22,
  "Delito de bajo impacto" = 23,
  "Robo a transeúnte en vía pública con violencia" = 24,
  "Robo a transeúnte en vía pública sin violencia" = 25,
  "Robo a pasajero a bordo del metro con violencia" = 26,
  "Robo a pasajero a bordo del metro sin violencia" = 27,
  "Robo a pasajero a bordo de metrobús con violencia" = 28
)

# FORMATO DE TABLA Y NOMBRES DE COLUMNAS
# SE PUEDEN QUITAR DELITOS DE LA TABLA
tabla_formateada_sin_visibles <- tabla_formateada %>%
  filter(
    !categoria_delito_modificada %in% c("Robo a pasajero a bordo de metrobús con violencia")
  ) %>%
  mutate(
    orden = orden_delitos[categoria_delito_modificada]
  ) %>%
  arrange(orden)

# MES ANTERIOR COMPLETO (ULTIMO MES CON DATOS CERRADOS)
fecha_ultimo_mes <- fecha_corte - months(1)
# OBTENER NOMBRE COMPLETO DEL MES EN ESPANIOL
nombre_mes <- format(fecha_ultimo_mes, "%b") |> 
  tools::toTitleCase() |> 
  sub("\\.", "", x = _)

# ANIOS REELEVANTES
anios <- c(2019,2023,2024,2025)
etiquetas_totales <- setNames(
  glue("Ene-{nombre_mes} {anios}"),
  paste0("total_", anios)
)

# CREAR TABLA GT
gt_tabla <- tabla_formateada_sin_visibles %>%
  select(
    categoria_delito_modificada,
    total_2019, total_2023, total_2024, total_2025,
    var_2025_vs_2019, var_2025_vs_2023, var_2025_vs_2024
  ) %>%
  gt() %>%
  cols_label(.list = c(
    categoria_delito_modificada = "Categoría del delito",
    etiquetas_totales,
    var_2025_vs_2019 = "Var % vs 2019",
    var_2025_vs_2023 = "Var % vs 2023",
    var_2025_vs_2024 = "Var % vs 2024"
  )) %>%
  fmt_number(
    columns = c(total_2025, total_2023, total_2024, total_2019),
    decimals = 0,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  # ESTILO ENCABEZADOS
  tab_style(
    style = list(
      cell_fill(color = "#9d2041ff"),
      cell_text(color = "white", weight = "bold", size = px(16))
    ),
    locations = cells_column_labels(everything())
  ) %>%
  # FONDO FILA TOTAL DELITOS DE ALTO IMPACTO
  tab_style(
    style = list (cell_fill(color = "#DAD1C5"), cell_text(weight = "bold")),
    locations = cells_body(rows = categoria_delito_modificada == "Alto Impacto")
  ) %>%
  # SANGRIA PARA TODOS EXCEPTO ALTO IMPACTO Y SUBTIPOS DE ROBO DE VEHICULO
  tab_style(
    style = cell_text(indent = px(16)),
    locations = cells_body(columns = categoria_delito_modificada, 
                           rows = categoria_delito_modificada != "Alto Impacto" &
                             categoria_delito_modificada != "Robo de vehículo con violencia" &
                             categoria_delito_modificada != "Robo de vehículo sin violencia")
  ) %>%
  # SANGRIA GRANDE PARA LOS SUBTIPOS DE ROBO DE VEHICULO Y VICTIMAS
  tab_style(
    style = cell_text(indent = px(32)),
    locations = cells_body(
      columns = categoria_delito_modificada,
      rows = categoria_delito_modificada %in% c("Homicidio doloso (víctimas)", "Robo de vehículo con violencia", "Robo de vehículo sin violencia")
    )
  ) %>%
  # FONDO VERDE PARA VARIACION NEGATIVA (DISMINUCION)
  tab_style(
    style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = str_detect(var_2025_vs_2019, "-")),
      cells_body(columns = var_2025_vs_2023, rows = str_detect(var_2025_vs_2023, "-")),
      cells_body(columns = var_2025_vs_2024, rows = str_detect(var_2025_vs_2024, "-"))
    )
  ) %>%
  # FONDO ROJO PARA VARIACION POSITIVA (AUMENTO)
  tab_style(
    style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = !str_detect(var_2025_vs_2019, "-") & var_2025_vs_2019 != "0%"),
      cells_body(columns = var_2025_vs_2023, rows = !str_detect(var_2025_vs_2023, "-") & var_2025_vs_2023 != "0%"),
      cells_body(columns = var_2025_vs_2024, rows = !str_detect(var_2025_vs_2024, "-") & var_2025_vs_2024 != "0%")
    )
  ) %>%
  # FONDO GRIS PARA SIN CAMBIO (0 POR CIENTO)
  tab_style(
    style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = var_2025_vs_2019 == "0%"),
      cells_body(columns = var_2025_vs_2023, rows = var_2025_vs_2023 == "0%"),
      cells_body(columns = var_2025_vs_2024, rows = var_2025_vs_2024 == "0%")
    )
  )%>%
  tab_options(
    table.font.names = "roboto", 
    table.width = "100%",
    table.font.size = px(13),
    data_row.padding = px(4)
  )

# MOSTRAR
gt_tabla

ruta_salida <- here("salidas", "Tbl_acumulados.png")

if (file.exists(ruta_salida)) {
  file.remove(ruta_salida)
}

gtsave(
  data = gt_tabla,
  filename = ruta_salida,
  vwidth = 1200,
  vheight = 1000
)

# SECCION MINI TABLAS DE ACUMULADOS

# DEFINIR ETIQUETAS DINAMICAS POR ANIO
fecha_ultimo_mes <- fecha_corte - months(1)
nombre_mes <- format(fecha_ultimo_mes, "%b") |> tools::toTitleCase()  # JUNIO,JULIO...
nombre_mes <- sub("\\.$", "", nombre_mes)  # ELIMINA PUNTO FINAL

# ETIQUETAS DE TOTALES ACUMULADOS
anios <- c(2019,2023,2024,2025)
etiquetas_totales <- setNames(
  glue("Ene-{nombre_mes} {anios}"),
  paste0("total_", anios)
)

# ETIQUETAS DE VARIACIONES
etiquetas_variaciones <- c(
  var_2025_vs_2019 = "Var % vs 2019",
  var_2025_vs_2023 = "Var % vs 2023",
  var_2025_vs_2024 = "Var % vs 2024"
)

# TODAS LAS ETIQUETAS JUNTAS
etiquetas_finales <- c(
  categoria_delito_modificada = "Delito",
  etiquetas_totales,
  etiquetas_variaciones
)

# CREAR LISTAS PARA TITULOS DE LA PPT
titulos_tarjetas <- tibble(archivo = character(), titulo_acumulado = character(),flecha = character())

# FUNCION PARA GENERAR MINI TABLA GT POR DELITO

generar_mini_tabla_delito <- function(nombre_delito, df) {
  df_filtrado <- df %>%
    filter(categoria_delito_modificada == nombre_delito)
  
  # MINI TABLA
  mini_tabla <- df_filtrado %>%
    select(
      categoria_delito_modificada,
      total_2019, total_2023, total_2024, total_2025,
      var_2025_vs_2019, var_2025_vs_2023, var_2025_vs_2024
    ) %>%
    gt() %>%
    cols_label(.list = etiquetas_finales) %>%
    fmt_number(columns = starts_with("total_"), decimals = 0, sep_mark = ",") %>%
    tab_style(
      style = list(cell_fill(color = "#9d2041ff"),
                   cell_text(color = "white", weight = "bold", size = px(24))),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = categoria_delito_modificada == nombre_delito)) %>%
    tab_style(style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
              locations = list(
                cells_body(columns = var_2025_vs_2019, rows = str_detect(var_2025_vs_2019, "-")),
                cells_body(columns = var_2025_vs_2023, rows = str_detect(var_2025_vs_2023, "-")),
                cells_body(columns = var_2025_vs_2024, rows = str_detect(var_2025_vs_2024, "-"))
              )) %>%
    tab_style(style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
              locations = list(
                cells_body(columns = var_2025_vs_2019, rows = !str_detect(var_2025_vs_2019, "-") & var_2025_vs_2019 != "0%"),
                cells_body(columns = var_2025_vs_2023, rows = !str_detect(var_2025_vs_2023, "-") & var_2025_vs_2023 != "0%"),
                cells_body(columns = var_2025_vs_2024, rows = !str_detect(var_2025_vs_2024, "-") & var_2025_vs_2024 != "0%")
              )) %>%
    tab_style(style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
              locations = list(
                cells_body(columns = var_2025_vs_2019, rows = var_2025_vs_2019 == "0%"),
                cells_body(columns = var_2025_vs_2023, rows = var_2025_vs_2023 == "0%"),
                cells_body(columns = var_2025_vs_2024, rows = var_2025_vs_2024 == "0%")
              )) %>%
    tab_style(style = list(cell_fill(color="#f2f2f2ff"), cell_text(color = "#58595A")), #color gris para el cuerpo de la tabla
                locations = cells_body(
                  columns = c(categoria_delito_modificada,total_2019, total_2023, total_2024, total_2025)
                )) %>%
    tab_options(table.font.names = "roboto", table.width = "100%",
                table.font.size = px(24), data_row.padding = px(4))
  
  # LAS ABREVIACIONES SIRVEN PARA QUE LOS NOMBRES SEAN MAS CORTOS, 
  # SI SE AGREGA UN DELITO AL LISTADO DE DELITOS_DESEADOS HABRIA QUE PENSAR EN UNA ABREVIACION
  
  abreviaciones <- c(
    "Homicidio doloso" = "HD",
    "Homicidio doloso (víctimas)" = "HDV",
    "Lesiones dolosas por disparo de arma de fuego" = "LD",
    "Robo de vehículo con y sin violencia" = "RV",
    "Alto Impacto" = "AI"
    # AGREGAR MAS SI SE REQUIERE
  )
  
  nombre_archivo <- if (!is.na(abreviaciones[nombre_delito])) {
    abreviaciones[nombre_delito]
  } else {
    str_replace_all(str_to_lower(nombre_delito), "[^a-z0-9]+", "_")
  }
  
  ruta_salida <- here("salidas", glue("TblAcum_{nombre_archivo}.png"))
  
  if (file.exists(ruta_salida)) {
    file.remove(ruta_salida)
  }
  
  gtsave(
    data = mini_tabla,
    filename = ruta_salida,
    vwidth = 1150,
    vheight = 200
  )
  
  # EXTRAER VARIACION Y GENERAR TARJETA
  variacion19 <- df_filtrado$var_2025_vs_2019
  if (length(variacion19) > 0 && !is.na(variacion19)) {
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "acumulado",
      tipo_tarjeta = "T"
    )
  }
  
  if (length(variacion19) > 0 && !is.na(variacion19)) {
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "acumulado",
      tipo_tarjeta = "G"
    )
  }
  
  variacion24 <- df_filtrado$var_2025_vs_2024
  if (length(variacion24) > 0 && !is.na(variacion24)) {
    generar_tarjeta_variacion(
      porcentaje = variacion24,
      anio_base = 2024,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "acumulado",
      tipo_tarjeta = "G"
    )
  }
  
  interpretar_titulo <- function(valor) {
    if (str_detect(valor, "-")) {
      return("Continúa la disminución")
    } else if (valor == "0.0%" || valor == "0%") {
      return("No se observaron cambios")
    } else if (!is.na(valor) && valor != "") {
      return("Se observa un incremento")
    } else {
      return("Evolución reciente")
    }
  }
  
  asignar_flecha <- function(valor) {
    if (str_detect(valor, "-")) {
      return("../auxiliares/verde.png")
    } else if (valor == "0.0%" || valor == "0%") {
      return("../auxiliares/gris.png")
    } else if (!is.na(valor) && valor != "") {
      return("../auxiliares/rojo.png")
    } else {
      return(NA_character_)
    }
  }
  
  # NOTA: LOS TITULOS DE LAS TABLAS DONDE DICE SI AUMENTA O INCREMENTA SE BASAN EN LAS VARIACIONES  DE 2019
  
  # ARCHIVO Y RESULTADO
  archivo_tarjeta <- glue("VarAcum_2019T_{nombre_archivo}.png")
  titulo_acumulado <- interpretar_titulo(variacion19)
  icono_flecha <- asignar_flecha(variacion19)
  
  # DEVUELVE TIBBLE (SI YA HICISTE REFACTOR CON MAP_DFR)
  return(tibble(
    archivo = nombre_archivo,
    titulo_acumulado = titulo_acumulado,
    flecha = icono_flecha
  ))

}

# EJECUTAR PARA DELITOS ESPECIFICOS
delitos_deseados <- c(
  "Alto Impacto",
  "Homicidio doloso",
  "Homicidio doloso (víctimas)",
  "Lesiones dolosas por disparo de arma de fuego",
  "Robo de vehículo con y sin violencia"
  # AGREGAR MAS SI SE REQUIERE
)

titulos_tarjetas <- tibble(archivo = character(), titulo_acumulado = character(), flecha = character())
titulos_tarjetas <- map_dfr(delitos_deseados, generar_mini_tabla_delito, df = tabla_formateada_sin_visibles)
write_csv(titulos_tarjetas, file = here("auxiliares", "titulos_tarjetas.csv"))

# SECCION PARA MINI TABLA DE DELITOS EN EL ESPACIO PUBLICO
# DEFINIR DELITOS QUE FORMAN EL TOTAL
delitos_espacio_publico <- c(
  "Robo a transeúnte en vía pública con violencia",
  "Robo a transeúnte en vía pública sin violencia",
  "Robo a pasajero a bordo de microbús con y sin violencia",
  "Robo a pasajero a bordo del metro con violencia",
  "Robo a pasajero a bordo del metrobús con violencia"
)

# CALCULAR FILA TOTAL
fila_total_espacio <- tabla_formateada %>%
  filter(categoria_delito_modificada %in% delitos_espacio_publico) %>%
  summarise(
    categoria_delito_modificada = "Robo a personas en el espacio público",
    total_2019 = sum(total_2019, na.rm = TRUE),
    total_2023 = sum(total_2023, na.rm = TRUE),
    total_2024 = sum(total_2024, na.rm = TRUE),
    total_2025 = sum(total_2025, na.rm = TRUE)
  ) %>%
  mutate(
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  )

# UNIR TOTAL MAS DESGLOSE
tabla_mini <- bind_rows(
  fila_total_espacio,
  tabla_formateada %>%
    filter(categoria_delito_modificada %in% delitos_espacio_publico)
)

# CREAR ETIQUETAS DINAMICAS DE COLUMNA
fecha_ultimo_mes <- fecha_corte - months(1)
nombre_mes <- format(fecha_ultimo_mes, "%b") |> tools::toTitleCase()
nombre_mes <- sub("\\.$", "", nombre_mes)  # ELIMINA EL PUNTO FINAL

etiquetas_totales <- setNames(
  glue("Ene-{nombre_mes} {c(2019,2023,2024, 2025)}"),
  paste0("total_", c(2019, 2023, 2024, 2025))
)

etiquetas_variaciones <- c(
  var_2025_vs_2019 = "Var % vs 2019",
  var_2025_vs_2023 = "Var % vs 2023",
  var_2025_vs_2024 = "Var % vs 2024"
)

etiquetas_finales <- c(
  categoria_delito_modificada = "Delito",
  etiquetas_totales,
  etiquetas_variaciones
)

# GENERAR LA MINI TABLA GT
tabla_gt_mini <- tabla_mini %>%
  gt() %>%
  cols_label(.list = etiquetas_finales) %>%
  fmt_number(
    columns = starts_with("total_"),
    decimals = 0,
    sep_mark = ","
  ) %>%
  # ESTILO ENCABEZADOS
  tab_style(
    style = list(
      cell_fill(color = "#9d2041ff"),
      cell_text(color = "white", weight = "bold", size = px(20))
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f2f2f2ff"), cell_text(color = "#58595A", weight = "bold", size = px(20))),
    locations = cells_body(rows = categoria_delito_modificada == "Robo a personas en el espacio público")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = str_detect(var_2025_vs_2019, "-")),
      cells_body(columns = var_2025_vs_2023, rows = str_detect(var_2025_vs_2023, "-")),
      cells_body(columns = var_2025_vs_2024, rows = str_detect(var_2025_vs_2024, "-"))
    )
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = !str_detect(var_2025_vs_2019, "-") & var_2025_vs_2019 != "0%"),
      cells_body(columns = var_2025_vs_2023, rows = !str_detect(var_2025_vs_2023, "-") & var_2025_vs_2023 != "0%"),
      cells_body(columns = var_2025_vs_2024, rows = !str_detect(var_2025_vs_2024, "-") & var_2025_vs_2024 != "0%")
    )
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = var_2025_vs_2019 == "0%"),
      cells_body(columns = var_2025_vs_2023, rows = var_2025_vs_2023 == "0%"),
      cells_body(columns = var_2025_vs_2024, rows = var_2025_vs_2024 == "0%")
    )
  ) %>%
  tab_options(
    table.font.names = "roboto", 
    table.width = "100%",
    table.font.size = px(18),
    data_row.padding = px(4)
  )

# GUARDAR IMAGEN
ruta_salida <-  here("salidas", "robo_espacio_publico.png")

if (file.exists(ruta_salida)) {
  file.remove(ruta_salida)
}

gtsave(
  data = tabla_gt_mini,
  filename = ruta_salida,
  vwidth = 1150,
  vheight = 800
)

roboPersonas = tabla_mini %>% 
  filter(categoria_delito_modificada == 'Robo a personas en el espacio público')

variacion19= roboPersonas$var_2025_vs_2019
if (length(variacion19) > 0 && !is.na(variacion19)) {
  generar_tarjeta_variacion(
    porcentaje = variacion19,
    anio_base = 2019,
    mes_corte = fecha_corte,
    nombre_delito = "Robo a personas en el espacio público",
    tipo_periodo = "acumulado",
    tipo_tarjeta = "T"
  )
}

variacion24= roboPersonas$var_2025_vs_2024
if (length(variacion24) > 0 && !is.na(variacion24)) {
  generar_tarjeta_variacion(
    porcentaje = variacion24,
    anio_base = 2024,
    mes_corte = fecha_corte,
    nombre_delito = "Robo a personas en el espacio público",
    tipo_periodo = "acumulado",
    tipo_tarjeta = "T"
  )
}
