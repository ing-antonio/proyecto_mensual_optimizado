
# IMAGENES TBLMES_...PNG
# IMAGENES VARMES_...PNG

# GUARDAR RDATA
save(datos5, file = here("datos", "datos_crudos_semestral.RData"))

# PIVOTEAR Y CALCULAR VARIACIONES
tabla <- datos5 %>%
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

# SACAR TOTALES DE ALTO IMPACTO Y DE ROBO CON Y SIN VIOLENCIA

# FILA ALTO IMPACTO
fila_total <- tabla %>%
  filter(categoria_delito_modificada != "Homicidio doloso (víctimas)" & categoria_delito_modificada != "Robo a pasajero a bordo del metrobús con violencia") %>%   # 👈 excluye víctimas y robo metrobus
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

# CREAR VECTOR DE ORDEN
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

# SE PUEDEN QUITAR DELITOS DE LA TABLA (SI SE REQUIERE)
tabla_formateada_sin_visibles <- tabla_formateada %>%
  filter(
    !categoria_delito_modificada %in% c("")
  ) %>%
  mutate(
    orden = orden_delitos[categoria_delito_modificada]
  ) %>%
  arrange(orden)

# FECHA DE CORTE: PRIMER DIA DEL MES ACTUAL
fecha_corte <- floor_date(Sys.Date(), unit = "month")

# MES ANTERIOR COMPLETO (ULTIMO MES CON DATOS CERRADOS)
fecha_ultimo_mes5 <- fecha_corte - months(1)

# VARIABLE FECHA_ULTIMO_MES_REAL PARA TITULOS DE CUADROS
# fecha_ultimo_mes_real <- fecha_corte5
nombre_mes5 <- format(fecha_ultimo_mes5, "%B") |> tools::toTitleCase()

fecha_ultimo_mes5 <- fecha_corte - months(1)

# NOMBRE DEL MES (EN ESPANIOL)
nombre_mes5 <- format(fecha_ultimo_mes5, "%B") |> tools::toTitleCase()

# ANIOS RELEVANTES
anios <- c(2019,2023,2024,2025)
etiquetas_totales <- setNames(
  glue("{nombre_mes5} {anios_objetivo}"),
  paste0("total_", anios_objetivo)
)

# SE CREA LA TABLA GT
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
    columns = c(total_2025, total_2024, total_2019),
    decimals = 0,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  
  # ESTILO ENZABEZADOS
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
  
  # FONDO GRIS PARA SIN CAMBIO (CERO POR CIENTO)
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

# MINI TABLAS DE ACUMULADOS

# DEFINIR ETIQUETAS DINAMICAS POR ANIO
fecha_corte <- floor_date(Sys.Date(), unit = "month")
nombre_mes <- format(fecha_ultimo_mes5, "%B") |> tools::toTitleCase()  # "Junio", "Julio", etc.

# ETIQUETAS TOTALES ACUMULADOS
anios <- c(2019,2023,2024,2025)
etiquetas_totales <- setNames(
  glue("{nombre_mes5} {anios}"),
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

# LECTURA DE CSV EXISTENTE GENERADO POR ACUMULADOS
# DONDE SE PONEN TITULOS QUE DICEN SI HUBO INCREMENTO O DISMINUCION Y
# SE ASIGNA LA FLECHA DEL COLOR QUE CORRESPONDA 
ruta_csv <- here("auxiliares", "titulos_tarjetas.csv")
titulos_existentes <- read_csv(ruta_csv, show_col_types = FALSE)

# FUNCINON PARA GENERAR MINI TABLA GT POR DELITO
generar_mini_tabla_delito <- function(nombre_delito, df) {
  mini_tabla <- df %>%
    filter(categoria_delito_modificada == nombre_delito) %>%
    select(
      categoria_delito_modificada,
      total_2019, total_2023, total_2024, total_2025,
      var_2025_vs_2019, var_2025_vs_2023, var_2025_vs_2024
    ) %>%
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
        cell_text(color = "white", weight = "bold", size = px(24))
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = categoria_delito_modificada == nombre_delito)
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
    )  %>%
    tab_style(
      style = list(cell_fill(color="#f2f2f2ff"), cell_text(color = "#58595A")), #color gris para el cuerpo de la tabla
      locations = cells_body(
        columns = c(categoria_delito_modificada,total_2019, total_2023, total_2024, total_2025)
      )
    ) %>%
    tab_options(
      table.font.names = "roboto", 
      table.width = "100%",
      table.font.size = px(24),
      data_row.padding = px(4)
    )
  
  # NOMBRE LIMPIO PARA EL ARCHIVO
  abreviaciones <- c(
    "Homicidio doloso" = "HD",
    "Homicidio doloso (víctimas)" = "HDV",
    "Lesiones dolosas por disparo de arma de fuego" = "LD",
    "Robo de vehículo con y sin violencia" = "RV",
    "Alto Impacto" = "AI"
    # AGREAGAR MAS SI SE REQUIERE
  )
  
  nombre_archivo <- if (!is.na(abreviaciones[nombre_delito])) {
    abreviaciones[nombre_delito]
  } else {
    str_replace_all(str_to_lower(nombre_delito), "[^a-z0-9]+", "_")
  }
  
  # GUARDAR COMO PNG
  gtsave(
    data = mini_tabla,
    filename = here("salidas", glue("TblMes_{nombre_archivo}.png")),
    vwidth = 1350,
    vheight = 200
  )
  
  # EXTRAER VARIACION Y GENERAR TARJETAS
  variacion19 <- df %>%
    filter(categoria_delito_modificada == nombre_delito) %>%
    pull(var_2025_vs_2019)
 
  
  if (length(variacion19) > 0 && !is.na(variacion19)) {
   
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "mensual",
      tipo_tarjeta = "T"
    )
  }
  
  if (length(variacion19) > 0 && !is.na(variacion19)) {
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "mensual",
      tipo_tarjeta = "G"
    )
  }
  
  variacion24 <- df %>%
    filter(categoria_delito_modificada == nombre_delito) %>%
    pull(var_2025_vs_2024)
  if (length(variacion24) > 0 && !is.na(variacion24)) {
    
    generar_tarjeta_variacion(
      porcentaje = variacion24,
      anio_base = 2024,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "mensual",
      tipo_tarjeta = "G"
    )
  }
  
  interpretar_titulo <- function(valor) {
    if (str_detect(valor, "-")) {
      return("Continúa la disminución")
    } else if (valor == "0.0%" || valor == "0%") {
      return("Sin cambio mensual")
    } else if (!is.na(valor) && valor != "") {
      return("Incremento mensual")
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
  
  # CONSTRUIR NOMBRE DE ARCHIVO CON LA TARJETA MENSUAL
  archivo_tarjeta <- glue("VarMes_2019T_{nombre_archivo}.png")
  
  # CALCULAR TITULO Y FLECHA
  titulo_mensual <- interpretar_titulo(variacion24)
  flecha_mensual <- asignar_flecha(variacion24)
  
  # RETORNAR COMO TIBBLE
  return(tibble(
    archivo = nombre_archivo,
    titulo_mensual = titulo_mensual,
    flecha_mensual = flecha_mensual
  ))
  
  
}

# EJECUTAR PARA TODOS LOS DELITOS
delitos_deseados <- c(
  "Alto Impacto",
  "Homicidio doloso",
  "Homicidio doloso (víctimas)",
  "Lesiones dolosas por disparo de arma de fuego",
  "Robo de vehículo con y sin violencia"
  # AGREAGAR MAS SI SE REQUIERE
)

titulos_mensuales <- map_dfr(delitos_deseados,generar_mini_tabla_delito,df = tabla_formateada_sin_visibles)
titulos_actualizados <- titulos_existentes %>%
  left_join(titulos_mensuales, by = "archivo")

write_csv(titulos_actualizados, ruta_csv)

