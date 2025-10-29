
# TABLA_VARIACION_MENSUAL_PROMEDIOS.PNG

fecha_corte <- today() - day(today())  
#fecha_corte <- today()

# SE ESTABLECEN CONSTANTES DE PERIODOS CON CORTE AL MES COMPLETO
mes_actual <- month(fecha_corte)
mes_anterior <- ifelse(mes_actual == 1, 12, mes_actual - 1)
anio_actual <- year(fecha_corte)
anio_anterior <- anio_actual - 1
anio_pasado <- anio_actual - 2

# DIAS DE CADA MES
dias_mes_actual <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_actual, mes_actual)))
dias_mes_anterior <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_actual, mes_anterior)))
dias_mes_anio_prev <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_anterior, mes_actual)))
dias_mes_anio_past <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_pasado, mes_actual)))

# AGRUPAR DATOS POR PERIODOS
# SE USAN FUNCIONES DEL SCRIPT FUNCIONES_MENSUAL
df_mes_actual     <- filtrar_mes(carpetas, fecha_inicio, anio_actual, mes_actual)
df_mes_anterior   <- filtrar_mes(carpetas, fecha_inicio, anio_actual, mes_anterior)
df_mes_anio_prev  <- filtrar_mes(carpetas, fecha_inicio, anio_anterior, mes_actual)
df_mes_anio_past  <- filtrar_mes(carpetas, fecha_inicio, anio_pasado, mes_actual)

# SE SUMAN LOS DELITOS POR PERIODO
actual   <- contar_por_delito(df_mes_actual, "Actual")
previo   <- contar_por_delito(df_mes_anterior, "Mes_Anterior")
anterior <- contar_por_delito(df_mes_anio_prev, "Anterior")
pasado   <- contar_por_delito(df_mes_anio_past, "Pasado")

# SE OBTIENEN PROMEDIOS DIARIOS DE DELITOS POR PERIODO
actual <- actual %>% mutate(Actual = Actual / dias_mes_actual)
previo <- previo %>% mutate(Mes_Anterior = Mes_Anterior / dias_mes_anterior)
anterior <- anterior %>% mutate(Anterior = Anterior / dias_mes_anio_prev)
pasado <- pasado %>% mutate(Pasado = Pasado / dias_mes_anio_past)

# SE USA FUNCION REDUCE DEL PAQUETE PURR PARA HACER UNA CADENA DE UNIONES (JOINS)
# FULL_JOIN JUNTA TODOS LOS DATAFRAMES POR LA COLUMNA CATEGORIA_DELITO
# SI ALGUNA CATEGORIA ESTA EN UNO Y OTRO GRUPO, SE CONSERVA IGUAL (POR ESO ES FULL)
df <- reduce(list(previo, actual, anterior, pasado), full_join, by = "categoria_delito") %>%
  replace(is.na(.), 0) %>%
  mutate(
    variacion_mensual = variaciones(Actual, Mes_Anterior),
    variacion_anual = variaciones(Actual, Anterior)
  )

# AGRUPAR DATOS DE VICTIMAS POR PERIODOS
df_victimas <- tibble(
  categoria_delito = "Homicidio doloso (víctimas)",
  Actual = victimas_mes(anio_actual, mes_actual) / dias_mes_actual,
  Mes_Anterior = victimas_mes(anio_actual, mes_anterior) / dias_mes_anterior,
  Anterior = victimas_mes(anio_anterior, mes_actual) / dias_mes_anio_prev,
  Pasado = victimas_mes(anio_pasado, mes_actual) / dias_mes_anio_past
) %>%
  mutate(
    variacion_mensual = variaciones(Actual, Mes_Anterior),
    variacion_anual = variaciones(Actual, Anterior)
  )

# SE UENEN LAS DOS TABLAS: CARPETAS Y VICTIMAS
df <- bind_rows(df, df_victimas)

# FILA RESUMEN: TOTAL ALTO IMPACTO
fila_alto_impacto <- df %>%
  filter(categoria_delito != 'Homicidio doloso (víctimas)') %>%
  summarise(
    categoria_delito = "Alto Impacto",
    Mes_Anterior = sum(Mes_Anterior),
    Actual = sum(Actual),
    Anterior = sum(Anterior),
    Pasado = sum(Pasado),
    variacion_mensual = variaciones(sum(Actual), sum(Mes_Anterior)),
    variacion_anual = variaciones(sum(Actual), sum(Anterior))
  )

# FILA ROBO DE VEHICULO C/S VIOLENCIA
fila_robo <- df %>%
  filter(categoria_delito == "Robo de vehículo con violencia" | categoria_delito == 'Robo de vehículo sin violencia') %>%
  summarise(
    categoria_delito = "Robo de vehículo con y sin violencia",
    Mes_Anterior = sum(Mes_Anterior),
    Actual = sum(Actual),
    Anterior = sum(Anterior),
    Pasado = sum(Pasado),
    variacion_mensual = variaciones(sum(Actual), sum(Mes_Anterior)),
    variacion_anual = variaciones(sum(Actual), sum(Anterior))
  )

# SE AGREGAN FILAS DE TOTAL ALTO IMPACTO Y ROBO CON Y SIN VIOLENCIA
df <- bind_rows(fila_alto_impacto,fila_robo, df)

# ORDEN DE DELITOS PARA QUE ASI SALGAN EN LA TABLA
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
  "Robo a pasajero a bordo del metro sin violencia" = 27
)

# ORDEN DE COLUMNAS
df <- df %>%
  mutate(orden = orden_delitos[categoria_delito]) %>%
  arrange(orden) %>%
  mutate(Actual_repetido = Actual) %>% # SE CREA NUEVA COLUMNA PARA QUE SALGA DE NUEVO EL MES ACTUAL
  select(
    categoria_delito,
    Mes_Anterior,
    Actual,
    variacion_mensual,
    Pasado,
    Anterior,
    Actual_repetido,
    variacion_anual
  )

# SE GENERA GRAFICA
tabla_gt <- df %>%
  gt() %>%
  cols_label(
    categoria_delito = "Categoría del Delito",
    Mes_Anterior = paste0(mes_anterior_esp, " ", anio_actual),
    Actual = paste0(mes_actual_esp, " ", anio_actual),
    Anterior = paste0(mes_actual_esp, " ", anio_anterior),
    Pasado = paste0(mes_actual_esp, " ", anio_pasado),
    Actual_repetido = paste0(mes_actual_esp, " ", anio_actual),
    variacion_mensual = paste0("Var ", mes_anterior_esp, " ", anio_actual, " vs ", mes_actual_esp, " ", anio_actual),
    variacion_anual = paste0("Var ", mes_actual_esp, " ", anio_anterior, " vs ",mes_actual_esp, " ", anio_actual)
  ) %>%
  # FORMATO NUMERICO
  fmt_number(
    columns = c(Mes_Anterior, Actual, Pasado, Anterior, Actual_repetido),
    decimals = 1,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  # FORMATO PARA VARIACIONES
  fmt_number(
    columns = c(variacion_mensual,variacion_anual),
    decimals = 1,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  # ENCABEZADOS COLOR VINO
  tab_style(
    style = list(
      cell_fill(color = "#9d2041ff"),
      cell_text(color = "white", weight = "bold", size = px(16))
    ),
    locations = cells_column_labels(everything())
  ) %>%
  # TOTAL DE ALTO IMPACTO
  tab_style(
    style =list(cell_fill(color = "#DAD1C5"), cell_text(weight = "bold")),
    locations = cells_body(rows = categoria_delito == "Alto Impacto")
  ) %>%
  # SANGRIA PARA TODOS, EXCEPTO ALTO IMPACTO Y SUBTIPOS DE ROBO DE VEHICULO
  tab_style(
    style = cell_text(indent = px(16)),
    locations = cells_body(columns = categoria_delito, 
                           rows = categoria_delito != "Alto Impacto" &
                             categoria_delito != "Robo de vehículo con violencia" &
                             categoria_delito != "Robo de vehículo sin violencia")
  ) %>%
  # SANGRIA GRANDE PARA SUBTIPOS DE ROBO DE VEHICULO Y VICTIMAS
  tab_style(
    style = cell_text(indent = px(32)),
    locations = cells_body(
      columns = categoria_delito,
      rows = categoria_delito %in% c("Homicidio doloso (víctimas)", "Robo de vehículo con violencia", "Robo de vehículo sin violencia")
    )
  ) %>%
  # VARIACIONES NEGATIVAS (VERDE)
  tab_style(
    style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
    locations = list(
      cells_body(columns = variacion_mensual, rows = str_detect(variacion_mensual, "-")),
      cells_body(columns = variacion_anual, rows = str_detect(variacion_anual, "-"))
    )
  ) %>%
  # VARIACIONES POSITIVAS (ROJO)
  tab_style(
    style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
    locations = list(
      cells_body(columns = variacion_mensual, rows = !str_detect(variacion_mensual, "-") & variacion_mensual != "0%"),
      cells_body(columns = variacion_anual, rows = !str_detect(variacion_anual, "-") & variacion_anual != "0%")
    )
  ) %>%
  # VARIACIONES SIN CAMBIO (GRIS)
  tab_style(
    style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
    locations = list(
      cells_body(columns = variacion_mensual, rows = variacion_mensual == "0.0%"),
      cells_body(columns = variacion_anual, rows = variacion_anual == "0.0%")
    )
  ) %>%
  # OPCIONES GENERALES
  cols_width(
    categoria_delito ~ px(320),
    variacion_mensual ~ px(120),
    variacion_anual ~ px(120),
    everything() ~ px(100)  # las demás columnas
  ) %>%
  tab_options(
    table.font.names = "roboto", 
    table.width = "100%",
    table.font.size = px(13),
    data_row.padding = px(4)
  )

tabla_gt

# GUARDADO DE TABLA COMO PNG
ruta_salida <- here("salidas", "tabla_variacion_mensual_promedios.png")

if (file.exists(ruta_salida)) {
  file.remove(ruta_salida)
}

gtsave(
  tabla_gt,
  filename = ruta_salida,
  vwidth = 1200,
  vheight = 1000
)

