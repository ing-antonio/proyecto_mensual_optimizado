
# REEMPLAZAR VALORES NULOS EN ALCALDIA_HECHO POR SIN ALCALDIA
semanal9$alcaldia_hecho[semanal9$alcaldia_hecho %in% c(NA, "", "NA", "nulo")] <- "Sin Alcaldía"

fecha_actual9 <- Sys.Date() - dias_a_restar
semana_actual9 <- month(fecha_actual9)  # MES ACTUAL
semana_anterior9 <- month(fecha_actual9 %m-% months(1))  # MES ANTERIOR
semana_pasada9 <- month(fecha_actual9 %m-% months(2))  # DOS MESES ANTES

# CREAR DATAFRAME CON LOS TRES PERIODOS SEMANALES
df_semanal9 <- semanal9 %>% 
  mutate(
    numero_semana9 = month(fecha_inicio),
    periodo = case_when(
      numero_semana9 == semana_anterior9 ~ "Anterior",
      numero_semana9 == semana_actual9 ~ "Actual"
    )
  ) %>% 
  filter(!is.na(periodo)) 

agrupado <- df_semanal9 %>% 
      filter(categoria_delito == "Homicidio doloso" | categoria_delito == 'Lesiones dolosas por disparo de arma de fuego') %>% 
      procesar_datos(agrupaciones = c("alcaldia_hecho", "colonia_hecho", "nombre_sec", "Nomenclatu", "categoria_delito", "periodo"), 
                     periodo = c("categoria_delito")) %>% 
      pivotear_por_delito(periodo = "periodo", columnas = c("Homicidio doloso", "Lesiones dolosas por disparo de arma de fuego"))

# GENERAR GRAFICA PARTICIONADA
datos <- tabla_desglosada_final(agrupado)

datos <- datos %>% slice(-1)

datos <- datos %>%
      filter(Nomenclatu != "TOTALES")

datos <- datos[, -5]
datos <- datos[, -7]
datos <- datos %>%
      group_by(Nomenclatu) %>%
      mutate(Nomenclatu = ifelse(Nomenclatu == "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)",
                                 paste0(Nomenclatu, "_", row_number()),
                                 Nomenclatu)) %>%
      ungroup()

# TRANSFORMAR LOS DATOS PARA INCLUIR AMBOS PERIODOS EN EL PRIMER NIEVEL
filtered_data <- datos %>%
      mutate(
            Homicidios2023 = `Homicidio doloso_Anterior`,
            Homicidios2024 = `Homicidio doloso_Actual`,
            Lesiones2023 = `Lesiones dolosas por disparo de arma de fuego_Anterior`,
            Lesiones2024 = `Lesiones dolosas por disparo de arma de fuego_Actual`
      )

# NIVEL 1: GENERAR DATOS PARA LAS BARRAS DE 2023 Y 2024, ORDENARLOS POR EL VALOR TOTAL
nivel1 <- filtered_data %>%
      group_by(alcaldia_hecho) %>%
      summarise(
            total_delitos_2023 = sum(Homicidios2023 + Lesiones2023, na.rm = TRUE),
            total_delitos_2024 = sum(Homicidios2024 + Lesiones2024, na.rm = TRUE),
            Homicidios2023 = sum(Homicidios2023, na.rm = TRUE),
            Homicidios2024 = sum(Homicidios2024, na.rm = TRUE),
            Lesiones2023 = sum(Lesiones2023, na.rm = TRUE),
            Lesiones2024 = sum(Lesiones2024, na.rm = TRUE),
            .groups = "drop"
      ) %>%
      arrange(desc(total_delitos_2023 + total_delitos_2024)) %>%
      mutate(
            name = alcaldia_hecho,
            drilldown_2023 = paste0(alcaldia_hecho, "_2023"),
            drilldown_2024 = paste0(alcaldia_hecho, "_2024"),
            tooltip = paste0(
                  "<b>Alcaldía:</b> ", alcaldia_hecho, "<br>",
                  "<b>Homicidios mes actual:</b> ", Homicidios2024, "<br>",
                  "<b>Homicidios mes pasado:</b> ", Homicidios2023, "<br>",
                  "<b>Lesiones mes actual:</b> ", Lesiones2024, "<br>",
                  "<b>Lesiones mes pasado:</b> ", Lesiones2023
                  
            )
      )

# NIVEL2: DETALLES POR CUADRANTE, ORDENARLOS DE MAYOR A MENOR DENTRO DE CADA ALCALDIA
nivel2 <- filtered_data %>%
      group_by(alcaldia_hecho, Nomenclatu) %>%
      summarise(
            total_delitos_2023 = sum(Homicidios2023 + Lesiones2023, na.rm = TRUE),
            total_delitos_2024 = sum(Homicidios2024 + Lesiones2024, na.rm = TRUE),
            Homicidios2023 = sum(Homicidios2023, na.rm = TRUE),
            Homicidios2024 = sum(Homicidios2024, na.rm = TRUE),
            Lesiones2023 = sum(Lesiones2023, na.rm = TRUE),
            Lesiones2024 = sum(Lesiones2024, na.rm = TRUE),
            .groups = "drop"
      ) %>%
      arrange(desc(total_delitos_2023 + total_delitos_2024))

# CREAR DRILLDOWN PARA 2023 Y 2024
drilldown_nivel2 <- list()

for (periodo in c("2023", "2024")) {
      drilldown_nivel2[[periodo]] <- nivel2 %>%
            group_by(alcaldia_hecho) %>%
            group_split() %>%
            lapply(function(data) {
                  data <- data %>% arrange(desc(data[[paste0("total_delitos_", periodo)]]))
                  list(
                        id = paste0(unique(data$alcaldia_hecho), "_", periodo),
                        data = lapply(1:nrow(data), function(i) {
                              list(
                                    name = data$Nomenclatu[i],
                                    y = data[[paste0("total_delitos_", periodo)]][i],
                                    customTooltip = paste0(
                                          "<b>", data$Nomenclatu[i], "</b><br>",
                                          "Homicidios mes actual: ", data$Homicidios2024[i], "<br>",
                                          "Homicidios mes pasado: ", data$Homicidios2023[i], "<br>",
                                          "Lesiones mes actual: ", data$Lesiones2024[i], "<br>",
                                          "Lesiones mes pasado: ", data$Lesiones2023[i]
                                          
                                    )
                              )
                        })
                  )
            })
}

# CREAR GRAFICO CON COLORES ESPECIFICOS PARA CADA SERIE
grafico <- highchart() %>%
      hc_chart(type = "column", zoomType = "xy") %>%
      hc_title(text = "Total de homicidios y lesiones dolosas por arma de fuego por alcaldía", style = list(fontSize = "35px")) %>%
      hc_xAxis(type = "category",labels = list(style = list(fontSize = "16px"))) %>%
      hc_yAxis(labels = list(style = list(fontSize = "24px", color = "black", face = "bold")))%>% 
      hc_tooltip(
            useHTML = TRUE,
            style = list(fontSize = "17px"),
            formatter = JS(
                  "function() {
        if (this.point.customTooltip) {
          return this.point.customTooltip;
        } else {
          return '<b>' + this.point.name + '</b><br>Total: ' + this.y;
        }
      }"
            )
      ) %>%
      hc_plotOptions(
            series = list(
                  dataLabels = list(enabled = TRUE,style = list(fontSize = "21px", color = "black")),
                  keys = c("name", "y", "customTooltip", "drilldown")
            )
      ) %>%
      hc_add_series(
            name = "Total delitos mes pasado",
            color = "#BC955C",
            data = nivel1 %>%
                  select(name, total_delitos_2023, drilldown_2023, tooltip) %>%
                  rename(y = total_delitos_2023, drilldown = drilldown_2023, customTooltip = tooltip) %>%
                  list_parse()
      ) %>%
      hc_add_series(
            name = "Total delitos mes actual",
            color = "#B02858",
            data = nivel1 %>%
                  select(name, total_delitos_2024, drilldown_2024, tooltip) %>%
                  rename(y = total_delitos_2024, drilldown = drilldown_2024, customTooltip = tooltip) %>%
                  list_parse()
      ) %>%
      hc_drilldown(
            series = c(drilldown_nivel2[["2023"]], drilldown_nivel2[["2024"]])
      )

# GUARDAR GRAFICOS COMO HTML
grafico <- grafico %>%
  hc_legend(
    itemStyle = list(
      fontSize = "25px" # SE AJUSTA EL TAMANIO
    )
  )

# GUARDAR GRAFICO
saveWidget(grafico, 
           file = here('salidas/barras_homi_les_alc.html') , 
           selfcontained = TRUE)

# CAPTIRAR HTML COMO PNG
webshot2::webshot(
  url = here('salidas/barras_homi_les_alc.html'),
  file = here('salidas/barras_homi_les_alc.png'),
  vwidth = 2500,
  vheight = 1000
)
