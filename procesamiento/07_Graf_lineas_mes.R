
# IMAGENES GRAFICO_AMES...PNG

# DICCIONARIO DE ABREVIACIONES
abreviaciones <- c(
  "Alto Impacto" = "AI",
  "Homicidio doloso" = "HD",
  "Homicidio doloso (víctimas)" = "HDV",
  "Lesiones dolosas por disparo de arma de fuego" = "LD",
  "Robo de vehículo con y sin violencia" = "RV"
)

# FUNCION PARA GENERAR GRAFICO POR DELITO
generar_grafico_delito <- function(nombre_delito, datos) {

  # FILTRAR DATOS DE ENERO AL ULTIMO MES COMPLETO
  datos_filtrados <- datos %>%
    mutate(
      year = year(fecha_inicio),
      month = month(fecha_inicio)
    )
  
  if (nombre_delito == "Alto Impacto") {
    # TODOS LOS DELITOS MENOS 'HOMICILIO DOLOSO (VICTIMAS)'
    datos_filtrados <- datos_filtrados %>%
      filter(categoria_delito != "Homicidio doloso (víctimas)")
    
  } else if (nombre_delito == "Robo de vehículo con y sin violencia") {
    datos_filtrados <- datos_filtrados %>%
      filter(categoria_delito %in% c(
        "Robo de vehículo con violencia",
        "Robo de vehículo sin violencia"
      ))
    
  } else {
    # COINCIDENCIA EXACTA
    datos_filtrados <- datos_filtrados %>%
      filter(categoria_delito == nombre_delito)
  }
  
  resumen <- datos_filtrados %>%
    group_by(year) %>%
    summarise(
      total_registros = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      dias_mes = days_in_month(as.Date(paste0(year, "-", ultimo_mes_completo, "-01"))),
      promedio_diario = total_registros / dias_mes
    )
  
  if (nrow(resumen) == 0) {
    message(glue("⚠️ No hay datos para {nombre_delito}, se omite la gráfica"))
    return(NULL)
  }
  
  ultimo_anio <- max(resumen$year)
  
  p <- ggplot(resumen, aes(x = factor(year), y = promedio_diario)) +
    geom_line(aes(group = 1), color = "#9f2240", size = 2) +
    geom_point(color = "#9f2240", size = 5) +
    geom_text(
      data = resumen %>% filter(year != ultimo_anio),
      aes(label = round(promedio_diario, 2)), 
      vjust = 0.1, hjust = -0.3, size = 14, color = "black", fontface = "bold"
    ) +
    geom_text(
      data = resumen %>% filter(year == ultimo_anio),
      aes(label = round(promedio_diario, 2)), 
      vjust = -0.5, hjust = 0.1, size = 19, color = "#007A33", fontface = "bold"
    ) +
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "gray40", linetype = "dashed", alpha = 0.7) +
    labs(
      title = "",
      subtitle = "",
      x = "", y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
      axis.text.x = element_text(angle = 0, hjust = 1, size = 20, color = "black", face = "bold"),
      axis.text.y = element_text(angle = 0, hjust = 1, size = 20, color = "black", face = "bold"),
      
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 9, color = "gray50")
    ) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 0.1), limits = c(min(resumen$promedio_diario) - 1, max(resumen$promedio_diario) + 1))
    #scale_y_continuous(labels = scales::comma_format(accuracy = 0.1),limits = c(0, NA))
    #scale_y_continuous(labels = scales::comma_format(accuracy = 0.1))
  
  # Usar abreviación si existe
  nombre_archivo <- if (!is.na(abreviaciones[nombre_delito])) {
    abreviaciones[nombre_delito]
  } else {
    str_replace_all(str_to_lower(nombre_delito), "[^a-z0-9]+", "_")
  }
  
  ggsave(
    filename = here("salidas", glue("grafico_mes_{nombre_archivo}.png")),
    plot = p,
    width = 20,
    height = 10,
    dpi = 100
  )
  
  
  message(glue("✅ Gráfico guardado para: {nombre_delito} → {nombre_archivo}.png"))
}

# EJECUTAR PARA TODOS LOS DELITOS DESEADOS
delitos_deseados <- c(
  "Alto Impacto",
  "Homicidio doloso",
  "Homicidio doloso (víctimas)",
  "Lesiones dolosas por disparo de arma de fuego",
  "Robo de vehículo con y sin violencia"
)

# PARA LA FUNCION WALK DEL PAQUETE PURRR
# EL PRIMER ARGUMENTO (DELITO) SERA EL DE LA LISTA DE DELITOS DESEADOS
# EL SEGUNDO ARGUMENTO ES LA FUNCION QUE GENERA EL GRAFICO
# EL ARGUMENTO DATOS = CONSULTA_TOTAL7 SE OBTUVO DEL QUERY Y SE PASA PARA TODAS LAS EJECUCIONES
walk(delitos_deseados, generar_grafico_delito, datos = consulta_total7)
