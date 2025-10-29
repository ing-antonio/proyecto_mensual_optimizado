
# IMAGEN GRAFICAS_COMBINADAS.PNG

if (bandera == 1) {
  fecha_corte <- today() - day(today())  
} else {
  fecha_corte <- today()
}

anio_actual <- year(fecha_corte)
anio_anterior <- anio_actual - 1
anio_pasado <- anio_actual - 2
mes_actual <- month(fecha_corte)
dia_final_mes <- day(fecha_corte)

# FORMATOS
mes_fmt <- sprintf("%02d", mes_actual)
dia_fmt <- sprintf("%02d", dia_final_mes)

# PERIODOS MENSUALES PARA GRAFICO MENSUAL
mes_anterior <- ifelse(mes_actual == 1, 12, mes_actual - 1)

df_mensual_alcaldia <- df_alcaldia %>%
  filter(fecha_inicio >= as.Date("2024-12-30")) %>%
  mutate(
    mes = month(fecha_inicio),
    periodo = case_when(
      mes == mes_anterior ~ "Anterior",
      mes == mes_actual ~ "Actual"
    )
  ) %>%
  filter(!is.na(periodo))

# PERIODOS ANUALES COMPARABLES (MISMO MES - DISTINTO ANIO)
df_anual_alcaldia <- df_alcaldia %>%
  mutate(periodo = case_when(
    between(fecha_inicio,
            as.Date(paste0(anio_anterior, "-", mes_fmt, "-01")),
            as.Date(paste0(anio_anterior, "-", mes_fmt, "-", dia_fmt))) ~ "Anterior",
    between(fecha_inicio,
            as.Date(paste0(anio_actual, "-", mes_fmt, "-01")),
            as.Date(paste0(anio_actual, "-", mes_fmt, "-", dia_fmt))) ~ "Actual"
  )) %>%
  filter(!is.na(periodo))

# GENERAR GRAFICOS COMPARATIVOS
# SE USAN FUNCIONES DEL SCRIPT FUNCIONES_MENSUAL.R
grafico_mensual <- grafico_comparada(df_mensual_alcaldia, "mensual")  # antes llamado "semanal"
grafico_anual <- grafico_comparada(df_anual_alcaldia, "anual")

grafico_combinado <- grafico_mensual | grafico_anual

# GUARDAR
ggsave(
  here('salidas', 'graficas_combinadas.png'),
  plot = grafico_combinado,
  width = 15,
  height = 7
)
