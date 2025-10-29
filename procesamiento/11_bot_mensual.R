
library(telegram)

here::i_am("procesamiento/11_bot_mensual.R")
ppt <- here("procesamiento/crear_ppt.py")

# TOQUEN
token_bot <- "7754193507:AAH6E6Li-iQrkDq9ZBZqjEVQqG-HurZVYo8"
# ID GRUPO
chat_id <- "-1003036703770"   
# TEMA: REPORTE MENSUAL
thread_id <- 3            

enviar_mensaje_telegram <- function(mensaje, token_bot, id_chat, thread_id) {
  url <- paste0("https://api.telegram.org/bot", token_bot, "/sendMessage")
  respuesta <- httr::POST(
    url,
    body = list(
      chat_id = id_chat,
      message_thread_id = thread_id,
      text = mensaje,
      parse_mode = "HTML"
    ),
    encode = "form"
  )
  
  if (respuesta$status_code != 200) {
    cat("❌ Error enviando mensaje:", httr::content(respuesta)$description, "\n")
  }
  
  return(respuesta)
}

directorio <- here("procesamiento")
auxiliares_dir <- here("auxiliares")
datos_dir <- here("datos")  
salidas_dir <- here("salidas")
procesamiento_dir <- here("procesamiento")

ejecutar_script <- function(ruta_script) {
  tryCatch(
    {
      setwd(directorio)
      source(ruta_script)
      enviar_mensaje_telegram(
        paste0("✅ El script ", basename(ruta_script), " se ejecutó correctamente."),
        token_bot,
        chat_id,
        thread_id
      )
    },
    error = function(e) {
      enviar_mensaje_telegram(
        paste0("❌ Error al ejecutar el script ", basename(ruta_script), ": ", e$message),
        token_bot,
        chat_id,
        thread_id
      )
    }
  )
}

# INICIO DE PROCESO
enviar_mensaje_telegram("🚀 <b>MONITOREO REPORTE MENSUAL</b>", token_bot, chat_id, thread_id)

# LISTA DE SCRIPTS A EJECUTAR
scripts <- c(
  "00_objetos_rdata.R",
  "01_Tbl_absolutos.R",
  "02_Tbl_promedios.R",
  "03_Barras_alcaldias.R",
  "04_Tbl_acumulados.R",
  "05_Tbl_comparativo_mensual.R",
  "06_Graf_lineas_acum.R",
  "07_Graf_lineas_mes.R",
  "08_barras_alc_sec_cuad.R",
  "09_barras_homicidios_lesiones_alcaldia.R",
  "10_mapa.R"
)

# EJECUCION CON PAUSAS
for (script in scripts) {
  cat("Ejecutando:", script, "\n")
  ejecutar_script(here(procesamiento_dir, script))
  enviar_mensaje_telegram("⏰ 30 segundos para autoguardar", token_bot, chat_id, thread_id)
  Sys.sleep(30)
}

cat("▶ Ejecutando script Python para generar presentación...\n")
result_py <- system2("python", args = ppt, stdout = TRUE, stderr = TRUE)
cat("Resultado del script Python:\n", paste(result_py, collapse = "\n"), "\n")

este_anio <- format(Sys.Date(), "%Y")
este_mes <- format(Sys.Date(), "%m")
este_dia <- format(Sys.Date(), "%d")

# SUBIR ARCHIVO PPT MENSUAL
nombre_archivo <- paste0(este_anio,este_mes,este_dia, "_reporte_mensual.pptx")
output_path <- here("salidas", nombre_archivo)

if (!file.exists(output_path)) {
  #enviar_mensaje_telegram("❌ Error: No se pudo encontrar el archivo PPT generado", token_bot, chat_id, thread_id)
} else {
  #enviar_mensaje_telegram("📊 PPT generada exitosamente. Subiendo a Google Drive...", token_bot, chat_id, thread_id)
  
  # AUTENTICACION CON CUENTA DE SERVICIO
  service_json <- here("auxiliares/servicio_de_cuenta.json")
  drive_auth(path = service_json)
  
  # ID DE CARPETA GOOGLE DRIVE
  folder_id <- "1HsS-IAXql5SKM989WenIxL7emiNrPQxd"
  
  # SUBIR ARCHIVO A GOOGLE DRIVE
  tryCatch({
    drive_upload(
      media = output_path,
      path = as_id(folder_id),
      name = nombre_archivo,
      overwrite = TRUE
    )
    #enviar_mensaje_telegram("✅ Archivo subido exitosamente a Google Drive", token_bot, chat_id, thread_id)
  }, error = function(e) {
    #enviar_mensaje_telegram(paste("❌ Error al subir a Google Drive:", e$message), token_bot, chat_id, thread_id)
  })
  
  # ENVIAR PPT POR TELEGRAM
  tryCatch({
    enviar_archivo_telegram(
      ruta_archivo = output_path,
      token_bot = token_bot,
      id_chat = chat_id,
      thread_id = thread_id,
      descripcion = paste0("📈 Reporte mensual - ", format(fecha_corte, "%d/%m/%Y"))
    )
    enviar_mensaje_telegram("📎 Archivo PPT enviado por Telegram", token_bot, chat_id, thread_id)
  }, error = function(e) {
    enviar_mensaje_telegram(paste("❌ Error al enviar archivo por Telegram:", e$message), token_bot, chat_id, thread_id)
  })
}

enviar_archivo_telegram <- function(ruta_archivo, token_bot, id_chat, thread_id, descripcion = "") {
  if (!file.exists(ruta_archivo)) {
    cat("❌ Error: No se encontró el archivo", ruta_archivo, "\n")
    return(FALSE)
  }
  
  url <- paste0("https://api.telegram.org/bot", token_bot, "/sendDocument")
  respuesta <- httr::POST(
    url,
    body = list(
      chat_id = id_chat,
      message_thread_id = thread_id,
      document = upload_file(ruta_archivo),
      caption = descripcion
    )
  )
  
  if (respuesta$status_code == 200) {
    cat("✅ Archivo enviado correctamente:", basename(ruta_archivo), "\n")
    return(TRUE)
  } else {
    cat("❌ Error enviando archivo:", httr::content(respuesta)$description, "\n")
    return(FALSE)
  }
}

enviar_mensaje_telegram(
  "✅ <b>SEGUIMIENTO MENSUAL FINALIZADO</b>\n\n📁 Carpetas actualizadas.\n\n<a href='https://drive.google.com/drive/folders/1HsS-IAXql5SKM989WenIxL7emiNrPQxd'>📂 Ver carpeta en Drive</a>",
  token_bot,
  chat_id,
  thread_id
)
