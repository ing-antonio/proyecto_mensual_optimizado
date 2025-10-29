
here::i_am("procesamiento/14_mapa.R") 

# Verificar si PhantomJS está instalado y, si no, instalarlo
if (webshot::is_phantomjs_installed() == FALSE) {
  webshot::install_phantomjs()
}

config_file <- here("auxiliares/config.yml")
config <- yaml::read_yaml(config_file)

simplified_polygon <- st_simplify(alcaldias, dTolerance = 20)

Colores<-data.frame(delito=c("ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
                             "VIOLACIÓN", 
                             "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA", 
                             "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA", 
                             "HECHO NO DELICTIVO", 
                             "ROBO DE VEHÍCULO CON VIOLENCIA", 
                             "ROBO DE VEHÍCULO SIN VIOLENCIA", 
                             "ROBO DE MOTOCICLETA CON VIOLENCIA", 
                             "ROBO DE MOTOCICLETA SIN VIOLENCIA", 
                             "ROBO A NEGOCIO CON VIOLENCIA", 
                             "HOMICIDIO DOLOSO",
                             "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
                             "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
                             "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
                             "ROBO A REPARTIDOR CON VIOLENCIA", 
                             "ROBO A REPARTIDOR SIN VIOLENCIA", 
                             "ROBO A PASAJERO A BORDO DE MICROBUS",
                             "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA", 
                             "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA", 
                             "SECUESTRO", 
                             "ROBO A TRANSPORTISTA CON VIOLENCIA",
                             "ROBO A TRANSPORTISTA SIN VIOLENCIA", 
                             "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA",
                             NA)) %>% arrange(delito)

Colores_aux<-Colores %>% filter(delito %in% fgj_aux$categoria_delito)

# Reasignación de DF
fgj_colonia<-fgj_aux

# Se elimna la geometry?
st_geometry(fgj_colonia) <- NULL

# Agregados?
fgj_colonia <-fgj_colonia %>% 
  select(Cuadrante, categoria_delito) %>% 
  group_by(Cuadrante, categoria_delito) %>% 
  summarise(tot=n()) %>% 
  left_join(colonias, by="Cuadrante")

cuadrantes<-data.frame(tooltip=character(), Cuadrante=character(), tot=integer())
for(i in unique(fgj_colonia$Cuadrante)){
  # i<-unique(fgj_colonia$Cuadrante)[2]
  aux_colonia<-fgj_colonia %>% filter(Cuadrante==i) %>% arrange(desc(tot))
  texto<-paste0("<b>","AlcaldC-a: ","</b>",str_to_sentence(unique(aux_colonia$alcaldia)),"<br>",
                "<b>","Sector: ","</b>", str_to_sentence(unique(aux_colonia$Sector)),"<br>",
                "<b>","Colonia: ","</b>", str_to_sentence(unique(aux_colonia$colonia)),"<br>",
                "<b>","Cuadrante: ","</b>", unique(aux_colonia$Cuadrante),"<br>",
                "<b>","Total de carpetas: ","</b>", sum(aux_colonia$tot),"<br>")
  for(x in 1:length(aux_colonia$categoria_delito)){
    # aux_colonia$categoria_delito[[1]]
    texto<-paste0(texto,"<b>-",str_to_sentence( aux_colonia$categoria_delito[[x]]),":</b>", aux_colonia$tot[[x]],"<br>")
  }
  aux_texto<-data.frame(tooltip=texto, Cuadrante=unique(aux_colonia$Cuadrante), tot=sum(aux_colonia$tot))
  cuadrantes<-rbind(cuadrantes, aux_texto)
  
}

## Unión de datos
cuadrantes<- colonias %>% 
  left_join(cuadrantes, by="Cuadrante")  %>% 
  select(tooltip, geometry, tot) %>% 
  filter(!is.na(tooltip))

# Colores
paletteer_c("grDevices::YlOrRd", 5)
conpal <- colorNumeric(palette = "Reds", domain = c(min(cuadrantes$tot), max(cuadrantes$tot)))


# Se elabora el mapa de Leaflet

leyenda<-fgj_aux %>% select(categoria_delito) 

st_geometry(leyenda) <- NULL 

leyenda<-leyenda %>% unique() %>% arrange(categoria_delito) %>% pull()

backg <- htmltools::tags$style(".leaflet-container { background: #FFFFFF; }" )

## Objeto Leaflet

# Crear el mapa leaflet
mapa <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group="Carto DB Positron") %>%
  setView( -99.15, 19.35, 10, zoom = 11 ) %>%
  addPolygons(data = cuadrantes, group = "Cuadrantes",
              stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
              fillColor = ~conpal(cuadrantes$tot)) %>%
  addPolygons(data = colonias, weight = 0.5, fillOpacity = -1, color = 'black' )%>%
  addPolygons(data = alcaldias, weight = 2, fillOpacity = -1, color = 'black' )%>%
  addPolygons(data = cuadrantes, group = "Cuadrantes",
              stroke = FALSE, smoothFactor = 0.2, fillOpacity = -1,
              popup = paste0(cuadrantes$tooltip)) %>%
  addMarkers(data= fgj_aux, group = fgj_aux$categoria_delito, 
             icon = makeIcon(
               iconUrl= paste0( here('auxiliares', 'pins/'), fgj_aux$categoria_delito, ".png"),
               iconWidth = 27, iconHeight = 28.5,
               iconAnchorX = 0, iconAnchorY = 0) ,
             popup  = paste0(fgj_aux$tooltip)) %>% 
  hideGroup(c(Colores_aux %>% filter(delito!="HOMICIDIO DOLOSO" & delito!="LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO") %>% select(delito) %>% pull())) %>%
  addLegend("topleft", pal = conpal, values = c(min(cuadrantes$tot), max(cuadrantes$tot)), 
            title = "Carpetas", opacity = 1) %>% 
  addLayersControl(overlayGroups = c(Colores_aux$delito, "Cuadrantes", "Carto DB Positron"),
                   options = layersControlOptions(collapsed = T),
                   position = 'bottomleft')%>%
  htmlwidgets::prependContent(backg)

mapa
# Guardar como HTML
saveWidget(mapa, here('salidas', "mapa_interactivo_homicidio_lesiones.html"))

# Convertir el HTML a imagen
webshot(here("salidas/mapa_interactivo_homicidio_lesiones.html"), here("salidas/mapa_interactivo_homicidio_lesiones.png"), vwidth = 1200, vheight = 800)

