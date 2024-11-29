# Mapa da ReViS Meiembipe com Zona de Amortecimento e pontos de referência
# Desenvolvido por Marcio Baldissera Cure
# Atualizado em 05/03/2023

setwd("/home/mcure/Documents/ruinas")

#dsn <- "/home/mcure/Documents/meiembipe/meiembipe_limites"

library(rgdal)
library(sp)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
#library(rgeos)

# Lê o shape file que contém os limites das unidades de conservação de Florianópolis
#revis <- readOGR(dsn = dsn, layer = "ma_uc", verbose = F, GDAL1_integer64_policy = F)

# Separa apenas a ReViS Meiembipe
#ne_revis <- subset(revis, revis$nm_uc %in% c(
#  "MEIEMBIPE"
#))

# Transforma as coordenadas para latitude e longitude
#shapeRevis <- spTransform(ne_revis, CRS("+proj=longlat +ellps=GRS80"))

# Cria o shape com a zona de amortecimento (ZA)
#za <- gBuffer(shapeRevis, width = 0.0003, byid = TRUE, quadsegs = 2)

# Coordenadas da Cachoeira do ponto 16
#lon <- -48.46367913
#lat <- -27.54353368

# Coordenadas do Engenho
lat1 <- -27.5657798
lon1<- -48.463697

# Labels e informações para cada um dos marcadores.
#label <- "Cachoeira do 16"
engenho <- paste0("<b>Engenho de farinha</b>", "</br><img width='60%', src = './fotos/engenho.jpeg'>",
#                " </br>Fonte da imagem: https://ndmais.com.br/noticias/costa-da-lagoa-uma-heranca-colonial-esta-ameacada/","</br>",
            "<p><b>Engenho do Século XIX.</b> O engenho encontra-se com as 4 paredes, cobertura, e maquinário. Entretanto não pode ser colocado em funcionamento devido à fragilidade estrurural. Esta condição interrompeu a sequência de <i>farinhandas</i>, festa anual em que a comunidade reproduz todo o processo da confecção da farinha. </p>Há indícios estruturais e informações que fazem crer que a estrutura frontal foi residência familiar antes da construção do Engenho.<p><br>Status atual: Encontra-se em propriedade privada. Necessita de reforma. Área de uso comunitário para encontros. Há uma horta comunitária ao lado. Conflitos emergentes sobre uso, ocupação e direitos sobre a ruina.</p><p><b>Cuide do patrimônio histórico e cultural!</b></p>")

label1 <- "Engenho de farinha"
#cachu <-  paste0("<b>Cachoeira do ponto 16</b>", "</br><img width='80%', src = 'https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.facebook.com%2Fphoto.php%3Ffbid%3D1806689359400076%26id%3D266598283409199%26set%3Da.397351763667183&psig=AOvVaw1V6xGr2h6NLEnOy8NcKd3E&ust=1727572072377000&source=images&cd=vfe&opi=89978449&ved=0CBQQjRxqFwoTCOibtIO65IgDFQAAAAAdAAAAABAE'>",
#                   " </br>Fonte da imagem: https://trilhasconectam.com.br/trilha-costa-da-lagoa/trilhasconectam/","</br>",
 #                  "</br><b>Visite a Costa da Lagoa!</b>")

# Combina as informações para os dois pontos de referência
pontos <- data.frame(engenho)
labels <- data.frame(label1)
lons <- lon1
lats <- lat1

# Adiciona perfumarias
mydrawPolylineOptions <- function(allowIntersection = TRUE,
      drawError = list(color = "#b00b00", timeout = 2500),
      guidelineDistance = 20, metric = TRUE, feet = FALSE,
      zIndexOffset = 2000,
      shapeOptions = drawShapeOptions(fill = FALSE), repeatMode = FALSE) {
  leaflet::filterNULL(list(allowIntersection = allowIntersection,
            drawError = drawError, guidelineDistance =
              guidelineDistance,
            metric = metric,
            feet = feet,
            zIndexOffset = zIndexOffset,
            shapeOptions = shapeOptions,  repeatMode = repeatMode)) }

# Gera o mapa interativo com as informações
mapa <- leaflet()  %>% addTiles() %>%
  #  setView(lng = -106.363590, lat=31.968483,zoom=11) %>%
#  addPolygons(data=za,weight=2,col = 'blue') %>%
#  addPolygons(data=shapeRevis,weight=2,col = 'red') %>%
#  addProviderTiles('Esri.WorldImagery') %>%
  addCircles(lng=lons, lat=lats) %>%
  addMarkers(lons,lats,popup = c(engenho), label = labels) |>
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagem") %>%
  addScaleBar(position = "topright") %>%
  addDrawToolbar(
    polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
    editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()))%>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom nível 3 - América do Sul",
    onClick=JS("function(btn, map){ map.setZoom(3); }"))) %>%
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE,
    minimized = TRUE) |>
  addLayersControl(
    baseGroups = c("Satélite","Street Map"),
#    overlayGroups = c("Árvores"),
    options = layersControlOptions(collapsed = FALSE)
   ) # |>
  # addControlGPS(
  #   options = gpsOptions(
  #     position = "topleft",
  #     activate = TRUE,
  #     autoCenter = TRUE,
  #     setView = TRUE)) %>%
  # activateGPS()
