setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(leaflet)
library(readxl)
library(sp)
library(leafem)
library(htmltools)

soon_out <- read_excel("C:/Users/isaac/OneDrive/Documentos/Programming/R/02 - 100VoicesProject/Global Map/commingsoon_out.xlsx")

soon_out$lng <- as.numeric(soon_out$lng)
soon_out$lat <- as.numeric(soon_out$lat)
link <- as.character(soon_out$image)

soon <- read_excel("C:/Users/isaac/OneDrive/Documentos/Programming/R/02 - 100VoicesProject/Global Map/commingsoon.xlsx")

soon$lng <- as.numeric(soon$lng)
soon$lat <- as.numeric(soon$lat)
link <- as.character(soon$image)

done <- read_excel("C:/Users/isaac/OneDrive/Documentos/Programming/R/02 - 100VoicesProject/Global Map/videosdone.xlsx")

done$lng <- as.numeric(done$lng)
done$lat <- as.numeric(done$lat)

left <- read_excel("C:/Users/isaac/OneDrive/Documentos/Programming/R/02 - 100VoicesProject/Global Map/missingcountries.xlsx")

left$lng <- as.numeric(left$lng)
left$lat <- as.numeric(left$lat)

bulgaria <- read_excel("C:/Users/isaac/OneDrive/Documentos/Programming/R/02 - 100VoicesProject/Global Map/bulgaria.xlsx")

bulgaria$lng <- as.numeric(bulgaria$lng)
bulgaria$lat <- as.numeric(bulgaria$lat)

awesome <- makeAwesomeIcon(
  icon='glyphicon-globe', 
  library='glyphicon', 
  markerColor = "darkgreen", 
  iconColor = 'white'
)

awesome2 <- makeAwesomeIcon(
  icon='glyphicon-globe', 
  library='glyphicon', 
  markerColor = "green", 
  iconColor = 'white'
)

awesome3 <- makeAwesomeIcon(
  icon='glyphicon-globe', 
  library='glyphicon', 
  markerColor = "lightgray", 
  iconColor = 'black'
)


m <- leaflet(
  height=650,
  options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 2,
                                       dragging = TRUE)) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Toner Lite") %>%
  addLayersControl(
    position = "topright",
    baseGroups = c("Toner Lite", "World Imagery"),
    overlayGroups = c("Done!", "Soon!", "Missing"),
    options = layersControlOptions(collapsed = FALSE)) %>%
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Status of the Voices!</label>');
        }
    ") %>%
  addAwesomeMarkers(data = done, 
                    lng = ~lng, 
                    lat = ~lat, 
                    popup = ~paste("<h4 style=color:#6FAC25;> Meet ",name,"from ", country ,"</h4>",
                                   "<b>German Watch Climate Risk Index: N°</b> ",rank,"<br>",
                                   "<b></b>","<br>",
                                   video,
                                   sep = " "),
                    icon = awesome2,
                    group = "Done!") %>%
    addAwesomeMarkers(data = soon, 
                      lng = ~lng, 
                      lat = ~lat, 
                      popup = ~paste("<h4 style=color:#76862C;> Meet ",name,"from ", country,"</h4>",
                                     "<b>German Watch Climate Risk Index:</b> ",rank,"<br>",
                                     "<b></b> ","<br>",
                                     picture,
                                     sep = " "),
                      icon = awesome,
                      group = "Soon!") %>%
    addCircleMarkers(data = left, lng = ~lng,
                                  lat = ~lat,
                                  color = "green",
                                  radius = 5,
                                  group = "Missing") %>%
    addAwesomeMarkers(data = bulgaria, 
                      lng = ~lng, 
                      lat = ~lat, 
                      popup = ~paste("<h4 style=color:#6FAC25;> Meet our participant from ", country,"</h4>",
                                     "<b>German Watch Climate Risk Index:</b> ",rank,"<br>",
                                     "<b></b> ","<br>",
                                     video,
                                     sep = " "),
                      icon = awesome2,
                      group = "Done!") %>%
    addAwesomeMarkers(data = soon_out, 
                      lng = ~lng, 
                      lat = ~lat, 
                      popup = ~paste("<h4 style=color:#76862C;> Meet ",name,"from ", country,"</h4>",
                                     "<b>German Watch Climate Risk Index:</b> ",rank,"<br>",
                                     "<b></b> ","<br>",
                                     picture,
                                     sep = " "),
                      icon = awesome,
                      group = "Soon!") %>%
    # addMiniMap(toggleDisplay = TRUE,
    #          tiles = providers$CartoDB.Positron,
    #          aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
    #          shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE, opacity =
    #                                     0, fillOpacity = 0),
    #          strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
    #          mapOptions = list()) %>%
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))

# library(magick)
# library(png)
# #read file
# img<-readPNG("img/Logo_Alt.png")
# img <- image_read(img)
# 
# p <- m %>% 
#   addLogo("img/Logo_Alt.png", src= "local",
#           width = 100,
#           height = 100) 
  
browsable(
  tagList(
    tags$style(".leaflet-control-layers-expanded{color: #007900}",
               ".leaflet-control-layers-overlays{color: #0C2A1B; text-align: center}",
               ".leaflet-control-layers{background-color: #F9FFED; border-color: #F9FFED;}"
               ),
    m
  )
)

#https://stackoverflow.com/questions/56835890/is-there-an-option-to-colour-text-in-the-layer-options-box?noredirect=1&lq=1
#https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map?rq=1


	

  
  
  
