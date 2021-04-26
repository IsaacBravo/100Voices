setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(leaflet)
library(readxl)
library(sp)

soon <- read_excel("C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/02 - 100VoicesProject/commingsoon.xlsx")

soon$lng <- as.numeric(soon$lng)
soon$lat <- as.numeric(soon$lat)
link <- as.character(soon$image)

done <- read_excel("C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/02 - 100VoicesProject/videosdone.xlsx")

done$lng <- as.numeric(done$lng)
done$lat <- as.numeric(done$lat)

left <- read_excel("C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/02 - 100VoicesProject/missingcountries.xlsx")

left$lng <- as.numeric(left$lng)
left$lat <- as.numeric(left$lat)

bulgaria <- read_excel("C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/02 - 100VoicesProject/bulgaria.xlsx")

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

  leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 2,
                                       dragging = TRUE)) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addAwesomeMarkers(data = done, 
                    lng = ~lng, 
                    lat = ~lat, 
                    popup = ~paste("<h4 style=color:#6FAC25;> Meet ",name,"from ", country ,"</h4>",
                                   "<b>German Watch Climate Risk Index: N°</b> ",rank,"<br>",
                                   #"<b>Number of Extreme Weather Events:</b> ",n_events,"<br>",
                                   #"<b>Level of Development:</b> ",development,"<br>",
                                   "<b></b> ","<br>",
                                   video,
                                   sep = " "),
                    icon = awesome2
                    
                    ) %>%
    
    addAwesomeMarkers(data = soon, 
                      lng = ~lng, 
                      lat = ~lat, 
                      popup = ~paste("<h4 style=color:#76862C;> Meet ",name,"from ", country,"</h4>",
                                     "<b>German Watch Climate Risk Index:</b> ",rank,"<br>",
                                     #"<b>Number of Extreme Events:</b> ",n_events,"<br>",
                                     #"<b>Level of Development:</b> ",development,"<br>",
                                     "<b></b> ","<br>",
                                     picture,
                                     sep = " "),
                      icon = awesome
                      
    ) %>%
    addCircleMarkers(data = left, 
                                       lng = ~lng,
                                       lat = ~lat,
                                       color = "black",
                                      radius = 5 
    
    ) %>%
    addAwesomeMarkers(data = bulgaria, 
                      lng = ~lng, 
                      lat = ~lat, 
                      popup = ~paste("<h4 style=color:#6FAC25;> Meet our participant from ", country,"</h4>",
                                     "<b>German Watch Climate Risk Index:</b> ",rank,"<br>",
                                     #"<b>Number of Extreme Events:</b> ",n_events,"<br>",
                                     #"<b>Level of Development:</b> ",development,"<br>",
                                     "<b></b> ","<br>",
                                     picture,
                                     sep = " "),
                      icon = awesome
                      
    ) %>%
    addMiniMap(toggleDisplay = TRUE,
             tiles = providers$Stamen.TonerLite,
             aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
             shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE, opacity =
                                        0, fillOpacity = 0),
             strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
             mapOptions = list())

  

