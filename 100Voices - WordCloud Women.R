setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

Packages <- c("dplyr", "ggplot2", "readr",
              "pdftools","stopwords","tidytext",
              "stringi", "stringr", "scales",
              "tidyr", "widyr", "ggraph", "igraph",
              "quanteda", "topicmodels","lattice",
              "robustbase", "cvTools", "NLP", "tm",
              "readxl", "ggnet", "network", "sna",
              "visNetwork", "threejs", "networkD3",
              "ndtv", "htmltools","xlsx")
lapply(Packages, library, character.only = TRUE)

Voices <- file("data.txt")
Voices <- readLines(Voices)
Voices<- paste(Voices,collapse = " ")
Voices <- strsplit(Voices, split = " ") %>% unlist()
VoicesTable <- table(Voices) %>% as.data.frame()

Voices <- sapply(Voices,"removePunctuation",USE.NAMES = FALSE) 
Voices <- sapply(Voices,"tolower",USE.NAMES = FALSE) 
Voices <- sapply(Voices,"stripWhitespace",USE.NAMES = FALSE) 
Voices <- sapply(Voices,"removeNumbers",USE.NAMES = FALSE) 
Voices <- Voices[Voices!=""]
Voices <- Voices[Voices!=" "]

remove <- c(stopwords("eng"))
Voices <- sapply(Voices,"removeWords",words=remove, USE.NAMES = FALSE)
Voices <- Voices[Voices!=""]
Voices <- Voices[Voices!=" "]

VoicesTable <- as.data.frame(line = 1:1107, Voices)

# count
count_table <- VoicesTable %>%
  dplyr::count(Voices, sort = TRUE)

#####################################################
##################### Wordcloud #####################
#####################################################

Packages2 <- c("SnowballC", "RColorBrewer", "ggthemes",
               "extrafont","readr","wordcloud",
               "wordcloud2", "ggwordcloud","gganimate")
lapply(Packages2, library, character.only = TRUE)


write.xlsx(count_table,"table.xlsx")
finaltable <- read_excel("table.xlsx")

sample <- finaltable %>% filter(n>=1)
##B3EE53

p<- ggplot(sample, aes(label = Voices, size = n)) +
  geom_text_wordcloud(area_corr = TRUE, color= '#4D7142',
                      eccentricity = 1.3) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(text = element_text(family="Roboto"),
        #plot.title = element_text(hjust = 0.5, color = "#4D7142", size = 14, face= "bold"),
        #plot.subtitle = element_text(hjust = 0.5, color = "#7f6000", size = 12,face= "bold"),
        #plot.caption = element_text(hjust = 0, color = "#7f6000", size = 8,face= "bold"),
        plot.background = element_rect(fill = "white")) 
#  labs(title = "Wordcloud 100 Voices!",
#     subtitle = "Frequency by word video",
#       caption = "Data source: Videos collected")




for (shape in c("triangle-forward"
                #"circle", "cardioid", "diamond",
                #"square"#, "triangle-forward", "triangle-upright",
                #"pentagon", "star"
)) {
  set.seed(42)
  print(ggplot(sample, aes(label = Voices, size = n)) +
          geom_text_wordcloud(area_corr = TRUE, color= '#76A9EA',
                              eccentricity = 0.3) +
          scale_size_area(max_size = 7) +
          theme_minimal() +
          theme(text = element_text(family="Segoe UI"))
        + ggtitle(shape))
}  



##### 

wordcloud(words = sample$Voices, freq = sample$n,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

wordcloud2(sample, size=1.6)
wordcloud2(sample, size=1.6, color='random-dark')
wordcloud2(sample, size=1.6, color=rep_len( c("green","blue"), nrow(sample) ) )
wordcloud2(sample, size=1.6, color='random-light', backgroundColor="black")

# circle # cardioid # diamond # triangle-forward
# triangle # pentagon # star

wordcloud2(sample, size = 0.7, shape = 'star')

wordcloud2(sample, size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

library(webshot)
webshot::install_phantomjs()

# Make the graph
my_graph <- wordcloud2(sample, size=1.5)

# save it in html
library("htmlwidgets")
saveWidget(my_graph,"worldcloud.html",selfcontained = F)

# and in png or pdf
webshot("tmp.html","fig_1.pdf", delay =5, vwidth = 480, vheight=480)

###############################
###############################
############ MAPS #############
###############################
###############################

library(leaflet)
library(readxl)
library(sp)

worldcities <- read_excel("C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/100VoicesProject/worldcities1.xlsx")
View(worldcities)

worldcities$lng <- as.numeric(worldcities$lng)
worldcities$lat <- as.numeric(worldcities$lat)

awesome <- makeAwesomeIcon(
  icon='glyphicon-globe', 
  library='glyphicon', 
  markerColor = "green", 
  iconColor = 'white'
)

map<- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 2,
                                       dragging = TRUE)) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>% 
  addAwesomeMarkers(data = worldcities, lng = ~lng, 
                    lat = ~lat, 
                    popup = ~paste("<h3>",country,"</h3>",
                                   "<b>German Rank Global Risk:</b> ",rank,"<br>",
                                   "<b>Rank Fatalities (1999-2018):</b> ",rank1,"<br>",
                                   "<b>Rank Losses in million US$ (1999-2018):</b> ",rank3,"<br>",
                                   "<b>Rank Losses per unit GDP in % (1999-2018):</b> ",rank4,"<br>",
                                   "<b></b> ","<br>",
                                   video,
                                   sep = " "),
                    icon = awesome ) %>%
  #clusterOptions = markerClusterOptions()
  addMiniMap(toggleDisplay = TRUE,
             tiles = providers$Stamen.TonerLite,
             aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
             shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE, opacity =
                                        0, fillOpacity = 0),
             strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
             mapOptions = list())

saveNetwork(map,"C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/100VoicesProject/Map100Voices.html", selfcontained = TRUE)


