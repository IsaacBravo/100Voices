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

Voices <- file("females.txt")
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

count_table %>%
  head(20) %>%
  mutate(Voices = reorder(Voices, n)) %>%
  ggplot(aes(Voices, n)) +
  geom_col(fill = "#333D79FF") +
  theme_gray()+
  theme(text = element_text(family="Segoe UI"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10))+
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(
    x = " ",
    y = "Mentions",
    title = "Text Analysis",
    subtitle = "Word frequency of references used") +
  geom_text(aes(label = n, hjust = 1.2), 
            color = "white", fontface = 2)

### Bigrams

bigrams<-lapply(ngrams(Voices,2), paste, collapse=" ") %>% unlist()

bigrams <- table(bigrams) %>% as.data.frame()
bigrams <- bigrams %>% separate(bigrams,into=c("word1","word2"),sep=" ") 

write.xlsx(bigrams, "bigrams.xlsx")
######################################

bigrams <- read_excel("bigrams.xlsx") %>% as.data.frame()

sample <- bigrams %>% filter(Freq>=1)

firstposition <- sample$word1
secondposition <- sample$word2

network <- data.frame(firstposition,secondposition, stringsAsFactors = FALSE)

# make a nodes data frame out of all unique nodes in networkData
nodes <- data.frame(name = unique(c(network$firstposition,
                                    network$secondposition)))

# make a group variable where nodes in networkData$src are identified
nodes$group <- ifelse(nodes$name %in% network$firstposition, "Stronger", "Weaker")

# make a links data frame using the indexes (0-based) of nodes in 'nodes'
links <- data.frame(source = match(network$firstposition, nodes$name) - 1,
                    target = match(network$secondposition, nodes$name) - 1)

### the color of the background
network <-forceNetwork(Links = links, 
                       Nodes = nodes, 
                       Source = "source",
                       Target = "target", 
                       NodeID ="name", 
                       Group = "group",
                       opacity = 1, 
                       opacityNoHover = -1,
                       height = NULL, width = NULL,
                       colourScale = JS('d3.scaleOrdinal().domain(["Stronger","Weaker"]).range(["#B3EE53", "#4D7142"]);'), 
                       fontSize = 15,
                       fontFamily = "serif", linkDistance = 50,
                       linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
                       radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"), charge = -20,
                       linkColour = "#4D7142", zoom = TRUE, legend = FALSE,
                       arrows = FALSE, bounded = FALSE, clickAction = FALSE)

network <- htmlwidgets::prependContent(network, htmltools::tags$h1("Network of correlation between words"))

network <- htmlwidgets::onRender(
  network,
  'function(el, x) { 
    d3.selectAll(".legend text").style("fill", "#4F7344");
    d3.select("body").style("background-color", "White");
    d3.select("h1").style("color", "#4F7344").style("font-family", "Roboto", "sans-serif");
    d3.select("body");
  }'
)

clickjs <- 
  "function(el, x) { 
  var options = x.options;
  var svg = d3.select(el).select('svg');
  var node = svg.selectAll('.node');
  var link = svg.selectAll('link');
  var mouseout = d3.selectAll('.node').on('mouseout');
  function nodeSize(d) {
    if (options.nodesize) {
      return eval(options.radiusCalculation);
    } else {
      return 6;
    }
  }

  d3.selectAll('.node').on('click', onclick);

  function onclick(d) {
    if (d3.select(this).on('mouseout') == mouseout) {
      d3.select(this).on('mouseout', mouseout_clicked);
    } else {
      d3.select(this).on('mouseout', mouseout);
    }
  }

  function mouseout_clicked(d) {
    node.style('opacity', +options.opacity);
    link.style('opacity', +options.opacity);

    d3.select(this).select('circle').transition()
      .duration(750)
      .attr('r', function(d){return nodeSize(d);});
    d3.select(this).select('text').transition()
      .duration(1250)
      .attr('x', 0)
      .style('font', options.fontSize + 'px ');
  }
}
"

network <- onRender(network, clickjs)


customJS <- '
function(el,x) { 
    var link = d3.selectAll(".link")
    var node = d3.selectAll(".node")

    var options = { opacity: 1,
                    clickTextSize: 10,
                    opacityNoHover: 0.1,
                    radiusCalculation: "Math.sqrt(d.nodesize)+6"
                  }

    var unfocusDivisor = 4;

    var links = HTMLWidgets.dataframeToD3(x.links);
    var linkedByIndex = {};

    links.forEach(function(d) {
      linkedByIndex[d.source + "," + d.target] = 1;
      linkedByIndex[d.target + "," + d.source] = 1;
    });

    function neighboring(a, b) {
      return linkedByIndex[a.index + "," + b.index];
    }

    function nodeSize(d) {
            if(options.nodesize){
                    return eval(options.radiusCalculation);
            }else{
                    return 6}
    }

    function mouseover(d) {
      var unfocusDivisor = 4;

      link.transition().duration(200)
        .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });

      node.transition().duration(200)
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d)+5;});

      node.select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", 24 + "px ")
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
    }

    function mouseout() {
      node.style("opacity", +options.opacity);
      link.style("opacity", +options.opacity);

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d);});
      node.select("text").transition()
        .duration(1250)
        .attr("x", 0)
        .style("font", options.fontSize + "px ")
        .style("opacity", 0);
    }

    d3.selectAll(".node").on("mouseover", mouseover).on("mouseout", mouseout);
}
'

network2 <- onRender(network, customJS)


saveNetwork(network2, "C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/100VoicesProject/100Voices-v5.html", selfcontained = TRUE)




library(webshot)
webshot::install_phantomjs()
# and in png or pdf
webshot("100Voices.html","100Voices.pdf", vwidth = 680, vheight=680,
        cliprect = NULL, selector = NULL, expand = NULL,
        delay = 0.2, zoom = 0.5, eval = NULL, debug = FALSE,
        useragent = NULL)



#####################################################
##################### Wordcloud #####################
#####################################################

Packages2 <- c("SnowballC", "RColorBrewer", "ggthemes",
              "extrafont","readr","wordcloud",
              "wordcloud2", "ggwordcloud","gganimate")
lapply(Packages2, library, character.only = TRUE)


write.xlsx(count_table,"table.xlsx")
finaltable <- read_excel("table.xlsx")

sample <- finaltable %>% filter(n>1)
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


