##### "Kinder- und Jugendfreizeiteinrichtungen in Pankow Berlin" - leaflet MAP #####

library(pacman)
  p_load(ggmap, leaflet, rgdal, tidyverse, widgetframe)

#### Register to Google ####
#register_google(key = "put your key here", write = TRUE) # run once
#has_google_key()
#has_google_signature()
####

#### Data ####
# Data sources
  adr_url <- "https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/JugendzentrenPankow.csv"
  demograph_url <- "https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/BerlinAlterstruktur2019.csv"
  youth_centre_url <- "https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/GeoAdrJugPank.csv"
  youth_centre_clubs_url <- "https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/GeoAdrJugPank_clubs_only.csv"
  line_innenstadt_url <- "https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/Pankow_line.csv"
  bezirke_url <- "/Users/magdalenablum-oeste/Documents/FDP/BezirkStatistik/Bezirke__Berlin/Bezirke__Berlin.shp"
  LOR_shp_url <- "/Users/magdalenablum-oeste/Google Drive/GitHubMBO/R_Projects/4. Leaflet Map Youth Centres Pankow Berlin/Data/LOR_Planungsräume__Berlin.shp"
  
  adr <- read.csv(adr_url) %>% 
         unite("address", 2,3, sep = " D-") 
    adrs <- adr %>% unite("address", 2, 3, sep = " ")
    adrs$address <- as.character(adrs$address)
  
  demograph_data <- read.csv(demograph_url)
  youth_centre <- read.csv(youth_centre_url)
  youth_centre_clubs <- read.csv(youth_centre_clubs_url)
  line_innenstadt <- read.csv(line_innenstadt_url)
  
# Read shp file using readOGR() from rgdal package
  bezirke <- readOGR(bezirke_url)
  LOR_shp <- readOGR(LOR_shp_url)

# Create geocoordinates to addresses
  #geo_adrs <- geocode(adrs$address)
  #GeoAdrJugPank <- bind_cols(adrs, geo_adrs)
  #write.csv(GeoAdrJugPank, "/Users/magdalenablum-oeste/Desktop/Jugendzentren/GeoAdrJugPank.csv") # run once
  
# Add demographic data to shape file
  LOR_shp@data <- LOR_shp@data %>% left_join(demograph_data, 
                                             by = c("spatial_al" = "Planungsraumname"))
   
#### Leaflet map ####
# Color palette
  pal_num <- colorNumeric("RdPu", domain = LOR_shp@data$X6_18, reverse = F)

# Labels
  border <- "Grenze der Berliner Innenstadt in Pankow"
  proper <- "Angemessene Jugendfreizeiteinrichtungen in Pankow"
  all <- "Alle Kinder- und Jugendfreizeiteinrichtungen in Pankow"
  all_legend <- "Kinder- und Jugendfreizeiteinrichtungen | berlin.de/jugendamt-pankow | Juli 2020"
  proper_legend <- "Nur angemessene Jugendfreizeiteinrichtungen | eigene Recherchen"
  
# Icons
  iconUrl_pink <- 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/128/Map-Marker-Marker-Outside-Pink-icon.png'
  iconUrl_azure <- 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/128/Map-Marker-Marker-Outside-Azure-icon.png'

# Leaflet map
  map_Pankow_youth_centres <- leaflet() %>%
      setView(lng = 13.4317, lat = 52.5928, zoom = 12) %>% # Coordinates of Pankow Bezirk Berlin
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(data = bezirke,
                  weight = 6.5,
                  color = "black",
                  fillColor = "transparent") %>% 
      addPolygons(data = LOR_shp,
                  weight = 1.2,
                  color = "white",
                  fillColor = ~pal_num(X6_18),
                  fillOpacity = 0.7,
                  label = paste0(LOR_shp@data$spatial_al, " | 6-18 jährige:  ", 
                                 LOR_shp$X6_18, ", davon 15-18 jährige:  ", 
                                 LOR_shp$X15_18),
                  labelOptions = labelOptions(textsize = "20px", 
                                              direction = "top")) %>% 
      addPolylines(data = line_innenstadt, 
                   lng = ~longitude, 
                   lat = ~latitude, 
                   group = border,
                   weight = 4.5,
                   color = "red") %>% 
      addMarkers(data = youth_centre, 
                 group = all,
                 icon = list(iconSize = c(45, 45),
                 iconUrl = iconUrl_azure),
                 label = youth_centre$Einrichtung,
                 labelOptions = labelOptions(style = list(direction = "top",
                                                          "color" = "#00A7EB",
                                                          "font-family" = "Clear Sans",
                                                          "font-style" = "italic",
                                                          "box-shadow" = "3px 3px rgba(0,0,0,0.9)",
                                                          "font-size" = "16px",
                                                          "border-color" = "#00A7EB"))) %>% 
      addMarkers(data = youth_centre_clubs, 
                 group = proper, 
                 icon = list(iconSize = c(45, 45),
                 iconUrl = iconUrl_pink),
                 label = youth_centre_clubs$Einrichtung,
                 labelOptions = labelOptions(style = list(direction = "top",
                                                          "color" = "#e5007d",
                                                          "font-family" = "Clear Sans",
                                                          "font-style" = "italic",
                                                          "box-shadow" = "3px 3px rgba(0,0,0,0.9)",
                                                          "font-size" = "16px",
                                                          "border-color" = "#e5007d"))) %>% 
      addLegend(pal = pal_num, 
                values = LOR_shp@data$X6_18, 
                opacity = 0.8, 
                title = "6-18 jährige",
                position = "bottomright") %>% 
      addLegend(values = 0, 
                group = all, 
                position = "bottomleft",
                labels = all_legend, 
                colors = "#00A7EB") %>%
      addLegend(values = 1, 
                group = proper, 
                position = "bottomleft",
                labels =  proper_legend, 
                colors = "#eb007d") %>%
      addLegend(values = line_innenstadt, 
                group = border,
                position = "bottomleft",
                labels = border, 
                colors = "red") %>%
      addLayersControl(overlayGroups = c(all, 
                                         proper,
                                         border),
                        options = layersControlOptions(collapsed = FALSE))
    
    
  map_Pankow_youth_centres 
  
# Save leaflet map
  saveWidget(map_Pankow_youth_centres, file = "Karte_Pankow_6-18jung-Jugendzentren_Innenstadt.html", 
             knitrOptions = list(width = 800, height = 800),
             selfcontained = TRUE)
  
  
  