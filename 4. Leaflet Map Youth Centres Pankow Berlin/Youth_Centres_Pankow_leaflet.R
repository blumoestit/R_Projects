##### Youth centres in Pankow Berlin - leaflet map #####
##### Jugendzentren in Pankow Berlin - leaflet Karte #####

library(pacman)
  p_load(ggmap, leaflet, rgdal, tidyverse, widgetframe)

#### Register to Google ####
#register_google(key = "AIzaSyDSnLMmM413MsOcxO5LnA6rZBmtmOMw6Ls", write = TRUE)  
#has_google_key()
#has_google_signature()
####

#### Data ####
  adr <- read.csv("/Users/magdalenablum-oeste/Desktop/Jugendzentren/JugendZentren_Karte/JugendzentrenPankow.csv") %>% 
         unite("address", 2,3, sep = " D-") 
    adrs <- adr %>% unite("address", 2, 3, sep = " ")
    adrs$address <- as.character(adrs$address)
  
  demograph_data <- read.csv("/Users/magdalenablum-oeste/Desktop/Jugendzentren/BerlinAlterstruktur2019.csv")
  
  youth_centre <- read.csv("/Users/magdalenablum-oeste/Desktop/Jugendzentren/GeoAdrJugPank.csv")

# Read shp file using readOGR() from rgdal package
  LOR_shp <- readOGR("/Users/magdalenablum-oeste/Desktop/Jugendzentren/LOR_Planungsräume__Berlin 2/LOR_Planungsräume__Berlin.shp")
  
# Create geocoordinates to addresses
  #geo_adrs <- geocode(adrs$addres
  geo_adrs_JugPank <- bind_cols(adrs, geo_adrs)
  #write.csv(geo_adrs_JugPank, "/Users/magdalenablum-oeste/Desktop/Jugendzentren/GeoAdrJugPank.csv")
  
# Add demographic data to shape file
  LOR_shp@data <- LOR_shp@data %>% left_join(demograph_data, by = c("spatial_al" = "Planungsraumname"))

#### Leaflet map ####

# Color palette
  #bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf)
  #pal <- colorBin("RdYlBu", domain = LOR_shp@data$X6_18, bins = bins, reverse = TRUE)
  
  pal_num <- colorNumeric("RdYlBu", domain = LOR_shp@data$X6_18, reverse = TRUE)

# leaflet map
  map_Pankow_youth_centres <- leaflet() %>%
      setView(lng = 13.4317, lat = 52.5928, zoom = 12) %>% # Coordinates of Pankow Bezirk Berlin
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(data = bezirke,
                  weight = 5,
                  color = "black",
                  fillColor = "transparent") %>% 
      addPolygons(data = LOR_shp,
                  weight = 1.5,
                  color = "white",
                  fillColor = ~pal_num(X6_18),
                  fillOpacity = 0.5,
                  label = LOR_shp@data$spatial_al,
                  labelOptions = labelOptions(textsize = "20px", direction = "top")) %>% 
      addMarkers(data = youth_centre,
                 icon = list(iconSize = c(15, 15)),
                   #iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
                 label = youth_centre$Einrichtung,
                 labelOptions = labelOptions(style = list(direction = "top",
                                                          "color" = "#e5007d",
                                                          "font-family" = "Clear Sans",
                                                          "font-style" = "italic",
                                                          "box-shadow" = "3px 3px rgba(0,0,0,0.9)",
                                                          "font-size" = "16px",
                                                          "border-color" = "#e5007d"
                 ))) %>% 
      addLegend(pal = pal_num, 
                values = LOR_shp@data$X6_18, 
                opacity = 0.8, 
                title = "6-18 jährige",
                position = "bottomright") #%>% 
    #frameWidget()

# Save leaflet map
  saveWidget(map_Pank_youth_centres, file = "map_Pank.html", 
             knitrOptions = list(width = 800, height = 800),
             selfcontained = TRUE)
