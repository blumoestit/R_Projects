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
  adr <- read.csv("https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/JugendzentrenPankow.csv") %>% 
         unite("address", 2,3, sep = " D-") 
    adrs <- adr %>% unite("address", 2, 3, sep = " ")
    adrs$address <- as.character(adrs$address)
  
  demograph_data <- read.csv("https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/BerlinAlterstruktur2019.csv")
  
  youth_centre <- read.csv("https://raw.githubusercontent.com/blumoestit/R_Projects/master/4.%20Leaflet%20Map%20Youth%20Centres%20Pankow%20Berlin/Data/GeoAdrJugPank.csv")

# Read shp file using readOGR() from rgdal package
  LOR_shp <- readOGR("/Users/magdalenablum-oeste/Google Drive/GitHubMBO/R_Projects/4. Leaflet Map Youth Centres Pankow Berlin/Data/LOR_Planungsräume__Berlin.shp")

# Create geocoordinates to addresses
  #geo_adrs <- geocode(adrs$address)
  GeoAdrJugPank <- bind_cols(adrs, geo_adrs)
  #write.csv(GeoAdrJugPank, "/Users/magdalenablum-oeste/Desktop/Jugendzentren/GeoAdrJugPank.csv")
  
# Add demographic data to shape file
  LOR_shp@data <- LOR_shp@data %>% left_join(demograph_data, by = c("spatial_al" = "Planungsraumname"))
  
  # test_LORS <- LOR_shp@data %>% left_join(demograph_data, by = c("spatial_al" = "Planungsraumname"))
  # test_Lors <- data.frame(LOR_shp@data$spatial_al, demograph_data$Planungsraumname)
  # 
  # demograph_data$Planungsraumname <- as.character(demograph_data$Planungsraumname)
  # LOR_shp@data$spatial_al <- as.character(LOR_shp@data$spatial_al)
  # anti_join(LOR_shp@data$spatial_al, demograph_data$Planungsraumname)
  # test_cols <- bind_cols(demograph_data$Planungsraumname, LOR_shp@data$spatial_al)
  # 
  # write.csv(demograph_data$Planungsraumname, "demo_name.csv")
  # write.csv(LOR_shp@data$spatial_al, "LOR_name.csv")
  # 
  atest <- LOR_shp@data
  LOR_shp@data$spatial_al <- replace(LOR_shp@data$spatial_al =="Schloßstraße", "Schloßstraße_ChB")
  junk$nm <- replace(junk$nm, junk$nm == "B", "b")
  LOR_shp@data$spatial_al[124] <- "Sch"
#### Leaflet map ####

# Color palette
  #bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf)
  #pal <- colorBin("RdYlBu", domain = LOR_shp@data$X6_18, bins = bins, reverse = TRUE)
  
  pal_num <- colorNumeric("YlGnBu", domain = LOR_shp@data$X6_18, reverse = T)

# Leaflet map
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
                  label = paste0(LOR_shp@data$spatial_al, " | 6-18 jährige:  ", LOR_shp$X6_18, ", davon 15-18 jährige:  ", LOR_shp$X15_18),
                  labelOptions = labelOptions(textsize = "20px", direction = "top")
                  ) %>% 
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
  map_Pankow_youth_centres 
# Save leaflet map
  saveWidget(map_Pankow_youth_centres, file = "map_Pankow_6-18jung.html", 
             knitrOptions = list(width = 800, height = 800),
             selfcontained = TRUE)
  
  
  
  
  
  
  # Leaflet map >65 y. old
  pal_num_65 <- colorNumeric("BuPu", domain = LOR_shp@data$X65._mehr, reverse = F)
  
  map_Pankow_65 <- leaflet() %>%
    setView(lng = 13.4317, lat = 52.5928, zoom = 12) %>% # Coordinates of Pankow Bezirk Berlin
    addProviderTiles("Esri.WorldTopoMap") %>%
    addPolygons(data = bezirke,
                weight = 5,
                color = "black",
                fillColor = "transparent") %>% 
    addPolygons(data = LOR_shp,
                weight = 1.5,
                color = "white",
                fillColor = ~pal_num_65(X65._mehr),
                fillOpacity = 0.7,
                label = paste0(LOR_shp@data$spatial_al, " | 65 jährige und älter:  ", LOR_shp$X65._mehr),
                labelOptions = labelOptions(textsize = "20px", direction = "top")) %>% 
    addLegend(pal = pal_num_65, 
              values = LOR_shp@data$X65._mehr, 
              opacity = 0.8, 
              title = "65 jährige und älter",
              position = "bottomright") #%>% 
  #frameWidget()
  map_Pankow_65 
  # Save leaflet map
  saveWidget(map_Pankow_65, file = "map_Pankow_65_und_älter.html", 
             knitrOptions = list(width = 800, height = 800),
             selfcontained = TRUE)
  
