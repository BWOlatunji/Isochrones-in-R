library(tidyverse)
library(sf)
library(leaflet)
library(hereR)

# 1. Import data for Colorado Hospitals

el_paso_hospitals <- read_rds("data/el_paso_hospitals.rds")

# 2. Convert El paso hospital data to simple feature
el_paso_hospitals_sf <- st_as_sf(x = el_paso_hospitals,
                                 coords = c("long", "lat"),
                                 crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# 3. Store and set Your HERE API key
here_key <- Sys.getenv("here_key")
set_key(here_key)


# 4. Download isochrones for all hospitals
el_paso_hospitals_isochrones <- isoline(
  poi = el_paso_hospitals_sf,
  range = seq(10, 30, 10) * 60,
  range_type = "time",
  datetime <- as.POSIXct(paste0(Sys.Date()," 10:00")) 
) %>%
  mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins"))

# 5. Convert Isochrones to shapefile      
sp_for_all_isochrones <- as(el_paso_hospitals_isochrones, "Spatial")

# 6. Create a color palette for the catchment areas
isochrones_colors <- c("#E5989B", "#B5838D", "#6D6875")

isochrones_palette <- colorFactor(isochrones_colors, sp_for_all_isochrones@data$name)

# 7. El Paso County coordinates
el_paso_coords <- c(38.84, -104.52)

# 8. Plot with leaflet
leaflet() %>% 
  setView(lng = el_paso_coords[2], lat = el_paso_coords[1], zoom = 10) %>%
  addProviderTiles("CartoDB.Positron", group="Greyscale") %>% 
  addPolygons(data = sp_for_all_isochrones,
              fill=TRUE,
              fillColor = ~isochrones_palette(sp_for_all_isochrones@data$name),
              fillOpacity=0.35,
              stroke=TRUE,
              color = "black",
              weight=0.5, 
              popup = sp_for_all_isochrones@data$name,
              group = "Catchment Area") %>%  
  addCircles(el_paso_hospitals$long,
             el_paso_hospitals$lat,
             label = el_paso_hospitals$clinician_data_facility_name,
             radius = 5,
             opacity = 1,
             group = "Hospitals") %>%
  
  # Add legends and layer control
  addLegend("bottomleft",
            values = sp_for_all_isochrones@data$name,
            pal = isochrones_palette, 
            opacity = 0.35,
            title = "El Paso hospitals range",
            group = "All") %>%
  addLayersControl(options = layersControlOptions(collapsed = FALSE),
                   overlayGroups = c("County",
                                     "Hospitals"))


