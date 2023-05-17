library(tidyverse)
library(tibble)
library(maps)
library(mapdata)
library(maptools)
library(sp)

# used this tutorials: 
# https://stackoverflow.com/questions/45928898/how-do-i-use-ggplot2-to-create-a-border-around-a-group-of-us-counties/45931429


# PREP DATA FOR MAPPING
ca_df <- map_data("state") %>% filter(region == "california") 
ca_co_df <- map_data("county", "california")


# UCSF list source: https://cancer.ucsf.edu/catchment-area-dashboard
ucsf_list <- c("alameda","butte","colusa","contra costa","fresno","glenn",
               "lake","madera","marin","mendocino","merced","monterey","napa",
               "sacramento","san benito","san francisco","san joaquin",
               "san mateo","santa clara","santa cruz","solano","sonoma",
               "stanislaus","sutter","yolo")

# UC Davis list source: https://health.ucdavis.edu/cancer/community/coe-catchment.html
ucd_list <- c("alpine","amador","butte","calaveras","colusa","el dorado",
              "glenn","merced","nevada","placer","sacramento","san joaquin",
              "sierra","solano","stanislaus","sutter", "tehama","yolo","yuba")

# Stanford: https://gis.cancer.gov/ncicatchment/app/
stanford_list <- c("alameda","contra costa","merced","monterey","san benito",
                   "san joaquin","san mateo","santa clara","santa cruz",
                   "stanislaus")


no_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)




# USED THIS EXAMPLE: 

# get map (as map object)
county_map <- map("county", regions = "california",
                  fill = T, plot = FALSE)
county_poly <- maptools::map2SpatialPolygons(county_map, ID = county_map$names)

## UCSF
ucsf_match <- data.frame(name = county_map$names) %>%
  separate(name, c("region", "subregion"), sep = ",", remove = FALSE) %>%
  mutate(ucsf = subregion %in% ucsf_list) %>% 
  column_to_rownames("name")
ucsf_map <- sp::SpatialPolygonsDataFrame(county_poly, ucsf_match)
rm(ucsf_match)
ucsf_map <- rgeos::gBuffer(ucsf_map, byid = TRUE, width = 0)
ucsf_map <- ucsf_map %>% 
  unionSpatialPolygons(IDs = ucsf_map$ucsf) %>% 
  fortify() %>% 
  filter(group == "TRUE.1")


## UC Davis
ucd_match <- data.frame(name = county_map$names) %>%
  separate(name, c("region", "subregion"), sep = ",", remove = FALSE) %>%
  mutate(ucd = subregion %in% ucd_list) %>% 
  column_to_rownames("name")
ucd_map <- sp::SpatialPolygonsDataFrame(county_poly, ucd_match)
rm(ucd_match)
ucd_map <- rgeos::gBuffer(ucd_map, byid = TRUE, width = 0)
ucd_map <- ucd_map %>% 
  unionSpatialPolygons(IDs = ucd_map$ucd) %>% 
  fortify() %>% 
  filter(group == "TRUE.1")

## Stanford
stanford_match <- data.frame(name = county_map$names) %>%
  separate(name, c("region", "subregion"), sep = ",", remove = FALSE) %>%
  mutate(stanford = subregion %in% stanford_list) %>% 
  column_to_rownames("name")
stanford_map <- sp::SpatialPolygonsDataFrame(county_poly, stanford_match)
rm(stanford_match)
stanford_map <- rgeos::gBuffer(stanford_map, byid = TRUE, width = 0)
stanford_map <- stanford_map %>% 
  unionSpatialPolygons(IDs = stanford_map$stanford) %>% 
  fortify() %>% 
  filter(group == "TRUE.1")

# plot all the counties as one polygon
p<- ggplot() + 
  geom_polygon(data = ca_df, mapping = aes(x = long, y = lat), color = "black", fill = "gray") + 
  geom_polygon(data = ca_co_df, mapping = aes(x = long, y = lat, group = group), color = "dark gray", fill = "transparent") +
  geom_polygon(data = ucsf_map,
               aes(x = long, y = lat, group = group),
               fill = "#006BE9", colour = "#006BE9", alpha = 0.4, size = 1) +
  geom_polygon(data = ucd_map,
               aes(x = long, y = lat, group = group),
               fill = "darkgreen", colour = "darkgreen", alpha = 0.4, size = 1) +
  geom_polygon(data = stanford_map,
               aes(x = long, y = lat, group = group),
               fill = "#8C1515", colour = "#8C1515", alpha = 0.4, size = 1) + 
  no_axes

