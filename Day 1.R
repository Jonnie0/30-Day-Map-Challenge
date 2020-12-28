library(tidyverse)
library(osmdata)
library(osmplotr)
library(sf)
library (magrittr)



data_sanfran <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

bb <- getbb('San Francisco, CA', format_out = 'polygon')
bb1 <- getbb('San Francisco, CA')
ggplot() + theme_void() +geom_sf(data = data_sanfran$osm_lines)

data_sanfran_trim <-  trim_osmdata(data_sanfran, bb[[1]][2])

data_sanfran_water <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf() %>%
  trim_osmdata(bb[[1]][2])

data_sanfran_tree <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'natural', value = 'tree') %>%
  osmdata_sf() %>%
  trim_osmdata(bb[[1]][2])

data_sanfran_wood <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'natural', value = 'wood') %>%
  osmdata_sf() %>%
  trim_osmdata(bb[[1]][2])

ggplot() + theme_void() + geom_sf(data = data_sanfran_trim[["osm_lines"]], size = .1)

data_sanfran_natural <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'natural') %>%
  osmdata_sf()

data_final <- unique_osmdata(data_sanfran_natural)

data_sanfran_natural_trim <- data_sanfran_natural #%>% trim_osmdata(bb[[1]][2])

#replace_NA <- data_sanfran_natural_trim$osm_polygons$natural == 'bay'
#replace_NA[is.na(replace_NA)] <- TRUE
#natural_feature <- data_sanfran_natural_trim[["osm_polygons"]][replace_NA,]


ggplot() + theme_void() + geom_sf(data = natural_feature, fill = 'blue') + geom_sf(data = data_sanfran_trim[["osm_lines"]], size = .1)

data_sanfran_natural_trim <- trim_osmdata(data_sanfran_natural, bb[[1]][2])

data_sanfran_park <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf() %>%
  trim_osmdata(bb[[1]][2])

data_sanfran_nationalpark <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'boundary', value = 'national_park') %>%
  osmdata_sf() %>%
  trim_osmdata(bb[[1]][2])

data_sanfran_commercial <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'landuse', value = 'commercial') %>%
  osmdata_sf() %>%
  trim_osmdata(bb[[1]][2])
data_sanfran_commercial <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf() #%>%
  
data_sanfran_commercial$osm_lines %>% osm_line2poly(bb1)
  #trim_osmdata(bb[[1]][2])
bb1 <- osmdata::getbb ("San Francisco, CA")
coast <- extract_osm_objects (bbox = bb1, key = "natural", value = "coastline",
                              return_type = "line")
coast <- osm_line2poly (coast, bbox = bb1)

theme_set(theme_void()) + theme_update(
  panel.background = element_rect(fill = "cornflowerblue", 
                                  color = "grey92"),
  plot.background = element_rect(fill = "wheat", 
                                 color = "wheat"))

ggplot() +
#theme_void() + #geom_sf(data = data_sanfran_commercial$osm_lines, fill = 'wheat', color = 'red') +
  geom_sf(data = land, fill = 'grey92', color = NA) +
  #geom_sf(data = tester, fill = "wheat", color = NA) +
  geom_sf(data = data_sanfran_park$osm_multipolygons, fill = 'darkolivegreen3', color = NA) +
  geom_sf(data = data_sanfran_park$osm_polygons, fill = 'darkolivegreen3', color = NA) +
  geom_sf(data = data_sanfran_water$osm_polygons, fill = 'cornflowerblue', color = NA) + 
  #geom_sf(data = data_sanfran_wood$osm_polygons, fill = 'darkolivegreen3', color = NA) + 
  #geom_sf(data = data_sanfran_tree$osm_points, size = .01, color = 'olivedrab') + 
  geom_sf(data = data_sanfran_trim[["osm_lines"]], size = .1)
  



coast1 <- st_polygonize(data_sanfran_commercial$osm_lines)

coast_poly <- osm_line2poly (coast$osm_lines, bbox)
osm_line2poly()

data_coast <- read_sf('/Users/jonathankabel/Downloads/land-polygons-split-4326/land_polygons.shp')

bbox <- getbb('San Francisco', format_out = 'sf_polygon')
poly <- st_polygon((bbox[["multipolygon"]])[[1]][[1]][[1]][2]) %>%
  st_sfc(crs = 4326)

land <- st_intersection(tester, poly)
tester <- data_coast[data_coast$x == -123 & data_coast$y == 37,] %>% 
  st_
  #st_crop(xmin = -122.6, xmax = -122.35, ymin = 37.64031, ymax = 37.9)



