library(tidyverse)
library(osmdata)
library(sf)


data_sanfran <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

bb <- getbb('San Francisco, CA', format_out = 'polygon')

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



ggplot() + theme_void() + geom_sf(data = data_sanfran_water$osm_polygons, fill = 'cornflowerblue', color = NA) + geom_sf(data = data_sanfran_wood$osm_polygons, fill = 'darkolivegreen3', color = NA)+ geom_sf(data = data_sanfran_tree$osm_points, size = .05, color = 'olivedrab') + 
  geom_sf(data = data_sanfran_trim[["osm_lines"]], size = .1)
