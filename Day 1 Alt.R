


text_font <- 'DM Serif Display'

font_add_google('DM Serif Display')

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

ggplot() + theme_void() + geom_sf(data = data_sanfran_trim[["osm_lines"]], size = .1)

data_sanfran_natural <- opq('San Francisco, CA') %>%
  add_osm_feature(key = 'natural') %>%
  osmdata_sf()

data_final <- unique_osmdata(data_sanfran_natural)

data_sanfran_natural_trim <- data_sanfran_natural #%>% trim_osmdata(bb[[1]][2])

#replace_NA <- data_sanfran_natural_trim$osm_polygons$natural == 'bay'
#replace_NA[is.na(replace_NA)] <- TRUE
#natural_feature <- data_sanfran_natural_trim[["osm_polygons"]][replace_NA,]


#data_sanfran_bakery <- opq('San Francisco, CA') %>%
#  add_osm_feature(key = 'shop', value = 'bakery') %>%
#  osmdata_sf() %>%
#  trim_osmdata(bb[[1]][2])


#data_sanfran_natural_trim <- trim_osmdata(data_sanfran_natural, bb[[1]][2])

#data_sanfran_park <- opq('San Francisco, CA') %>%
#  add_osm_feature(key = 'leisure', value = 'park') %>%
#  osmdata_sf() %>%
#  trim_osmdata(bb[[1]][2])

#data_sanfran_nationalpark <- opq('San Francisco, CA') %>%
#  add_osm_feature(key = 'boundary', value = 'national_park') %>%
#  osmdata_sf() %>%
#  trim_osmdata(bb[[1]][2])

#data_sanfran_commercial <- opq('San Francisco, CA') %>%
# add_osm_feature(key = 'landuse', value = 'commercial') %>%
#osmdata_sf() %>%
#trim_osmdata(bb[[1]][2])

#data_sanfran_commercial <- opq('San Francisco, CA') %>%
#  add_osm_feature(key = 'natural', value = 'coastline') %>%
#  osmdata_sf() #%>%

#data_sanfran_shops <- opq('San Francisco, CA') %>%
#  add_osm_feature(key = 'shop') %>%
#  osmdata_sf() %>%
#  trim_osmdata(bb[[1]][2])

#data_sanfran_leisure <- opq('San Francisco, CA') %>%
#  add_osm_feature(key = 'leisure') %>%
#  osmdata_sf() %>%
#  trim_osmdata(bb[[1]][2])

#leisure <- count(data_sanfran_leisure$osm_points %>% group_by(leisure))  
#shops <- count(data_sanfran_shops$osm_points %>% group_by(shop))

#data_sanfran_commercial$osm_lines %>% osm_line2poly(bb1)
#trim_osmdata(bb[[1]][2])
#bb1 <- osmdata::getbb ("San Francisco, CA")
#coast <- extract_osm_objects (bbox = bb1, key = "natural", value = "coastline",
#                             return_type = "line")


theme_set(theme_void()) + theme_update(
  panel.background = element_rect(fill = "darkslategray", 
                                  color = NA),
  plot.background = element_rect(fill = 'ivory3',
                                 color = NA), 
  plot.margin = unit(c(5, 5, 5, 5), 'mm'),
  plot.caption = element_text(family = text_font, hjust = .5)
)

showtext_auto(enable = TRUE)
ggplot() +
  geom_sf(data = land[c(1:18, 20, 22),], fill = 'seashell', color = NA) +
  geom_sf(data = data_sanfran_park$osm_multipolygons, fill = 'darkolivegreen', color = NA) +
  geom_sf(data = data_sanfran_park$osm_polygons, fill = 'darkolivegreen', color = NA) +
  geom_sf(data = data_sanfran_water$osm_polygons, fill = 'darkslategray', color = NA) + 
  geom_sf(data = data_sanfran_trim[["osm_lines"]][-c(3863, 23379),], color = 'black', size = .1) +
  geom_sf(data = leisure[leisure$leisure == 'fitness_centre',], color = 'darkgoldenrod', alpha = .7, stroke = 0, size = 3) +
  geom_sf(data = shops[shops$shop == 'bakery',], color = 'coral4', alpha = .7, stroke = 0, size = 3) +
  annotate(geom = 'richtext', x = (-122.53 - 122.35) / 2, y = 37.858, 
           label = "<span style = 'font-size: 36pt'> <b style =  'color:coral4' >Bakeries </b>& <b style = 'color:darkgoldenrod'>gyms </b>in </span><br> <b style = 'font-size: 60pt'>San Francisco</b>", 
           color = 'seashell',
           family = text_font,
           fontface = 'bold',
           fill = NA,
           label.color = NA) +
  coord_sf(xlim = c(-122.53, -122.35), ylim = c(37.70824, 37.88), expand = FALSE) +
  labs(caption = 'Data: OpenStreetMaps | Viz: @KabelJonathan', family = text_font) +
  ggsave('test.png', height = 10.4, width = 8.5, units = 'in', dpi = 1200)

#coast1 <- st_polygonize(data_sanfran_commercial$osm_lines)

#coast_poly <- osm_line2poly (coast$osm_lines, bbox)
#osm_line2poly()

#data_coast <- read_sf('/Users/jonathankabel/Downloads/land-polygons-split-4326/land_polygons.shp')

#bbox <- getbb('San Francisco', format_out = 'sf_polygon')
#poly <- st_polygon((bbox[["multipolygon"]])[[1]][[1]][[1]][2]) %>%
# st_sfc(crs = 4326)

#land <- st_intersection(tester, poly) #%>% st_crop(xmin = -180, xmax = 180, ymin = -180, ymax = -37.83475)
#tester <- data_coast[data_coast$x == -123 & data_coast$y == 37,] %>% 
#  st_
#st_crop(xmin = -122.6, xmax = -122.35, ymin = 37.64031, ymax = 37.9)

#land <- st_intersection(tester, poly) %>% st_crop(xmin = -180, xmax = 180, ymin = -180, ymax = 37.83475)

#land <- st_intersection(data_coast, st_sfc(st_polygon((bbox[["multipolygon"]])[[1]][[1]][[1]][2]), crs = 4326)) %>% st_crop(xmin = -180, xmax = 180, ymin = -180, ymax = 37.83475)

#land <- st_intersection(data_coast, st_sfc(st_polygon(getbb('San Francisco', format_out = 'sf_polygon')[['multipolygon']]), crs = 4326)) %>% st_crop(xmin = -180, xmax = 180, ymin = -180, ymax = 37.83475)

