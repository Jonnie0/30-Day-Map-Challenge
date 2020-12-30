library(tidyverse)
library(osmdata)
library(sf)
library(ggtext)
library(showtext)
# Import Data -------------------------------------------------------------
font_add_google('DM Serif Display')

showtext_auto(enable = TRUE)

theme_set(theme_void()) + 
  theme_update(panel.background = element_rect(fill = "darkslategray", color = NA), 
               plot.background = element_rect(fill = 'ivory3',color = NA), 
               plot.margin = unit(c(5, 5, 5, 5), 'mm'), 
               plot.caption = element_text(family = 'DM Serif Display', hjust = .5))

data_coast <- read_sf('/Users/jonathankabel/Downloads/land-polygons-split-4326/land_polygons.shp')
land <- (st_intersection(data_coast, 
                        st_sfc(st_polygon((getbb('San Francisco, CA', 
                                                 format_out = 'sf_polygon'))[['multipolygon']][[1]][[1]][[1]][2]),
                               crs = 4326)))
# structure of features list: (key, value, shape, fill, color, alpha, stroke, size, excluded shapes)

features <- list(list(NULL, NULL, 1, 'seashell', NA, 1, NA, NA, c(2, 15, 16, 22, 24, 26, 27, 29:33)),
                 list('leisure', 'park', 'osm_polygons', 'darkolivegreen', NA, 1, NA, NA, 10 ** 10),
                 list('leisure', 'park', 'osm_multipolygons', 'darkolivegreen', NA, 1, NA, NA, 10 ** 10),
                 list('natural', 'water', 'osm_polygons', 'darkslategrey', NA, 1, NA, NA, 10 ** 10),
                 list('highway', NULL, 'osm_lines', NA, 'black', 1, NA, .1, c(3863, 23379)),
                 list('leisure', 'fitness_centre', 'osm_points', NA, 'darkgoldenrod', .7, 0, 3, 10 ** 10),
                 list('shop', 'bakery', 'osm_points', NA, 'coral4', .7, 0, 3, 10 ** 10)
                 )

get_osm <- function(x,y) {
  bb <- getbb(y, format_out = 'polygon')
  output <- vector(mode = 'list', length = length(x))
  output[[1]] <- list(land)
  #fig <- ggplot()
  fig <- vector(mode = 'list', length = length(x))
  for (i in 2:length(x)) {
    output[[i]]<- opq(y) %>%
      add_osm_feature(key = x[[i]][[1]], value = x[[i]][[2]]) %>%
      osmdata_sf() %>%
      trim_osmdata(bb[[1]][2]) 
  }
  for (i in 1:length(x)) {
    fig[[i]] <- geom_sf(data = output[[i]][[x[[i]][[3]]]][-c(x[[i]][[9]]),], 
                         fill = x[[i]][[4]], 
                         color = x[[i]][[5]], 
                         alpha = x[[i]][[6]], 
                         stroke = x[[i]][[7]], 
                         size = x[[i]][[8]]
                         )
  }
  ggplot() + unlist(fig, recursive = FALSE) + 
    annotate(geom = 'richtext', x = (-122.53 - 122.35) / 2, y = 37.858, 
             label = "<span style = 'font-size: 36pt'> <b style =  'color:coral4' >Bakeries </b>& 
                      <b style = 'color:darkgoldenrod'>gyms </b>in </span><br> 
                      <b style = 'font-size: 60pt'>San Francisco</b>",
             color = 'seashell',
             family = 'DM Serif Display',
             fontface = 'bold',
             fill = NA,
             label.color = NA) +
    coord_sf(xlim = c(-122.53, -122.35), 
             ylim = c(37.70824, 37.88), 
             expand = FALSE) +
    labs(caption = 'Data: OpenStreetMaps | Viz: @KabelJonathan')
}

fig <- get_osm(features, 'San Francisco, CA')

ggsave('final.png', plot = fig, height = 10.4, width = 8.5, units = 'in', dpi = 1200)
