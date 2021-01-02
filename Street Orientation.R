library(tidyverse)
library(osmdata)
library(sf)
library(geosphere)
library(showtext)
library(patchwork)
library(parallel)

font_add_google('Bebas Neue')
showtext_auto(enable = TRUE)

get_streets_ind <- function(location, bin) {
  streets <- opq(bbox = location, timeout = 500) %>% add_osm_feature(key = 'highway') %>%
    osmdata_sf() %>% pluck('osm_lines') %>% select(geometry) %>%
    st_coordinates() %>% as_tibble() %>% group_by(L1) %>%
    mutate(length = distHaversine(cbind(X, Y), cbind(lead(X), lead(Y))),
           bearing = bearing(cbind(X,Y), cbind(lead(X), lead(Y)))) %>%
    mutate(bearing = (bearing + 180) %% 180) %>% ungroup() %>% select(length, bearing) %>% drop_na()
  streets <- bind_rows(streets, tibble(bearing = streets$bearing - 180, length = streets$length)) %>% 
    mutate(bearing = (round(bearing/bin, 0) * bin) %% 360 - (bin/2))
  plot <- ggplot_build(ggplot(data = streets, mapping = aes(x = bearing, weight = length)) +
                         geom_histogram(aes(y=..count../sum(..count..)), binwidth = bin, closed = 'left'))
  output <- ggplot(data = streets, mapping = aes(x = bearing, weight = length)) + theme_void() +
    theme(plot.title = element_text(size = 30, family = 'Bebas Neue', hjust = .5), axis.text.x = element_text(size = 16, family = 'Bebas Neue')) +
    geom_histogram(aes(y=..count../sum(..count..)), binwidth = bin, closed = 'left', fill = 'royalblue4', color = 'black') + 
    scale_x_continuous(breaks = c(0, 90, 180, 270), labels = c('N', 'E', 'S', 'W'), limits = c(0 - (bin/2), 360 - (bin/2))) +
    coord_polar(start = 0 - ((bin/2) * pi / 180)) + 
    scale_y_continuous(limits = c(-.2 * max(plot[[1]][[1]][[1]]), max(plot[[1]][[1]][[1]]))) + 
    geom_hline(yintercept = max(plot[[1]][[1]][[1]]), size = 1) + labs(title = str_split(location, ',')[[1]][1])
}

get_streets <- function(location_list, bin) {
  streets <- mclapply(location_list, get_streets_ind, bin = bin, mc.cores = 3)
  wrap_plots(streets)
}

mclapply()

cities <- list('Venice, Italy', 'San Francisco, CA', 'Amsterdam, Netherlands', 'Berlin, Germany', 'Paris, France', 
            'Osaka, Japan')#, 'Shanghai, China', 'Rome, Italy', 
             #'London, UK', 'Miami, Florida', 'Toronto, Canada', 'Cairo, Egypt')

fig <- get_streets(cities, 10)


