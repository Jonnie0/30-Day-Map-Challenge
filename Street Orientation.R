library(tidyverse)
library(osmdata)
library(sf)
library(geosphere)
library(showtext)
library(patchwork)
library(parallel)

font_add_google('Bebas Neue') # Get font for visualization
showtext_auto(enable = TRUE) # Needed to display font on plots

get_streets_ind <- function(location, bin) { # Function which takes a string corresponding to a city ('City, state/country') and returns a polar histogram for the orientations of that city's streets. Bin determines how many bins the histogram has.
  streets <- opq(bbox = location, timeout = 500) %>% add_osm_feature(key = 'highway') %>%
    osmdata_sf() %>% pluck('osm_lines') %>% select(geometry) %>% # Get street data from OSM and isolate points which define each street (each street is a multistring, made up of a set of points). These data are quite large, which is why we pluck/select only relevant bits; takes up a lot of memory in general, so this whole process can be quite slow.  
    st_coordinates() %>% as_tibble() %>% group_by(L1) %>% #Convert to tibble so we can reference leads/lags of observations (we are interested in the relationship between points, in terms of angle and length). L1 defines which street a point belongs to, so grouping by it helps prevent us from looking at line connecting two points not on the same street.
    mutate(length = distHaversine(cbind(X, Y), cbind(lead(X), lead(Y))), #distHaversine gives us the distance between points on the globe. Cbind speeds up this computation.
           bearing = bearing(cbind(X,Y), cbind(lead(X), lead(Y)))) %>% #Bearing gives us the direction (in degrees, from -180 to 180, where 0 is North) of a street segment
    mutate(bearing = (bearing + 180) %% 180) %>% ungroup() %>% select(length, bearing) %>% drop_na() # Add 180 to get bearing from 0 to 360, and then find modulo 180, since every road that goes in one direction also goes in the opposite. We have NA's for bearing/length for every last point on a road (it doesn't have a lead observation for the same road), so drop those.
  streets <- bind_rows(streets, tibble(bearing = streets$bearing - 180, length = streets$length)) %>% #Right now we have bearings from 0-180, so subtract 180 and bind below in order to get bearings ranging from -180 to 180 (Intuitively, this is symmetric around 0)
    mutate(bearing = (round(bearing/bin, 0) * bin) %% 360 - (bin/2)) # Round into bins and recenter bins.
  plot <- ggplot_build(ggplot(data = streets, mapping = aes(x = bearing, weight = length)) +
                         geom_histogram(aes(y=..count../sum(..count..)), binwidth = bin, closed = 'left')) # We build this plot so that we can pull out the max bin height to use later
  output <- ggplot(data = streets, mapping = aes(x = bearing, weight = length)) + theme_void() + #Build final plot for individual city; Weight street segment orientation by length of street segment
    theme(plot.title = element_text(size = 30, family = 'Bebas Neue', hjust = .5), axis.text.x = element_text(size = 16, family = 'Bebas Neue')) +
    geom_histogram(aes(y=..count../sum(..count..)), binwidth = bin, closed = 'left', fill = 'royalblue4', color = 'black') + 
    scale_x_continuous(breaks = c(0, 90, 180, 270), labels = c('N', 'E', 'S', 'W'), limits = c(0 - (bin/2), 360 - (bin/2))) +
    coord_polar(start = 0 - ((bin/2) * pi / 180)) + 
    scale_y_continuous(limits = c(-.2 * max(plot[[1]][[1]][[1]]), max(plot[[1]][[1]][[1]]))) + 
    geom_hline(yintercept = max(plot[[1]][[1]][[1]]), size = 1) + labs(title = str_split(location, ',')[[1]][1])
}

get_streets <- function(location_list, bin) { #We want to ultimately create plots for a bunch of cities, so we create a function to apply the function above to a list of cities.
  streets <- mclapply(location_list, get_streets_ind, bin = bin, mc.cores = 3) #To speed up we can use a multicore lapply on this list. I used one fewer cores than my laptop has: 3. We pass the bin argument through this function so it's set for the nested function.
  wrap_plots(streets) # Put individual plots (output of get_streets_ind function) together
}


cities <- list('Venice, Italy', 'San Francisco, CA', 'Amsterdam, Netherlands', 'Berlin, Germany', 'Paris, France', #Here's my sample list. Applying the function immediately above takes a looong time, idk why exactly, still working on things to speed it up further. lmk if you have any ideas!
            'Osaka, Japan')#, 'Shanghai, China', 'Rome, Italy', 
             #'London, UK', 'Miami, Florida', 'Toronto, Canada', 'Cairo, Egypt')

fig <- get_streets(cities, 10) #Apply function to list of cities in order to generate plots. Output is the final plot.  


