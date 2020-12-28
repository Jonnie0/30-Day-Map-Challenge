library(tidyverse)
library(sf)
sf_data <- read_csv('/Users/jonathankabel/Downloads/Assessor_Historical_Secured_Property_Tax_Rolls.csv')
loc_data <- read_csv('/Users/jonathankabel/Downloads/Parcels___Active_and_Retired.csv')


sf_data_2019 <- sf_data[sf_data$`Closed Roll Year` == 2019,]

sf_data_merged <- inner_join(loc_data, sf_data_2019, by = c('blklot' = 'Parcel Number'))

sf_as_sf <- st_as_sf(sf_data_merged, wkt = 'shape')
sf_as_sf <- sf_as_sf[sf_as_sf$`Property Class Code Definition` != 'Under Water Lot',]

test_data <- sf_as_sf[sample(nrow(sf_as_sf), 100000),]

ggplot(data = sf_as_sf) + geom_sf(mapping = aes(fill = zoning_district), color = NA) + theme(legend.position = 'none')

ggplot(data = sf_as_sf[sf_as_sf$`Use Definition` == 'Commercial Retail',]) + geom_sf(mapping = aes(fill = `Year Property Built`), color = NA) + theme_void() + theme(legend.position = 'none')


ggplot() + geom_sf(data = sf_as_sf[sf_as_sf$`Use Definition` == 'Commercial Retail',], 
                   fill = 'Black', color = NA) + 
          geom_sf(data = sf_as_sf[sf_as_sf$`Use Definition` != 'Commercial Retail',], 
                  fill = 'Grey', color = NA) + 
  theme_void() + theme(legend.position = 'none')



ggsave(file = 'test.png', plot = age_plot, width = 20, height = 16, dpi = 1200)
