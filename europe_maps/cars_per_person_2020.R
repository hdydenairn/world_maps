#load libraries
library(ggplot2) 
library(plyr) 
library(rgdal)
library(rgeos)
library(grid) 
library(dplyr)
library(classInt)
library(raster)
library(maptools)
library(eurostat)
library(sf)

# find, attach & clean dataset

eurostat_search <- search_eurostat( "cars" )
eurostat_search$title

road_eqs_carhab <- get_eurostat("road_eqs_carhab",
                                time_format = "num")
road_eqs_carhab$new_col <- cbind(road_eqs_carhab$values/1000)
names(road_eqs_carhab)[5] <- "value"

cars <- road_eqs_carhab %>%
  dplyr::filter(unit=="NR" &
                  time==2020 ) %>%
  dplyr::select(geo, value)
names(cars)[1] <- "CNTR_CODE"

europe <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = "0",
  year = "2016",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  crs = "4326",
  make_valid = FALSE
)

eu_cars_2020 <- st_as_sf(right_join(cars, europe, by="CNTR_CODE")) %>% 
  dplyr::select(CNTR_CODE, value, geometry) %>%
  na.omit()

# create choropleth

eu_cars_2020%>%
  ggplot(aes(fill = value))+
  geom_sf(color = "black", size = 0.1)+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse", 
                       labels = function(x) paste0(x))+
  theme_void()+
  coord_sf(xlim=c(-10.6600,39), ylim=c(32.5000,71.0500))+
  labs(x = "",
       title="Passenger Cars per Person ",
       subtitle ="EU27+ in 2020",
       caption="Source: https://ec.europa.eu/eurostat/databrowser/view/road_eqs_carhab/")+
  theme(
    legend.position = c(0.9, 0.5),
    plot.caption = element_text(hjust = 0.5, size = 8, vjust = 20),
    plot.title = element_text(hjust = 0.5, size = 14, vjust = -10),
    plot.subtitle = element_text(hjust = 0.5, size = 12, vjust = -10),
    plot.background = element_rect(fill="grey80", linetype = 0),
    panel.background = element_rect(fill="grey80", linetype = 0 ))
ggsave("eu_cars_2020.png", height = 9, width = 12)
