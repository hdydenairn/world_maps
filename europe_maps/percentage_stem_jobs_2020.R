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

# get shapefile (nuts2)
shape <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = "2",
  year = "2016",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  crs = "4326",
  make_valid = FALSE
)

# search for datasets on NUTS 2 level in their title
all_eurostat <- eurostat::get_eurostat_toc()
nuts_2_level <- subset(toc, grepl('NUTS 2', title))

# get NUTS2-level data on employment in STEM
htec_emp_reg2 <- eurostat::get_eurostat("htec_emp_reg2",
                                        time_format = "num")

stem <- htec_emp_reg2 %>%
  filter(time==2021,
         unit=="PC_EMP", #% of total employment
         sex=="T", # all genders
         nace_r2 =="HTC") %>% 
  dplyr::select (geo, values)


stem1 <- st_as_sf(right_join(stem, shape, by="geo")) %>% 
  dplyr::select(geo, values, geometry) %>%
  na.omit()

# create choropleth

stem1%>%
  ggplot(aes(fill = values))+
  geom_sf(color = "black", size = 0.1)+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse", 
                       labels = function(x) paste0(x))+
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )+
  theme_minimal()+
  coord_sf(xlim=c(-10.6600,39), ylim=c(32.5000,71.0500))+
  labs(x = "",
       title="Employment in technology and knowledge-intensive sectors",
       subtitle ="NUTS 2",
       caption="Source: https://ec.europa.eu/eurostat/databrowser/view/htec_emp_reg2/")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .04),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=20, color="#CE4433", hjust=0.5, vjust=-10),
        plot.subtitle = element_text(size=14, color="#F29A4F", hjust=0.5, vjust=-15, face="bold"),
        plot.caption = element_text(size=9, color="grey60", hjust=0.5, vjust=9),
        axis.title.x = element_text(size=7, color="grey60", hjust=0.5, vjust=5),
        legend.text = element_text(size=10, color="grey20"),
        legend.title = element_text(size=11, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave("eu_stem_2020.png", height = 9, width = 12)
