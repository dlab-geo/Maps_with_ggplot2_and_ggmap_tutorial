library(ggplot2)
library(ggmap)

# Set your working directory to the tutorial folder
setwd("~/Documents/Dlab/dlab_workshops/Maps_with_ggplot2_and_ggmap_tutorial")

# Load the data
sf_apts <- read.csv("data/sf_airbnb_2bds.csv")
#

airbnb_map <- ggmap(sfmap) + geom_point(data=sf_apts, aes(longitude, latitude), 
                                        colour="red", size=3, alpha=0.35) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Airbnb 2 Bedroom Rentals, San Francisco")

bbox <- c(min(sf_apts1k$longitude), min(sf_apts1k$latitude), 
          max(sf_apts1k$longitude), max(sf_apts1k$latitude))

sfmap_bbox <- get_map(location = bbox, source = "stamen",
                      maptype = "toner-lite")

mymap <- ggmap(sfmap_bbox) +
  geom_point(data=sf_apts1k, aes(x=longitude, y=latitude, color=price)) +
  scale_color_distiller(palette="YlOrRd", direction=1)


library(classInt)

mymap <- ggmap(sfmap_bbox) +
  geom_point(data=sf_apts1k, aes(x=longitude, y=latitude, color=price), size=2, alpha=1) +
  scale_color_distiller(name="price",palette = "Spectral",
                    breaks = classIntervals(sf_apts1k$price, n=6, style="quantile")$brks  )