setwd("~/Documents/Dlab/dlab_workshops/Maps_with_ggplot2_and_ggmap_tutorial")

# Airbnb data
# fetch the data from http://insideairbnb.com/get-the-data.html
# Unzip & process as follows
sf_listings <- read.csv('data/listings.csv', stringsAsFactors = FALSE)
#sf2<- read.csv("http://data.insideairbnb.com/united-states/ca/san-francisco/2017-10-02/data/listings.csv.gz")
#mycols <- c("id","listing_url","name","summary","latitude","longitude","property_type","room_type", "accommodates","bathrooms","bedrooms","beds","price","review_scores_rating")
mycols <- c("id","name","latitude","longitude","property_type","room_type","accommodates","bathrooms","bedrooms","beds","price","review_scores_rating","neighbourhood","listing_url")
names(sf_listings)
sf_listings <- sf_listings[mycols]
sf_listings <- sf_listings[complete.cases(sf_listings),]
sf_listings <- sf_listings[sf_listings$room_type == 'Entire home/apt',]
table(sf_listings$property_type)
my_ptypes <-c("Apartment","Condominium","House")
sf_listings[!(sf_listings$property_type %in% my_ptypes),]$property_type <- "Other"
table(sf_listings$property_type)
#sf_listings <- sf_listings[sf_listings$property_type == 'Apartment',]
head(sf_listings)

# convert price to a number
convertCurrency <- function(currency) {
  currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  currency2
}

sf_listings$price <-convertCurrency(sf_listings$price)

table(sf_listings$bedrooms)
write.csv(sf_listings,file="data/sf_airbnb.csv", row.names = F)

write.csv(sf_listings[sf_listings$bedrooms == 2,],file="data/sf_airbnb_2bds.csv", row.names = F)

write.csv(sf_listings[sf_listings$bedrooms == 3,],file="data/sf_airbnb_3bds.csv", row.names = F)


hist(as.numeric(sf_listings$price))
class(sf_listings$price)

# SF Boundary File
library(rgdal)
sf <- readOGR("data/sf_boundary.shp")
plot(sf)

#install.packages("broom")
library(broom)
sf_df <- tidy(sf)
head(sf_df)
head(sf@data)

write.csv(sf_df, file="data/sf_boundary.csv", row.names=F)

sf_city <- read.csv("data/sf_boundary.csv", stringsAsFactors = F)
head(sf_city)
ggplot(sf_city, aes(long, lat)) + geom_polygon()

ggplot() + geom_polygon(data=sf_city, aes(x=long, y=lat, fill="red", group=id), colour="green", size=4, alpha=0.5)

ggplot() + geom_polygon(data=sf_city, aes(x=long, y=lat, fill="black", group=id), alpha=0.5)

ggplot() + geom_polygon(data=sf_city, aes(x=long, y=lat, group=id), color="blue", fill="gold", ltd=3, alpha=0.5)

colors() #returns all available color names.

library(ggmap)
sfmap <- get_map(location="San Francisco", zoom=13)
ggmap(sfmap)


sfmap2 <- get_map(location="San Francisco", zoom=12, source="stamen", maptype = "toner")

ggmap(sfmap)
ggmap(sfmap2, base_layer = ggplot(data=sf_apts, aes(x=longitude, y=latitude))) + geom_point()

plot(sf_apts$longitude, sf_apts$latitude)

str(sf_apts)
ggplot() + geom_point(sf_apts, aes(longitude,latitude))
ggplot() + geom_point(aes(longitude,latitude), sf_apts)
ggplot(sf_apts, aes(longitude,latitude)) + geom_point()
ggplot(sf_apts, aes(x=longitude, y=latitude)) + geom_point()

ggplot() + geom_point(data=sf_apts, aes(longitude,latitude), colour="red", size=5, alpha=0.25) #+ coord_map("mercator")


ggplot() + 
  geom_point(data=sf_apts, aes(longitude,latitude), colour="red", size=4, alpha=0.25) +
  geom_point(aes(-122.419900, 37.776154), size=5, shape=15) +
  geom_text(aes(-122.419900 + 0.01, 37.776154), label = "Civic Center")
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Airbnb 2 Bedroom Rentals, San Francisco")

ggmap(sfmap) + 
  geom_point(data=sf_apts, aes(longitude,latitude, colour=property_type), size=2, alpha=1)

ggmap(sfmap) + 
  geom_point(data=sf_apts, aes(longitude,latitude, colour=as.factor(bedrooms)), size=2, alpha=1)

ggmap(sfmap, 
    base_layer=ggplot(data=sf_apts, aes(longitude, latitude))) +
    geom_point(aes(colour=property_type), size=2, alpha=1)
  
ggmap(sfmap, 
    base_layer=ggplot(data=sf_apts, aes(longitude, latitude))) +
    geom_point(size=2, alpha=1) + facet_wrap(~property_type)

ggmap(sfmap) + 
  geom_point(data=sf_apts, aes(longitude,latitude, colour=price), size=3, alpha=0.9)

hist(sf_apts$price)

ggmap(sfmap) +
  geom_point(data=sf_apts[sf_apts$price < 1000,], aes(x=longitude, y=latitude, color=cut_number(price,6)), size=3, alpha=0.75) +
  scale_color_brewer("price", palette = "OrRd")

ggmap(sfmap) +
  geom_point(data=sf_apts[sf_apts$price < 1000,], aes(x=longitude, y=latitude, color=cut_number(price,6)), size=3, alpha=0.75) +
  scale_color_brewer("price", palette = "OrRd")


#scale_fill_gradient(name=“My var”, limits=c(0,100), low=“white”, high=“red”)
ggmap(sfmap) +
  geom_point(data=sf_apts[sf_apts$price < 1000,], aes(x=longitude, y=latitude, color=price), size=3, alpha=1) +
  scale_color_gradient(low="green", high="red")

sf_apts2 <- sf_apts[order(sf_apts$price),] 
ggmap(sfmap) +
  geom_point(data=sf_apts2[sf_apts2$price < 1000,], aes(x=longitude, y=latitude, color=price), size=3, alpha=1) +
  scale_color_gradient(low="green", high="red")

sf3 <- sf_apts2[sf_apts2$review_scores_rating >=95,]
ggmap(sfmap) +
  geom_point(data=sf3[sf3$price < 1000,], aes(x=longitude, y=latitude, color=price), size=3, alpha=1) +
  scale_color_gradient(low="blue", high="red")


library(scales)
library(RColorBrewer)


ggmap(sfmap, extent="normal", maprange=F) +
  geom_point(data=sf3[sf3$price < 500,], aes(x=longitude, y=latitude, color=price), size=2, alpha=1) +
  scale_color_distiller(name="price",palette = "Spectral", breaks = pretty_breaks(n = 5))

display.brewer.all(type="div")


library(classInt)
#natural.interval = classIntervals(population$CENSUS2010PO, n = 6, style = 'jenks')$brks
?classIntervals
classIntervals(sf3$price, n=6, style="quantile")$brks 
classIntervals(sf3$price, n=6, style="jenks")$brks
classIntervals(sf3$price, n=6, style="equal")$brks

ggmap(sfmap) + 
  geom_point(data=sf3, aes(longitude,latitude, colour=price), size=3, alpha=0.9)

ggmap(sfmap, extent="normal", maprange=F) +
  geom_point(data=sf3, aes(x=longitude, y=latitude, color=price), size=2, alpha=1) +
  scale_color_distiller(name="price",palette = "Spectral", breaks = classIntervals(sf3$price, n=6, style="quantile")$brks  )

ggmap(sfmap, extent="normal", maprange=F) +
  geom_point(data=sf3, aes(x=longitude, y=latitude, color=price), size=2, alpha=1) +
  scale_color_distiller(name="price",palette = "Spectral", breaks = classIntervals(sf3$price, n=6, style="jenks")$brks  )

ggmap(sfmap, extent="normal", maprange=F) +
  geom_point(data=sf3, aes(x=longitude, y=latitude, color=price), size=2, alpha=1) +
  scale_color_distiller(name="price",palette = "Spectral", breaks = classIntervals(sf3$price, n=6, style="equal")$brks, guide="legend" )

ggmap(sfmap, extent="normal", maprange=F) +
  geom_point(data=sf3, aes(x=longitude, y=latitude, color=price)) +
  scale_color_distiller(palette="YlOrRd", direction=1)

mmap <- get_map(location = c(11.33,44.49,11.36,44.50), source = "stamen", maptype = "toner-background")
bbox <- c(min(sf3$longitude), min(sf3$latitude), max(sf3$longitude), max(sf3$latitude))

mmap <- get_map(location = c(min(sf3$longitude),min(sf3$latitude),max(sf3$longitude), max(sf3$latitude)), source = "osm")
mmap <- get_map(location = c(-122.50936 ,37.70738,-122.38357,37.80738), source = "stamen", maptype = "toner-lite")
ggmap(mmap)

mmap <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")
ggmap(mmap)

statepop <- read.csv("data/uspop_2016.csv", stringsAsFactors = F)
head(statepop)
str(statepop)
statepop$pop2016 <- convertCurrency(statepop$pop2016)
head(statepop)

write.csv(statepop,file="data/uspop2016.csv",row.names = F)
statepop <- read.csv("data/uspop2016.csv", stringsAsFactors = F)
head(statepop)
str(statepop)

# quick geocode
geocode(statepop$state[1])

# geocode all states and add the coordinates to our table
state_coords <- geocode(statepop$state)
state_coords #take a look
statepop <- data.frame(cbind(statepop, state_coords))

# we missing data for kentucky & utah so lets just add it
statepop[statepop$state=='Kentucky',]$lon <- -85.001116
statepop[statepop$state=='Kentucky',]$lat <- 37.531427
statepop[statepop$state=='Utah',]$lon <- -111.493920
statepop[statepop$state=='Utah',]$lat <- 39.308401
write.csv(statepop,file="data/uspop2016_geo.csv",row.names = F)

## map it
ggplot() + geom_point(data=statepop, aes(lon,lat, size=pop2016), shape=1, color="red", stroke=1.5)

## map it
ggplot() + 
  geom_point(data=statepop, aes(lon,lat, size=pop2016), shape=1, color="red", stroke=1.5) +
  scale_size(range=c(2,10))

## map it
ggplot() + 
  geom_point(data=statepop, aes(lon,lat, size=pop2016), shape=1, color="red", stroke=1) +
  scale_size_continuous(name="pop2016",range = c(2, 9), breaks=classIntervals(statepop$pop2016, n=4, style="equal")$brks) +
  coord_map()

    geom_label(data=statepop$state)


usamap <- get_map(location="USA", source="stamen", maptype="toner-lines", zoom=3)
ggmap(usamap)

### map pop
ggmap(usamap) +
  geom_point(data=statepop, aes(lon,lat, size=pop2016), shape=1, color="red", stroke=1.5)

library(maps)
ggplot(states, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group))
