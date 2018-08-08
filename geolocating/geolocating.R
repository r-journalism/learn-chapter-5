
# if you don't have the following packages installed, uncomment and run the lines below

#install.packages(c("dplyr", "ggplot2", "tidyr", "ggmap", "DT", "knitr", "readr"))

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(DT)
library(knitr)

stations <- read_csv("data/Police_Departments.csv")
glimpse(stations)

stations <- stations %>% 
mutate(ZIP=paste0("0", as.character(ZIP))) %>% 
mutate(location = paste0(ADDRESS, ", ", CITY, ", CT ", ZIP))

geo <- mutate_geocode(stations, location)

# If it's taking too long, you can cancel and load the output by uncommenting the line below
geo <- read_csv("data/geo_stations.csv")

# Bringing over the longitude and latitude data
stations$lon <- geo$lon
stations$lat <- geo$lat

revgeocode(c(lon = -77.030137, lat = 38.902986))

# If you don't have tigris or ggplot2 or sf installed yet, uncomment and run the line below
#install.packages("tigris", "sf", "ggplot2")

library(tigris)
library(sf)
library(ggplot2)

# set sf option

options(tigris_class = "sf")

ct <- county_subdivisions("CT", cb=T)

# If cb is set to TRUE, download a generalized (1:500k) counties file. 
# Defaults to FALSE (the most detailed TIGER file).

ggplot(ct) + 
  geom_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Connecticut towns")

ggplot(ct) + 
  geom_sf() +
  geom_point(data=stations, aes(x=lon, y=lat), color="blue") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Police stations")

set.seed(7)

stations$staff <- sample(200, size=nrow(stations), replace=T)

ggplot(ct) + 
geom_sf(fill="transparent") +
geom_point(data=stations, aes(x=lon, y=lat, size=staff, color=DESCRIPTION),  
fill="white", shape=1) +
theme_void() +
theme(panel.grid.major = element_line(colour = 'transparent')) +
labs(title="Police stations in Connecticut") +
coord_sf()
