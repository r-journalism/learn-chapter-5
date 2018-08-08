
# Uncomment and run "install.packages" functions below if you have not yet installed these packages

#install.packages("leaflet")

# IF YOU GET AN ERROR BECAUSE THERE IS NO PACKAGE CALLED httpuv

#install.packages("httpuv")
#install.packages("leaflet")
library(leaflet)

#install.packages("dplyr")
library(dplyr)

# Insert your latitude and longitude in the code below
# NOTE: Don't get them reversed otherwise you'll end up in the South Pole.

# Initialize and assign m as the leaflet object
m <- leaflet() %>%
# Now add tiles to it
addTiles() %>%  
# Setting the middle of where the map should be and the zoom level
setView(lng=-77.030137, lat=38.902986, zoom = 16) %>%
# Now, add a marker with a popup, 
addMarkers(lng=-77.030137, lat=38.902986, popup="<b>Hello</b><br><a href='https://www.washingtonpost.com'>-Me</a>")

m 

library(readr)
dunkin <- read_csv("data/dunkin.csv")

str(dunkin)

# Pick a state, any state.
# I'll use Massachusetts here because that's where Dunkin started

dd_state <- dunkin %>% 
filter(state=="MA")

m <- leaflet(dd_state) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
setView(-71.931180, 42.385453, zoom = 7) %>% 
addCircles(~lon, ~lat, popup=dunkin$type, weight = 3, radius=40, 
color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 
m

starbucks <- read.csv("data/starbucks.csv", stringsAsFactors=F)

str(starbucks)

sb_state <- starbucks %>% 
  filter(Province=="MA")

# isolating just the 3 columns we're interested in-- type, lat, and lon
sb_loc <- select(sb_state, type, lat, lon)
dd_loc <- select(dd_state, type, lat, lon)

# joining the two data frames together
ddsb <- rbind(sb_loc, dd_loc)

# creating a coffee color palette

cof <- colorFactor(c("#ffa500", "#13ED3F"), domain=c("Dunkin Donuts", "Starbucks"))
# mapping based on type
m <- leaflet(ddsb) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-71.931180, 42.385453, zoom = 7) %>% 
  addCircleMarkers(~lon, ~lat, popup=ddsb$type, weight = 3, radius=4, 
                   color=~cof(type), stroke = F, fillOpacity = 0.5) 
m

m <- leaflet(ddsb)  %>% 
addProviderTiles(providers$CartoDB.DarkMatter) %>% 
setView(-71.931180, 42.385453, zoom = 7) %>% 
addCircleMarkers(~lon, ~lat, popup=ddsb$type, weight = 3, radius=4, 
color=~cof(type), stroke = F, fillOpacity = 0.5)  %>%
addLegend("bottomright", colors= c("#ffa500", "#13ED3F"), labels=c("Dunkin'", "Starbucks"), title="Coffee places") 

m
