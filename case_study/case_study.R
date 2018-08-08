

# if you don't have any of these packages installed yet, uncomment and run the lines below
#install.packages("tidycensus", "ggplot2", "dplyr", "sf", "readr")

library(tidycensus)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)

stops <- read_csv("data/hamden_stops.csv")

View(stops)

stops <- filter(stops, InterventionLocationLatitude!=0)

# If you don't have tigris installed yet, uncomment the line below and run
#install.packages("tigris")

library(tigris)

# set sf option

options(tigris_class = "sf")

new_haven <- tracts(state="CT", county="New Haven", cb=T)

# If cb is set to TRUE, download a generalized (1:500k) counties file. Defaults to FALSE
# (the most detailed TIGER file).

ggplot(new_haven) + 
  geom_sf() +
  geom_point(data=stops, 
             aes(x=InterventionLocationLongitude, 
                 y=InterventionLocationLatitude), color="red") +
  #geom_point(data=stops_spatial, aes(x=lon, y=lat), color="blue") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Traffic stops in Hamden")

### Deeper analysis



### Heat map


ggplot() +
  geom_sf(data=new_haven, color = "black", fill=NA, size=0.5) +
  stat_density2d(data=stops, show.legend=F, 
  aes(x=InterventionLocationLongitude, y=InterventionLocationLatitude, 
  fill=..level.., alpha=..level..), geom="polygon", size=4, bins=10) +
  scale_fill_gradient(low="deepskyblue2", high="firebrick1", name="Distribution") +
  coord_sf(xlim=c(-73.067649, -72.743739), ylim=c(41.280972, 41.485011)) +
  labs(x=NULL, y=NULL, 
  title="Traffic stops distribution around Hamden",
  subtitle=NULL,
  caption="Source: data.ct.gov") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))



# Creating a race column
stops$race <- ifelse(stops$SubjectEthnicityCode=="H", "H", stops$SubjectRaceCode)
  stops <- stops %>% 
  mutate(race=case_when(
  race=="A" ~ "Asian",
  race=="B" ~ "Black",
  race=="H" ~ "Hispanic",
  race=="W" ~ "White"
  ))


  ggplot() +
  geom_sf(data=new_haven, color = "black", fill=NA, size=0.5) +
  stat_density2d(data=stops, show.legend=F, 
  aes(x=InterventionLocationLongitude, y=InterventionLocationLatitude, 
  fill=..level.., alpha=..level..), geom="polygon", size=4, bins=10) +
  scale_fill_gradient(low="deepskyblue2", high="firebrick1", name="Distribution") +
  coord_sf(xlim=c(-73.067649, -72.743739), ylim=c(41.280972, 41.485011)) +
  #This is the new line being added
  facet_wrap(~race) +
  labs(x=NULL, y=NULL, 
  title="Traffic stops distribution around Hamden by race",
  subtitle=NULL,
  caption="Source: data.ct.gov") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))


  
stops %>% 
  group_by(race) %>% 
  count()

### Points in a Polygon


stops_spatial <- stops %>% 
st_as_sf(coords=c("InterventionLocationLongitude", "InterventionLocationLatitude"), 
crs = "+proj=longlat") %>% 
st_transform(crs=st_crs(new_haven))

points_in <- st_join(new_haven, stops_spatial, left=T)

View(points_in)

by_tract <- points_in %>%
  filter(!is.na(X)) %>% 
  group_by(GEOID) %>%
  summarise(total=n())

head(by_tract)

# If you don't have viridis installed yet, uncomment and run the line below
#install.packages("viridis")

library(viridis)

ggplot(by_tract) +
  geom_sf(aes(fill = total), color=NA) +
  coord_sf(datum=NA) +
  labs(title = "Total traffic stops by Hamden police",
       subtitle = "In 2013 and 2014",
       caption = "Source: data.ct.gov",
       fill = "Total stops") +
  scale_fill_viridis(option="magma", direction=-1)


new_haven_towns <- county_subdivisions(state="CT", county="New Haven", cb=T)
hamden_town <- filter(new_haven_towns, NAME=="Hamden")

ggplot() +
  geom_sf(data=by_tract, aes(fill = total), color=NA) +
  geom_sf(data=hamden_town, fill=NA, color="black") +
  coord_sf(datum=NA) +
  labs(title = "Total traffic stops by Hamden police",
       subtitle = "In 2013 and 2014",
       caption = "Source: data.ct.gov",
       fill = "Total stops") +
  scale_fill_viridis(option="magma", direction=-1) +
  NULL

by_tract_race <- points_in %>%
  filter(!is.na(X)) %>% 
  group_by(GEOID, race) %>%
  summarise(total=n())

head(by_tract_race)

ggplot() +
  geom_sf(data=by_tract_race, aes(fill = total), color=NA) +
  geom_sf(data=hamden_town, fill=NA, color="black") +
  coord_sf(datum=NA) +
  labs(title = "Total traffic stops by Hamden police",
  subtitle = "In 2013 and 2014",
  caption = "Source: data.ct.gov",
  fill = "Total stops") +
  scale_fill_viridis(option="magma", direction=-1) +
  facet_wrap(~race)

by_tract_race_percent <- by_tract_race %>% 
  mutate(type=case_when(
  race=="White" ~ "White",
  TRUE ~ "Minority")) %>% 
  group_by(GEOID, type) %>% 
  summarize(total=sum(total)) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,2)) 

head(by_tract_race_percent)

ggplot() +
  geom_sf(data=by_tract_race_percent, aes(fill = percent), color=NA) +
  geom_sf(data=hamden_town, fill=NA, color="black") +
  coord_sf(datum=NA) +
  labs(title = "Total traffic stops by Hamden police",
  subtitle = "In 2013 and 2014",
  caption = "Source: data.ct.gov",
  fill = "Percent of all stops") +
  scale_fill_viridis(option="magma", direction=-1) +
  facet_wrap(~type)

#Don't forget to load your Census API key.


census_api_key("YOUR API KEY GOES HERE")


racevars <- c(Total_Pop = "B02001_001E", White_Pop = "B02001_002E")

hamden_pop <- get_acs(geography = "tract", variables = racevars, 
state = "CT", county = "New Haven County") 
head(hamden_pop)

library(tidyr)

hamden_pop_perc <- hamden_pop %>% 
  mutate(variable=case_when(
    variable=="B02001_001" ~ "Total",
    variable=="B02001_002" ~ "White")) %>% 
  # dropping Margin of Error-- I know this is not ideal but for this purpose, 
  # we'll get rid of it for now
  select(-moe) %>% 
  spread(variable, estimate) %>% 
  mutate(white_residents=round(White/Total*100,2), 
         minority_residents=100-white_residents)

head(hamden_pop_perc)

by_tract_race_percent_spread <- by_tract_race_percent %>% 
select(-total) %>% 
spread(type, percent) %>% 
rename(minority_stopped=Minority, white_stopped=White) %>% 
filter(!is.na(minority_stopped) & !is.na(white_stopped))

stops_population <- left_join(by_tract_race_percent_spread, hamden_pop_perc) 


# If you don't have scales installed yet, uncomment and run the line below
#install.packages("scales")

library(scales)

stops_population$gap <- (stops_population$minority_stopped - 
                           stops_population$minority_residents)/100

ggplot() +
  geom_sf(data=stops_population, aes(fill = gap), color="white", size=.25) +
  geom_sf(data=hamden_town, fill=NA, color="black") +
  coord_sf(datum=NA) +
  labs(title = "Hamden: Minority traffic stops versus population",
       subtitle = "In 2013 and 2014",
       caption = "Source: data.ct.gov") +
  scale_fill_distiller(type="seq", trans="reverse", palette = "PuOr", 
                       label=percent, breaks=pretty_breaks(n=10), name="Gap") +
  #continuous_scale(limits=c(-30, 30)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  NULL

### Add annotations

stops_population$gap <- (stops_population$minority_stopped - 
                           stops_population$minority_residents)/100

ggplot() +
  geom_sf(data=stops_population, aes(fill = gap), color="white", size=.25) +
  geom_sf(data=hamden_town, fill=NA, color="black") +
  coord_sf(datum=NA) +
  labs(title = "Hamden: Minority traffic stops versus population",
       subtitle = "In 2013 and 2014",
       caption = "Source: data.ct.gov") +
  scale_fill_distiller(type="seq", trans="reverse", palette = "PuOr", label=percent,
                       breaks=pretty_breaks(n=10), name="Gap") +
  #continuous_scale(limits=c(-30, 30)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  # NEW CODE HERE
  annotate("segment", x = -72.93, xend = -72.87, y = 41.325, yend = 41.325, 
           colour = "lightblue", size=.5) +
  annotate("point", x = -72.93, y = 41.325, colour = "lightblue", size = 2) +
  annotate("text", x = -72.85, y = 41.325, label = "New Haven", size=5, 
           colour="gray30") +
  annotate("segment", x = -72.89, xend = -72.86, y = 41.375, yend = 41.375, 
           colour = "lightblue", size=.5) +
  annotate("point", x = -72.89, y = 41.375, colour = "lightblue", size = 2) +
  annotate("text", x = -72.845, y = 41.375, label = "Hamden", size=5, 
           colour="gray30") +
  annotate("point", x = -72.83, y = 41.375, colour="white", size=.2) +
  NULL
