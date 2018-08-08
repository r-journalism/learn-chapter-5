
# If you don't have the following packages installed yet, uncomment and run the lines below
#install.packages("tigris")
#install.packages("dplyr")
#install.packages("leaflet")

library(tigris)
library(dplyr)
library(leaflet)

# Downloading the shapefiles for states at the lowest resolution
states <- states(cb=T)

states %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)

starbucks <- read.csv("data/starbucks.csv", stringsAsFactors=F)

# First, we'll use dplyr to summarize the data
# count by state
sb_state <- starbucks %>%
  group_by(Province) %>%
  summarize(total=n()) %>% 
  # Some quick adjustments to the the dataframe to clean up names
  mutate(type = "Starbucks") %>%  
  rename(state=Province)

# Now we use the Tigris function geo_join to bring together 
# the states shapefile and the sb_states dataframe -- STUSPS and state 
# are the two columns they'll be joined by

states_merged_sb <- geo_join(states, sb_state, "STUSPS", "state")

# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Greens", domain=states_merged_sb$total)

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because 
# we're dealing with a SpatialPolygonsDataFrame and 
# not a normal data frame, thus filter() wouldn't work

states_merged_sb <- subset(states_merged_sb, !is.na(total))

# Setting up the pop up text
popup_sb <- paste0("Total: ", as.character(states_merged_sb$total))

# Mapping it with the new tiles CartoDB.Positron
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_sb , 
  fillColor = ~pal(states_merged_sb$total), 
  fillOpacity = 0.7, 
  weight = 0.2, 
  smoothFactor = 0.2, 
  popup = ~popup_sb) %>%
  addLegend(pal = pal, 
  values = states_merged_sb$total, 
  position = "bottomright", 
  title = "Starbucks")

# If you don't have censusapi installed yet, uncomment the line below and run
#install.packages("censusapi")

library(censusapi)


#Don't forget your [census key](https://api.census.gov/data/key_signup.html).


# Add key to .Renviron
Sys.setenv(CENSUS_KEY="YOURKEYHERE")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")
```


# Alright, getting total population by state from the API
state_pop <-  getCensus(name="acs5", 
vintage=2015,
key=census_key, 
vars=c("NAME", "B01003_001E"), 
region="state:*")

head(state_pop)

# Cleaning up the column names
colnames(state_pop) <- c("state_id", "NAME", "population")
state_pop$state_id <- as.numeric(state_pop$state_id)
# Hm, data comes in numbers of fully spelled out, not abbreviations

# Did you know R has its own built in list of State names and State abbreviations?
# Just pull it in this way to create a dataframe for reference

state_off <- data.frame(state.abb, state.name)
head(state_off)


# So I needed to create the dataframe above because the Census API data 
# gave me states with full names while the Starbucks data came with abbreviated state names
# So I needed a relationship dataframe so I could join the two

# Cleaning up the names for easier joining
colnames(state_off) <- c("state", "NAME")

# Joining state population dataframe to relationship file
state_pop <- left_join(state_pop, state_off)

# The relationship dataframe didnt have DC or Puerto Rico, so I'm manually putting those in
state_pop$state <- ifelse(state_pop$NAME=="District of Columbia", "DC", as.character(state_pop$state))
state_pop$state <- ifelse(state_pop$NAME=="Puerto Rico", "PR", as.character(state_pop$state))

# Joining Starbucks dataframe to adjusted state population dataframe
sb_state_pop <- left_join(sb_state, state_pop)

# Calculating per Starbucks stores 100,000 residents and rounding to 2 digits
sb_state_pop$per_capita <- round(sb_state_pop$total/sb_state_pop$population*100000,2)

# Eliminating rows with NA
sb_state_pop <- filter(sb_state_pop, !is.na(per_capita))
head(sb_state_pop)

states_merged_sb_pc <- geo_join(states, sb_state_pop, "STUSPS", "state")

pal_sb <- colorNumeric("Greens", domain=states_merged_sb_pc$per_capita)
states_merged_sb_pc <- subset(states_merged_sb_pc, !is.na(per_capita))

# Here's the pop up
popup_sb <- paste0("<strong>", states_merged_sb_pc$NAME, 
                   "</strong><br />Total: ", states_merged_sb_pc$total,
                   "<br />Per capita: ", 
                   as.character(states_merged_sb_pc$per_capita))
head(popup_sb)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_sb_pc , 
              fillColor = ~pal_sb(states_merged_sb_pc$per_capita), 
              fillOpacity = 0.9, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_sb) %>%
  addLegend(pal = pal_sb, 
            values = states_merged_sb_pc$per_capita, 
            position = "bottomright", 
            title = "Starbucks<br />per 100,000<br/>residents")

  
## Final map II

popup_sb <- paste0("Per capita: ", as.character(states_merged_sb_pc$per_capita))

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_sb_pc , 
  fillColor = ~pal_sb(states_merged_sb_pc$per_capita), 
  fillOpacity = 0.9, 
  weight = 0.2, 
  smoothFactor = 0.2,
  highlight = highlightOptions(
  weight = 5,
  color = "#666",
  dashArray = "",
  fillOpacity = 0.7,
  bringToFront = TRUE),
  label=popup_sb,
  labelOptions = labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "15px",
  direction = "auto")) %>%
  addLegend(pal = pal_sb, 
  values = states_merged_sb_pc$per_capita, 
  position = "bottomright", 
  title = "Starbucks<br />per 100,000<br/>residents")



