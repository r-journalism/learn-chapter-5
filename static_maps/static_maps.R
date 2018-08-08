
# If you haven't installed ggplot2 or sf yet, uncomment and run the lines below
#install.packages("ggplot2")
#install.packages("sf")

library(ggplot2)
library(sf)

# If you're using a Mac, uncomment and run the lines below
#options(device = "X11") 
#X11.options(type = "cairo")

fifty_location <- "data/cb_2017_us_state_20m/cb_2017_us_state_20m.shp"
fifty_states <- st_read(fifty_location)

View(fifty_states)

ggplot(fifty_states) + geom_sf()

# If you don't have readr installed yet, uncomment and run the line below
#install.packages("readr")

library(readr)
populations <- read_csv("data/acs2016_1yr_B02001_04000US55.csv")

View(populations)

ncol(fifty_states)

library(dplyr)

fifty_states <- left_join(fifty_states, populations,
by=c("NAME"="name"))

ncol(fifty_states)

colnames(fifty_states)

forty_eight <- fifty_states %>% 
filter(NAME!="Hawaii" & NAME!="Alaska" & NAME!="Puerto Rico")


ggplot(forty_eight) +
  geom_sf(aes(fill=B02001001)) +
  scale_fill_distiller(direction=1, name="Population") +
  labs(title="Population of 48 states", caption="Source: US Census")

# If you don't have tigris installed yet, uncomment the line below and run
#install.packages("tigris")

library(tigris)

# set sf option

options(tigris_class = "sf")

tx <- counties("TX", cb=T)

# If cb is set to TRUE, download a generalized (1:500k) counties file. 
# Defaults to FALSE (the most detailed TIGER file).

# Excluding Non-Continguous states (sorry!)

ggplot(tx) + 
  geom_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Texas counties")


## Downloading Census data into R via API


# First, sign up for a [census key](https://api.census.gov/data/key_signup.html).

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="YOURKEYHERE")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")



# If you don't have censusapi installed yet, uncomment the line below and run
#install.packages("censusapi")

library(censusapi)

apis <- listCensusApis()
View(apis)

acs_vars <- listCensusMetadata(name="acs/acs5", type="variables", vintage=2016)

View(acs_vars)

tx_income <- getCensus(name = "acs/acs5", vintage = 2016, 
  vars = c("NAME", "B19013_001E", "B19013_001M"), 
  region = "county:*", regionin = "state:48")

head(tx_income)

# Can't join by NAME because tx_income data frame has "County, Texas" at the end
# We could gsub out the string but we'll join on where there's already a consistent variable, even though the names don't line up

tx4ever <- left_join(tx, tx_income, by=c("COUNTYFP"="county"))


ggplot(tx4ever) + 
  geom_sf(aes(fill=B19013_001E), color="white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title="2016 Median income in Texas counties", caption="Source: US Census/ACS5 2016")


# if you don't have tidycensus installed yet, uncomment and run the line below

#install.packages("tidycensus")
library(tidycensus)

# Pass it the census key you set up before

census_api_key("YOUR API KEY GOES HERE")


jobs <- c(labor_force = "B23025_005E", 
          unemployed = "B23025_002E")

jersey <- get_acs(geography="tract", year=2016, variables= jobs, county = "Hudson", 
                  state="NJ", geometry=T)

head(jersey)

library(tidyr)

jersey %>% 
  mutate(variable=case_when(
    variable=="B23025_005" ~ "Unemployed",
    variable=="B23025_002" ~ "Workforce")) %>%
  select(-moe) %>% 
  spread(variable, estimate) %>% 
  mutate(percent_unemployed=round(Unemployed/Workforce*100,2)) %>% 
  ggplot(aes(fill=percent_unemployed)) + 
  geom_sf(color="white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Reds", direction=1, name="Estimate") +
  labs(title="Percent unemployed in Jersey City", caption="Source: US Census/ACS5 2016") +
  NULL

## faceting

racevars <- c(White = "P0050003", 
              Black = "P0050004", 
              Asian = "P0050006", 
              Hispanic = "P0040003")

harris <- get_decennial(geography = "tract", variables = racevars, 
                        state = "TX", county = "Harris County", geometry = TRUE,
                        summary_var = "P0010001") 

head(harris)

# If you dont have the viridis package installed yet, uncomment and run the line below
#install.packages("viridis")

library(viridis)

harris %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis(direction=-1) +
  scale_color_viridis(direction=-1) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Racial geography of Harris County, Texas", caption="Source: US Census 2010")

### About Alaska and Hawaii


county_pov <- get_acs(geography = "county",
  variables = "B17001_002",
  summary_var = "B17001_001",
  geometry = TRUE,
  shift_geo = TRUE) %>% 
  mutate(pctpov = 100 * (estimate/summary_est))

ggplot(county_pov) +
  geom_sf(aes(fill = pctpov), color=NA) +
  coord_sf(datum=NA) +
  labs(title = "Percent of population in poverty by county",
  subtitle = "Alaska and Hawaii are shifted and not to scale",
  caption = "Source: ACS 5-year, 2016",
  fill = "% in poverty") +
  scale_fill_viridis(direction=-1)


