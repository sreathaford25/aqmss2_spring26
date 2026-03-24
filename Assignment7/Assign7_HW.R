# Sara Reathaford
# Assignment 7 Homework
# Point Data and Spatial Joins

library(spData)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

events = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/spatial/conflict_events.csv")

# 2.1 Converting tabular data to sf

# a) Convert the events data frame to an sf object. Hint: use st as sf(), specifying the
  # coordinate columns with the coords argument and the CRS with crs = 4326. Run
  # class() and st crs() on the result to verify it worked. In a comment, explain what
  # st as sf() does: what does the coords argument specify, and what does crs = 4326 mean?

events_sf <- st_as_sf(events, coords = c("longitude", "latitude"), crs = 4326)

class(events_sf)
st_crs(events_sf)

# st_as_sf() converts a data frame into a 'sf' object, allowing spatial operations. 
# the coords argument specifies which columns in the data frame hold the coordinates.
# crs = 4326 sets the Coordinate Reference System (CRS) for the data. the 4326 is the EPSG
  # code for WGS84, which is the standard geographic coordinate system (lat/long) used by GPS 
  # systems and mapping services. 

# b) How many events are in the dataset? Use nrow() and table(events sf$event type)
  # to show the count by event type. In a comment, which event type is most common?
  
nrow(events_sf)  
# 68354 events

table(events_sf$event_type)
# non-state = 10418
# one-sided = 24449
# state-based = 33487
  
# The state-based event is the most common. 

# c) Make a map of conflict events overlaid on the world polygon. Use ggplot() with
  # two geom sf() layers: the first for the world polygons (as a grey background) and the
  # second for events sf colored by event type. Save it with ggsave(). In a comment, 
  # describe the geographic pattern. In which regions are conflict events most concentrated?

data(world)  

ggplot() + geom_sf(data = world, fill = "grey", color = "white") + geom_sf(data = events_sf, aes(color = event_type)) + theme_minimal() + labs(title = "Global Conflict Events") 

ggsave("events_map.png")

# Conflict events are concentrated in Africa


#-------------------------------------------------------------------------------------------------------

# 2.2 Spatial Join: events to countries

# a) Use st join() to assign country attributes (e.g. name long, continent, gdpPercap)
  # from the world polygon to each conflict event. Before joining, verify that both objects
  # share the same CRS. Run nrow() on the result and verify it equals nrow(events sf).
  # In a comment, explain what st join() is doing: how does it determine which country polygon each event point falls within? Why is checking the CRS before joining
  # important?

st_crs(world) == st_crs(events_sf)
# TRUE

events_joined <- st_join(events_sf, world)
nrow(events_joined) == nrow(events)
# TRUE

# the st_join merges two datasets and assigns each point by using spatial location to match data.
  # Matching CRS is essential, otherwise the coordinates won't align correctly.

#b) Some events may not match any country polygon (e.g., events at sea, on islands, or
  # exactly on a border). Check with sum(is.na(events joined$name long)). 
  # What fraction of events has no matching country? In a comment, list two possible reasons why
  # a point might not match any polygon.

sum(is.na(events_joined$name_long))
# 1576

sum(is.na(events_joined$name_long)) / nrow(events_joined)
# 0.023 --> about 2.3% of the conflicts did not match any country polygon during the 
  # spatial join. This could happen because the points may fall in the ocean like an
  # island or be located along aborder, so the coordinates are not precise. 


# c) Count the number of events and total fatalities per country. Hint: filter out events
  # with no matching country, then use group by() and summarise() with n() and sum().
  # Arrange by descending event count and print the top 10 (use st drop geometry() to 
  # get a clean table). In a comment, are the results consistent with your knowledge of
  # contemporary armed conflicts?
  
country_stats <- events_joined %>% filter(!is.na(name_long)) %>% group_by(name_long) %>% summarize(n_events = n(), total_fatalities = sum(fatalities, na.rm = TRUE)) %>% arrange(desc(n_events)) %>% st_drop_geometry()

head(country_stats, 10)  
  
# The top 10 countries with highest number of events are the DR of Congo, Nigeria, Somalia, Ethiopia,
  # Algeria, Sudan, Burundi, Mali, Rwanda, and South Africa. As far as I am aware, these results are
  # consistent with contemporary armed conflicts because of the constant civil unrest.

#-------------------------------------------------------------------------------------------------------------


# 2.3 Choropleth of Conflict Intensity

# a) Join the event counts back to the world polygon data. Hint: first use st drop geometry()
  # on the event counts (since it is still an sf object), then use left join() to merge by
  # country name. Replace NA values with 0 for countries with no events (see replace na()
  # from tidyr). Verify that the row count matches nrow(world).

country_stats_df <- country_stats

# merge counts
world_events <- world %>% left_join(country_stats_df, by = "name_long") %>% mutate(n_events = replace_na(n_events, 0))

nrow(world_events)
# 177

nrow(world_events) == nrow(world)
# TRUE

# b) Make a choropleth map of conflict event counts by country using geom sf() with
  # n events as the fill variable. Use scale fill distiller() with the "Reds" palette.
  # Save with ggsave(). In a comment, describe the map. Does the geographic pattern
  # match the event-level map from question 2.1c?

ggplot(world_events) + geom_sf(aes(fill = n_events)) + scale_fill_distiller(palette = "Reds")+ theme_void() + labs(title = "Conflict Event Counts by Country", fill = "Number of Events")

ggsave("choropletmap.png")

# The high conflict intensity appears in Africa, which matches the earlier event-level map. 
  # In 2.1c there are individual points used for each conflict, but in this map, the points are 
  # used to create a national total. 


# c) Make a second map using log-transformed counts: use log1p(n events) as the fill
  # variable (so countries with zero events are handled). Use scale fill distiller()
  # with palette = "YlOrRd", direction = 1, and name = "Log(events+1)". Save as
  # conflict log map.pdf. In a comment, explain why the log transformation is useful
  # and what it reveals that the raw count map did not.

ggplot(world_events) + geom_sf(aes(fill = log1p(n_events))) + scale_fill_distiller(palette = "Y10rRd", direction = 1, name = "Log(events+1") + theme_void() + labs("Conflict Log Map")

ggsave("conflict_log_map.pdf")

# The log transformation reduces skewness and reveals variation among countries with lower
  # event counts that are not visible in the raw map. 

#-------------------------------------------------------------------------------------------------------------

# 2.5 Discussion

# a) Discuss one limitation of the spatial join approach used in this assignment. 
  # For example: what happens to events that fall exactly on the border between two countries?
  # How might you handle events that fall just outside a polygon due to small coordinate
  # imprecisions?

# One limitation of spatial joins is figuring out what to do with points on borders. These points
  # may be assigned arbitrarily or not matched it all. Small coordinate errors can also place
  # points just outside polygons, so I could solve this issue by adding join=st_within the code to
  # place them inside the polygon. 


# b) What is the difference between st join() and left join()? What information does
  # each use to match rows, and when would you prefer one over the other?

# st_join() matches observations based on spatial relationships (in or out of polygon), using
  # geometric info. The left_join() matches rows based on shared attribute values and ignores
  # spatial location. st_join() is preferred for geographic matching, while left_join() can be
  # used to merge data in tables after spatial relationships are calculated. 











