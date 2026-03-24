# Sara Reathaford
# In-class Assignment 7
# Spatial Data I

install.packages("sf")
library(sf)
install.packages("spData")
library(spData)
library(dplyr)
library(tidyr)
library(ggplot2)

data(world)

# 1.1 Inspecting an sf object

# a) Load the world dataset and inspect its structure. Run class(world), names(world),
  # and nrow(world). In a comment, describe what makes an sf object different from a
  # regular R data frame. What is the geometry column, and how is it stored?

class(world)
# sf, tbl_df, tbl, data.frame
names(world)
nrow(world)
# 177

# An sf object is a regular data frame augmented with an extra geometry column (of class sfc)
  # that stores the spatial shapes (polygons, points, lines), while a regular R data frame uses
  # tabular data with vectors of the same length. The geometry column is "sticky" and persists 
  # during most data manipulation operations (like filtering or selecting columns), ensuring data
  # integrity for spatial analysis. 

# b) Check the coordinate reference system (CRS) with st crs(world). What EPSG code
  # does the dataset use? In a comment, explain what WGS84 means and why it is the
  # standard CRS for global geographic data.

st_crs(world)

# The dataset uses EPSG:4326 (WGS84 — World Geodetic System 1984). WGS84 is the global standard coordinate
  # system used by GPS and most web mapping tools. Coordinates are expressed in decimal degrees of longitude
  # (east-west) and latitude (north-south), making it suitable for global datasets where a common datum is needed
  # across all regions.

# c) Use st geometry type(world) and unique(st geometry type(world)) to inspect the
  # geometry type. In a comment, explain what a MULTIPOLYGON is and give two concrete examples of countries that 
  # would require multiple polygons to represent their territory.

unique(st_geometry_type(world))

# multipolygon, 18 levels, triangle


# d) Produce a quick map of GDP per capita using base R graphics:

pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

# display inline as well

plot(world["gdpPercap"], main = "GDP per capita by country")

# The map reveals a sharp global inequality pattern. Western and Northern Europe, 
  # North America, and Australia/New Zealand appear as the wealthiest regions 
  # (dark end of the scale). Sub-Saharan Africa and parts of South and Southeast 
  # Asia occupy the lowest end. East Asia shows intermediate-to-high values, reflecting 
  # rapid economic growth in countries such as South Korea and Japan.

#----------------------------------------------------------------------------------------------

# 1.2 Attribute Operation

# a) Using filter(), create a subset of world containing only African countries. Call it
  # africa. How many African countries are in the dataset? Plot africa["gdpPercap"]
  # using base graphics. In a comment, note whether the country count matches your
  # expectations.

africa = filter(world, continent == "Africa")
nrow(africa)
# 51

plot(africa["gdpPercap"], main = "GDP per capita -- Africa")

# The dataset contains 51 African countries. The UN recognizes 54 sovereign African states, 
  # so this count is slightly below expectations and likely reflects missing data or the exclusion 
  # of very small territories from the spData world polygon dataset.


# b) Add a new variable pop millions equal to population divided by 1,000,000 using
  # mutate(). Then compute the average GDP per capita by continent using group by()
  # and summarise():

world = world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent = world %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

print(gdp_by_continent)

# When summarise() is called on a grouped sf object, it unions the geometries within each group and 
  # retains the resulting geometry column. To obtain a plain data frame without spatial information, 
  # use st_drop_geometry() before or after the summary step. This avoids carrying unneeded geometry 
  # through purely tabular analyses.

# c) Sort the African countries by GDP per capita (descending) using arrange(). Print the
  # top 5 rows with name long and gdpPercap. Name the five countries in a comment.


africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))

# The five African countries with the highest GDP per capita in this dataset are shown above. 
  # Equatorial Guinea ranks high due to its oil revenues relative to a small population; Gabon 
  # and Libya are also oil-dependent economies;Botswana benefits from diamond exports and relatively 
  # strong institutions; the fifth position is typically taken by a North African economy (Mauritius 
  # or Algeria depending on the dataset vintage)

#-------------------------------------------------------------------------------------------------------

# 1.3 Simple Visualization with ggplot2

# a) Make a choropleth map of the world colored by gdpPercap:

ggplot(world) + geom_sf(aes(fill = gdpPercap)) + scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = "GDP per capita") + theme_void() + labs(title = "GDP per capita by country")

ggsave("world_gpd.pdf", width = 10, height = 5)

# The geographic pattern mirrors what the base-R map showed. Western Europe, North America, and Oceania
  # 5 stand out as the wealthiest cluster. East Asia shows a gradient from high (Japan, South Korea) to 
  # middle (China). Sub-Saharan Africa and South Asia concentrate the lowest values, with a few exceptions 
  # (e.g., Equatorial Guinea’s oil wealth).


# b) Make the same map restricted to the africa object. Use scale fill viridis c() with
  # option = "magma" and save as africa gdp.pdf. Describe the variation in GDP per
  # capita across African countries.

ggplot(africa) + geom_sf(aes(fill = gdpPercap)) + scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "GDP per capita") + theme_void() + labs(title = "GDP per capita -- Africa")

ggsave("africa_gdp.pdf", width = 7, height = 6)


# c) Improve the Africa map by adding white country borders: modify geom sf() to include 
  # color = "white" and linewidth = 0.3. Save as africa gdp borders.pdf. In a
  # comment, explain how the border layer improves readability.

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")

ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

# Adding white country borders significantly improves readability, especially for smaller countries where adjacent fill
  # colours alone make it hard to distinguish units. The thin white lines demarcate each country without competing
  # visually with the fill scale, making it easier to identify specific countries of interest and to compare neighbours.









