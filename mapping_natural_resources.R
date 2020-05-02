### Get the preliminary stuff ready

# load packages
library(rio)
library(tidyverse)
library(googlesheets4)
library(labelled)
library(data.table)
library(varhandle)
library(ggrepel)
library(geosphere) 
library(rgeos)
library(mapview) 
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
library(rnaturalearthhires) # devtools::install_github("ropensci/rnaturalearthhires")
library(raster)
library(sp)
library(sf)
library(Imap) # nice mapping/color functions


### Getting the World Shapefile ready for checking the map

# getting world map shapefile
world_borders <- st_read("World_Borders.shp", stringsAsFactors=FALSE)

# transform it to WGS84 coordinate system
borders <- st_transform(world_borders, "+proj=longlat +ellps=WGS84 +datum=WGS84")
rm(world_borders)

### get the natural resource data ready

# load the data
orig_nr = import("v10almost_newer_natural_resources.dta")

# collapse the data by latitude/longitude combo
# we are working with a panel dataset

orig_nr$lat_long = orig_nr$latitude + orig_nr$longitude # create fake variable to summarize over
nr = orig_nr %>%
  group_by(country,continent, resource, latitude, longitude) %>%
  summarize(lat_long = mean(lat_long, na.rm=TRUE)) %>% 
  dplyr::select(-c(lat_long))

### transform the natural resources dataset to sf format

# drop rows with missing data that we can't map
nr <- na.omit(subset(nr, select=c("latitude","longitude", "resource", "country", "continent")))

# convert the dataset to sf format so we can map it
# specifically, it converts latitude and longitude to a geometry variable
# the geometry variable basically is vector/point shapes by latitude/longitude
nr_sf <- st_as_sf(nr, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# rename
setnames(nr_sf, "resource", "Resource")

# put everything that is not gold, oil or diamond as "other"
final = transform(nr_sf,
          Resource = factor(replace(as.character(Resource), 
                              list = !Resource %in% c("oil","gold", "diamond"),
                              values = "other")))
final = st_sf(final)

#################################################
# Mapping Point Data
################################################

## World 

# getting world map from natural earth packages
world <- ne_countries(scale = "large", returnclass = "sf")

# World map (basic)
world_basic = ggplot() + geom_sf(data = world) + geom_sf(data = final)
world_basic

# World map (with legend, theme change, and shapes for resources)
world_all = ggplot() +
  geom_sf(data = world) +
  geom_sf(data = final, aes(shape=Resource)) +
  theme_void() +
  scale_shape_manual(values= c("gold" = 11, 
                               "diamond" = 18,
                               "oil" = 10,
                               "other" = 20)) +
  theme(legend.position = "right")
print(world_all)
ggsave(world_all, filename = "world_map.png", width = 6.5, height = 6)

## Africa

# getting a map of Africa from natural earth packages 
africa <- ne_countries(continent = 'africa', scale = "large", returnclass = "sf")

# subset to only get African natural resource data
africa_data = subset(final, continent=="africa")

# make the Africa map (with legend, theme change, and shapes for resources)
africa_map = ggplot() +
  geom_sf(data = africa) +
  geom_sf(data = africa_data, aes(shape=Resource)) +
  theme_void() +
  scale_shape_manual(values= c("gold" = 11, 
                       "diamond" = 5,
                       "oil" = 10,
                       "other" = 20)) +
  theme(legend.position = "right")
print(africa_map)
ggsave(africa_map, filename = "africa_map.png", width = 6.5, height = 6)

## Colombia

# get the map of Colombia
colombia <- ne_countries(country = 'colombia', scale = "large", returnclass = "sf")

# put everything that is not gold, oil or emerald as "other"
final2 = transform(nr_sf,
                  Resource = factor(replace(as.character(Resource), 
                                            list = !Resource %in% c("oil","gold", "emerald"),
                                          values = "other")))
final2 = st_sf(final2)

# subset to only get colombia data
colombia_data <- subset(final2, country=="colombia")

# map
ggplot() +
  geom_sf(data = colombia) +
  geom_sf(data = colombia_data, aes(color=Resource)) +
  theme_void() +
  scale_color_manual(values= c("gold" = "gold", 
                               "emerald" = "green",
                               "oil" = "black",
                               "other" = "red")) +
  theme(legend.position = "right")

## Interactive point mapping with mapview
red <- colorRampPalette(c("red"))
gray <- colorRampPalette(c("gray"))
mapview(colombia, col.regions = gray(100)) + mapview(colombia_data, col.regions = red(100))

# let's remove old objects to speed everything up
rm(nr,nr_sf,africa,colombia,colombia_data,final,final2,world_all,world_basic, red,gray)

#############################################################################
# Mapping Polygons (Filling in states by world values)
#############################################################################

# drop the NAs for selected variables
poly_nr_world <- na.omit(subset(orig_nr, select=c("latitude","longitude", "resource", "country", "continent", "log_wb_val")))

# convert the poly_sf dataset to sf format
poly_nr_sf <- st_as_sf(poly_nr_world, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

### With spatial join (based on latitude/longitude)

# spatial join the points to the country polygons
# note: largest= TRUE to ensure only one classification with largest overlap
# note: left =TRUE to have left join
joined_df = st_join(borders, poly_nr_sf,
                    join=st_intersects,
                    largest=TRUE, 
                    left=TRUE)

# World map with colored in states by value (world market prices)
ggplot(data = borders) +
  geom_sf(data=joined_df, aes(fill = log_wb_val)) 


### Without spatial join (merging 2 sf data frames)
library(stringr)

## get country code variable for merging

# put country into proper names 
# (they're currently in lower case)
orig_nr$country = str_to_title(orig_nr$country)

# check to ensure that there is no weird capitalization
table(orig_nr$country)

# make fixes
orig_nr$country[orig_nr$country =="Democratic Republic Of Congo"] = "Democratic Republic of Congo"
orig_nr$country[orig_nr$country =="Republic Of Congo"] = "Republic of Congo"
orig_nr$country[orig_nr$country =="Myanmar (Burma)"] = "Myanmar"
orig_nr$country[orig_nr$country =="Swaziland (Eswatini)"] = "Swaziland"

# get the ISO3 country code from the country code package
library(countrycode)
orig_nr$ISO3 = countrycode(sourcevar = orig_nr$country, 
                                     origin = "country.name", 
                                     destination = "iso3c",
                                     warn = TRUE)
# fix one that didn't go through
orig_nr$ISO3[orig_nr$country=="Kazahkstan"] = "KAZ"

# subset to make the dataset smaller
# otherwise, it takes up too much memory 
orig_nr_small <- na.omit(subset(orig_nr, select=c("latitude","longitude", "resource", "country", "continent", "log_wb_val", "ISO3")))

# now merge the natural resource data over to country polygons
merged_data = left_join(borders, orig_nr_small, by=c("ISO3"))

# now, let's get the map from China
south_america <- ne_countries(continent = 'south america', scale = "large", returnclass = "sf")

# subset the natural resources data by asia (to speed up mapping)
sa_nr <- subset(merged_data, country=="Chile" | 
                  country=="Peru" | country=="Argentina" | 
                  country=="Bolivia" | country=="Brazil" | country=="Suriname" |
                country=="Guyana"| country=="Ecuador" |country=="Venezuela" |
                country=="Paraguay" | country=="Uruguay" |country=="Colombia" )


ggplot(data = south_america) +
  geom_sf(data = sa_nr, aes(fill=log_wb_val)) +
  theme_void() +
  labs(fill='Resource Value')

ggplot() +
  geom_sf(data = south_america) +
  geom_sf(data = sa_nr, aes(fill=log_wb_val)) 






