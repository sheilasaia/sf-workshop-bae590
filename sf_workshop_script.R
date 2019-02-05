# exploring spatial data with sf workshop for bae 590

# last updated: 20190205
# script prepared by: sheila saia
# contact: ssaia at ncsu dot edu

# ---- Activity 1 ----

# let's get started using sf

# install sf package and tidyverse (3 ways)
# if you already have done this you don't need to do it again

# instalation method #1
# if you want the offical CRAN version of sf...
# install.packages("sf")
# install.packages("here")
# install.packages("tidyverse")
# install.packages("Cairo") # ONLY FOR WINDOWS USERS!

# instalation method #2
# you can also go to tools > install packages... on rstudio menu
# you'll need the sf, here, and tidyverse packages (Cairo too if you use Windows)

# instalation method #3
# if you want the development version of sf use...
# install.package("devtools")
# library(devtools)
# install_github("r-spatial/sf")
# install.packages("here") # normal installation
# install.packages("tidyverse") # normal
# install.packages("Cairo") # ONLY FOR WINDOWS USERS!

# load sf, here, and tidyverse libraries
library(here)
library(sf)
library(tidyverse)

# save page to your R project
project_path <- here::here()

# save path to your workhop_data directory
tabular_data_path <- paste0(project_path, "/workshop_data/tabular_data/")
spatial_data_path <- paste0(project_path, "/workshop_data/spatial_data/")
results_path <- paste0(project_path, "/results/")
# NOTE! if you have a mac you can use / but if you have a pc you need to switch /'s to \\

# import se state bounds
se_states_raw <- st_read(paste0(spatial_data_path,"southeast_state_bounds.shp"))

# use st_crs() to check CRS
st_crs(se_states_raw)
# can you find the proj4 string?
# what do we notice about the EPSG?
# this shp file was originally NAD83/Conus Albers
# what number EPSG would that be? (hint: look it up here http://epsg.io/)

# it has a projection but the EPSG is not showing up
se_states <- se_states_raw %>%
  st_set_crs(5070)
# NOTE: we get a warning here because we are setting the projection NOT projecting!

# use st_crs() again to check CRS
st_crs(se_states)
# looks better! we set the EPSG.

# import watershed bounds (for non-ref se plains) and set CRS
ws_bounds_seplains <- st_read(paste0(spatial_data_path, "bas_nonref_SEPlains.shp")) %>%
  st_set_crs(5070) %>%
  mutate(ws_id = row_number()) # gives watershed a unique number id (we need this for later)

# check watershed bounds CRS
st_crs(ws_bounds_seplains)

# import stream gages and define projection (for all US) and set CRS
gages <- st_read(paste0(spatial_data_path, "gagesII_9322_sept30_2011.shp")) %>%
  st_set_crs(5070)

# check gage locations CRS
st_crs(gages)

# import gage attribute data
basin_id_data <- read_csv(paste0(tabular_data_path, "conterm_basinid.txt"))
climate_data <- read_csv(paste0(tabular_data_path, "conterm_climate.txt"))
geology_data <- read_csv(paste0(tabular_data_path, "conterm_geology.txt"))
hydrology_data <- read_csv(paste0(tabular_data_path, "conterm_hydro.txt"))
topo_data <- read_csv(paste0(tabular_data_path, "conterm_topo.txt"))
# errors are ok here! it's R just telling you what it's doing.

# look at se_states
class(se_states)
# what type of data class is it?

# look at se_states frame a little more
glimpse(se_states)
# what do you notice that's different about it than a regular dataframe?
# hint: there's one column at the end in particular...

# a third way to look at se_states
attributes(se_states)


# ---- Activity 2 ----

# let's practice using different sf functions

# project to NAD83/UTM Zone 17N by transforming EPSG (looked EPSG up on epsg.io)
se_states_utm18n_epsg <- se_states %>%
  st_transform(crs = 26917)

# check CRS
st_crs(se_states_utm18n_epsg)

# project to NAD83/UTM Zone 17N by transforming proj4 (looked proj4 up on epsg.io)
utm18n_proj4 <- "+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
se_states_utm18n_proj4 <- se_states %>%
  st_transform(crs = utm18n_proj4)

# check CRS
st_crs(se_states_utm18n_proj4)
# you might want to set EPSG code here too...

# separate out geometry using st_geometry()
se_states_geom <- st_geometry(se_states)
names(se_states_geom) # this is NULL b/c we removed the attribute data
class(se_states_geom) # now we see it's sfc for simple features with only geometry (simple features collection of geometry)

# separate out attributes using st_set_geometry(NULL)
se_states_attribs <- se_states %>%
  st_set_geometry(NULL)
names(se_states_attribs) # this works like any data frame or tibble
head(se_states_attribs) # this works too
class(se_states_attribs) # we see it's just a data frame now, not an sf class

# plot all se states
setwd(results_path)
pdf("se_state_bounds.pdf",width=11,height=8.5)
plot(se_states_geom) # for now we'll just use base R ;)
dev.off()

# NOTE on plotting:
# througout this script we'll use pdf() because plotting spatial data with sf in the rstudio
# interactive plotting window can take a while this is likely because rstudio interactive
# plotting window uses an application that has trouble plotting lots of points 
# while exporting a pdf uses a different application that is more efficient computationally
# if you must plot in the rstudio interactive window, try using st_simplify() before plotting

# pick your state
my_state <- se_states %>%
  filter(NAME == "Virginia") # change this to your favorite (options are: NC, SC, VA, GA, FL)
# there's a funny coincendence that all sf tutorials use NC data from here (https://cran.r-project.org/web/packages/spdep/vignettes/sids.pdf) so feel free to go with this wave or not ;)

# save your geometry separately
my_state_geom <- st_geometry(my_state)

# plot your state
setwd(results_path)
pdf("my_state_bounds.pdf",width=11,height=8.5)
plot(my_state_geom) # for now we'll just use base R ;)
dev.off()

# find area of my_state_geom using st_area()
st_area(my_state_geom) # in square meters
as.numeric(st_area(my_state_geom)) / 1000^2 # in square km
as.numeric(st_area(my_state_geom)) * 0.00062137^2 # in square mi

# find centroid of my_state_geom using st_centroid()
st_centroid(my_state_geom)

# extra: can you plot the centroid of your state with that state's bounds?

# remove my_state from se_states using st_difference()
se_states_geom_without_my_state <- st_difference(se_states_geom, my_state_geom)

# plot se states without your state
setwd(results_path)
pdf("se_state_bounds_without_my_state.pdf",width=11,height=8.5)
plot(se_states_geom_without_my_state)
dev.off()

# some others!
# use st_join(<object you want as output>, <object info to join>) - spatial join (including attributes)
# st_intersects() - help id sf data that intersect, result is R list
# st_contains() - help id sf data within polygons, result is R list
# st_intersection() - clip sf data that intersect, results in sf object
# st_distance() - include point and spatial object and will calculate distance matrix from one feature to the other


# ---- Activity 3 ----

# let's try some sf operations with a real-world dataset

# plot my_state and se plains ws's together to take a look at our data together using geom_sf()
setwd(results_path)
pdf("se_ws_bounds_and_my_state.pdf",width=11,height=8.5)
# this time we'll use ggplot!
ggplot() + 
  geom_sf(data = ws_bounds_seplains, aes(fill = GAGE_ID)) +
  geom_sf(data = my_state_geom, alpha = 0.5) +
  theme_bw() +
  theme(legend.position="none") # this supresses the legend because the key is really big
dev.off()

# let's look at se plains ws's in more detail by clipping them to our state bounds using st_intersection()
my_state_ws <- st_intersection(ws_bounds_seplains, my_state_geom)
# first one will define dataframe of output
glimpse(my_state_ws)
# there are 90 observations here because the watersheds are overlapping...let's look into this a little more

# plot intersection, use alpha to show that they're overlapping
setwd(results_path)
pdf("ws_bounds_in_my_state.pdf",width=11,height=8.5)
ggplot() +
  geom_sf(data = my_state_geom) +
  geom_sf(data = my_state_ws, aes(fill = GAGE_ID), alpha = 0.5) +
  #geom_sf(data = my_state_ws[1:20,], aes(fill = GAGE_ID), alpha = 0.5) + # just look at first 20 gages
  theme_bw() #+
  #theme(legend.position="none")
dev.off()

# extra: how would we get the whole watershed and not just the part inside the state boundary (i'm stumped too!)?
# maybe use st_within() and some dplyr functions? it gives 1 for True (it's within) 0 for False...


# ---- Activity 4 ----

# let's see how sf works with the tidyverse

# select gages in your state using st_intersection()
my_gages_in_state <- st_intersection(gages, my_state_geom)
# first one will define dataframe of output

# plot watersheds and gages for your state
setwd(results_path)
pdf("ws_bounds_and_gages_in_my_state.pdf",width=11,height=8.5)
ggplot() +
  geom_sf(data = my_state_geom) +
  geom_sf(data = my_state_ws, aes(fill = GAGE_ID), alpha = 0.5) +
  geom_sf(data = my_gages_in_state, size = 2) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

# select gages inside the watersheds
my_gages_in_ws <- st_intersection(gages, st_geometry(my_state_ws))

# plot watersheds and gages for your state's watersheds
setwd(results_path)
pdf("select_gages.pdf",width=11,height=8.5)
ggplot() +
  geom_sf(data = my_state_geom) +
  geom_sf(data = my_state_ws, aes(fill = GAGE_ID), alpha = 0.5) +
  geom_sf(data = my_gages_in_ws, size = 2) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

# plot gages colored by reference or non-reference
# reference refers to 
setwd(results_path)
pdf("my_state_gage_types.pdf",width=11,height=8.5)
ggplot() +
  geom_sf(data = my_state_geom) +
  geom_sf(data = my_gages_in_state, size = 5, aes(color = CLASS)) + # reference vs non-reference
  theme_bw()
dev.off()
# reference = less to no human impact on streamflow (e.g., forested with no development)
# non-reference = human impact on streamflow (e.g., has an urban area or town)

# use dplry left_join() to join gage spatial data with gage attributes
my_gages_join <- my_gages_in_state %>%
  left_join(climate_data, by = "STAID") # you can also join geology_data or hydrology_data depending on what you prefer

# plot gages colored by reference or non-reference
setwd(results_path)
pdf("my_state_gage_climate.pdf",width=11,height=8.5)
ggplot() +
  geom_sf(data = my_state_geom) +
  geom_sf(data = my_gages_join, size = 5, aes(color = PPTAVG_BASIN)) + # average annual precipitation for the associated watershed in cm
  theme_bw()
dev.off()

# using gather and facet wrap to compare variables of interest
my_gages_sel <- my_gages_join %>%
  select(STAID, PPTAVG_BASIN, PPTAVG_SITE) %>%
  gather(PPTAVG_BASIN, PPTAVG_SITE, key = "SCALE", value = "PPTAVG") # saves geometry too!
# comparing average annual precipitation at the gage site to average annual precipitation at the associated watershed

# extra: what if we add in elevation? is there a visual spatial relationship between precipitation and elevation? (you'll need topo_data)

# plot PPTAVG_BASIN and PPTAVG_SITE together for comparison
setwd(results_path)
pdf("test.pdf",width=11,height=8.5)
ggplot() +
  geom_sf(data = my_state_geom) +
  geom_sf(data = my_gages_sel, size = 5, aes(color = PPTAVG)) +
  facet_wrap(~SCALE) +
  theme_bw()
dev.off()


# ---- unsolved mysteries... (please let me and your classmates know if you solve them!) ----

# using/learning sf is still a work in progress!

# some questions (i also don't know the answers to these!)
# 1. what is the difference between filter.sf() and filter()? why do we get an error when we try to use filter.sf() "count not find function filter.sf"
# 2. why do some projections not read in automatically and other's do (UTM does vs Albers doesn't)
# 3. how would we get the whole watershed inside the state and not just the watershed clipped to the state boundary?
# 4. < place holder for more you or i might think of! >
