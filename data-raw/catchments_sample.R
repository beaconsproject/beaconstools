## code to prepare `catchments_sample` dataset goes here

library(sf)
library(dplyr)
library(raster)
library(exactextractr)

# load catchments from kba project (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.shp")

# load led dataset
led <- raster("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/criteria/led/led_wwf9_Bprj_reclass.tif")

# make a dummy reference area using lat long coords
df <- data.frame(
  lon = c(-83.7, -78.9, -81.2, -86.3),
  lat = c(52.8, 49.5, 47.8, 51.1)
)
ref_poly <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  st_transform(st_crs(catchments))

# clip led to ref_poly
led_crop <- crop(led, ref_poly)
led_sample <- mask(led_crop, ref_poly)

usethis::use_data(ref_poly, overwrite = TRUE)
usethis::use_data(led_sample, overwrite = TRUE)

# make a subset of catchments to act as a dummy benchmark
sub_catchnums <- c(187888,187919,187927,187942,187948,187979,187996,188051,188057,188071,188091,188097,188105,188154,188278,191104,191141,191161,191332,191337,
  191369,191396,191450,191465,191525,191554,191560,191576,191614,191625,191635,191649,191654,191670,191696,191714,191824,191838,191890,191900,
  191951,191960,191982,191987,192002,192052,192061,192106,192130,192144,192348,192553,192941,193106,196520,197231,197397,198090,198355,198628,
  198632,199214,199219,199223,199226,199237,199243,199248,199251,199255,199260,199265,199292,199357,199362,199367,199372,199392,199401,199405,
  199658,199662,199667,199672,199703,199708,199717)

catchments_sample <- catchments %>%
  dplyr::filter(CATCHNUM %in% sub_catchnums) %>%
  dplyr::select(CATCHNUM) %>%
  sf::st_snap(x = ., y = ., tolerance = 0.0001)

# add led areas to catchment sample
catchments_sample <- criteria_to_catchments(catchments_sample, led_sample, "led", 1:15)

usethis::use_data(catchments_sample, overwrite = TRUE)


# Make sample benchmarks_table
benchmark_table_sample <- tibble(
  "PB_0001" = c(187888,187919,187948,187979,187996,191104,191141,191161,191332,191337,191369,191396,191525,191560,191635,196520, NA, NA, NA, NA, NA, NA, NA, NA ), # overlaps with PB_0003 but not PB_0002
  "PB_0002" = c(188154,188278,192553,192941,193106,197231,198355,198628,198632,199226,199255,199260,199265,199292,199362,199367,199372,199662,199667,199703,199708,199717, NA, NA), # overlaps with PB_0003 but not PB_0001
  "PB_0003" = c(187979,187996,188057,191396,191525,191576,191625,191649,191838,191890,191951,191960,191982,191987,192941,199226,199237,199243,199251,199255,199357,199362,199367,199372) # overlaps with both PB_0001 and PB_0002
  )
usethis::use_data(benchmark_table_sample, overwrite = TRUE)