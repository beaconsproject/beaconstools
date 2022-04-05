## code to prepare `vignette data

library(sf)
library(dplyr)
library(raster)

# load catchments from kba project (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.shp")

# load led dataset
led <- raster("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/criteria/led/led_wwf9_Bprj_reclass.tif")

# load benchmarks table
builder <- read_csv("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/wwf9/builder/output/catch_wwf9_i0/2021_11_12_1230_ROW_BLIT_i0_t100_COLUMN_All_Unique_BAs.csv")

# make a dummy reference area using lat long coords
df <- data.frame(
  lon = c(-88, -102, -102, -88),
  lat = c(51.5, 51.5, 56.5, 56.5)
)
vignette_reference <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  st_transform(st_crs(catchments))

# clip led to ref_poly
led_crop <- crop(led, vignette_reference)
vignette_led <- mask(led_crop, vignette_reference)
vignette_led[vignette_led == 128] <- NA

# extract catchments for full reference area
vignette_catchments <- catchments %>%
  st_intersects(., vignette_reference, sparse = FALSE)[1,] %>%
  sf::st_snap(x = ., y = ., tolerance = 0.0001)

# Subset builder table
benchmarks <- dissolve_catchments_from_table(catchments_sf = catchments, input_table = builder, out_feature_id = "network", dissolve_list = names(builder[2:ncol(builder)])) # make sf benchmarks
benchmark_list <- benchmarks$network[as.data.frame(st_contains(vignette_reference, benchmarks))$col.id] # get list of benchmarks in ref area
benchmark_list <- sample(benchmark_list, 100) # sample 100
vignette_builder <- builder[colnames(builder) %in% benchmark_list] # subset builder table


usethis::use_data(vignette_reference, overwrite = TRUE)
usethis::use_data(vignette_led, overwrite = TRUE)
usethis::use_data(vignette_builder, overwrite = TRUE)
usethis::use_data(vignette_catchments, overwrite = TRUE)
