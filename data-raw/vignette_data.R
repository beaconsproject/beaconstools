## code to prepare `vignette data

# library(sf)
# library(dplyr)
# library(raster)
# 
# # load catchments from kba project (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
# catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.shp")
# 
# # load led dataset
# led <- raster("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/criteria/led/led_wwf9_Bprj_reclass.tif")
# 
# # load benchmarks table
# builder <- read_csv("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/wwf9/builder/output/catch_wwf9_i0/2021_11_12_1230_ROW_BLIT_i0_t100_COLUMN_All_Unique_BAs.csv")
# 
# # make a dummy reference area using lat long coords
# df <- data.frame(
#   lon = c(-88, -102, -102, -88),
#   lat = c(51.5, 51.5, 56.5, 56.5)
# )
# vignette_reference <- df %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON") %>%
#   st_transform(st_crs(catchments))
# 
# # clip led to ref_poly
# led_crop <- crop(led, vignette_reference)
# vignette_led <- mask(led_crop, vignette_reference)
# vignette_led[vignette_led == 128] <- NA
# 
# # extract catchments for full reference area
# vignette_catchments <- catchments %>%
#   st_intersects(., vignette_reference, sparse = FALSE)[1,] %>%
#   sf::st_snap(x = ., y = ., tolerance = 0.0001)
# 
# # Subset builder table
# benchmarks <- dissolve_catchments_from_table(catchments_sf = catchments, input_table = builder, out_feature_id = "network", dissolve_list = names(builder[2:ncol(builder)])) # make sf benchmarks
# benchmark_list <- benchmarks$network[as.data.frame(st_contains(vignette_reference, benchmarks))$col.id] # get list of benchmarks in ref area
# benchmark_list <- sample(benchmark_list, 100) # sample 100
# vignette_builder <- builder[colnames(builder) %in% benchmark_list] # subset builder table
# 
# 
# usethis::use_data(vignette_reference, overwrite = TRUE)
# usethis::use_data(vignette_led, overwrite = TRUE)
# usethis::use_data(vignette_builder, overwrite = TRUE)
# usethis::use_data(vignette_catchments, overwrite = TRUE)






## create same datasets as in builder package

library(sf)
library(dplyr)
library(devtools)
library(raster)

# load catchments from kba project (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/RENR491 Capstone 2022/gisdata/catchments/YRW_catch50K.shp")

vignette_catchments <- catchments %>%
  dplyr::filter(FDAHUC8 == "09EA") %>%
  dplyr::select(CATCHNUM, SKELUID, STRAHLER, ORDER1, ORDER2, ORDER3, BASIN, Area_Land, Area_Water, Area_Total, STRMLEN_1, FDAHUC8, ZONE, MDA, Isolated, intact) %>%
  sf::st_snap(x = ., y = ., tolerance = 0.1)

vignette_catchments$CATCHNUM <- as.integer(vignette_catchments$CATCHNUM)
vignette_catchments$SKELUID <- as.integer(vignette_catchments$SKELUID)
names(vignette_catchments)[names(vignette_catchments) == "STRMLEN_1"] <- "STRMLEN"

vignette_catchments$ZONE <- as.character(vignette_catchments$ZONE)
vignette_catchments$BASIN <- as.character(vignette_catchments$BASIN)
vignette_catchments$Isolated <- as.integer(vignette_catchments$Isolated)

usethis::use_data(vignette_catchments, overwrite = TRUE)


# get existing reserves in FDA 09EA
temp <- file.path(tempdir(), "CPCAD-BDCAPC_Dec2021.gdb.zip")
download.file("https://cws-scf.ca/CPCAD-BDCAPC_Dec2021.gdb.zip", temp) # unzip manually in temp file
reserves <- st_read(file.path(tempdir(), "CPCAD-BDCAPC_Dec2021.gdb/CPCAD-BDCAPC_Dec2021.gdb"), layer = "CPCAD_BDCAPC_Dec2021")

reserves <- st_transform(reserves, st_crs(vignette_catchments))
names(reserves)[names(reserves) == "Shape"] <- "geometry"
st_geometry(reserves)="geometry"

fda <- vignette_catchments %>%
  summarise(geometry = st_union(geometry))

reserves <- reserves[reserves$NAME_E == "Tombstone",]

reserves_fda <- reserves %>%
  st_intersection(fda) %>%
  st_cast("POLYGON") %>%
  select("NAME_E") %>%
  mutate(area_km2 = as.numeric(st_area(geometry))/1000000) %>%
  filter(area_km2 > 1)

reserves_fda$NAME_E <- c("Tombstone_1", "Tombstone_2", "Tombstone_3")
names(reserves_fda)[names(reserves_fda) == "NAME_E"] <- "reserve"
reserves_fda <- reserves_fda[c("reserve", "area_km2", "geometry")]
vignette_reserves <- reserves_fda
row.names(vignette_reserves) <- c("1","2","3")

usethis::use_data(vignette_reserves, overwrite = TRUE)


## Now make the benchmarks table used in the builder vignette
library(benchmarkbuilder)
nghbrs <- neighbours(vignette_catchments)
seed <- seeds(catchments_sf = vignette_catchments,
              filter_intactness_col = "intact", filter_intactness_threshold = 1,
              areatarget_value = 500000000)
benchmarks_tab <- builder(catchments_sf = vignette_catchments, seeds = seed, neighbours = nghbrs,
                          catchment_level_intactness = 0.8, benchmark_level_intactness = 0.95)
vignette_benchmark_tab <- benchmarks_tab
usethis::use_data(vignette_benchmark_tab)


# prep the CMI and NALC rasters
nalc <- raster("C:/Users/MAEDW7/Dropbox (BEACONs)/gisdata/landcover/NALC_2015_Canada_30m/canada_2015_v2/CAN_NALCMS_2015_v2_land_cover_30m/CAN_NALCMS_2015_v2_land_cover_30m_albers_nearest.tif")

# clip
nalc_crop <- crop(nalc, fda)
vignette_nalc <- mask(nalc_crop, fda)
vignette_nalc[vignette_nalc > 19] <- NA
usethis::use_data(vignette_nalc, overwrite = TRUE)
