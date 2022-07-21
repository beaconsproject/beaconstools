## code to prepare `vignette data

## create same datasets as in builder package

library(sf)
library(dplyr)
library(devtools)
library(raster)

# load catchments from RENR folder (just using this dataset because its synced on my laptop, could also load full dataset from gisdata)
#catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/RENR491 Capstone 2022/gisdata/catchments/YRW_catch50K.shp")
catchments <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/gisdata/catchments/boreal_vPB23.shp")

vignette_catchments <- catchments %>%
  #dplyr::filter(FDAHUC8 == "09EA") %>%
  dplyr::filter(FDA_M == "09EA") %>%
  #dplyr::select(CATCHNUM, SKELUID, STRAHLER, ORDER1, ORDER2, ORDER3, BASIN, Area_Land, Area_Water, Area_Total, STRMLEN_1, FDAHUC8, ZONE, MDA, Isolated, intact) %>%
  dplyr::select(CATCHNUM, SKELUID, STRAHLER, ORDER1, ORDER2, ORDER3, BASIN, Area_land, Area_water, Area_total, length_m, FDA_M, MDAzone, Isolated, CA2010) %>%
  sf::st_snap(x = ., y = ., tolerance = 0.1)

# change names to match previously used catchments
names(vignette_catchments)[names(vignette_catchments) == "Area_land"] <- "Area_Land"
names(vignette_catchments)[names(vignette_catchments) == "Area_water"] <- "Area_Water"
names(vignette_catchments)[names(vignette_catchments) == "Area_total"] <- "Area_Total"
names(vignette_catchments)[names(vignette_catchments) == "FDA_M"] <- "FDA"
names(vignette_catchments)[names(vignette_catchments) == "length_m"] <- "STRMLEN"
names(vignette_catchments)[names(vignette_catchments) == "MDAzone"] <- "MDA"
names(vignette_catchments)[names(vignette_catchments) == "CA2010"] <- "intact"

vignette_catchments$CATCHNUM <- as.integer(vignette_catchments$CATCHNUM)
vignette_catchments$SKELUID <- as.integer(vignette_catchments$SKELUID)
vignette_catchments$ORDER2 <- as.numeric(vignette_catchments$ORDER2)


vignette_catchments$MDA <- as.character(vignette_catchments$MDA)
vignette_catchments$BASIN <- as.character(vignette_catchments$BASIN)
vignette_catchments$Isolated <- as.integer(vignette_catchments$Isolated)

usethis::use_data(vignette_catchments, overwrite = TRUE)


# Get the stream network in the fda
fda <- vignette_catchments %>%
  summarise(geometry = st_union(geometry))

streams <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/RENR491 Capstone 2022/gisdata/catchments/YRW_fdaHUC8_stream_50K.shp")
streams_intersect <- streams %>%
  st_intersects(fda) %>%
  as.data.frame()

streams$key <- 1:nrow(streams)
vignette_streams <- streams[streams$key %in% streams_intersect$row.id,]
vignette_streams <- vignette_streams[c("BASIN")]

usethis::use_data(vignette_streams, overwrite = TRUE)

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
vignette_existing_reserves <- reserves_fda
row.names(vignette_existing_reserves) <- c("1","2","3")

usethis::use_data(vignette_existing_reserves, overwrite = TRUE)


## Now make the benchmarks table used in the builder vignette
library(benchmarkbuilder)
nghbrs <- neighbours(vignette_catchments)
seed <- seeds(catchments_sf = vignette_catchments,
              filter_intactness_col = "intact", filter_intactness_threshold = 1,
              areatarget_value = 500000000)
benchmarks_tab <- builder(catchments_sf = vignette_catchments, seeds = seed, neighbours = nghbrs,
                          catchment_level_intactness = 0.8, benchmark_level_intactness = 0.95)
vignette_builder_tab <- benchmarks_tab
usethis::use_data(vignette_builder_tab, overwrite = TRUE)


# prep the NALC raster
nalc <- raster("C:/Users/MAEDW7/Dropbox (BEACONs)/gisdata/landcover/NALC_2015_Canada_30m/canada_2015_v2/CAN_NALCMS_2015_v2_land_cover_30m/CAN_NALCMS_2015_v2_land_cover_30m_albers_nearest.tif")

# clip
nalc_crop <- crop(nalc, fda)
vignette_nalc <- mask(nalc_crop, fda)
vignette_nalc[vignette_nalc > 19] <- NA
usethis::use_data(vignette_nalc, overwrite = TRUE)


# Get intactness map using IFl 2020
temp <- file.path("C:/Temp/", "ifl_2020.zip")
download.file("https://intactforests.org/shp/IFL_2020.zip", temp) # unzip manually in temp file
ifl <- st_read(file.path("C:/Temp/", "ifl_2020"), layer = "ifl_2020")
ifl <- st_transform(ifl, st_crs(catchments))

vignette_intact <- ifl %>%
  st_intersection(fda) %>%
  summarise(geometry = sf::st_union(.data$geometry))

usethis::use_data(vignette_intact, overwrite = TRUE)


# Adding LED raster from NWB
temp <- file.path("C:/Temp", "led.zip")
download.file("http://nwb.ualberta.ca/downloads/nwb_datasets/led.zip", temp) # unzip manually in temp file
# reproject in acrmap
led <- raster("C:/Temp/led/led250_albers.tif")

led_crop <- crop(led, fda)
vignette_led <- mask(led_crop, fda)
usethis::use_data(vignette_led, overwrite = TRUE)