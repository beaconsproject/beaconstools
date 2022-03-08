#' A subset of the BEACONs Lake Edge Density dataset for a sample reference area.
#' 
#' A classified version of the original dataset that ranged from values of 0 to 1.48
#' km/km2. This version groups values into 30 interval classes, but only 15 classes
#' exist in the sample area.
#'
#' @format A RasterLayer with values between 1 and 15.
#' @source \url{https://beaconsproject.ualberta.ca/}
"led_sample"

#' Sample of catchments with LED areas in km2.
#'
#' A subset of the BEACONs catchments dataset intersecting the led_sample raster.
#' LED areas (in km2) have been added to the sample catchments using criteria_to_catchments().
#'
#' @format A sf object with 87 features and 16 fields.
#' @source \url{https://beaconsproject.ualberta.ca/}
"catchments_sample"

#' Sample benchmarks table.
#'
#' A tibble containing the name of three fictional benchmarks as the column names (PB_0001, PB_0002, PB_0003), 
#' and values representing the catchment CATCHNUMs making up each benchmark.
#' As per the BUILDER output table, NA is used to make even column lengths.
#'
#' @format A data.frame object with 3 columns and 24 rows.
#' @source \url{https://beaconsproject.ualberta.ca/}
"benchmark_table_sample"

#' Example reference area.
#'
#' Polygon of an example reference area that we want to be represented.
#'
#' @format A sf object with 1 feature.
#' @source \url{https://beaconsproject.ualberta.ca/}
"ref_poly"

#' Example stream network.
#'
#' An example Stream network file with stream segments grouped into BASINs. 
#'
#' @format A sf object with 2273 features.
#' @source \url{https://beaconsproject.ualberta.ca/}
"streams_sample"

#' Vignette reference area.
#'
#' Polygon of a larger example reference area for use in vignettes.
#'
#' @format A sf object with 1 feature.
#' @source \url{https://beaconsproject.ualberta.ca/}
"vignette_reference"

#' A subset of the BEACONs Lake Edge Density dataset for the vignette reference area.
#'
#' A classified version of the original dataset that ranged from values of 0 to 1.48
#' km/km2. This version groups values into 30 interval classes.
#'
#' @format A RasterLayer with values between 1 and 24.
#' @source \url{https://beaconsproject.ualberta.ca/}
"vignette_led"

#' Sample of catchments for the vignette reference area.
#'
#' A subset of the BEACONs catchments dataset intersecting the vignette reference area.
#'
#' @format A sf object with 18123 features and 1 field.
#' @source \url{https://beaconsproject.ualberta.ca/}
"vignette_catchments"

#' Sample benchmarks table containing 100 benchmarks from the vignette study region.
#'
#' A tibble containing column names of benchmark ids and rows of catchment CATCHNUM values 
#' found in the benchmark.
#'
#' @format A data.frame object with 100 columns and 827 rows.
#' @source \url{https://beaconsproject.ualberta.ca/}
"vignette_builder"