#' Lake Edge Density.
#'
#' A subset of the BEACONs Lake Edge Density dataset for a sample reference area.
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
#' @format A sf object with 87 features and 14 fields.
#' @source \url{https://beaconsproject.ualberta.ca/}
"catchments_sample"