benchmarks_sf <- st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/RENR491 Capstone 2022/gisdata/benchmarks/pb_i80_t100_zone1.shp")
grid_size <- 10*1000

### group_benchmarks_using_grid ###
#
#' Group benchmarks based on overlap. Groups are assigned based on polygon centroid membership within 
#' an intersecting grid. Centroids in the same grid cell are assigned the same group id.
#'
#' @param benchmarks_sf sf object of benchmarks or reserve polygons.
#' @param grid_size Numeric size of grid cells to create in units matching the crs of \code{benchmarks_sf}.
#'
#' @return Vector of group ids matching the input polygons.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' reserves <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002", "PB_0003"))
#' group_benchmarks_using_grid(reserves, 10000)
group_benchmarks_using_grid <- function(benchmarks_sf, grid_size){
  
  sf::st_agr(benchmarks_sf) = "constant"
  
  # Make centroids - or pointOnPoly
  points_sf <- benchmarks_sf %>%
    sf::st_centroid()
  
  # Make grid covering points
  grid_sf <- sf::st_make_grid(points_sf, cellsize = c(grid_size, grid_size), what = 'polygons') %>% 
    sf::st_sf(geometry = ., data.frame('grid_id' = 1:length(.)))
  
  # spatial join fishnet grid id to benchmarks
  points_sf <- points_sf %>%
    dplyr::select(-grid_id) %>%
    sf::st_join(grid_sf)
  
  # convert grid ids to ordered vector starting at 1
  mappings <- data.frame(grid_id = unique(points_sf$grid_id), new_val = 1:length(unique(points_sf$grid_id)))
  
  points_sf <- points_sf %>%
    dplyr::left_join(mappings, by = "grid_id")
  
  return(points_sf$new_val)
}