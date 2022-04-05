### group_benchmarks_using_grid ###
#
#' Group benchmarks based on overlap. 
#' 
#' Groups are assigned based on polygon centroid membership within 
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
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample,
#'   "network", 
#'   dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
#' group_benchmarks_using_grid(reserves, 10000)
group_benchmarks_using_grid <- function(benchmarks_sf, grid_size){
  
  sf::st_agr(benchmarks_sf) = "constant"
  
  # Make centroids - or pointOnPoly
  points_sf <- benchmarks_sf %>%
    sf::st_centroid()
  
  # Make grid covering points
  grid_sf <- sf::st_make_grid(points_sf, cellsize = c(grid_size, grid_size), what = 'polygons')
  grid_sf <- sf::st_sf(geometry = grid_sf, data.frame('grid_id' = 1:length(grid_sf)))
  
  # remove grid_id if it already exists
  if("grid_id" %in% colnames(points_sf)){
    points_sf <- points_sf %>%
      dplyr::select(-.data$grid_id)
  }
  
  # spatial join fishnet grid id to benchmarks
  points_sf <- points_sf %>%
    sf::st_join(grid_sf)
  
  # convert grid ids to ordered vector starting at 1
  mappings <- data.frame(grid_id = unique(points_sf$grid_id), new_val = 1:length(unique(points_sf$grid_id)))
  
  points_sf <- points_sf %>%
    dplyr::left_join(mappings, by = "grid_id")
  
  return(points_sf$new_val)
}

### group_networks_using_grid ###
#
#' Group networks based on overlap of their component benchmarks. 
#' 
#' Individual benchmarks are first grouped using [group_benchmarks_using_grid()]. Networks are then grouped 
#' based on the combination of their component benchmarks. Two networks with component benchmarks in matching groups will be grouped together.
#' 
#' e.g. for two networks PA1__PA2 and PB3__PB4, if PB1 and PB3 fall in the same grid cell, and PB2 and PB4 fall
#' in the same grid cell, networks will receive the same unique group id.
#'
#' @param network_list Vector of network names to group. Must be in the format x1__x2__x3 where x1, x2 and x3 are 
#' values in the \code{benchmarks_sf} \code{benchmark_id} column. Any number of x can be used (e.g. x1__x2, 
#' x1__x2__x3__x4 etc.).
#' @param benchmarks_sf sf object of benchmarks or reserve polygons.
#' @param benchmark_id unique identifier column in \code{benchmarks_sf}.
#' @param grid_size Numeric size of grid cells to create in units matching the crs of \code{benchmarks_sf}.
#'
#' @return Vector of group ids matching \code{network_list}.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' benchmarks <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   benchmark_table_sample,
#'   "network", 
#'   dissolve_list = colnames(benchmark_table_sample))
#' network_names <- gen_network_names(benchmarks$network, 2)
#' networks_sf <- benchmarks_to_networks(benchmarks, network_names)
#' 
#' group_networks_using_grid(networks_sf$network, benchmarks, "network", 10000)

group_networks_using_grid <- function(network_list, benchmarks_sf, benchmark_id, grid_size){
  
  # get all benchmark names, check they are all in benchmarks_sf, subset benchmarks_sf
  benchmark_names <- unique(unlist(sep_network_names(network_list)))
  
  if(all(benchmark_names %in% benchmarks_sf[[benchmark_id]])){
    
    benchmarks_sf <- benchmarks_sf %>%
      dplyr::filter(.data[[benchmark_id]] %in% benchmark_names)
  } else{
    stop(paste0("Benchmarks missing from benchmarks_sf: ", benchmark_names[!benchmark_names %in% benchmarks_sf[[benchmark_id]]]))
  }
  
  # group benchmarks
  benchmarks_sf$grid_id <- group_benchmarks_using_grid(benchmarks_sf, grid_size)
  
  # make tibble of network names
  tb <- dplyr::tibble(network_list = network_list)
  
  # calculate group_id combinations and add to tibble
  tb$grid_id <- sapply(network_list, function(x){
    nets <- sep_network_names(x)
    paste0(sort(sapply(nets, function(y){benchmarks_sf$grid_id[benchmarks_sf[[benchmark_id]] == y]})), collapse = "_")
  })
  
  # make table of mappings for unique grid code combinations
  mappings <- data.frame(grid_id = unique(tb$grid_id), new_val = 1:length(unique(tb$grid_id)))
  
  # join output grid ids to tb
  tb <- tb %>%
    dplyr::left_join(mappings, by = "grid_id")
  
  return(tb$new_val)
}