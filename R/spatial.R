### catchments_to_benchmarks ###
#
#' Dissolve a set of catchments to create benchmark polygons.
#'
#' Takes a list of catchments defining a benchmark or a network of multiple benchmarks, and dissolves the 
#' catchments into a single polygon feature.
#'
#' @param benchmark_table Data frame where columns are benchmark names and rows are catchments making up the benchmark. e.g. the "COLUMN_All_Unique_BAs" table 
#'   output by BUILDER.
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM 
#' @param network_list Vector of networks to build - usually just the list of benchmarks in the benchmark_table (e.g. PB_0001), but can also be combinations of 
#'   benchmarks (e.g. PB_0001__PB_0002) made using \code{gen_network_names()} (usually it is more efficient to build individual benchmarks then merge them into
#'   networks using \code{benchmarks_to_networks()}).
#'
#' @return A sf object of networks
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
# need to load sf in the example otherwise filtering an sf object causes an error. Could also solve this by putting example inside 
#' library(sf)
#' 
#' # Individual benchmarks
#' benchmarks <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002"))
#' plot(benchmarks)
#' 
#' # Benchmarks combined into a network
#' network <- catchments_to_benchmarks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001__PB_0002"))
#' plot(network)
catchments_to_benchmarks <- function(benchmark_table, catchments_sf, network_list){
  
  catchments_sf <- check_catchnum(catchments_sf) # check for CATCHNUM and make character
  
  saveCount <- 1
  for(net in network_list){
    
    # separate names if network
    nets <- sep_network_name(net)
    
    # get list of catchments
    net_catchments_list <- get_catch_list(nets, benchmark_table)
    
    # subset
    net_catchments <- catchments_sf %>%
      dplyr::filter(CATCHNUM %in% net_catchments_list)
    
    # dissolve
    net_catchments_sfc <- net_catchments %>%
      #sf::st_snap(x = ., y = ., tolerance = 0.0001) %>% # st_snap is very slow. Run st_snap on provided catchments instead.
      sf::st_union()
    net_catchments_diss <- sf::st_sf(network = net, geometry = net_catchments_sfc)
    
    # append to df
    if(saveCount == 1){
      out_sf <- net_catchments_diss
      saveCount <- saveCount + 1
    } else{
      out_sf <- rbind(out_sf, net_catchments_diss)
    }
  }
  return(out_sf)
}

### append_reserve ###
#
#' Adds a polygon to a sf object.
#'
#' Used to add reserves that are not output from BUILDER to a sf object of benchmarks.
#' For example, adding an existing protected area (PA) to an sf object of benchmarks. This allows
#' networks of combined benchmarks and existing PAs to be built using \code{\link{benchmarks_to_networks()}}.
#'
#' @param benchmarks_sf sf object with unique id column named \emph{network}, typically the output from
#'  \code{\link{catchments_to_benchmarks()}}.
#' @param add_reserve sf object to add as a single additional reserve to benchmarks_sf. All features will
#' be dissolved into a single POLYGON or MULTIPOLYGON feature to append to benchmarks_sf. If multiple add_reserve
#' object are required, add them using multiple calls to \code{\link{append_reserve}}.
#' @param reserve_name String that will become the reserve name in the benchmarks_sf \emph{network} column.
#'
#' @return sf object matching benchmarks_sf
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
#' pa_1 <- data.frame(lon = c(-85, -82.5, -83), lat = c(51, 51.5, 50.5)) %>%
#'           sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#'           dplyr::summarise(geometry = st_combine(geometry)) %>%
#'           sf::st_cast("POLYGON") %>%
#'           sf::st_transform(st_crs(benchmarks)) %>%
#'           dplyr::mutate(pa_name="a protected area", ha=9999, class="provincial")
#' append_reserve(benchmarks, pa_1, "PA_1")
append_reserve <- function(benchmarks_sf, add_reserve, reserve_name){
  
  # dissolve add_reserve into one feature
  add_reserve <- add_reserve %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::mutate(network = reserve_name)
  
  # append to benchmarks_sf
  benchmarks_sf <- rbind(benchmarks_sf, add_reserve)
  
  return(benchmarks_sf)
}


### benchmarks_to_networks ###
#
#' Combine a set of benchmark polygons into a network.
#'
#' Takes a set of individual benchmark polygons and combines them into network polygons based on a list of network names.
#'
#' @param 
#' @param 
#' @param 
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples

benchmarks_to_networks <- function(benchmark_table, catchments_sf, network_list){}