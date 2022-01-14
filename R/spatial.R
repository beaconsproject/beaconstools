### catchments_to_networks ###
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
#' benchmarks <- catchments_to_networks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001", "PB_0002"))
#' plot(benchmarks)
#' 
#' # Benchmarks combined into a network
#' network <- catchments_to_networks(
#'   benchmark_table_sample, 
#'   catchments_sample, 
#'   c("PB_0001__PB_0002"))
#' plot(network)
catchments_to_networks <- function(benchmark_table, catchments_sf, network_list){
  
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