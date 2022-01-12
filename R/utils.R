sep_network_name <- function(network_name){
  strsplit(network_name, "__")[[1]]
}