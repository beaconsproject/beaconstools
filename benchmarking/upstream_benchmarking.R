getOrder1 <- function(catchment_tab, current_catchment){
  # get ORDER1 value from the provided CATCHNUM in a data.table
  as.character(catchment_tab[CATCHNUM==current_catchment, ORDER1,])
}

getOrder1_base <- function(catchment_df, current_catchment){
  as.character(catchment_df$ORDER1[catchment_df$CATCHNUM==current_catchment])
}

getOrder1_dplyr <- function(catchment_tib, current_catchment){
  catchment_tib %>%
    dplyr::filter(.data$CATCHNUM == current_catchment) %>%
    dplyr::pull(.data$ORDER1)
}

getOrder2 <- function(catchment_tab, current_catchment){
  # get ORDER2 value from the provided CATCHNUM in a data.table
  # all ORDER2 values are numeric, so convert to numbers here.
  # this can been seen using levels(catchment_tab[CATCHNUM==current_catchment, ORDER2,])
  as.numeric(as.character(catchment_tab[CATCHNUM==current_catchment, ORDER2,]))
}

getOrder2_base <- function(catchment_df, current_catchment){
  as.numeric(as.character(catchment_df$ORDER2[catchment_df$CATCHNUM==current_catchment]))
}

getOrder2_dplyr <- function(catchment_tib, current_catchment){
  catchment_tib %>%
    dplyr::filter(.data$CATCHNUM == current_catchment) %>%
    dplyr::pull(.data$ORDER2) %>%
    as.numeric()
}

getOrder3 <- function(catchment_tab, current_catchment){
  # get ORDER3 value from the provided CATCHNUM in a data.table
  as.character(catchment_tab[CATCHNUM==current_catchment, ORDER3,])
}

getOrder3_base <- function(catchment_df, current_catchment){
  as.character(catchment_df$ORDER3[catchment_df$CATCHNUM==current_catchment])
}

getOrder3_dplyr <- function(catchment_tib, current_catchment){
  catchment_tib %>%
    dplyr::filter(.data$CATCHNUM == current_catchment) %>%
    dplyr::pull(.data$ORDER3)
}

getBasin <- function(catchment_tab, current_catchment){
  # get BASIN value from the provided CATCHNUM in a data.table
  as.numeric(as.character(catchment_tab[CATCHNUM==current_catchment, BASIN,]))
}

getBasin_base <- function(catchment_df, current_catchment){
  # get BASIN value from the provided CATCHNUM in a data.table
  as.numeric(as.character(catchment_df$BASIN[catchment_df$CATCHNUM==current_catchment]))
}

getBasin_dplyr <- function(catchment_tib, current_catchment){
  catchment_tib %>%
    dplyr::filter(.data$CATCHNUM == current_catchment) %>%
    dplyr::pull(.data$BASIN)
}

order3ToOrder2 <- function(order3){
  base64Codes <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "=", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  idx0 <- match(substr(order3, 1, 1), base64Codes) - 1 # C# indexes start at zero so minus 1
  idx1 <- match(substr(order3, 2, 2), base64Codes) - 1
  return(idx0 * length(base64Codes) + idx1)
}

utf32_greaterthan <- function(x, y){
  # is x greater than y?
  # used to identify upstream catchments based on comparison of ORDER1 and ORDER1_3 strings.
  
  # get vectors of code points to compare
  xx <- utf8ToInt(x)
  yy <- utf8ToInt(y)
  
  # what is the length of the shortest vector?
  ii <- min(c(length(xx), length(yy)))
  
  # for each index...
  for(i in 1:ii){
    
    # if the value is not the same...
    if(xx[i] != yy[i]){
      
      # test if x > y
      if(xx[i] > yy[i]){
        return(TRUE)
      } else{
        return(FALSE)
      }
    }
    # otherwise move on to next index
  }
  
  # If they are the same up to length ii, the upstream catchment will have more characters, so if x is longer than y, return true.
  if(length(xx) > length(yy)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

isUpstream <- function(catchment_tab, current_catchment, other_catchment){
  
  # Test if other_catchment is upstream of current_catchment
  #
  # First test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
  if(getBasin(catchment_tab, other_catchment) == getBasin(catchment_tab, current_catchment)){
    if(getOrder1(catchment_tab, other_catchment) == getOrder1(catchment_tab, current_catchment)){
      if(getOrder2(catchment_tab, other_catchment) >= getOrder2(catchment_tab, current_catchment)){
        return(TRUE)
      }
    }
  }
  # Second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
  if(getBasin(catchment_tab, other_catchment) == getBasin(catchment_tab, current_catchment)){
    if(grepl(getOrder1(catchment_tab, current_catchment), getOrder1(catchment_tab, other_catchment))){
      if(
        utf32_greaterthan(
          getOrder1(catchment_tab, other_catchment),
          paste0(getOrder1(catchment_tab, current_catchment), getOrder3(catchment_tab, current_catchment))
        )
        #getOrder1(catchment_tab, other_catchment) > 
        #paste0(getOrder1(catchment_tab, current_catchment), getOrder3(catchment_tab, current_catchment)) # this old test used base R sorting which does not appear ot use utf code points
      ){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

isUpstream_base <- function(catchment_df, current_catchment, other_catchment){
  
  # Test if other_catchment is upstream of current_catchment
  #
  # First test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
  if(getBasin_base(catchment_df, other_catchment) == getBasin_base(catchment_df, current_catchment)){
    if(getOrder1_base(catchment_df, other_catchment) == getOrder1_base(catchment_df, current_catchment)){
      if(getOrder2_base(catchment_df, other_catchment) >= getOrder2_base(catchment_df, current_catchment)){
        return(TRUE)
      }
    }
  }
  # Second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
  if(getBasin_base(catchment_df, other_catchment) == getBasin_base(catchment_df, current_catchment)){
    if(grepl(getOrder1_base(catchment_df, current_catchment), getOrder1_base(catchment_df, other_catchment))){
      if(
        utf32_greaterthan(
          getOrder1_base(catchment_df, other_catchment),
          paste0(getOrder1_base(catchment_df, current_catchment), getOrder3_base(catchment_df, current_catchment))
        )
        #getOrder1(catchment_df, other_catchment) > 
        #paste0(getOrder1(catchment_df, current_catchment), getOrder3(catchment_df, current_catchment)) # this old test used base R sorting which does not appear ot use utf code points
      ){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

isUpstream_dplyr <- function(catchment_tib, current_catchment, other_catchment){
  
  # Test if other_catchment is upstream of current_catchment
  #
  # First test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
  if(getBasin_dplyr(catchment_tib, other_catchment) == getBasin_dplyr(catchment_tib, current_catchment)){
    if(getOrder1_dplyr(catchment_tib, other_catchment) == getOrder1_dplyr(catchment_tib, current_catchment)){
      if(getOrder2_dplyr(catchment_tib, other_catchment) >= getOrder2_dplyr(catchment_tib, current_catchment)){
        return(TRUE)
      }
    }
  }
  # Second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
  if(getBasin_dplyr(catchment_tib, other_catchment) == getBasin_dplyr(catchment_tib, current_catchment)){
    if(grepl(getOrder1_dplyr(catchment_tib, current_catchment), getOrder1_dplyr(catchment_tib, other_catchment))){
      if(
        utf32_greaterthan(
          getOrder1_dplyr(catchment_tib, other_catchment),
          paste0(getOrder1_dplyr(catchment_tib, current_catchment), getOrder3_dplyr(catchment_tib, current_catchment))
        )
        #getOrder1(catchment_tab, other_catchment) > 
        #paste0(getOrder1(catchment_tab, current_catchment), getOrder3(catchment_tab, current_catchment)) # this old test used base R sorting which does not appear ot use utf code points
      ){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

genNeighbours <- function(catchment_sf, out_file){
  
  # generate the neighbours csv file
  # this was originally done in Python using the GenerateSpatialWeightsMatrix() function
  # here we use the SF package
  # pattern = "****T****" in the st_relate() function matches any intersecting polygons.
  # More info on st_relate at:
  # https://www.rdocumentation.org/packages/sf/versions/0.7-7/topics/st_relate
  # Queen pattern found here: https://github.com/r-spatial/sf/issues/234
  
  # Generate neighbours where single point is shared
  st_queen <- function(a, b = a) st_relate(a, b, pattern = "****T****") # this tests for an intersect
  # catchment_sf <- catchment_sf %>% 
  #   st_buffer(dist = 0.1) %>% # buffer is needed to detect many catchments that do not wuite touch
  #   mutate(NB_QUEEN = st_queen(.))
  
  # # Updated Sept 27 2021 by ME: above dplyr code no longer works. I think because a sparse matrix cannot be stored as a column in an sf table. Call st_queen directly. buffer is needed to detect many catchments that do not quite touch. Convert to data.frame to get pairwise intersects, then to data.table. Might be a faster option that avoids as.data.frame().
  nbr_df <- as.data.table(as.data.frame(st_queen(st_buffer(catchment_sf, dist=0.1)))) 
  
  # convert output to data table
  #nbr_df <- as.data.table(catchment_sf$NB_QUEEN)
  
  # replace index values with catchnum values using data.table
  catchment_sf$key <- 1:nrow(catchment_sf) # add a key column to sf table. Must be an index so it matches the index assigned to the NB_QUEEN column
  sf_catch_key <- as.data.table(catchment_sf[c("key","CATCHNUM")])[,1:2] # make a key - catchnum table
  
  setkey(nbr_df, row.id) # set the row.id key on the neighbours table
  setkey(sf_catch_key, key) # set the key on the catchnum table
  nbr_df <- sf_catch_key[nbr_df][,c("CATCHNUM","col.id")] # join the tables on the key, only keep the catchnum and the col.id's
  setkey(nbr_df, col.id) # now repeat the set key and the join for the col.id column
  nbr_df <- sf_catch_key[nbr_df][,c("CATCHNUM", "i.CATCHNUM")]
  names(nbr_df) <- c("catchnum", "neighbours") # rename output columns
  
  # remove cases where CATCHNUM is its own NEIGHBOUR
  nbr_df <- nbr_df[catchnum != neighbours,]
  
  if(out_file == ""){
    return(nbr_df)
  } else{
    # write output
    fwrite(nbr_df, out_file)
  }
}

genNeighbours_dplyr <- function(catchment_sf){
  
  # generate the neighbours csv file
  # this was originally done in Python using the GenerateSpatialWeightsMatrix() function
  # here we use the SF package
  # pattern = "****T****" in the st_relate() function matches any intersecting polygons.
  # More info on st_relate at:
  # https://www.rdocumentation.org/packages/sf/versions/0.7-7/topics/st_relate
  # Queen pattern found here: https://github.com/r-spatial/sf/issues/234
  
  # Generate neighbours where single point is shared
  st_queen <- function(a, b = a) sf::st_relate(a, b, pattern = "****T****") # this tests for an intersect
  nbr_df <- as.data.frame(st_queen(sf::st_buffer(catchment_sf, dist=0.1)))
  
  # replace index values with catchnum values using data.table
  catchment_sf$key <- 1:nrow(catchment_sf) # add a key column to sf table. Must be an index so it matches the index assigned to the NB_QUEEN column
  sf_catch_key <- sf::st_drop_geometry(catchment_sf[c("key","CATCHNUM")])
  
  nbr_df <- nbr_df %>%
    dplyr::left_join(sf_catch_key, by = c("row.id" = "key")) %>%
    dplyr::left_join(sf_catch_key, by = c("col.id" = "key")) %>%
    dplyr::select(.data$CATCHNUM.x, .data$CATCHNUM.y)
  
  names(nbr_df) <- c("catchnum", "neighbours") # rename output columns
  
  # remove cases where CATCHNUM is its own NEIGHBOUR
  nbr_df <- nbr_df %>%
    dplyr::filter(.data$catchnum != .data$neighbours)
  
  return(nbr_df)
}


getAggregationUpstreamCatchments <- function(catchment_tab, agg_catchments, neighbours_tab){
  # Get all upstream catchments of an aggregation of catchments (e.g. a list of catchnums in a PA).
  
  #print("Generating upstream catchments...")
  
  # create inspected list to track upstream catchments that have been inspected
  lst_inspected <- c()
  
  # create list to hold upstream catchments
  lst_upstream <- c()
  
  for(aggCatchmntIdx in agg_catchments){ # for each catchment in the aggregation
    
    # create a pending queue
    pendingQueue <- c()
    
    # add aggCatchmntIdx to the queue
    pendingQueue <- c(pendingQueue, aggCatchmntIdx)
    
    while(length(pendingQueue) > 0){
      
      currentCatchment <- pendingQueue[1]
      
      # remove current catchment from pending queue
      pendingQueue <- pendingQueue[-1]
      
      # get current catchments neighbours
      neighbourCatchments <- neighbours_tab %>%
        filter(catchnum == currentCatchment) %>%
        pull(neighbours)
      #cat("neighbours: ", neighbourCatchments)
      
      for(nbr in neighbourCatchments){
        #print(paste0("nbr: ", nbr))
        # if not already inspected
        if(!nbr %in% lst_inspected){
          
          # if not in the aggregation
          if(!nbr %in% agg_catchments){
            
            # test if neighbour is upstream of current agg catchment
            if(isUpstream(catchment_tab = catchment_tab, current_catchment = aggCatchmntIdx, other_catchment = nbr)){
              
              # if yes, add to upstream list
              lst_upstream <- c(lst_upstream, nbr)
              
              # put this catchment in the queue so its neighbours can be tested
              pendingQueue <- c(pendingQueue, nbr)
              
              # add this catchment to the inspected list
              lst_inspected <- c(lst_inspected, nbr)
            } else{
              # not upstream, put it in the pending queue so its neighbours can be tested
              #pendingQueue <- c(pendingQueue, nbr)
              #lst_inspected <- c(lst_inspected, nbr)
            }
          } else{
            # if it's in the aggregation, put it in the pending queue so its neighbours can be tested
            pendingQueue <- c(pendingQueue, nbr)
            lst_inspected <- c(lst_inspected, nbr)
          }
        }
        #cat("inspected: ", lst_inspected)
      }
    }
  }
  return(lst_upstream)
}

getAggregationUpstreamCatchments_dplyr <- function(catchment_tib, agg_catchments, neighbours_tib){
  # Get all upstream catchments of an aggregation of catchments (e.g. a list of catchnums in a PA).
  
  #print("Generating upstream catchments...")
  
  # create inspected list to track upstream catchments that have been inspected
  lst_inspected <- c()
  
  # create list to hold upstream catchments
  lst_upstream <- c()
  
  for(aggCatchmntIdx in agg_catchments){ # for each catchment in the aggregation
    
    # create a pending queue
    pendingQueue <- c()
    
    # add aggCatchmntIdx to the queue
    pendingQueue <- c(pendingQueue, aggCatchmntIdx)
    
    while(length(pendingQueue) > 0){
      
      currentCatchment <- pendingQueue[1]
      
      # remove current catchment from pending queue
      pendingQueue <- pendingQueue[-1]
      
      # get current catchments neighbours
      neighbourCatchments <- neighbours_tib %>%
        dplyr::filter(catchnum == currentCatchment) %>%
        dplyr::pull(neighbours)
      
      for(nbr in neighbourCatchments){
        # if not already inspected
        if(!nbr %in% lst_inspected){
          
          # if not in the aggregation
          if(!nbr %in% agg_catchments){
            
            # test if neighbour is upstream of current agg catchment
            if(isUpstream_dplyr(catchment_tib = catchment_tib, current_catchment = aggCatchmntIdx, other_catchment = nbr)){
              
              # if yes, add to upstream list
              lst_upstream <- c(lst_upstream, nbr)
              
              # put this catchment in the queue so its neighbours can be tested
              pendingQueue <- c(pendingQueue, nbr)
              
              # add this catchment to the inspected list
              lst_inspected <- c(lst_inspected, nbr)
            } else{
              # not upstream, put it in the pending queue so its neighbours can be tested
              #pendingQueue <- c(pendingQueue, nbr)
              #lst_inspected <- c(lst_inspected, nbr)
            }
          } else{
            # if it's in the aggregation, put it in the pending queue so its neighbours can be tested
            pendingQueue <- c(pendingQueue, nbr)
            lst_inspected <- c(lst_inspected, nbr)
          }
        }
      }
    }
  }
  return(lst_upstream)
}

getAggregationUpstreamCatchments_base <- function(catchment_df, agg_catchments, neighbours_tib){
  # Get all upstream catchments of an aggregation of catchments (e.g. a list of catchnums in a PA).
  
  #print("Generating upstream catchments...")
  
  # create inspected list to track upstream catchments that have been inspected
  lst_inspected <- c()
  
  # create list to hold upstream catchments
  lst_upstream <- c()
  
  for(aggCatchmntIdx in agg_catchments){ # for each catchment in the aggregation
    
    # create a pending queue
    pendingQueue <- c()
    
    # add aggCatchmntIdx to the queue
    pendingQueue <- c(pendingQueue, aggCatchmntIdx)
    
    while(length(pendingQueue) > 0){
      
      currentCatchment <- pendingQueue[1]
      
      # remove current catchment from pending queue
      pendingQueue <- pendingQueue[-1]
      
      # get current catchments neighbours
      neighbourCatchments <- neighbours_tib %>%
        dplyr::filter(catchnum == currentCatchment) %>%
        dplyr::pull(neighbours)
      
      for(nbr in neighbourCatchments){
        # if not already inspected
        if(!nbr %in% lst_inspected){
          
          # if not in the aggregation
          if(!nbr %in% agg_catchments){
            
            # test if neighbour is upstream of current agg catchment
            if(isUpstream_base(catchment_df = catchment_df, current_catchment = aggCatchmntIdx, other_catchment = nbr)){
              
              # if yes, add to upstream list
              lst_upstream <- c(lst_upstream, nbr)
              
              # put this catchment in the queue so its neighbours can be tested
              pendingQueue <- c(pendingQueue, nbr)
              
              # add this catchment to the inspected list
              lst_inspected <- c(lst_inspected, nbr)
            } else{
              # not upstream, put it in the pending queue so its neighbours can be tested
              #pendingQueue <- c(pendingQueue, nbr)
              #lst_inspected <- c(lst_inspected, nbr)
            }
          } else{
            # if it's in the aggregation, put it in the pending queue so its neighbours can be tested
            pendingQueue <- c(pendingQueue, nbr)
            lst_inspected <- c(lst_inspected, nbr)
          }
        }
      }
    }
  }
  return(lst_upstream)
}




# Input table must be data.table, order1 and order3 columns must be characters, order2 must be numeric
catchment_tab <- foreign::read.dbf("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.dbf")
catchment_tab <- data.table::as.data.table(catchment_tab)
catchment_tab$ORDER1 <- as.character(catchment_tab$ORDER1)
catchment_tab$ORDER2 <- as.numeric(catchment_tab$ORDER2)
catchment_tab$ORDER3 <- as.character(catchment_tab$ORDER3)

catchment_tib <- foreign::read.dbf("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.dbf")
catchment_tib <- dplyr::as_tibble(catchment_tib)
catchment_tib$ORDER1 <- as.character(catchment_tib$ORDER1)
catchment_tib$ORDER2 <- as.numeric(catchment_tib$ORDER2)
catchment_tib$ORDER3 <- as.character(catchment_tib$ORDER3)

catchment_df <- foreign::read.dbf("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.dbf")
catchment_df <- as.data.frame(catchment_df)
catchment_df$ORDER1 <- as.character(catchment_df$ORDER1)
catchment_df$ORDER2 <- as.numeric(catchment_df$ORDER2)
catchment_df$ORDER3 <- as.character(catchment_df$ORDER3)

catchment_sf <- sf::st_read("C:/Users/MAEDW7/Dropbox (BEACONs)/KBA Case Study/catchments/catch_wwf9.shp")


system.time({
  genNeighbours(catchment_sf, "")
}) # 138
system.time({
  genNeighbours_dplyr(catchment_sf)
}) # 120


agg_catchments <- catchment_sf$CATCHNUM[500:510]
neighbours_tab <- genNeighbours(catchment_sf, "")
dt_up <- getAggregationUpstreamCatchments(catchment_tab, agg_catchments, neighbours_tab)

neighbours_tib <- genNeighbours_dplyr(catchment_sf)
dplyr_up <- getAggregationUpstreamCatchments_dplyr(catchment_tib, agg_catchments, neighbours_tib)

identical(dt_up,base_up)

base_up <- getAggregationUpstreamCatchments_base(catchment_df, agg_catchments, neighbours_tib)
identical(base_up,dt_up)

agg_catchments <- catchment_sf$CATCHNUM[500:510]
system.time({getAggregationUpstreamCatchments(catchment_tab, agg_catchments, neighbours_tab)}) # 148
system.time({getAggregationUpstreamCatchments_dplyr(catchment_tib, agg_catchments, neighbours_tib)}) # 244
system.time({getAggregationUpstreamCatchments_base(catchment_df, agg_catchments, neighbours_tib)}) # 56 !!!!

agg_catchments <- catchment_sf$CATCHNUM[700:720]
system.time({dt_up <- getAggregationUpstreamCatchments(catchment_tab, agg_catchments, neighbours_tab)}) # 7.20
system.time({base_up <- getAggregationUpstreamCatchments_base(catchment_df, agg_catchments, neighbours_tib)}) # 2.23
identical(base_up, dt_up)

agg_catchments <- catchment_sf$CATCHNUM[500:520]
system.time({dt_up <- getAggregationUpstreamCatchments(catchment_tab, agg_catchments, neighbours_tab)}) # 143
system.time({base_up <- getAggregationUpstreamCatchments_base(catchment_df, agg_catchments, neighbours_tib)}) # 52
identical(base_up, dt_up)

# dplyr neighbour function and base aggregation function are fastest