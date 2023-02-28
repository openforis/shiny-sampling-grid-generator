#########################################################
# This module generates cluster locations
# Edited: 17.1.2022  From spatial dataframe to use sf 
# Lauri Vesa
#########################################################

# Too many arguments - will refactor
generate_clusters <- function(cluster_start_random,
                              cluster_hexagonal,
                              cluster_hexagonal_dist,
                              map_projection,
                              min_x,
                              max_x,
                              min_y,
                              max_y,
                              cluster_x,
                              cluster_y,
                              cluster_x_distance,
                              cluster_y_distance,
                              first_cluster_id 
                              ) {
  # Check parameters
  if (max_x <= min_x)          stop("Max x must be greater than min x")
  if (max_y <= min_y)          stop("Max y must be greater than min y")
  if (cluster_x_distance <= 0) stop("Cluster x distance must be greater than zero")
  if (cluster_y_distance <= 0) stop("Cluster y distance must be greater than zero")
  
  is.even <- function(x) x %% 2 == 0
  
  if (cluster_hexagonal==TRUE & cluster_hexagonal_dist==TRUE) {
    cluster_y_distance = sqrt(3/4) * cluster_x_distance
  }
  
  # if starting point is random, get random coordinates
  cluster_x <- ifelse( !cluster_start_random, min_x + cluster_x,
                               min_x + runif(1, min = 0, max = 0.99999) * cluster_x_distance )
  cluster_y <- ifelse( !cluster_start_random, max_y + cluster_y,
                               max_y - runif(1, min = 0, max = 0.99999) * cluster_y_distance )

  i_cols <- round((max_x - min_x) / cluster_x_distance + 0.5) 
  i_rows <- round((max_y - min_y) / cluster_y_distance + 0.5) 

  
  DT_Cluster <- as.data.frame(matrix(0, ncol = 3, nrow = i_cols * i_rows))
  colnames(DT_Cluster)[1] <- "Cluster_ID" ; colnames(DT_Cluster)[2] <- "X" ; colnames(DT_Cluster)[3] <- "Y"
  
  DT_Cluster$RowNo <- rep(seq(1, i_rows, 1), each = i_cols)
  DT_Cluster$ColNo <- rep(seq(1, i_cols, 1), times = i_rows)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Clusters are numbered in east-west direction
  ## Generate cluster centre X coordinates -----
  DT_Cluster$X <- rep(seq(cluster_x, cluster_x + (i_cols - 1) * cluster_x_distance, cluster_x_distance), times = i_rows)
  ## Generate cluster centre Y coordinates -----
  DT_Cluster$Y <- rep(seq(cluster_y, cluster_y - (i_rows - 1) * cluster_y_distance, -cluster_y_distance), each = i_cols)
  DT_Cluster$Cluster_ID <- seq(first_cluster_id, first_cluster_id + i_cols * i_rows - 1, 1)

  
 # write.csv(DT_Cluster,"d:/tmp/t.csv")
  
  # hexagonal grid
  if (cluster_hexagonal==TRUE) {
    DT_Cluster$is_even <-  is.even( rep(seq(1, i_rows), each = i_cols) )
    DT_Cluster$X[ DT_Cluster$is_even ] <- DT_Cluster$X[ DT_Cluster$is_even ] + 0.5 * cluster_x_distance
    DT_Cluster$is_even <- NULL 
  }
  
  print("CLUSTER: Step 1")

  if (map_projection == 4326) {
    DT_Cluster            <- dplyr::mutate(DT_Cluster, zone = (trunc((X + 180) / 6)  + 1))
    DT_Cluster$zone       <- as.integer(DT_Cluster$zone)
    DT_Cluster$hemisphere <- ifelse( DT_Cluster$Y < 0, "S", "N" )
    DT_Cluster$zone       <- paste(DT_Cluster$zone, DT_Cluster$hemisphere, sep="" )
    DT_Cluster$hemisphere <- NULL
    
    UTM_list <- unique(DT_Cluster$zone)

    clusterdata <- split(DT_Cluster, DT_Cluster$zone)
    for (i in 1:length(clusterdata)) {
      zone_str  <- as.character(names(clusterdata)[i])
      zone      <- as.integer(substr(zone_str, 0, nchar(zone_str)-1))
      hemispere <- substr(zone_str, nchar(zone_str), nchar(zone_str))
      
      df <- data.frame(clusterdata[[i]])

      # https://stackoverflow.com/questions/49181715/how-to-make-a-data-frame-into-a-simple-features-data-frame
#      projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      
      projcrs <- ifelse(hemispere=='S', 32700 + zone, 32600 + zone)
            
      data <- st_as_sf(x = df,                         
                 coords = c("X", "Y"),
                 crs    = paste0("EPSG:", projcrs),
                 remove = FALSE)
      
#      coordinates(data) <- c("X", "Y")
#      proj4string(data) <- map_proj4string
#      ress <- spTransform(data, conv_string)

      ress <- st_drop_geometry(data)
      ress$zone  <- NULL
      ress$ColNo <- NULL
      ress$RowNo <- NULL

      names(ress)[names(ress) == 'X'] <- 'X_map'
      names(ress)[names(ress) == 'Y'] <- 'Y_map'
      
      if (i == 1) {
        resAll <- ress
      } else {
        resAll <- rbind(resAll, ress)
      }
    }
    DT_Cluster <- merge(x=DT_Cluster, y=resAll, by="Cluster_ID", all.x = TRUE)
    rm(clusterdata)
    rm(ress)
    
  } else {

    DT_Cluster$X_map <- as.numeric(DT_Cluster$X)
    DT_Cluster$Y_map <- as.numeric(DT_Cluster$Y)
 }

  #####################################

#    print(str(DT_Cluster))
  
  print("CLUSTER: Step 2")

    sf_Cluster <- st_as_sf( x = DT_Cluster,                         
                       coords = c("X", "Y"),
                       crs    = st_crs(map_projection)$proj4string )

    if (map_projection != 4326) { 
      sf_Cluster <- st_transform( sf_Cluster, 4326 )
    }
    
    print("CLUSTER: Step 3")
#  if (!any(names(DT_Cluster) == 'X')) {
#    DT_Cluster@data$X <- DT_Cluster$X
#    DT_Cluster@data$Y <- DT_Cluster$Y
#  }
  
  coord_list   <- st_coordinates(sf_Cluster)

  DT_Cluster   <- st_drop_geometry(sf_Cluster)
  DT_Cluster$X <- coord_list[,1]
  DT_Cluster$Y <- coord_list[,2]
  rm(coord_list)
    
  DT_Cluster <- DT_Cluster %>% 
    dplyr::mutate(zone= (trunc((X + 180) / 6)) + 1 ) %>%
    dplyr::mutate(hemisphere = ifelse( Y < 0, "S", "N" )) %>%
    dplyr::mutate(zone = paste(zone, hemisphere, sep="" )) %>%
    dplyr::select(-hemisphere)

  sf_Cluster <- st_as_sf( x = DT_Cluster,                         
                          coords = c("X", "Y"),
                          crs    = "EPSG:4326",
                          remove = FALSE)
  
  print("CLUSTER: Step 4")

  return(sf_Cluster)
}
