## +++++++++++++++++++++++++++++++++++++++++++++++
## Generates plot locations (X, Y) ------
## p = plot point dataframe

# https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
decimalplaces <- function(x) {
  ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
         nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
         0)
}

########################################################

generate_plots <- function(clusters,
                           Plot_type,
                           plot_list,
                           map_projection) {
  
  DT_Clusters <- st_drop_geometry(clusters)
  
  deci_x <- max(decimalplaces(DT_Clusters$X_map)) 
  deci_y <- max(decimalplaces(DT_Clusters$Y_map)) 
  
  
  nclus       <- nrow(DT_Clusters)
  nplots      <- nrow(plot_list)

  if (Plot_type=='NFMA') {
    nplots <- 4
    rm(plot_list)
    plot_list <- data.frame( No=c(1:4), X=c(250,250,750,750), Y=c(250,750,750,250) )
  }

  # for 4326, compute bearing (degrees) and distance (m)
  if (map_projection == 4326) {
    for (i in (1:nplots)) {
      plot_list$azimuth[i]  <- nngeo::st_azimuth(st_point(c(0, 0)), st_point(c(plot_list$X[i], plot_list$Y[i])))
    }
    plot_list$distance <- sqrt((plot_list$X^2 + plot_list$Y^2))
    print(str(plot_list))
  }
  
  print("PLOT 1")
  p <- as.data.frame(matrix(0, ncol = 5, nrow = nclus * nplots))
  colnames(p)[1] <- "Cluster_ID";
  colnames(p)[2] <- "Plot_ID";
  colnames(p)[3] <- "zone";
  colnames(p)[4] <- "X_map";
  colnames(p)[5] <- "Y_map"
  
  p$Cluster_ID <- rep(DT_Clusters$Cluster_ID, each = nplots)
  p$X          <- rep(DT_Clusters$X, each = nplots)
  p$Y          <- rep(DT_Clusters$Y, each = nplots)
  
  p$Plot_ID    <- rep(plot_list$No, times = nclus)
  p$X_map      <- rep(plot_list$X,  times = nclus)
  p$Y_map      <- rep(plot_list$Y,  times = nclus)
  p$zone       <- rep(DT_Clusters$zone, each = nplots)
  print("PLOT 1+")
  if ( map_projection==4326 ) {
      iii = 1
      for (ic in 1:nclus) {
        for (ip in 1:nplots) {
          if (plot_list$distance[ip] > 0) {
            target_coord <- geosphere::destPoint(c(p$X[iii], p$Y[iii]), plot_list$azimuth[ip], plot_list$distance[ip] )
            print(str(target_coord))
            p$X_map[iii] <- target_coord[1] 
            p$Y_map[iii] <- target_coord[2]
          } else {
            p$X_map[iii] <- p$X[iii] 
            p$Y_map[iii] <- p$Y[iii]
          }
          iii = iii + 1
        }
      }
  } else {
    clusters_X   <- rep(DT_Clusters$X_map, each = nplots)
    clusters_Y   <- rep(DT_Clusters$Y_map, each = nplots)
    
    p$X_map      <- p$X_map + clusters_X
    p$Y_map      <- p$Y_map + clusters_Y
  }
  p$X <- NULL
  p$Y <- NULL
  rm(clusters_X); rm(clusters_Y)
  
  print("PLOT 1++")
  # Get longlat coordinates for plot points --------
  plotdata <- split(p, p$zone)
  
  print("PLOT 2")
  
  for (i in 1:length(plotdata)) {
    
    df <- data.frame(plotdata[[i]])

    if (map_projection == 4326) {
      zone_str  <- as.character(names(plotdata)[i])
      zone      <- as.integer(substr(zone_str, 0, nchar(zone_str)-1))
      hemispere <- substr(zone_str, nchar(zone_str), nchar(zone_str))
            
      projcrs <- ifelse(hemispere=='S', 32700+zone, 32600+zone)
                         
      data <- st_as_sf(x = df,                         
                       coords = c("X_map", "Y_map"),
                       crs    = paste0("EPSG:", projcrs),
                       remove = TRUE)
      
      rm(hemispere); rm(zone_str)
    } else {
      data <- st_as_sf( x = df,                         
                       coords = c("X_map", "Y_map"),
                       crs    = paste0("EPSG:", map_projection),
                       remove = TRUE)
    }
 
     rm(df)

    if (map_projection != 4326 ) {
      res <- st_transform( data, 4326 )
    } else {
      res <- data
    }
    coord_list <- st_coordinates(res)
    
    res   <- st_drop_geometry(res)
    res$X <- coord_list[,1]
    res$Y <- coord_list[,2]
    
    rm(coord_list)
    res$zone <- NULL

    if (i == 1) {
      resAll <- res
    } else {
      resAll <- rbind(resAll, res)
    }
  }
  
  print("PLOT 3")
  
  p <- merge(x = p, y = resAll, by = c("Cluster_ID", "Plot_ID"), all.x = TRUE)
  p$Cluster_ID <- as.integer(p$Cluster_ID)
  p$Plot_ID    <- as.integer(p$Plot_ID)

  # order by Cluster_ID, Plot_ID
  p <- p[with(p, order(Cluster_ID, Plot_ID)),]

  if (map_projection==4326 & nplots==1 & mean(plot_list$X)==0 & mean(plot_list$Y)==0) {
    p$X_map = round(p$X_map, deci_x)
    p$X     = round(p$X    , deci_x)
    p$Y_map = round(p$Y_map, deci_y)
    p$Y     = round(p$Y    , deci_y)
  }
  
  p <- st_as_sf( x = p,                         
                 coords = c("X", "Y"),
                 crs    = "EPSG:4326",
                 remove = FALSE)

  rm(res); rm(resAll); rm(zone); rm(i); rm(data)
  print("PLOT 4")
  return(p)
}
