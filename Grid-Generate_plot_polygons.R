########################################################
# This module generates plot polygons
#
# Edited: 21.2.2022
########################################################

# https://rdrr.io/cran/sf/man/st_bbox.html

generate_plot_polygons <- function(p,
                                   Plot_type,
                                   PlotCorner_X,
                                   PlotCorner_Y,
                                   Plot_ref_point,
                                   Plot_dim1,
                                   Plot_dim2, 
                                   map_projection) {
  
  
  print("POLY 0") 
  p_df <- st_drop_geometry( p )
  
  print("POLY 1")

  my_spatial_polys_df_All <- NA
  Plots_n <- ifelse(Plot_type == 'NFMA', 4, length(unique(p_df$Plot_ID)))
  
  zonelist  <- unique(p_df$zone)


  if (Plot_type=='Z' & length(PlotCorner_X)==1) {
    PlotCorner_X <- eval(parse(text=paste("c(", PlotCorner_X, ")", sep="")))
    PlotCorner_Y <- eval(parse(text=paste("c(", PlotCorner_Y, ")", sep="")))
  }

  # geom_plot_x, index: 1: no rotation, 2: -90 degrees, 3: 90 degrees
  
  # order of corner points (geom_plot_x,geom_plot_y): NW,NE,SE,SW  
  if (Plot_type == 'Z') {
      geom_plot_x <- PlotCorner_X
      geom_plot_y <- PlotCorner_Y
  } else if (Plot_ref_point == 'SW' | Plot_ref_point == 'WS') {
      geom_plot_x <- c(0, Plot_dim1, Plot_dim1, 0)
      geom_plot_y <- c(Plot_dim2, Plot_dim2, 0, 0)
  } else if (Plot_ref_point == 'NW' | Plot_ref_point == 'WN') {
      geom_plot_x <- c(0, Plot_dim1, Plot_dim1, 0)
      geom_plot_y <- c(0, 0, -Plot_dim2, -Plot_dim2)
  } else if (Plot_ref_point == 'C' & Plot_type == 'R') {
      geom_plot_x <- c(-Plot_dim1 / 2, Plot_dim1 / 2, Plot_dim1 / 2, -Plot_dim1 / 2)
      geom_plot_y <- c(Plot_dim2 / 2, Plot_dim2 / 2, -Plot_dim2 / 2, -Plot_dim2 / 2)
  } 
  # 
  # direction = 1 # counter-clock
  # new_height = Plot_dim1 * direction
  # new_width = -Plot_dim2 * direction
  # Plot_dim1a = new_width
  # Plot_dim2a = new_height
  # 
  # if (Plot_type == 'Z') {
  #   geom_plot_x[2] <- PlotCorner_X
  #   geom_plot_y[2] <- PlotCorner_Y
  # } else if (Plot_ref_point == 'SW' | Plot_ref_point == 'WS') {
  #   geom_plot_x[2] <- c(0, Plot_dim1a, Plot_dim1a, 0)
  #   geom_plot_y[2] <- c(Plot_dim2, Plot_dim2, 0, 0)
  # } else if (Plot_ref_point == 'NW' | Plot_ref_point == 'WN') {
  #   geom_plot_x[2] <- c(0, Plot_dim1a, Plot_dim1a, 0)
  #   geom_plot_y[2] <- c(0, 0, -Plot_dim2a, -Plot_dim2a)
  # } else if (Plot_ref_point == 'C' & Plot_type == 'R') {
  #   geom_plot_x[2] <- c(-Plot_dim1a / 2, Plot_dim1a / 2, Plot_dim1a / 2, -Plot_dim1a / 2)
  #   geom_plot_y[2] <- c( Plot_dim2a / 2, Plot_dim2a / 2, -Plot_dim2a / 2, -Plot_dim2a / 2)
  # } 
  # 
  # direction = -1 # counter-clock
  # new_height = Plot_dim1 * direction
  # new_width = -Plot_dim2 * direction
  # Plot_dim1a = new_width
  # Plot_dim2a = new_height
  # 
  # if (Plot_type == 'Z') {
  #   geom_plot_x[3] <- PlotCorner_X
  #   geom_plot_y[3] <- PlotCorner_Y
  # } else if (Plot_ref_point == 'SW' | Plot_ref_point == 'WS') {
  #   geom_plot_x[3] <- c(0, Plot_dim1a, Plot_dim1a, 0)
  #   geom_plot_y[3] <- c(Plot_dim2a, Plot_dim2a, 0, 0)
  # } else if (Plot_ref_point == 'NW' | Plot_ref_point == 'WN') {
  #   geom_plot_x[3] <- c(0, Plot_dim1, Plot_dim1, 0)
  #   geom_plot_y[3] <- c(0, 0, -Plot_dim2a, -Plot_dim2a)
  # } else if (Plot_ref_point == 'C' & Plot_type == 'R') {
  #   geom_plot_x[3] <- c(-Plot_dim1a / 2, Plot_dim1a / 2, Plot_dim1a / 2, -Plot_dim1a / 2)
  #   geom_plot_y[3] <- c( Plot_dim2a / 2, Plot_dim2a / 2, -Plot_dim2a / 2, -Plot_dim2a / 2)
  # } 
  
  
  print("POLY 2")
  
  for (z in 1:length(zonelist) ) {
    pz <- subset(p_df, zone == zonelist[z])
    
    if ( map_projection == 4326 ) {
      zone_str  <- zonelist[z]
      zone      <- as.integer(substr(zone_str, 0, nchar(zone_str)-1))
      hemispere <- substr(zone_str, nchar(zone_str), nchar(zone_str))
      EPSG_utm      <- ifelse(hemispere=='S', 32700+zone, 32600+zone)

      pz_sf <- st_as_sf(x = pz,                        
            coords = c("X", "Y"),
            crs    = "EPSG:4326")
      
      pz_sf <- st_transform(x = pz_sf, EPSG_utm)
      coord_list <- st_coordinates(pz_sf)
      rm(pz_sf)
      
      pz$X_map <- coord_list[,1]
      pz$Y_map <- coord_list[,2]
    
      }
      
      rm(coord_list)
      
   #   print(str(pz))
    
    
    if (Plot_type !='C') {
      
      if (Plot_type !='NFMA') {  # square or rectangular or custom
        x1 <- array(NA, c(length(geom_plot_x) + 1 , nrow(pz)))
        y1 <- array(NA, c(length(geom_plot_y) + 1 , nrow(pz)))

        for (i in 1:length(geom_plot_x)) {  # loop by plots
          x1[i,] <- rep(geom_plot_x[i], nrow(pz)) + pz$X_map
          y1[i,] <- rep(geom_plot_y[i], nrow(pz)) + pz$Y_map
        }
      } else {  #NFMA
        x1 <- array(NA, c(Plots_n + 1, nrow(pz)))
        y1 <- array(NA, c(Plots_n + 1, nrow(pz)))
        
        coord_p_x <- matrix(0, ncol = 4, nrow = 4)
        coord_p_y <- coord_p_x

        # plot 1
        coord_p_x[1,] <- c(-Plot_dim2 / 2, Plot_dim2 / 2, Plot_dim2 / 2, -Plot_dim2 / 2)
        coord_p_y[1,] <- c(0, 0, Plot_dim1, Plot_dim1)
        # plot 2
        coord_p_x[2,] <- c(0, Plot_dim1, Plot_dim1, 0)
        coord_p_y[2,] <- c(-Plot_dim2 / 2, -Plot_dim2 / 2, Plot_dim2 / 2, Plot_dim2 / 2)
        # plot 3
        coord_p_x[3,] <- c(-Plot_dim2 / 2,-Plot_dim2 / 2, Plot_dim2 / 2, Plot_dim2 / 2)
        coord_p_y[3,] <- c(0, -Plot_dim1, -Plot_dim1, 0)
        # plot 4
        coord_p_x[4,] <- c(0, -Plot_dim1, -Plot_dim1, 0)
        coord_p_y[4,] <- c(Plot_dim2 / 2, Plot_dim2 / 2, -Plot_dim2 / 2, -Plot_dim2 / 2)

        nclus  <- length(unique(pz$Cluster_ID))
        
        # https://stackoverflow.com/questions/20117458/replicate-rows-of-a-matrix-in-r
        coord_p_x <- apply(coord_p_x, 2, function(c) rep(c, nclus))
        coord_p_y <- apply(coord_p_y, 2, function(c) rep(c, nclus))

        x1[1,] <- coord_p_x[,1] + pz$X_map
        y1[1,] <- coord_p_y[,1] + pz$Y_map
        x1[2,] <- coord_p_x[,2] + pz$X_map
        y1[2,] <- coord_p_y[,2] + pz$Y_map
        x1[3,] <- coord_p_x[,3] + pz$X_map
        y1[3,] <- coord_p_y[,3] + pz$Y_map
        x1[4,] <- coord_p_x[,4] + pz$X_map
        y1[4,] <- coord_p_y[,4] + pz$Y_map
        i = 4
      }
      # last point same as first
      x1[i+1,] <- x1[1,]
      y1[i+1,] <- y1[1,]
  
  #    print("POLY 2+")
      # polygon corner points into matrix
      # matrix, there plot corner coordinates are by rows
      outer_x = matrix(x1, ncol = length( x1[,1] ), byrow = TRUE)
      outer_y = matrix(y1, ncol = length( x1[,1] ), byrow = TRUE)
      
      # Set indices for the new matrix (https://stackoverflow.com/questions/18861235/combining-matrices-by-alternating-columns)
      outer_x.idx <- seq(1, ncol(outer_x) * 2, by = 2)
      outer_y.idx <- seq(2, ncol(outer_y) * 2 + 1, by = 2)

#      print("POLY 2++")
      colnames(outer_x) <- seq(1, ncol(outer_x))
      colnames(outer_y) <- seq(1, ncol(outer_y))
      
      DF_outer_x <- as.data.frame(outer_x)
      DF_outer_y <- as.data.frame(outer_y)
      
      DF_outer_x$CL <- c(1:nrow(DF_outer_x))
      DF_outer_y$CL <- c(1:nrow(DF_outer_y))

 #     print("POLY 3")
      # tidyr
      # http://library.open.oregonstate.edu/computationalbiology/chapter/reshaping-and-joining-data-frames/
      DF_outer_x <- gather(DF_outer_x, Point, X, as.character(seq(1, ncol(DF_outer_x) - 1, 1)))
      DF_outer_y <- gather(DF_outer_y, Point, Y, as.character(seq(1, ncol(DF_outer_y) - 1, 1)))
      
      DF_outer <- cbind(DF_outer_x, DF_outer_y[3])
      DF_outer$Point <- as.integer(DF_outer$Point)
      
      # order by cluster, plot numbers
      DF_outer <- DF_outer[with(DF_outer, order(CL, Point)),]

      # drop out node number
      DF_outer$Point <- NULL

      # OLD:: https://gis.stackexchange.com/questions/171124/data-frame-to-spatialpolygonsdataframe-with-multiple-polygons
      # make a list by clusters, this is a list of dataframes
      clusterlist <- unique(pz$Cluster_ID)
      
      DF_outer$Cluster_ID <- c(rep(clusterlist, each = Plots_n)) 
      DF_outer$Plot_ID    <- c(rep(seq(1:Plots_n), times = length(clusterlist)))
      
      outer_list <- split(DF_outer, DF_outer$CL)
      # CL is polygon id
  #    outer_list <- lapply(outer_list, function(x) { x["CL"] <- NULL; x })

#      print("POLY 4")

      for (i in (1:length(outer_list))) {
        sps <-  sfheaders::sf_polygon(obj = outer_list[[i]], x = "X", y = "Y")
        
        if (i==1) {
          sps_sf <- sps
        } else
          sps_sf <-rbind(sps_sf, sps)
      }      

 #     print(str(sps_sf))
      
 #     print("POLY 4+")



      
      # create the SpatialPolygonsDataFrame, one unique ID per polygon
      # my_spatial_polys_df <- SpatialPolygonsDataFrame(sps, 
      #                                                 data.frame(id = unique(DF_outer$CL), 
      #                                                            row.names = unique(DF_outer$CL)))

      if (map_projection != 4326) {
        sps_sf  <- sps_sf %>% st_set_crs(map_projection)
      } else {
        sps_sf  <- sps_sf %>% st_set_crs(EPSG_utm)
      }    
      my_spatial_polys_df <- sps_sf
    }
 

    if (Plot_type == "C")  {       # circular plot
        zone_str  <- as.character(zonelist[z])
        zone      <- as.integer(substr(zone_str, 0, nchar(zone_str)-1))
        hemispere <- substr(zone, nchar(zone_str), nchar(zone_str))

        projcrs <- ifelse(hemispere=='S', 32700+zone,32600+zone)
        
        sps_sf <- st_as_sf(x = pz,
                           coords = c("X", "Y"),
                           crs    = 4326,
                           remove = FALSE)
        
        sps_sf <- st_transform(sps_sf, projcrs)
        
        # buffering
        my_spatial_polys_df <- st_buffer(sps_sf, Plot_dim1 )
    }
 
    my_spatial_polys_df_latlong  <- st_transform( my_spatial_polys_df, 4326)

 
 ##   my_spatial_polys_df_latlong <- spChFIDs(my_spatial_polys_df_latlong, as.character(paste(my_spatial_polys_df_latlong$Cluster_ID, my_spatial_polys_df_latlong$Plot_ID, sep="_" )))
    

    if ( is.na(my_spatial_polys_df_All )) {
      my_spatial_polys_df_All <- my_spatial_polys_df_latlong
    } else {  
      my_spatial_polys_df_All <-  rbind( my_spatial_polys_df_All, my_spatial_polys_df_latlong )
    }  
    rm( my_spatial_polys_df_latlong )
    rm(sps_sf); rm(pz)
  }
  
  return(my_spatial_polys_df_All)
}
