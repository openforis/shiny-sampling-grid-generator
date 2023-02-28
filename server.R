##########################################################################################################
# Plot grid generator
# (c) Lauri Vesa, Stefano Giaggio, FAO Forestry Department, Rome
#
# see
# https://cran.rstudio.com/web/packages/rgdal/vignettes/CRS_projections_transformations.html
#
# Last update:
# 24.2.2022 LV, all spatial object to sf (from sp)
# 28.1.2022 LV, taken out several loaded packages
# 30.9.2020 LV, replaced mapview package with leafem,  https://www.rdocumentation.org/packages/mapview/versions/2.7.8/topics/addMouseCoordinates 
#
#
##########################################################################################################
options("rgdal_show_exportToProj4_warnings"="none")
options(stringsAsFactors = FALSE)
options(digits = 15)

library('stringr')
library('sf')
library('tidyr')    
# library('raster')    # extent-function is in this package
library('leaflet')
library('dplyr')
library('sfheaders') # https://stackoverflow.com/questions/61215968/creating-sf-polygons-from-a-dataframe
library('leafem')
library('geosphere')
library('nngeo')


sf::sf_use_s2(FALSE)

my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")


try(source("Grid-Generate_clusters.R"))
try(source("Grid-Generate_plots.R"))
try(source("Grid-Generate_plot_polygons.R"))

writeOutputFiles <- function(clusters, plot_points, plot_polygons) {   
  prevWD <- getwd()
  tmpdir <- tempdir()
  setwd(tempdir())
  
  if (!file.exists(".//maps")){
    dir.create(file.path('.', 'maps'), showWarnings = FALSE)
  }
  
  clusters       <- clusters %>% dplyr::select(Cluster_ID,	RowNo, ColNo, zone,	X_map, Y_map, X, Y)
  clusters_df    <- st_drop_geometry(clusters)
  plot_points_df <- st_drop_geometry(plot_points)
  
  
  # cluster point data
  st_write(clusters_df, "cluster_point.csv", delete_dsn=TRUE)
  st_write(clusters, dsn="maps", layer="cluster_point", driver="ESRI Shapefile", overwrite_layer = TRUE, append=FALSE )

  # plot point data
  st_write(plot_points_df, "plot_points.csv", delete_dsn=TRUE)
  # 
  # # create plot input data for Collect Earth
  if (nrow(plot_points_df) == nrow(clusters_df)) {
     id <- c(plot_points_df$Cluster_ID)
   } else {
     id <- c(paste( plot_points_df$Cluster_ID, "_", plot_points_df$Plot_ID, sep="" ))
  }
  ceDF <- data.frame(id, stringsAsFactors = FALSE)
  ceDF$YCoordinate <- plot_points_df$Y
  ceDF$XCoordinate <- plot_points_df$X
  ceDF$elevation <- 0; ceDF$aspect <- 0; ceDF$slope <- 0
  st_write(ceDF, "plot_points_CE.csv", delete_dsn=TRUE)
  
  # plot point data, shape
  st_write(plot_points, dsn="maps", layer="plot_point", driver="ESRI Shapefile", overwrite_layer = TRUE, append=FALSE)
  # # plot polygon data, shape
  st_write(plot_polygons, dsn="maps", layer="plot_polygons", driver="ESRI Shapefile", overwrite_layer = TRUE, append=FALSE)
  
  setwd(prevWD)
} 

# see https://www.rdocumentation.org/packages/vetools/versions/1.3-28/topics/get.shape.range
get.shape.range <- function (shape) 
{
  
  SHP.range = matrix(ncol = 4, nrow = length(shape))
  for (i in 1:length(shape)) {
    d = slot(shape, "polygons")[[i]]
    SHP.sub = matrix(ncol = 4, nrow = length(slot(d, "Polygons")))
    for (j in 1:length(slot(d, "Polygons"))) {
      d.sub = slot(d, "Polygons")[[j]]
      d.sub = slot(d.sub, "coords")
      SHP.sub[j, 1:2] = range(d.sub[, 1])
      SHP.sub[j, 3:4] = range(d.sub[, 2])
    }
    d = matrix(apply(SHP.sub, 2, range), ncol = 4)
    SHP.range[i, 1:2] = diag(d[1:2, 1:2])
    SHP.range[i, 3:4] = diag(d[1:2, 3:4])
  }
  d = matrix(apply(SHP.range, 2, range), ncol = 2)
  d = matrix(apply(d, 2, range), ncol = 4)
  colnames(d) <- c("Long.start", "Long.end", "Lat.start", 
                   "Lat.end")
  return(d)
}


enableBookmarking("url")
options(shiny.maxRequestSize=35*1024^2) 
map_projection = 0

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

target_distance <- function(shape,no_target_clusters, input_hexa_equaldist){
  if (no_target_clusters > 2 & !is.null(shape)) {
    
    target_grid <- st_sample(shape, size=no_target_clusters, type='regular')

    target_dist <- getmode(diff(st_coordinates(target_grid)[,1]))
    rm(target_grid)
    if (input_hexa_equaldist) {target_dist = target_dist * 1/sqrt(sqrt(3/4)) }
      
    return(round(target_dist,2))
  } else {
    return(0)
  }
}


server <- function(input, output, session) {

    shape <- reactive({
    if (!is.null(input$shpFile)){
      shpDF <- input$shpFile
      if (nrow(shpDF) < 4) {
        cat(" Not all 4 files read /n")
        shpDF <- NULL
        return()  
      }
      
       prevWD <- getwd()
       uploadDirectory <- dirname(shpDF$datapath[1])
       setwd(uploadDirectory)
       for (i in 1:nrow(shpDF)){
         file.rename(shpDF$datapath[i], shpDF$name[i])
       }
       shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
       fName   <- substr(shpName,1, nchar(shpName)-4)
       if (is.vector(fName)) {
         shpName <- as.character( shpName[1] )
         fName   <- as.character( fName[1] )
       } else {
         return()
       }
#       print(str(fName))
       shpPath <- paste(uploadDirectory, shpName, sep="/")

       setwd(prevWD)
       shpFile <- sf::st_read(dsn=shpPath, layer= fName)

      
       # antimeridian case processing
       # https://stackoverflow.com/questions/55162548/is-there-a-better-way-for-handling-spatialpolygons-that-cross-the-antimeridian
       # shape <- shape %>%
       #   st_wrap_dateline(options = c("WRAPDATELINE=YES"), quiet = TRUE) %>%
       #   st_union()
       
       print("Shape imported")
       output$map_proj1 <- renderPrint({ st_crs(shpFile)$proj4string })
       return(shpFile)
    } else {
        return()
    }
  })
  
  shape4326 <- reactive({
    shape = shape()
    if (is.null(shape)) return(NULL)
    
    print("map_projection missing?")
 #   print(st_crs(shape))

    map_projection <- as.integer( sf::st_crs(shape)$epsg )
#    print(map_projection)
    if (is.na(map_projection)) {
      output$map_proj1 <- renderPrint({ "Missing or unsupported CRS" })
      return(NULL)
    }
    
    if (map_projection != 4326) { 
        shape = st_transform( shape, 4326 ) 
        updateNumericInput(session, "cluster_x_distance", value=100000)
        updateNumericInput(session, "cluster_y_distance", value=100000)
      } else {
        updateNumericInput(session, "cluster_x_distance", value=10)
        updateNumericInput(session, "cluster_y_distance", value=10)
      }
    
    
    return( shape )
  })
  
  # Insert and delete plots  
  inserted_div_ids <- c()
  inserted_ids <- c()
  
  observeEvent(input$insertPlotBtn, {
    id <- input$insertPlotBtn # incremental number
    insertPlotRow(id, 0, 0)
  }, ignoreInit = TRUE)
  

  output$fileUploaded <- reactive({
     if (!is.null(shape())) {
       shape = shape()
       return(!is.null(shape))
     } else {
       return(FALSE)
     }

  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  
  # https://stackoverflow.com/questions/49186866/using-checkbox-to-enable-disable-output
  react <- reactive({
    shape = shape()
    (input$use_map_extent==TRUE | input$use_clipping_map==TRUE) & !is.null(shape)
  })

   
  observeEvent(react(), {
    
    if (react()) {
      shinyjs::hide("minx")
      shinyjs::hide("maxx")
      shinyjs::hide("miny")
      shinyjs::hide("maxy")
    } else {
      shinyjs::show("minx")
      shinyjs::show("maxx")
      shinyjs::show("miny")
      shinyjs::show("maxy")
    }
  })


  observeEvent(shape4326(), {
    leafletProxy("plotmap") %>% clearShapes()
    # Clear the map elements
    leafletProxy("plotmap") %>% clearGroup("Cluster points")
    leafletProxy("plotmap") %>% clearGroup("Plot points")
    leafletProxy("plotmap") %>% clearGroup("Plot shapes")

    map = shape4326()

    leafletMap = leafletProxy("plotmap", data = map) %>%
      addPolygons(
        fillColor = "transparent",
        weight = 1,
        opacity = 0.7
      )
    
    # https://stackoverflow.com/questions/51392181/setting-boundaries-of-leaflet-with-sf-geometry-in-r
    e <- st_bbox(map) %>% as.vector()
    
    leafletMap %>% 
      addTiles() %>% 
      fitBounds(e[1], e[2], e[3], e[4])

  }, ignoreInit = TRUE)

  
  observeEvent(input$target_dist_button, {
      input_hexa_equaldist = FALSE
      if (input$cluster_hexagonal & input$cluster_hexagonal_dist) input_hexa_equaldist = TRUE 
      outText1 <- target_distance(shape(), input$target_clusters, input_hexa_equaldist)
      updateTextInput(session, "target_distance", value = outText1)
  })
  
  
    nDistance <- reactive( {
    new <- input$target_distance
    new <- ifelse(is.na(new) | is.null(new) | trimws(new)=="", "", new )
    new
  })
  
  observeEvent(input$applydistance, {
    if (nDistance() != "") {
      estim_distance <- nDistance()
      print(estim_distance)
      updateNumericInput(session, "cluster_x_distance", value=estim_distance)
      updateNumericInput(session, "cluster_y_distance", value=estim_distance)
    }
  })  
  

  observe( {
    if (input$cluster_hexagonal) {
      shinyjs::show("cluster_hexagonal_dist")
      shinyjs::toggleState("cluster_hexagonal_dist", input$cluster_hexagonal)
    } else {
      shinyjs::hide("cluster_hexagonal_dist")  
    }
    })

  observe( {
    if (input$cluster_hexagonal & input$cluster_hexagonal_dist) {
      shinyjs::disable("cluster_y_distance")
    } else {
      shinyjs::enable("cluster_y_distance")  
    }
  })
  
#  observe( {
#    if (input$drop_rowcol) {
#      shinyjs::show("dropfactor")
#    } else {
#      shinyjs::hide("dropfactor")
#    }
#  })
  
  
    

  observeEvent(input$run, {
    if (!is.null(shape())) map <- shape()
    
    if (exists('map') & (input$use_map_extent | input$use_clipping_map)) {
      e     <- st_bbox(map)
      min_x <- e$xmin
      min_y <- e$ymin
      max_x <- e$xmax
      max_y <- e$ymax
      rm(e)
      map_projection <- as.integer( st_crs(map)$epsg )
    } else {
      min_x <- input$minx
      min_y <- input$miny
      max_x <- input$maxx
      max_y <- input$maxy
      map_projection <- as.integer(substring(trimws(input$projection), 6,))
    }
    
    # the next line will show CRS also if no shape file is loaded
    output$map_proj1 <- renderPrint({ st_crs(map_projection)$proj4string })

    # Too many arguments - will refactor
    # clusters is a sf object
    print(map_projection)
    print("###")
    
    clusters <- generate_clusters(
      cluster_start_random = input$cluster_start_random,
      cluster_hexagonal = input$cluster_hexagonal,
      cluster_hexagonal_dist = input$cluster_hexagonal_dist,
      map_projection,
      min_x = min_x,
      max_x = max_x,
      min_y = min_y,
      max_y = max_y,
      cluster_x = input$cluster_x,
      cluster_y = input$cluster_y,
      cluster_x_distance = input$cluster_x_distance,
      cluster_y_distance = input$cluster_y_distance,
      first_cluster_id = input$first_cluster_id
    )
    
    ## Clipping by using shape ----

    if ( input$use_clipping_map & exists('map')) {
      tryCatch({
#        sf::sf_use_s2(FALSE)  # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

        if (map_projection != 4326) { 
            map_clip <- st_transform(map, 4326) 
        } else { 
            map_clip <- map
        }
        # https://community.rstudio.com/t/shinyapps-working-locally-but-failing-on-build-related-to-sf/78245/3
        map_clip <- sf::st_buffer(map_clip,0)
        # do clipping
        print("DOING CLIPPING")

        clusters <- st_intersection(clusters, map_clip)
        rm(map_clip)
        },
        # but if an error occurs
        error = function(error_message) {
          print("Failed st_itersection")
          return(NA)
        }
      )
      # renumber clusters
      clusters$Cluster_ID <- 1:nrow(clusters) + input$first_cluster_id - 1
    }
    
    clusters$Cluster_ID <- as.integer(clusters$Cluster_ID)

    #    if (is.na(clusters)) {break}
    print("END CLUSTER GENERATING")
    
    # Clear the map
    leafletProxy("plotmap") %>% clearGroup("Cluster points")
    leafletProxy("plotmap") %>% clearGroup("Plot points")
    leafletProxy("plotmap") %>% clearGroup("Plot shapes")
    # Add cluster point markers
    
    print("MAIN 1")

    leafletProxy("plotmap", data = st_drop_geometry(clusters)) %>%
      addCircleMarkers(~X,~Y,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.5,
        group = "Cluster points"
      )
    
    print("MAIN 2")
    # Generate plot points
    # Create three separate (ids, x, and y) vectors from plot coordinates to feed the generate_plots function
    ids <- c()
    xs  <- c()
    ys  <- c()
    # xy_swap <- c()
    
    # sum function is used to eliminate NA cases
    for (i in inserted_ids) {
      ids     <- c(ids, sum(0, as.numeric(input[[paste0("plot_id_input_", i)]]), na.rm=T) )
      xs      <- c(xs,  sum(0, as.numeric(input[[paste0("plot_x_input_",  i)]]), na.rm=T) )
      ys      <- c(ys,  sum(0, as.numeric(input[[paste0("plot_y_input_",  i)]]), na.rm=T) )
      # if ( input$plot_type=="R" & (input$plot_dim_1 != input$plot_dim_2)) {
      #   xy_swap <- c(xy_swap, input[[paste0("plot_xy_input_",  i)]])
      # } else {
      #   xy_swap <- c(xy_swap, FALSE)
      # }
    }
    
    
    if (length(inserted_ids)==0) {
      ids <- c(1)
      xs  <- c(0)
      ys  <- c(0)
#      xy_swap <- c(FALSE)
    }
    
    plot_list <- data.frame(ids, xs, ys)
    names(plot_list) <- c("No", "X", "Y") 
    # plot_list <- data.frame(ids, xs, ys, xy_swap)
    # names(plot_list) <- c("No", "X", "Y", "xy_swap") 
    
    
    # plot_points is a sf object
    plot_points <- generate_plots(
      clusters = clusters,
      Plot_type=input$plot_type,
      plot_list,
      map_projection
    )

    if (exists("clusters")) {
      output$number_clusters <- renderText({ paste0("Number of clusters: ", nrow(clusters)) }) }

    if (exists("plot_points")) {
      output$number_plots    <- renderText({ paste0("Number of plots: ", nrow(plot_points)) }) }

    print(str(plot_points))
    plot(plot_points)
    
    # Add plot point markers
    leafletProxy("plotmap", data = st_drop_geometry(plot_points)) %>%
        addCircleMarkers(~X,~Y,
          radius = 2,
          color = "#ff0000",
          stroke = FALSE,
          fillOpacity = 0.5,
          group = "Plot points"
        )
    
  
    # Generate plot polygons

    print("MAIN 3")
    plot_polygons <- generate_plot_polygons(
      p = plot_points,
      Plot_type =input$plot_type,
      PlotCorner_X =input$plot_corner_x,
      PlotCorner_Y =input$plot_corner_y,
      Plot_ref_point =input$plot_reference_point,
      Plot_dim1 =input$plot_dim_1,
      Plot_dim2 =input$plot_dim_2,
      map_projection
    )

    writeOutputFiles(clusters, plot_points, plot_polygons)
    
    
    output$downloadDataCSV <- downloadHandler(
      filename = "grid_point_csv.zip",
      content = function(fname) {
        prevWD <- getwd()
        setwd(tempdir())
        file_list <- list.files(pattern = "\\.csv$", full.names = FALSE, ignore.case=TRUE)
        zip( 
          zipfile=fname, 
          files=file_list, 
          zip=my_zip_tools
        )
        setwd(prevWD)
      },
      contentType = "application/zip"
    ) 

    output$downloadDataShape <- downloadHandler(
      filename = "grid_shapes.zip",
      content = function(fname) {
        prevWD <- getwd()
        setwd(tempdir())
        try(setwd("maps"), silent=T)
        file_list <- list.files( full.names = FALSE)
        zip(
          zipfile=fname,
          files=file_list,
          zip=my_zip_tools
        )
        setwd(prevWD)
      },
      contentType = "application/zip"
    )
  
    # Add plot polygon markers
    leafletProxy("plotmap", data = plot_polygons) %>%
      addPolygons(
        fillColor = "transparent",
        weight = 1,
        group = "Plot shapes"
      )
  }, ignoreInit = TRUE);

  
  output$map_proj1 <- renderPrint({ 
    if (map_projection!=0) st_crs(map_projection)$proj4string })
  
  # Add map
  output$plotmap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner Lite"),
        overlayGroups = c("Cluster points", "Plot points", "Plot shapes"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Plot points") %>%
      hideGroup("Plot shapes")  %>%
      leafem::addMouseCoordinates()
  })

  onRestore(function(state) {
    # Get the id field names (plot_id_input_1, plot_id_input_2, ...)
    plot_id_field_names <- names(state$input)[grep("plot_id_input", names(state$input))]
    m   <- regexpr('[0-9]+$', plot_id_field_names)
    ids <- regmatches(plot_id_field_names, m)
    # As the id increases when inserting new coords from the UI, we restore the order here
    ids <- sort(ids)
    
    for (i in 1:length(ids)) {
      x = state$input[[paste0("plot_x_input_", ids[i])]]
      y = state$input[[paste0("plot_y_input_", ids[i])]]
      insertPlotRow(ids[i], x, y)
    }
  })
  
  insertPlotRow <- function(plot_id, x, y) {
    div_id <- paste0("plot_div_", plot_id) # wrap in div to ease removal
    plot_id_input_id <- paste0("plot_id_input_", plot_id)
    plot_x_input_id  <- paste0("plot_x_input_", plot_id)
    plot_y_input_id  <- paste0("plot_y_input_", plot_id)
#    plot_xy_input_id <- paste0("plot_xy_input_", plot_id)
    delete_button_id <- paste0("delete_plot_", plot_id)
    
    # Insert id, x and y fields, with related values
    insertUI(
      selector = "#plots",
      ui = tags$div(
        fluidRow(
          # column(width = 2, textInput(plot_id_input_id, "Plot ID", plot_id)),
          # column(width = 3, numericInput(plot_x_input_id, "X", x)),
          # column(width = 3, numericInput(plot_y_input_id, "Y", y)),
          # column(width = 1, actionButton(delete_button_id, "Delete this plot"))
          column(width = 2, textInput(plot_id_input_id, "Plot ID", plot_id)),
          column(width = 3, numericInput(plot_x_input_id, "X", x)),
          column(width = 3, numericInput(plot_y_input_id, "Y", y)),
          # if ( input$plot_type=="R" & (input$plot_dim_1 != input$plot_dim_2)) {
          #   column(width = 3, selectInput("plot_xy_input_id", "Rotate plot",
          #               c("None" = "0",
          #                 "Counter-clockwise (-90)" = "-90",
          #                 "Clockwise (90)" = "90")))},
#            column(width = 3, checkboxInput(plot_xy_input_id, "Swap dimensions", FALSE))},
          column(width = 1, actionButton(delete_button_id, "Delete plot"))
        ),
        hr(),
        id = div_id
      )
    )
    
    # Delete button
    observeEvent(input[[delete_button_id]], {
      pos <- match(c(plot_id), inserted_ids)
      inserted_div_ids <<- inserted_div_ids[-pos]
      inserted_ids <<- inserted_ids[-pos]
      
      removeUI(selector = paste0("#", div_id))
    },
    ignoreInit = TRUE)
    
    inserted_div_ids <<- c(div_id, inserted_div_ids)
    inserted_ids <<- c(plot_id, inserted_ids)
  }

  # readShape2 <- function(shapefile_path, layer) {
  #   tryCatch(
  #     shapes <- st_read(dsn = shapefile_path, layer = layer),
  #     error = function(error_message) {
  #       message("Shape not could not be read")
  #       return(NULL)
  #     }
  #   )
  #   return(shapes)
  # }
  
}
