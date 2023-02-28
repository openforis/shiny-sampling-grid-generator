
library('shinydashboard')
library('leaflet')
library('shinyjs')

ui <- function(request) {
  
  header <- dashboardHeader(
    title = "Cluster and Plot Grid Design Tool",
    titleWidth = 350
  )

  body <- dashboardBody(

   navbarPage("",
    tabPanel("Application",
     sidebarPanel(
                 
##    fluidRow(
      shinyjs::useShinyjs(),
##      column(width = 3,
          shinydashboard::box(width = NULL, status = "warning",
          h4("Actions"),
          actionButton(inputId = "run", label = "RUN"),
#          bookmarkButton(),
          downloadButton('downloadDataCSV'  , label = 'Download CSVs'),
          downloadButton('downloadDataShape', label = 'Download Shapes')
          
#            checkboxInput("drop_rowcol", "Export subset of rows/columns", FALSE),
#            numericInput("dropfactor", "Keep every Nth row/column", 1, min = 1, max = 100, step = 1,)
        ),

        # Map file parameters
          shinydashboard::box(width = NULL, status = "warning",
          h4("Upload map (optional)"),
          p(tags$small("Choose a polygon shapefile if you want to get the extent from a map")),

          # Input: Select a file ----
          # see https://gist.github.com/RCura/9587685
          fileInput(inputId="shpFile", label="Select shp, shx, dbf and prj", multiple=TRUE, accept=c(".shp",".shx",".dbf",".prj")),
        
          conditionalPanel(
            condition = "output.fileUploaded == true",
            checkboxInput("use_map_projection", "Use map projection", TRUE),
            checkboxInput("use_clipping_map", "Use map for clipping", TRUE)
          ),

          conditionalPanel(
            condition = "output.fileUploaded == true && input.use_clipping_map == false",
            checkboxInput("use_map_extent", "Get area corner coordinates from map extent", TRUE)
          ),
          
          textOutput("map_proj1")
        ),
        
        # Cluster parameters
          shinydashboard::box(width = NULL, status = "warning",
          h4("Cluster parameters"),

          conditionalPanel(condition = "(output.fileUploaded == true && input.use_map_projection == false) || output.fileUploaded == false", textInput("projection", "Projection", "EPSG:4326")),
          splitLayout(
            numericInput("minx", "Min X (West)", -170),
            numericInput("maxx", "Max X (East)",  170)),
          splitLayout(
            numericInput("miny", "Min Y (South)", -80),
            numericInput("maxy", "Max Y (North)",  80)),
          splitLayout(
            numericInput("cluster_x_distance", "X distance between clusters", 10),
            numericInput("cluster_y_distance", "Y distance between clusters", 10)),
          tags$small("Distances in meters, or in case of longlat as degrees. ",
                     tags$br(),tags$strong("Note:"),
                       "if using map extent, these must in the same units as map distance units"
          ),
          hr(),
           
          numericInput("first_cluster_id", "1st cluster number in the grid", 1),
          checkboxInput("cluster_start_random", "Randomize first cluster coordinates", FALSE),
#          checkboxInput("cluster_nested", "Create 3 nested grids for Collect Earth survey", FALSE),
          splitLayout(
            checkboxInput("cluster_hexagonal", "Hexagonal grid", FALSE),
            checkboxInput("cluster_hexagonal_dist", "Equal cluster distance (=X)", FALSE)
          ),
          conditionalPanel(
            condition = "!input.cluster_start_random",
            hr(),
            h5("1st cluster ref. point distance from left-upper corner of the area"),
            splitLayout(
              numericInput("cluster_x", "X distance", 0),
              numericInput("cluster_y", "Y distance", 0))
        )),
        hr(),
        # Plot parameters
          shinydashboard::box(width = NULL, status = "warning",
          h4("Plot parameters"),
          p("Plot location in relation to the cluster's reference point"),
          p(tags$small("Give ID, X and Y coordinates")),
          tags$div(id = "plots"),
          actionButton("insertPlotBtn", "Add plot"),
          hr(),
  
          #textInput("plot_list_n", "Plot list ids", "1, 2, 3, 4"),
          #textInput("plot_list_x", "Plot list X coordinates", "250, 250, 750, 750"),
          #textInput("plot_list_y", "Plot list Y coordinates", "250, 750, 750, 250"),
          selectInput("plot_type", "Plot type",
                      c("Circular/cocentric/relascope" = "C",
                        "Rectangular/square" = "R",
                        "NFMA" = "NFMA",
                        "Custom" = "Z")),
          conditionalPanel(
            condition = "input.plot_type != 'NFMA' & input.plot_type != 'Z'",
            selectInput("plot_reference_point", "Plot reference point location",
                        c("Center" = "C",
                          "NW" = "NW",
                          "SW" = "SW"))
          ),
          conditionalPanel(
            condition = "input.plot_type != 'Z'",
            numericInput("plot_dim_1", "Length: Radius or X axis (long.), in meters", 20)
          ),
          conditionalPanel(
            condition = "input.plot_type != 'C' & input.plot_type != 'Z'",
            numericInput("plot_dim_2", "Length: Y axis (lat.), in meters", 20)
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Z'",
            wellPanel(
              p("Corner coordinates"),
              p(tags$small("Do not repeat the first coordinate")),
              textInput("plot_corner_x", "Plot corners' X coordinates", "0, 10, 10, -5"),
              textInput("plot_corner_y", "Plot corners' Y coordinates", "0, 0, 10, 10")
            )
          )
        )
      ),
mainPanel(
#      column(
#        width = 9,
        shinydashboard::box(width = NULL,
            solidHeader = TRUE,
            leafletOutput("plotmap", height = 800)
        ),
        
        shinydashboard::box(width = NULL, status = "warning",
          h4("Plot statistics"),
          tags$br(),
          splitLayout(
            h4(textOutput("number_clusters")),
            h4(textOutput("number_plots"))
          ),
          hr(),
          conditionalPanel(condition = "output.fileUploaded == true",
            splitLayout(
              numericInput("target_clusters", "Targeted number of clusters (optional)", 100),
              actionButton(inputId = "target_dist_button", label = "Estimate"),
              disabled(textInput("target_distance","Target distance"," ")),
              actionButton(inputId = "applydistance", label = "Apply distance")
            )
          )
        )
##            htmlOutput("html_link")
        )
      ),
 tabPanel("About", 
         includeMarkdown("www/releases.md")
         
 )
))
  
  dashboardPage(
    skin='green',
    header,
    dashboardSidebar(disable = TRUE),
    body
  )
  
}
