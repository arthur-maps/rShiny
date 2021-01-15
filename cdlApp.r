library(glue);
library(shiny);library(shinycssloaders);library(shinydashboard);library(shinyWidgets)
library(plotly);library(DT)
library(leaflet);library(leaflet.extras)
library(dplyr);library(sp);library(rgdal)

## CDL legend URL
legenduri <- "https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi?version=1.1.1&service=wms&request=getlegendgraphic&layer=cdl_2009&format=image/png"
## the most recent available CDL year
lastyr <- as.numeric(format(Sys.Date(), "%Y")) - 1


ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")

  )

server <- function(input, output, session) {
  
  ## Base Map ####
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.8)) %>%
      addWMSTiles(
        baseUrl = "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDM.wms",
        layers = "surveyareapolyoutline", group = "Soil Counties",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 crs = leafletCRS(crsClass = "L.CRS.EPSG4326")),
        attribution = sprintf('<a href="https://sdmdataaccess.sc.egov.usda.gov">USDA NRCS Soil Data Mart database</a>. Accessed [%s]', Sys.Date())
      )-> p
    #cdl.yr <- 2015:lastyr
    cdl.yr <- 2015:2019
    iter <- 0
    repeat{
      iter <- iter + 1
      p <- p %>%
        addWMSTiles(
          baseUrl = "https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi",
          layers = paste("cdl", cdl.yr[iter], sep = "_"),
          group = paste("Cropland Data Layer", cdl.yr[iter], sep = " - "),
          options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                   crs = leafletCRS(crsClass = "L.CRS.EPSG4326")),
          attribution = sprintf('<a href="https://nassgeodata.gmu.edu/CropScape/">USDA NASS Cropland Data Layer</a>. {%s}', cdl.yr[iter])
        )
      if(iter >= length(cdl.yr)) break
    }
    p <- p %>%
      addWMSLegend(uri = legenduri, position = "topright", layerId = "CDL legend") %>%
      addLayersControl(
      overlayGroups = c("Soil Counties"),  
      #overlayGroups = c("Soil Counties", "Map Unit Polygon", "Bounding Box"),
        baseGroups = c(paste0("Cropland Data Layer - ", cdl.yr),"None"),
        options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE),
        position = "bottomleft"
      ) %>%
      hideGroup(c("SDL - Map Unit Polygon", "SDL - Survey Area Polygon")) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")) ) %>%
      addSearchOSM() %>%
      setView(-93.65, 42.0285, zoom = 12) %>%
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = TRUE,
        rectangleOptions = TRUE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        # markerOptions = drawMarkerOptions(markerIcon = myMarkerIcon(2)),
        singleFeature = TRUE,
        editOptions = editToolbarOptions()
      )
    p
  })

}


shinyApp(ui=ui, server=server)
