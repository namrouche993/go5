#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import leaflet.providers
#' @import leaflet.extras
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  output$mtable1<-renderTable({
    head(before00,2)
  })

  output$m333<-renderTable({
    gt_attribution
  })

  output$m4441<-renderTable({
    head(patrimoine_fw,2)
  })

  output$m666<-renderTable({
    paste(algeria@data$coc)
  })

  output$plot1<-renderPlot({
    barplot(1:10, col = blues_pal(seq(0,1,length.out=10)))
  })

  output$plot1<-renderPlot({
    hist(mtcars$mpg,col=blues_pal(seq(0,1,length.out=5)))
  })


  output$leaflet1<-renderLeaflet({
    leaflet(algeria)%>%
      setView(lng = 1.63333 , lat = 28.3667, zoom = 5)%>%
      addProviderTiles("OpenStreetMap.BZH") %>%
      setMapWidgetStyle(list(background= "#ffffff")) %>%
      addPolygons()
  })


  output$leaflet58<-renderLeaflet({
    leaflet(algeria58)%>%
      setView(lng = 1.63333 , lat = 28.3667, zoom = 5)%>%
      addProviderTiles("OpenStreetMap.BZH") %>%
      setMapWidgetStyle(list(background= "#ffffff")) %>%
      addPolygons()
  })


}
