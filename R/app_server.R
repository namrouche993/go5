#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import leaflet.providers
#' @import leaflet.extras
#' @import tidyverse
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  output$mtable1<-renderTable({
    head(go5::m1,2)
  })

  output$m333<-renderTable({
    head(go5::m3,2)
  })

  output$m4441<-renderTable({
    head(m4,2)
  })

  output$m666<-renderTable({
    head(m6,2)
  })

  output$plot1<-renderPlot({
    hist(mtcars$mpg,col=blues_pal(seq(0,1,length.out=5)))
  })


  output$leaflet1<-renderLeaflet({
    leaflet(go5::countries)%>%
      setView(lng = 1.63333 , lat = 28.3667, zoom = 5)%>%
      addProviderTiles("OpenStreetMap.BZH") %>%
      setMapWidgetStyle(list(background= "#ffffff")) %>%
      addPolygons()
  })

}
