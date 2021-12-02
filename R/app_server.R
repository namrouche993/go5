#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  output$mtable1<-renderTable({
    head(go3::m1)
  })

  output$m333<-renderTable({
    go3::m3
  })

  output$m4441<-renderTable({
    m4
  })

  output$m666<-renderTable({
    m6
  })
}
