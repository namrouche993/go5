## code to prepare `maps_for_algeria_58` dataset goes here


algeria580 <- geojsonio::geojson_read(paste0(getwd(),"/data-raw/geoData.geojson"), what = "sp")
algeria58 <- rmapshaper::ms_simplify(algeria580, keep = 0.05, keep_shapes = TRUE)



indicateurWilaya <- readxl::read_excel(paste0(getwd(),"/data-raw/indicateurWilaya.xlsx"),skip=1)

indicateurWilaya <- indicateurWilaya %>% arrange(c(1,28,24,50,43,39,44,12,34,36,5,49,
                                                   15,21,35,33,38,42,40,18,55,14,54,52,
                                                   57,32,23,37,19,8,17,20,7,41,58,53,11,
                                                   26,47,48,51,31,56,29,13,16,27,22,2,3,
                                                   30,10,9,4,46,6,45,25))


wilayas58 <- readxl::read_excel(paste0(getwd(),"/data-raw/wilayas58.xlsx"))



wilayas58$field=round(rnorm(58,1500,400))


wilayas58=wilayas58 %>% arrange(c(1,28,24,50,43,39,44,12,34,36,5,49,
                                  15,21,35,33,38,42,40,18,55,14,54,52,
                                  57,32,23,37,19,8,17,20,7,41,58,53,11,
                                  26,47,48,51,31,56,29,13,16,27,22,2,3,
                                  30,10,9,4,46,6,45,25))



gps=data.frame(longitude=rep(0,58),latitude=rep(0,58))
gps=data.frame(longitude=rep(0,58),latitude=rep(0,58))
for(i in 1:58){
  gps[i,]=algeria58@polygons[[i]]@labpt
}
algeria58@data$longitude=gps$longitude
algeria58@data$latitude=gps$latitude
algeria58@data$wilayas=wilayas58$wilaya




mapdz58=leaflet::leaflet(algeria58)%>%
  setView(lng = 1.63333 , lat = 28.3667, zoom = 6)%>%
  #clearBounds()%>%

  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 6, maxZoom = 6,dragging = TRUE)) %>%   #or we can use addProviderTiles(one_providers)
  leaflet.extras::setMapWidgetStyle(list(background= "#ffffff"))


