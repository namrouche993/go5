## code to prepare `Logements_Logement` dataset goes here


livraison_wilayas <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/Logement/livraison_wilayas.xlsx"))
livraison_wilayas=livraison_wilayas%>%
  filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP/LPA"
                                  ,"ACLS"
                                  ,"Location-Vente"))


estimation_tolpopparc <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/Logement/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))
before2000 <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/Logement/before2000.xlsx"))
before00=before2000



lancement_wilayas <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/Logement/lancement_wilayas.xlsx"))

lancement_wilayas=lancement_wilayas %>%
  #filter(Segment %in% c( "LPL","Rural","LPP","LSP/LPA","Location-Vente")) %>%
  select(c(2,3,4,5)) %>%
  mutate(Annee=as.numeric(Annee))


estimation_tolpopparc[,3:5]=round(estimation_tolpopparc[,3:5])


# usethis::use_data(livraison_wilayas,
#                   estimation_tolpopparc,
#                   before00,
#                   lancement_wilayas,
#
#                   overwrite = TRUE,internal = TRUE)
