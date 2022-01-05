## code to prepare `CMR_UF` dataset goes here
datamc <- readxl::read_excel(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/Unite_de_fabrication/materiaux_construction.xlsx"))
datamc$`Année d'entrée en production`=as.numeric(datamc$`Année d'entrée en production`)

datamc$Arretee=as.Date(datamc$Arretee)


# usethis::use_data(datamc, overwrite = TRUE)
