## code to prepare `CMR_DTR` dataset goes here


dtr_files=data.frame(
  Niveau=c(rep("Niv2:Règles de Conception de Calcul",length(list.files(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/DTR/niveau2")))),rep("Niv3: Règle d'Exécution",length(list.files(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/DTR/niveau3"))))),
  fichier=c(list.files(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/DTR/niveau2")),list.files(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/DTR/niveau3")))
)


# usethis::use_data(dtr_files, overwrite = TRUE,internal = TRUE)
