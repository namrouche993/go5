## code to prepare `CMR_ACQ` dataset goes here


moyens_realisation <-readxl::read_excel(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/Agrements_et_certificat_de_qualifications/moyens_realisations.xlsx"),skip = 1)
moyens_realisation$Arretee=as.Date(moyens_realisation$Arretee)

moyens_realisation2 <-readxl::read_excel(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/Agrements_et_certificat_de_qualifications/moyens_realisations.xlsx"),sheet = "Feuil2",skip = 1)
moyens_realisation2$Arretee=as.Date(moyens_realisation2$Arretee)


etp_categoris59 <-readxl::read_excel(paste0(getwd(),"/data-raw/Construction_Moyens_realisations/Agrements_et_certificat_de_qualifications/moyens_realisations.xlsx"),sheet = "Feuil3")
etp_categoris59$Arretee=as.Date(etp_categoris59$Arretee)




# usethis::use_data(moyens_realisation,moyens_realisation2,etp_categoris59, overwrite = TRUE,internal = TRUE)
