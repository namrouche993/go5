## code to prepare `Logements_FicheWilaya` dataset goes here

data_fiche_wilaya <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"))
data_fiche_wilaya$arretee=as.Date.character(data_fiche_wilaya$arretee)


details_encours_lpl_lsp_lv_lpp <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "details_encours_lpl_lsp_lv_lpp")
details_encours_lpl_lsp_lv_lpp$arretee=as.Date.character(details_encours_lpl_lsp_lv_lpp$arretee)


details_nonlances_lpl <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "details_nonlances_lpl")
details_nonlances_lpl$arretee=as.Date.character(details_nonlances_lpl$arretee)

details_nonlances_lsp <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "details_nonlances_lsp")
details_nonlances_lsp$arretee=as.Date.character(details_nonlances_lsp$arretee)


details_nonlances_rural <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "details_nonlances_rural")
details_nonlances_rural$arretee=as.Date.character(details_nonlances_rural$arretee)

attribution_fw<-readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "attribution")
attribution_fw$arretee=as.Date.character(attribution_fw$arretee)


patrimoine_fw <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "Patrimoine")
patrimoine_fw$arretee=as.Date.character(patrimoine_fw$arretee)


etat_cession_fw <- readxl::read_excel(paste0(getwd(),"/data-raw/Logements/FicheWilaya/data_fiche_wilaya.xlsx"),sheet = "Etat de la cession")
etat_cession_fw$arretee=as.Date.character(etat_cession_fw$arretee)





gt_attribution=data.frame(matrix(0,ncol=9,nrow=3))
colnames(gt_attribution)=c("Etat",unique(attribution_fw$Segment),"Total")
gt_attribution$Etat=c("Attribué","Prévu","Taux")



aides_reha=data.frame(matrix(0,ncol=1+length(unique(data_fiche_wilaya$arretee)),nrow=3))
colnames(aides_reha)=c("t",unique(as.character(data_fiche_wilaya$arretee)))
aides_reha[,1]=c("tranférées au rural","Reste à affecter","Total")


######### Tips : à inverser prochainement :
aides_reha[,2]=c(1300,1200,95000)
aides_reha[,3]=c(1300,2948,95000)
aides_reha[,4]=c(1300,2344,95000)
aides_reha[,5]=c(1300,3275,95000)
aides_reha[,6]=c(1300,1545,95000)
aides_reha[,7]=c(1300,2948,95000)

#
# usethis::use_data(
#
#   data_fiche_wilaya,
#   details_encours_lpl_lsp_lv_lpp,
#   details_nonlances_lpl,
#   details_nonlances_lsp,
#   details_nonlances_rural,
#   attribution_fw,
#   patrimoine_fw,
#   etat_cession_fw,
#   gt_attribution,
#   gt_attribution,
#   aides_reha
#
#   , overwrite = TRUE,internal = TRUE)
