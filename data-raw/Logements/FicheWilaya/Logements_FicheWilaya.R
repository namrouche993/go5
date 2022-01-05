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





ab5=data.frame(matrix(0,nrow=2,ncol=7))
colnames(ab5)=c("Segment","LPL","LSP/LPA","Rural","Location-Vente","LPP","Total")
ab5[,1]=c("Prévus","Livrés")

ab6=data.frame(matrix(0,nrow=2,ncol=7))
colnames(ab6)=c("Segment","LPL","LSP/LPA","Rural","Location-Vente","LPP","Total")
ab6[,1]=c("Prévus","Lancés")


ab=data.frame(matrix(0,nrow=8,ncol=7))
ab[,1]=c("LPL","LSP/LPA","Rural","Location-Vente","LPP","LV CNEP Banque","ACLS","Total")
colnames(ab)=c("Segment","Consistance","Acheves","En Cours","Dont A l'Arret","Non Lances","Notifie 2020")

ab_suite=data.frame(matrix(0,nrow=3,ncol=7))
ab_suite[,1]=c("Autre social","Promotionnel Libre","Programme Global")
colnames(ab_suite)=c("Segment","Consistance","Acheves","En Cours","Dont A l'Arret","Non Lances","Notifie 2020")





# df2_ab=data.frame(matrix(0,nrow=6*48,ncol=9))
# colnames(df2_ab)=c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés","Notifie","Arrétée")
# df2_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya),6)
# #df2_ab$Arrétée="2021-12-31"
# df2_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP"),each=48)




# df2_ab=data.frame(matrix(0,nrow=6*61,ncol=9))
# colnames(df2_ab)=c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés","Notifie","Arrétée")
# df2_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya),6)
# #df2_ab$Arrétée="2021-12-31"
# df2_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP"),each=61)



df2_ab=data.frame(matrix(0,nrow=9*61,ncol=9))
colnames(df2_ab)=c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés","Notifie","Arrétée")
df2_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya)[c(1:58,59,60,61)],9)
#df2_ab$Arrétée="2021-12-31"
df2_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP","ACLS","Autre social","Promotionnel Libre"),each=61)



#
# df3_ab=data.frame(matrix(0,nrow=6*48,ncol=11))
# colnames(df3_ab)=c("Wilaya",
#                    "Segment",
#                    "Livraisons Réalisés","Livraisons Prévus","Taux de Livraisons",
#                    "Lancements Réalisés","Lancements Prévus","Taux de Lancements",
#                    "Annulées","Notifié","Trimestre"
#                    )
# df3_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya),6)
# #df3_ab$Arrétée="2021-12-31"
# df3_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP"),each=48)



df3_ab=data.frame(matrix(0,nrow=9*61,ncol=11))
colnames(df3_ab)=c("Wilaya",
                   "Segment",
                   "Livraisons Réalisés","Livraisons Prévus","Taux de Livraisons",
                   "Lancements Réalisés","Lancements Prévus","Taux de Lancements",
                   "Annulées","Notifié","Saison"
)

df3_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya)[c(1:58,59,60,61)],9)
#df3_ab$Arrétée="2021-12-31"
df3_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP","ACLS","Autre social","Promotionnel Libre"),each=61)



ab7=data.frame(matrix(0,nrow=7,ncol=9))
colnames(ab7)=c("Segment","Livres","Prevus_livres","Taux_livraison","Lances","Prevus_lances","Taux_lancement","Annules","Notifie")
ab7[1:7,1]=c("LPL","LSP/LPA","Rural","Location-Vente","LPP","LV CNEP Banque","ACLS")


ab7_suite=data.frame(matrix(0,nrow=2,ncol=9))
colnames(ab7_suite)=c("Segment","Livres","Prevus_livres","Taux_livraison","Lances","Prevus_lances","Taux_lancement","Annules","Notifie")
ab7_suite[1:2,1]=c("Autre social","Promotionnel Libre")




ab2=data.frame(matrix(0,nrow=1,ncol=8))
colnames(ab2)=c("Segment","Consistance Actuelle","Acheves","En Cours","Non Lances","AP","Consomomations","Taux")
ab2[1,1]="Nombre d'aides"





ab3=data.frame(matrix(0,nrow=1,ncol=10))
colnames(ab3)=c("Acheves","En Cours","Non Lances","Total","Achevee","En cours","Non lances","TotaI","Acheves à 60% et plus","Total general")

ab4=data.frame(matrix(0,nrow=6,ncol=2))
#colnames(ab4)=c("Acheves","En Cours","Non Lances","Total","Achevee","En cours","Non lances","TotaI","Acheves à 60% et plus","Total general")
ab4[,1]=c("Logements achevés, viabilisation achevés","Logements achevés, viabilisation en cours","Logements acheves, viabilisation non entamée","TOTAL","Logements dépassant 60% de taux d'avancement pouvant être pré-affectés","Total logements à attribuer et à pré-affecter")
############ fiche wilaya end






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
