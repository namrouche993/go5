## code to prepare `EquipementsPublic` dataset goes here
Statistiques_des_projets_par_secteurs <- readxl::read_excel(paste0(getwd(),"/data-raw/EquipementsPublic/Statistiques des projets par secteurs.xlsx"))
Statistiques_des_projets_par_secteurs$Arretee=as.Date.character(Statistiques_des_projets_par_secteurs$Arretee)

equip=Statistiques_des_projets_par_secteurs[,-1]
colnames(equip)[4]="En Cours"
colnames(equip)[2]="Nbre de Projets"
colnames(equip)[3]="Acheves"
colnames(equip)[5]="Non Lances"
colnames(equip)[7]="Arretee"
colnames(equip)[8]="Dont NIR"
colnames(equip)[9]="Geles"


equip11=equip[,c(1,6,2,3,4,5,8,9,7)]


# data_equip = equip %>%
#   group_by(Wilaya,Arretee) %>%
#   summarise("Nbre de Projets"=sum(`Nbre de Projets`),Acheves=sum(Acheves),"En Cours"=sum(`En Cours`),"Non Lances"=sum(`Non Lances`),"Dont NIR"=sum(`Dont NIR`),"Geles"=sum(Geles)) %>%
#   select(1,3,4,5,6,7,8,2)


#
# usethis::use_data(equip,equip11, overwrite = TRUE,internal = TRUE)
