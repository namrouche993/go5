## code to prepare `Villes` dataset goes here
data_ville=data.frame(matrix(0,nrow=7,ncol=13))
data_ville$X1=c("BOUINANE","SIDI ABDELLAH","BOUGHEZOUL","DRAA ERICH","ALI MENDJLI","EL MENEAA","HASSI MESSAOUD")
colnames(data_ville)=c("Nom","Wilaya_Matricule","Wilaya","zoom","setview_long","setview_lat","cor_long","cor_lat","logements","habitants","equipements","energie","transport")
data_ville$Wilaya_Matricule=c("09-BLIDA","16-ALGER","17-26 DJELFA MEDEA","23-ANNABA","25-Constantine","47-GHARDAIA","30-Ouargla")
data_ville$Wilaya=c("BLIDA","ALGER","DJELFA MEDEA","ANNABA","Constantine","GHARDAIA","OUARGLA")


#providers[[1]]  #OpenStreetMap.Mapnik
#providers[[44]] # ne s'affiche pas quand on zoom plus
#providers[[55]] # ne s'affiche pas quand on zoom plus
#providers[[57]]  #Satellite


####Sidi abdellah
data_ville[2,4]=14
data_ville[2,5:6]=c(2.853174,36.685945)
data_ville[2,7:8]=c(2.853174,36.689245)

#Bouinane
data_ville[1,4]=15
data_ville[1,5:6]=c(2.957531,36.533262)
data_ville[1,7:8]=c(2.957531,36.533262)

#BOUGHEZOUL
data_ville[3,4]=13
data_ville[3,5:6]=c(2.848758,35.721653)
data_ville[3,7:8]=c(2.848758,35.741653)



#DRAA ERRICH
data_ville[4,4]=13
data_ville[4,5:6]=c(7.528319,36.858245)
data_ville[4,7:8]=c(7.528319,36.872245)

#ALI MENDJLI
data_ville[5,4]=14
data_ville[5,5:6]=c(6.572546,36.249052)
data_ville[5,7:8]=c(6.572546,36.254052)

#EL MENEAA
data_ville[6,4]=14
data_ville[6,5:6]=c(2.905987,30.594278)
data_ville[6,7:8]=c(2.915987,30.594278)


#HASSI MESSAOUD
data_ville[7,4]=14
data_ville[7,5:6]=c(5.84492,32.34238)
data_ville[7,7:8]=c(5.84492,32.34238)

# usethis::use_data(data_ville, overwrite = TRUE,internal = TRUE)
