## code to prepare `Urbanisme` dataset goes here
sit_fin <- readxl::read_excel(paste0(getwd(),"/data-raw/Urbanisme/Situation Financiere et Loi 18-05 .xlsx"))
zones <- readxl::read_excel(paste0(getwd(),"/data-raw/Urbanisme/zones2.xlsx"))
zones00=zones
zones=zones[1:48,]



pos <- readxl::read_excel(paste0(getwd(),"/data-raw/Urbanisme/pos.xlsx"))
pos$`Non Lancées`[which(pos$URBANISME=='PDAU')]=rep(NA,48)
pos2=pos %>%
  mutate(Wilaya=rep(unique(livraison_wilayas$waw),3)) %>%
  select(Wilaya,URBANISME,Achevées,`En Cours`,`Non Lancées`)

pos3=matrix(0,ncol=5,nrow=147)
pos3=data.frame(pos3)
colnames(pos3)=colnames(pos2)
pos3=pos2[1:48,]
pos3[49,1]="Total"
pos3[49,2]="POS"
pos3[49,3]=6539
pos3[49,4]=372
pos3[49,5]=9

pos3[50:98,]=pos2[49:97,]
pos3[98,1]="TOTAL"
pos3[98,2]="PDAU"
pos3[98,3]=1508
pos3[98,4]=33
pos3[98,5]=NA

pos3[99:146,]=pos2[97:144,]
pos3[147,1]="TOTAL"
pos3[147,2]="EGU"
pos3[147,3]=1424
pos3[147,4]=251
pos3[147,5]=33
#
pos5=pos3
pos5=pos3[1:49,]
pos5[,6:8]=pos3[50:98,3:5]
pos5[,9:11]=pos3[99:147,3:5]
pos5=pos5[,-c(2,8)]

s1=sit_fin[1:48,2:5]
s1[,5:7]=sit_fin[49:96,3:5]
s1[,8:10]=sit_fin[97:144,3:5]
s1[,11:13]=sit_fin[145:192,3:5]
s1[49,1]="Total"
for(i in 2:ncol(s1)){
  s1[49,i]=sum(s1[1:48,i])
}

sit_fin1=sit_fin[,c(2,3,6)] %>% spread(key = Type,value = NOTIFICATION)
sit_fin2=sit_fin[,c(2,4,6)] %>% spread(key = Type,value = INSCRIPTION)
sit_fin3=sit_fin[,c(2,5,6)] %>% spread(key = Type,value = Reliquat)

ss=data.frame(sit_fin1[,1:2],sit_fin2[,2],sit_fin3[,2]
              , sit_fin1[,3],sit_fin2[,3],sit_fin3[,3],
              sit_fin1[,4],sit_fin2[,4],sit_fin3[,4],
              sit_fin1[,5],sit_fin2[,5],sit_fin3[,5])
ss1=ss %>% select(1,2,5,8,11,3,6,9,12,4,7,10,13)


#
# usethis::use_data(
#
#   sit_fin,zones,zones00,
#   pos,pos5,
#   s1,ss,ss1,
#
#   overwrite = TRUE,internal = TRUE)
