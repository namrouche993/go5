## code to prepare `my_dataset` dataset goes here
m1=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))
m6=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))

# m1tcars=mtcars
# m3=m1[1:5,c(1,3)]
#
# usethis::use_data(m1,m1tcars,m3, overwrite = FALSE)
#
# m4=m1[1:10,c(1,3)]
# usethis::use_data(m4,m6,overwrite = TRUE,internal = TRUE)

#countries <- geojsonio::geojson_read(paste0(getwd(),"/data-raw/polbnda_dza.json"), what = "sp")


##########
# Logements__Logements=c(livraison_wilayas,
#                       estimation_tolpopparc,
#                       before00,
#                       lancement_wilayas)
# Logements__FicheWilaya=c(data_fiche_wilaya,
#                            details_encours_lpl_lsp_lv_lpp,
#                            details_nonlances_lpl,
#                            details_nonlances_lsp,
#                            details_nonlances_rural,
#                            attribution_fw,
#                            patrimoine_fw,
#                            etat_cession_fw,
#                            gt_attribution,
#                            gt_attribution,
#                            aides_reha)
# ##
# Urbanisme__Urbanisme=c(sit_fin,zones,zones00,
#               pos,pos5,
#               s1,ss,ss1)
# ##
# EquipementsPublic__EquipementsPublic=c(equip,equip11)
# ##
# CMR__ACQ=c(moyens_realisation,moyens_realisation2,etp_categoris59)
# CMR__DTR=c(dtr_files)
# CMR__UF=c(datamc)
# CMR__P=c()
##########




usethis::use_data(     livraison_wilayas,
                       estimation_tolpopparc,
                       before00,
                       lancement_wilayas,

                       data_fiche_wilaya,
                         details_encours_lpl_lsp_lv_lpp,
                         details_nonlances_lpl,
                         details_nonlances_lsp,
                         details_nonlances_rural,
                         attribution_fw,
                         patrimoine_fw,
                         etat_cession_fw,
                         gt_attribution,
                         aides_reha,

                       sit_fin,zones,zones00,
                       pos,pos5,
                       s1,ss,ss1,

                       equip,equip11,

                       moyens_realisation,
                       moyens_realisation2,
                       etp_categoris59,

                       dtr_files,

                       datamc,

                       algeria,mapdz,

                       ##countries,

                       overwrite = TRUE,internal = TRUE)


#
# usethis::use_data(
#   Logements__Logements,Logements__FicheWilaya,
#   Urbanisme__Urbanisme,
#   EquipementsPublic__EquipementsPublic,
#   CMR__ACQ,CMR__DTR,CMR__UF,
#   #CMR__P,
#
#   algeria,mapdz,
#
#   ##countries,
#
#   overwrite = TRUE,internal = TRUE)

