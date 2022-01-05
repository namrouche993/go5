## code to prepare `my_dataset` dataset goes here
# m1=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))
# m6=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))
#
#


jsCode88 <- "shinyjs.opac88 = function(params){$('#well1 .pretty.p-default.p-switch.p-slim').css('display', params);}"

jsCode99 <- "shinyjs.opac99 = function(params){$('.indent').css('margin-left', params);}"


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

                       ab5,ab6,ab,ab_suite,df2_ab,df3_ab,ab7,ab7_suite,ab2,ab3,ab4,



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

                       algeria58,wilayas58,mapdz58,

                       jsCode88,jsCode99,

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

