#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import shinyWidgets
#' @importFrom highcharter highchartOutput
#' @importFrom shinyBS bsModal
#' @importFrom excelR excelOutput
#' @importFrom reactable reactableOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom rpivotTable rpivotTableOutput
#' @importFrom shinytreeview treeviewInput make_tree
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(

      navbarPage(
        HTML("Ministère de l'Habitat,<br/> de l'Urbanisme et de la Ville"),
        id = "main_navbar",
        selected='Logements',
        navbarMenu("Logements",
                   tabPanel("Logements",
                            dropdown(

                              tags$h3("Filtrage des données"),
                              br(),
                              pickerInput(
                                inputId = "wilayas",
                                label = "",
                                choices = unique(livraison_wilayas$waw),
                                options = list(`actions-box` = TRUE,style = "btn-primary",
                                               size = 23,
                                               `selected-text-format`= "count>2",
                                               `count-selected-text` = "{0} Wilayas séléctionnés",
                                               `none-selected-text` ="Wilaya",`select-all-text`=
                                                 tags$div(
                                                   "Séléctionner",
                                                   tags$br(),
                                                   "Tout"
                                                 ),
                                               `deselect-all-text`=tags$div(
                                                 "Deselectionner",
                                                 tags$br(),
                                                 "Tout"
                                               )),
                                multiple = TRUE
                              ),
                              pickerInput(
                                inputId = "segments",
                                label = "",
                                choices = unique(livraison_wilayas$type_de_logement),
                                options = list(`actions-box` = TRUE,style = "btn-primary",
                                               `selected-text-format`= "count>2",
                                               `count-selected-text` = "{0} Segment séléctionnés",
                                               `none-selected-text` ="Segment",`select-all-text`=
                                                 tags$div(
                                                   "Séléctionner",
                                                   tags$br(),
                                                   "Tout"
                                                 ),
                                               `deselect-all-text`=tags$div(
                                                 "Deselectionner",
                                                 tags$br(),
                                                 "Tout"
                                               )),
                                multiple = TRUE
                              ),
                              br(),
                              sliderInput("annees","Année :",
                                          min = min(unique(estimation_tolpopparc$Annee)), max = max(unique(estimation_tolpopparc$Annee)), value = c(min(unique(estimation_tolpopparc$Annee)),max(unique(estimation_tolpopparc$Annee))),sep = ""),


                              style = "material-circle", icon = icon("bars"),
                              status = "primary", width = "300px"
                              #, animate = animateOptions(
                              # enter = animations$fading_entrances$fadeInRightBig,
                              #exit = animations$fading_exits$fadeOutRightBig
                              # )
                            ),
                            fluidRow(
                              column(width=2,style = "background-color:#eaeaea;",
                                     fluidRow( tags$div(class="card",
                                                        tags$span(class="title","Livraison des logements",style="display:inline-block;"),textOutput("titre_livraison"),
                                                        tags$h3(class="metric",textOutput("livraisons")),
                                                        tags$span(class="color__nb",htmlOutput("taux_livraisons"),style="color:green;display: inline-block;"),textOutput("dernier_an") #style=paste0("color:",ifelse(34>0,"green","red"))
                                     )

                                      ),
                                     fluidRow( tags$div(class="card",
                                                        tags$span(class="title","Lancement des logements",style="display:inline-block;"),textOutput("titre_lancement"),
                                                        tags$h3(class="metric",textOutput("lancements"))
                                                        ,tags$span(class="color__nb",htmlOutput("taux_lancements"),style="color:green;display: inline-block;"),textOutput("dernier_an_lanc") #style=paste0("color:",ifelse(34>0,"green","red"))
                                     )),

                                     fluidRow( tags$div(class="card",
                                                        tags$h4(class="title",textOutput("max_annee_parclogement")),
                                                        tags$h3(class="metric",textOutput("parclogements")),
                                                        tags$span(class="color__nb",textOutput("taux_parclogement"),style="color:green;display: inline-block;"),textOutput("dernier_an2") #style=paste0("color:",ifelse(34>0,"green","red"))
                                     ) ),



                                     fluidRow( tags$div(class="card",
                                                        tags$h4(class="title",textOutput("max_annee_population")),
                                                        tags$h3(class="metric",textOutput("populations")),
                                                        tags$span(class="color__nb",textOutput("taux_population"),style="color:green;display: inline-block;"),textOutput("dernier_an3")
                                     ) ),



                                     fluidRow( tags$div(class="card",
                                                        tags$h4(class="title",textOutput("max_annee_tol")),
                                                        tags$h3(class="metric",textOutput("tols")),
                                                        tags$span(class="color__nb",textOutput("taux_tol"),style="color:green;display: inline-block;"),textOutput("dernier_an4")
                                     ) ),

                                     fluidRow( tags$div(class="card",
                                                        tags$h4(class="title",textOutput("max_annee_densite")),
                                                        tags$h3(class="metric",textOutput("densite")),
                                                        tags$span(class="color__nb",textOutput("taux_densite"),style="color:green;display: inline-block;"),textOutput("dernier_an8") #style=paste0("color:",ifelse(34>0,"green","red"))
                                     ) )

                              ),
                              column(width=5,style = "background-color:#eaeaea;",
                                     wellPanel(id="well1",
                                               tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie1_hchart1"),

                                                        actionBttn(
                                                          inputId = "tabmodalserie1",
                                                          label = NULL,
                                                          style = "bordered",
                                                          color = "success",
                                                          icon = icon("table")
                                                        ),

                                                        actionBttn(
                                                          inputId = "choose_line1",
                                                          label = NULL,
                                                          style = "simple",
                                                          color = "primary",
                                                          icon = icon("th-list",lib = "glyphicon")
                                                        ),
                                                        div(id="display_when_hover_choose_line1",

                                                            awesomeRadio(
                                                              inputId = "radio_choose_line1",
                                                              label = "",
                                                              choices = c("Livraisons de Logements","Lancements de Logements","TOL"),
                                                              selected = "Livraisons de Logements"
                                                              #,color="default"
                                                            )

                                                        )
                                               ),
                                               div(
                                                 style = "padding-top: 30px;zoom:100%;"
                                                 ,highchartOutput("hchart1", height = 330)

                                               )

                                               ,prettySwitch(
                                                 inputId = "Id027",
                                                 label = "Par Segment",
                                                 status = "primary",
                                                 slim = TRUE,
                                                 value=TRUE,
                                               )),
                                     fluidRow(
                                       column(width=12,style = "background-color: transparent;zoom:100%",
                                              wellPanel(id="well12",

                                                        tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput('titre_pie'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("periode"),

                                                                 actionBttn(
                                                                   inputId = "tabmodalserie2",
                                                                   label = NULL,
                                                                   style = "bordered",
                                                                   color = "success",
                                                                   icon = icon("table")
                                                                 ),
                                                                 actionBttn(
                                                                   inputId = "choose_line1_pie",
                                                                   label = NULL,
                                                                   style = "simple",
                                                                   color = "primary",
                                                                   icon = icon("th-list",lib = "glyphicon")
                                                                 ),
                                                                 div(id="display_when_hover_choose_line1_pie",

                                                                     awesomeRadio(
                                                                       inputId = "radio_choose_line1_pie",
                                                                       label = "",
                                                                       choices = c("Livraisons de Logements","Lancements de Logements"),
                                                                       selected = "Livraisons de Logements"
                                                                       #,color="default"
                                                                     )

                                                                 )



                                                        )
                                                        ,div(style = "margin-top:-1em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                                             highchartOutput("pie", height = 330)))
                                       )
                                     )
                              ),
                              column(width=5,style = "background-color:#eaeaea;",
                                     wellPanel(id="well1",
                                               tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("periode2"),

                                                        actionBttn(
                                                          inputId = "tabmodalserie3",
                                                          label = NULL,
                                                          style = "bordered",
                                                          color = "success",
                                                          icon = icon("table")
                                                        ),

                                                        actionBttn(
                                                          inputId = "choose_leaflet",
                                                          label = NULL,
                                                          style = "simple",
                                                          color = "primary",
                                                          icon = icon("th-list",lib = "glyphicon")
                                                        ),
                                                        div(id="display_when_hover_choose_leaflet",

                                                            awesomeRadio(
                                                              inputId = "radio_choose_leaflet",
                                                              label = "",
                                                              choices = c("Livraisons de Logements","Lancements de Logements","TOL", "Population","Parc Logements"),
                                                              selected = "Livraisons de Logements"
                                                              #,color="default"
                                                            )

                                                        )


                                               )

                                               ,div(style = "padding: 0px 0px;zoom:122%;height:620px;",
                                                    withSpinner(
                                                      leafletOutput("distPlot2",height=613
                                                                    #ifelse(abad()==1,700,568)
                                                      ))
                                               ))
                              )


                            ),
                            #tabmodalserie_eqp
                            bsModal("modal1", htmlOutput("tablededonnes1"), "tabmodalserie1", size = "large"
                                    ,excelOutput("excel1",height="500px")),

                            bsModal("modal2",htmlOutput("tablededonnes2"), "tabmodalserie2", size = "large"
                                    ,excelOutput("excel2")),

                            bsModal("modal3",htmlOutput("tablededonnes3"), "tabmodalserie3", size = "large"
                                    ,excelOutput("excel3")),
                            textOutput('verification_leaflet')
                   ),
                   ######### fin tabpanel Logements


                   #################################################### FICHE WILAYA :

                   tabPanel("Fiche Wilaya",
                            fluidRow(
                              id="tabfichewilaya",
                              prettySwitch(
                                inputId = "pourcentage",
                                label = "Pourcentage",
                                status = "primary",
                                slim = TRUE,
                                value=FALSE
                              ),
                              column(3,id="c1_fichewilaya",
                                     reactableOutput("table2_fichewilaya")
                              ),
                              column(9,id='c12_fichewilaya',style="margin-left:155px;",
                                     wellPanel(id="well_fichewilaya",
                                               style="box-shadow:4px 5px 11px grey;background-color:white;",
                                               icon(class="icon11","share-alt", lib = "glyphicon"),

                                               div(id = "box-score1",
                                                   div(id = "box-score-title1", "État d'Exécution du Programme - Situation au ",textOutput("boxscore1_arretee"),textOutput("boxscore1_wilaya")),
                                                   reactableOutput('gt1',width = '1082px'),
                                                   actionBttn(
                                                     inputId = "button_fw1",
                                                     label = NULL,
                                                     style = "bordered",
                                                     color = "success",
                                                     icon = icon("table")
                                                   ),uiOutput("divhover_nonlances_lpl"),
                                                   uiOutput("divhover_nonlances_lsp"),
                                                   uiOutput("divhover_nonlances_rural"),
                                                   uiOutput("divhover_encours_lpl"),
                                                   uiOutput("divhover_encours_lsp"),
                                                   uiOutput("divhover_encours_lv"),
                                                   uiOutput("divhover_encours_lpp"),
                                                   reactableOutput('gt1_suite',width = '1082px')


                                               ),
                                               div(id = "box-score7",

                                                   uiOutput("boxscore7_ui"),

                                                   reactableOutput('gt7',width = '1060px'),

                                                   actionBttn(
                                                     inputId = "button_fw7",
                                                     label = NULL,
                                                     style = "bordered",
                                                     color = "success",
                                                     icon = icon("table")
                                                   ),
                                                   reactableOutput('gt7_suite',width = '1060px')

                                               ),

                                               uiOutput("ui_boxscore2"),

                                               uiOutput("ui_boxscore_attribution"),


                                               # div(id = "box-score2",
                                               #     actionBttn(
                                               #       inputId = "button_fw2",
                                               #       label = NULL,
                                               #       style = "bordered",
                                               #       color = "success",
                                               #       icon = icon("table")
                                               #     ),
                                               #     div(id = "box-score-title2", "Aides à la réhabilitation au ",textOutput("boxscore2_arretee"),textOutput("boxscore2_wilaya")),
                                               #     reactableOutput('gt2',width = '860px')
                                               #
                                               # ),


                                               uiOutput("ui_boxscore3"),
                                               uiOutput("ui_boxscore8"),
                                               uiOutput("ui_boxscore9"),



                                               # div(class = "box-score",
                                               #     div(class = "box-score-title", "État des Livraisons au ",textOutput("boxscore5_arretee"),textOutput("boxscore5_wilaya")),
                                               #     reactableOutput('gt5'),
                                               #     actionBttn(
                                               #       inputId = "button_fw4",
                                               #       label = NULL,
                                               #       style = "bordered",
                                               #       color = "success",
                                               #       icon = icon("table")
                                               #     )
                                               # ),
                                               # uiOutput("ui_boxscore6"),

                                               bsModal("modal_fw1", htmlOutput("tablededonnes1_fw1"), "button_fw1", size = "large"
                                                       ,excelOutput("excel_fw1")),

                                               bsModal("modal_fw7", htmlOutput("tablededonnes1_fw7"), "button_fw7", size = "large"
                                                       ,excelOutput("excel_fw7")),

                                               # bsModal("modal_fw2", htmlOutput("tablededonnes1_fw2"), "button_fw2", size = "large"
                                               #         ,excelOutput("excel_fw2")),
                                               # bsModal("modal_fw3", htmlOutput("tablededonnes1_fw3"), "button_fw3", size = "large"
                                               #         ,excelOutput("excel_fw3"))
                                               # bsModal("modal_fw4", htmlOutput("tablededonnes1_fw4"), "button_fw4", size = "large"
                                               #         ,excelOutput("excel_fw4")),
                                               # bsModal("modal_fw5", htmlOutput("tablededonnes1_fw5"), "button_fw5", size = "large"
                                               #         ,excelOutput("excel_fw5"))

                                               #bsModal("modal_eqp", htmlOutput("tablededonnes1_eqp"), "tabmodalserie_eqp", size = "large"
                                               #,excelOutput("excel_eqp")),

                                     )
                              )
                            ),
                            pickerInput(
                              inputId = "select_arretee_fichewilaya",
                              label = "",
                              choices = sort(paste(c("Arrétée le :"),unique(data_fiche_wilaya$arretee)),TRUE),
                              #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
                            )
                   )



                   ################################################### FICHE WILAYA Fin
                   #, tabPanel("Situation Physique des Logements",
                   #          fluidRow(
                   #            id="tabsitphy",
                   #
                   #            column(6,id="c1",
                   #                   #
                   #                   # awesomeCheckboxGroup(
                   #                   #   inputId = "select_segment_gauge",
                   #                   #   label = "Segments : ",
                   #                   #   choices = c("Rural", "LPL", "Location-Vente","LSP/LPA","LPP"),
                   #                   #   inline = TRUE,
                   #                   #   status = "success"
                   #                   # ),
                   #                   #
                   #                   actionBttn(
                   #                     inputId = "tabmodal_sitphy",
                   #                     label = NULL,
                   #                     style = "bordered",
                   #                     color = "success",
                   #                     icon = icon("table")
                   #                   ),
                   #                   checkboxGroupButtons(
                   #                     inputId = "select_segment_gauge",
                   #                     choices = c("Rural", "LPL", "Location-Vente","LSP/LPA","LPP"),
                   #                     status = "primary",
                   #                     checkIcon = list(
                   #                       yes = icon("ok",
                   #                                  lib = "glyphicon")
                   #                     ),
                   #                     width = "150%"
                   #                   ),
                   #
                   #
                   #
                   #                   reactableOutput("table2")
                   #            ),
                   #            column(3,id="cg2",
                   #                   wellPanel(id="well_gauge",
                   #                             tags$div(class="chart-title-gauge",tags$p(id="titregauge1",textOutput("toutsegments_gauge"),textOutput("wilayaselectgauge1")#textOutput("wgauge")
                   #                             )),
                   #                             highchartOutput("gauge1",height = "247px"),tags$p(id="livraison_gauge1","Livraison"),tags$span(id="livraison_gauge1_val",textOutput("val_livraison_gauge1")),tags$p(id="prevision_gauge1","Prévision"),tags$span(id="prevision_gauge1_val",textOutput("val_prevision_gauge1"))
                   #
                   #
                   #                             #,verbatimTextOutput("selected")
                   #                   ),
                   #                   wellPanel(id="well_gauge2",
                   #                             tags$div(class="chart-title-gauge2",tags$p(id="titregauge2","LPL",textOutput("wilayaselectgauge2"))),
                   #                             highchartOutput("gauge2",height = "247px"),tags$p(id="livraison_gauge2","Livraison"),tags$span(id="livraison_gauge2_val",textOutput("val_livraison_gauge2")),tags$p(id="prevision_gauge2","Prévision"),tags$span(id="prevision_gauge2_val",textOutput("val_prevision_gauge2"))
                   #
                   #
                   #                   ),
                   #                   wellPanel(id="well_gauge3",
                   #                             tags$div(class="chart-title-gauge3",tags$p(id="titregauge3","LSP/LPA",textOutput("wilayaselectgauge3"))),
                   #                             highchartOutput("gauge3",height = "247px"),tags$p(id="livraison_gauge3","Livraison"),tags$span(id="livraison_gauge3_val",textOutput("val_livraison_gauge3")),tags$p(id="prevision_gauge3","Prévision"),tags$span(id="prevision_gauge3_val",textOutput("val_prevision_gauge3"))
                   #
                   #
                   #                             #,verbatimTextOutput("selected")
                   #                   )
                   #
                   #            ),
                   #            column(3,   #3eme colonne
                   #
                   #                   wellPanel(id="well_gauge4",
                   #                             tags$div(class="chart-title-gauge4",tags$p(id="titregauge4","Rural",textOutput("wilayaselectgauge4"))),
                   #                             highchartOutput("gauge4",height = "247px"),tags$p(id="livraison_gauge4","Livraison"),tags$span(id="livraison_gauge4_val",textOutput("val_livraison_gauge4")),tags$p(id="prevision_gauge4","Prévision"),tags$span(id="prevision_gauge4_val",textOutput("val_prevision_gauge4"))
                   #
                   #
                   #                             #,verbatimTextOutput("selected")
                   #                   ),
                   #
                   #                   #column(5,id="cgauge4",
                   #                   wellPanel(id="well_gauge5",
                   #                             tags$div(class="chart-title-gauge5",tags$p(id="titregauge5","Location-Vente",textOutput("wilayaselectgauge5"))),
                   #                             highchartOutput("gauge5",height = "247px"),tags$p(id="livraison_gauge5","Livraison"),tags$span(id="livraison_gauge5_val",textOutput("val_livraison_gauge5")),tags$p(id="prevision_gauge5","Prévision"),tags$span(id="prevision_gauge5_val",textOutput("val_prevision_gauge5"))
                   #
                   #
                   #                             #,verbatimTextOutput("selected")
                   #                   ),
                   #                   wellPanel(id="well_gauge6",
                   #                             tags$div(class="chart-title-gauge6",tags$p(id="titregauge6","LPP",textOutput("wilayaselectgauge6"))),
                   #                             highchartOutput("gauge6",height = "247px"),tags$p(id="livraison_gauge6","Livraison"),tags$span(id="livraison_gauge6_val",textOutput("val_livraison_gauge6")),tags$p(id="prevision_gauge6","Prévision"),tags$span(id="prevision_gauge6_val",textOutput("val_prevision_gauge6"))
                   #
                   #
                   #                             #,verbatimTextOutput("selected")
                   #                   )
                   #
                   #            ),
                   #
                   #            bsModal("modal_sitphy",htmlOutput("tablededonnes1_sitphy"), "tabmodal_sitphy", size = "large"
                   #                    ,excelOutput("excel_sitphy"))
                   #          ),
                   #          pickerInput(
                   #            inputId = "select_arretee_sitphy",
                   #            label = "",
                   #            choices = sort(paste(c("Arrétée le :"),unique(sitphy$Arretee)),TRUE),
                   #            #c('Arretee le : 2020-06-30','Arretee le : 2019-12-31'),
                   #          )
                   # )



        ),



        ########## fin navmenu logts

        tabPanel("Urbanisme",
                 fluidRow(
                   id='fluidrow1_urbanisme',
                   style = "display:flex; align-items:flex-start",
                   wellPanel(id="side_wilaya",
                             style = "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0",
                             reactableOutput("table")
                   ),
                   fluidRow( #~~ Main panel ~~#
                     style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px",
                     column(6,
                            wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                      tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Instruments d'Urbanisme : "),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselect1"),
                                               actionBttn(
                                                 inputId = "tabmodalserie_urbanisme1",
                                                 label = NULL,
                                                 style = "bordered",
                                                 color = "success",
                                                 icon = icon("table")
                                               )
                                      ),

                                      highchartOutput("urbanisme1")
                            ),

                            wellPanel(id="urbanisme_well2",style="width:100% ; height:380px;",
                                      tags$div(class="chart-title-urbanisme2",HTML('&nbsp;'),HTML('&nbsp;'),"Situation Financières : ",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselect2"),
                                               actionBttn(
                                                 inputId = "tabmodalserie_urbanisme2",
                                                 label = NULL,
                                                 style = "bordered",
                                                 color = "success",
                                                 icon = icon("table")
                                               )
                                      ),

                                      highchartOutput("urbanisme2")
                            )
                     ),
                     column(6,
                            wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                      tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Lotissement sociaux : ",HTML('&nbsp;'),
                                               actionBttn(
                                                 inputId = "tabmodalserie_urbanisme3",
                                                 label = NULL,
                                                 style = "bordered",
                                                 color = "success",
                                                 icon = icon("table")
                                               )
                                      ),

                                      div(style="padding-top:2px;",
                                          textOutput("ttwilayas"),
                                          textOutput("region"),
                                          tags$table(class="ta",style="font-size: 21px;font-family: system-ui;",
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nature juridique",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("zones1"),style="padding-bottom: 3px;text-align: end;font-weight: 500;font-size:19px;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nombre de Communes Concernées",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("zones2"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Superficie (ha)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("zones3"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nombre de lots",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("zones4"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                     )
                                                     ,
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nombre de lots retenues",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                             tags$td(class="f3",textOutput("zones5"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                     ),

                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nombre de lots dégagés (Porte feuille)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("zones6"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",style="display:none;",
                                                             tags$td(class="f1","Surface moyenne des lots (m²)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("zones7"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nombre de sites",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                             tags$td(class="f3",textOutput("zones8"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Nombre de permis",style="padding-bottom: 3px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                             tags$td(class="f3",textOutput("zones9"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                     )
                                          )

                                      )                                 ),

                            wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                      tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Loi 18-05 : ",HTML('&nbsp;'),
                                               actionBttn(
                                                 inputId = "tabmodalserie_urbanisme4",
                                                 label = NULL,
                                                 style = "bordered",
                                                 color = "success",
                                                 icon = icon("table")
                                               )

                                      ),


                                      div(style="padding-top:2px;",
                                          textOutput("ttwilayas2"),
                                          tags$table(class="ta",style="font-size: 21px;font-family: system-ui;",
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Déposés",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("loi1"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Traités",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("loi2"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Favorables",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("loi3"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Défavorables",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("loi4"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                     )
                                                     ,
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Instances",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 13px;"),
                                                             tags$td(class="f3",textOutput("loi5"),style="text-align: end;padding-bottom: 13px;font-weight: 500;")
                                                     ),

                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Déposés Instruction N°01",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("loi6"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                     ),
                                                     tags$tr(class="f0",
                                                             tags$td(class="f1","Traités_a",style="padding-bottom: 13px;padding-left: 27px;"),
                                                             tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                             tags$td(class="f3",textOutput("loi7"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                     )
                                          )

                                      )
                            )
                     )


                     #verbatimTextOutput("selected")
                   )
                 ),
                 pickerInput(
                   inputId = "select_arretee_urbanisme",
                   label = "",
                   choices = c('Arrétée le : 2019-12-31'),
                   #c('Arretee le : 2020-06-30','Arretee le : 2019-12-31'),
                 ),


                 bsModal("modal_urbanisme1","Table de données", "tabmodalserie_urbanisme1", size = "large"
                         ,excelOutput("excel_urbanisme1")),

                 bsModal("modal_urbanisme2","Table de données", "tabmodalserie_urbanisme2", size = "large"
                         ,excelOutput("excel_urbanisme2")),

                 bsModal("modal_urbanisme3","Table de données", "tabmodalserie_urbanisme3", size = "large"
                         ,excelOutput("excel_urbanisme3")),

                 bsModal("modal_urbanisme4","Table de données", "tabmodalserie_urbanisme4", size = "large"
                         ,excelOutput("excel_urbanisme4"))




        ),





        tabPanel("Equipements Publics",
                 fluidRow(
                   id="fluidrow1_eqp",

                   column(6,style="width:780px;",     #id="cequip",
                          reactableOutput("equip_reactable",width = "755px")

                   ),
                   column(6,style="box-shadow:4px 5px 11px grey;background-color:white;height:828px;width:933px;",
                          tags$div(class="chart-title-eqp",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Nombre de Projets Par Secteur : "),HTML('&nbsp;'),HTML('&nbsp;'),
                                   prettySwitch(
                                     inputId = "pourcentage_equip",
                                     label = "Pourcentage",
                                     status = "primary",
                                     slim = TRUE,
                                     value=FALSE
                                   ),
                                   HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselecteqp"),
                                   actionBttn(
                                     inputId = "tabmodalserie_eqp",
                                     label = NULL,
                                     style = "bordered",
                                     color = "success",
                                     icon = icon("table")
                                   )
                          ),
                          #HTML('<span style="color:green;" &#9679;<span/>'),
                          tags$div(id="divequip",style="margin-bottom:-26px;margin-top:-7px;text-align:right",

                                   tags$span("\u25CF",style="font-size:25px;color:#5F9E4F"),tags$span("Achevés",style=";padding-right:15px;"),
                                   tags$span("\u25CF",style="font-size:25px;color:rgb(239, 192, 0)"),tags$span("En Cours",style=";padding-right:15px;"),
                                   tags$span("\u25CF",style="font-size:25px;color:rgb(231, 25, 25)"),tags$span("Non Lancés",style=";padding-right:15px;"),
                                   tags$span("\u25CF",style="font-size:25px;color:#A9A9A9"),tags$span("Dont NIR",style=";padding-right:15px;"),
                                   tags$span("\u25CF",style="font-size:25px;color:#101010"),tags$span("Gelés",style=";padding-right:15px;")
                          ),


                          div(id = "box-score_equip",
                              div(id = "box-score-title_equip", "Nombre de Projets par Secteur",textOutput("wilayaselecteqp22")),
                              reactableOutput('equip_secteur_reactable')
                          )

                   ),

                   bsModal("modal_eqp", htmlOutput("tablededonnes1_eqp"), "tabmodalserie_eqp", size = "large"
                           ,excelOutput("excel_eqp")),

                 ),
                 pickerInput(
                   inputId = "select_arretee_eqp",
                   label = "",
                   choices = c('Arrétée le : 2020-06-30','Arrétée le : 2019-12-31')
                 )

        ),
        tabPanel(   HTML('
          <span style="opacity:0" class="glyphicon glyphicon-home"></span>
                     '),

                    fluidRow(
                      id="fluidrow1_home",
                      wellPanel(id="well_homepage",
                                style=
                                  '    min-height: 20px;
    padding: 19px;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    border: 0px;
    background-image: url(Bay_of_Algiers_by_night_2.jpg);
    height: 836px;
    background-size: cover;
    background-attachment: fixed;
    margin-left: -20px;
    margin-right: -15px;
    margin-top: -21px;
    margin-bottom:0px;
    height: 855px;' ,
                                #includeHTML(paste0(getwd(),'/www/index2.html'))
                                #,
                                #tags$head(includeScript(paste0(getwd(),'/www/script2.js'))),
                                #tags$head(includeCSS(paste0(getwd(),'/www/style2.css')))

                                #htmlTemplate(paste0(getwd(),"/www/index2.html"))
                                # ,
                                # includeCSS(paste0(getwd(),'/www/style2.css')),
                                # includeScript(paste0(getwd(),'/www/script2.js'))

                                htmlOutput("homepage_ren"),
                                useShinyjs(),

                                actionBttn(
                                  inputId = "homepage_button1",
                                  label = "Organigramme",
                                  style = "bordered",
                                  color = "primary"
                                ),
                                #tags$a(href="http://www.mhuv.gov.dz/fr/accueil/",target="_blank",
                                actionBttn(
                                  inputId = "homepage_button2",
                                  label = "Indicateur Wilaya",
                                  style = "bordered",
                                  color = "primary"
                                )
                                #)
                                ,
                                actionBttn(
                                  inputId = "homepage_button3",
                                  label = HTML("Organismes <br> sous tutelle"),
                                  style = "bordered",
                                  color = "primary"
                                ),

                                bsModal("modal_homepage1",HTML("<h3 style='display:inline'> Organigramme du MHUV</h3> <br> <h6> (Pour faire le Zoom : Appuyer sur Ctrl + ou Ctrl - sur le clavier)"), "homepage_button1", size = "large"
                                        ,htmlOutput("html_homepage1")),
                                bsModal("modal_homepage3",HTML("<h3 style='margin:-5px;font-family:sans-serif;font-size:23px;'>Organismes sous tutelle</h3>"), "homepage_button3", size = "large"
                                        #,uiOutput("html_homepage3")

                                        ,tags$div(id='organisme_fluidrow',
                                                  actionButton("preview", "Preview",style="display:none;"),

                                                  leafletOutput("mapsalgerie",height = 950)
                                        )


                                ),
                                bsModal("modal_wilayas58", textOutput("title_organismesoustutelle"), "preview", size = "large"
                                        ,reactableOutput("reactable_58wilayas")),



                                bsModal("modal_homepage2",HTML("<h3 style='margin:-5px;font-family:sans-serif;font-size:23px;'>Indicateur Wilaya</h3>"), "homepage_button2", size = "large"
                                        #,uiOutput("html_homepage3")

                                        ,tags$div(id='indicateur_fluidrow',
                                                  actionButton("preview_indicateur", "Preview",style="display:none;"),

                                                  leafletOutput("mapsalgerieindicateur",height = 950)
                                        )


                                ),
                                bsModal("modal_wilayas58_indicateur", textOutput("title_indicateur"), "preview_indicateur", size = "large"
                                        ,reactableOutput("reactable_58wilayas_indicateur"))




                      )


                    ),
                    textOutput('texta5')


        ),
        tabPanel("Villes",
                 fluidRow(
                   column(width=3,style="height: 800px;background-color:white;box-shadow: 4px 5px 11px grey;width: 21%;margin-right: 20px;margin-left: 36px;"),
                   column(width=9,style="margin-left: 0px;width: 74%;background-color:white;",
                          fluidRow(id="rowselect",style="height: 550px;",
                                   pickerInput(
                                     inputId = "select_villes",
                                     label = "",
                                     #color = "success",
                                     #style = "stretch",
                                     choices = c("Villes", "ALI MENDJLI", "BOUGHEZOUL","BOUINANE",
                                                 "DRAA ERICH","SIDI ABDELLAH","EL MENEAA","HASSI MESSAOUD")
                                     ,choicesOpt = list(
                                       style = c("font-size: 114%;font-weight: bold;")
                                       ,selectOnTab=TRUE
                                     )
                                     #             "color: firebrick; text-align: right;", "font-weight: bold;",
                                     #             "background: forestgreen; color: white;")
                                     #   )
                                     #
                                   ),
                                   column(width=9,style="box-shadow:4px 5px 11px grey;background-color:white;",
                                          withSpinner(
                                            leafletOutput("leaflet_ville",height="536px")
                                          )
                                   ),
                                   column(width=3,
                                          style="height: 534px;background-color:white;box-shadow: 4px 5px 11px grey;width: 23%;margin-left: 24px"
                                   )
                          ),
                          fluidRow(style="height: 250px;background-color:white;box-shadow: 4px 5px 11px grey;")
                   )
                 )
        ),


        #
        # tabPanel("Bilan2020",
        #          htmlOutput("bilan")
        #          ),
        #



        #########################################

        navbarMenu(tags$span("Construction et moyens",tags$br(),"de réalisation"),
                   tabPanel("Unités de fabrication",
                            fluidRow(
                              id="fluidrow1_datamc",

                              dropdown(

                                tags$h3("Filtrage des données"),
                                br(),
                                pickerInput(
                                  inputId = "wilayas_mc",
                                  label = "",
                                  #choices = unique(datamc$wilaya_matricule),

                                  choices = datamc %>%
                                    filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>%
                                    select(wilaya_matricule) %>%
                                    unique() %>% .$wilaya_matricule,


                                  #unique(datamc$wilaya_matricule),


                                  options = list(`actions-box` = TRUE,style = "btn-primary",
                                                 `selected-text-format`= "count>2",
                                                 `count-selected-text` = "{0} Wilayas séléctionnés",
                                                 `none-selected-text` ="Wilaya",`select-all-text`=
                                                   tags$div(
                                                     "Séléctionner",
                                                     tags$br(),
                                                     "Tout"
                                                   ),
                                                 `deselect-all-text`=tags$div(
                                                   "Désélectionner ",
                                                   tags$br(),
                                                   "Tout"
                                                 )),
                                  multiple = TRUE
                                ),
                                pickerInput(
                                  inputId = "filiere_mcmc",
                                  label = "",
                                  choices = datamc %>%
                                    filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>%
                                    select(Filiere) %>%
                                    unique() %>% .$Filiere,


                                  #unique(datamc$Filiere),
                                  options = list(`actions-box` = TRUE,style = "btn-primary",
                                                 `selected-text-format`= "count>2",
                                                 `count-selected-text` = "{0} Filière séléctionnés",
                                                 `none-selected-text` ="Filière",`select-all-text`=
                                                   tags$div(
                                                     "Séléctionner",
                                                     tags$br(),
                                                     "Tout"
                                                   ),
                                                 `deselect-all-text`=tags$div(
                                                   "Désélectionner",
                                                   tags$br(),
                                                   "Tout"
                                                 )),
                                  multiple = TRUE
                                ),
                                pickerInput(
                                  inputId = "statut2_mc",
                                  label = "",
                                  choices = datamc %>%
                                    filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>%

                                    #filter() %>%
                                    group_by(Statut2) %>%
                                    summarise(Nb=n()) %>%
                                    mutate(ar=c(3,5,4,6,1,2)) %>%
                                    arrange(ar) %>% .$Statut2,
                                  options = list(`actions-box` = TRUE,style = "btn-primary",
                                                 `selected-text-format`= "count>2",
                                                 `count-selected-text` = "{0} Staut séléctionnés",
                                                 `none-selected-text` ="Statut",`select-all-text`=
                                                   tags$div(
                                                     "Séléctionner",
                                                     tags$br(),
                                                     "Tout"
                                                   ),
                                                 `deselect-all-text`=tags$div(
                                                   "Désélectionner ",
                                                   tags$br(),
                                                   "Tout"
                                                 )),
                                  multiple = TRUE
                                ),
                                #br(),
                                #sliderInput("annees_mc","Annee dentree en production :",
                                #             min = min(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE])), max = max(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE])), value = c(min(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE])),max(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE]))),sep = ""),


                                style = "material-circle", icon = icon("bars"),
                                status = "primary", width = "300px"
                                #, animate = animateOptions(
                                # enter = animations$fading_entrances$fadeInRightBig,
                                #exit = animations$fading_exits$fadeOutRightBig
                                # )
                              ),
                              fluidRow(
                                column(width=3,style="height: 800px;width: 21%;margin-right: 20px;margin-left: 36px;",
                                       fluidRow(id="card_mc",
                                                style=
                                                  "height: 184px;margin-bottom:15px;;background-color:white;
                               box-shadow: 4px 5px 11px grey;
                               	border-radius:1px;
                               	font-family: sans-serif;
                               	padding: 1rem;
                               	box-shadow: 4px 5px 11px grey;
                               	transition: all .2s ease;
                               	border-bottom: 4px solid transparent;
                               	border-top: 4px solid #109485;",
                                                tags$h3(id="card_mc_titre",HTML("Nombre d'unités <br/>de fabrication des matériaux <br/>de construction"),
                                                        style="text-align: center;margin-top: 2px;font-size: 27px;font-family: unset;"),
                                                tags$h3(id="card_mc_chiffre",textOutput("chiffre_mc"),
                                                        style="text-align: center;margin-top: -8px;font-size: 71px;font-family: unset;font-weight:550"),

                                       ),
                                       fluidRow(id="filiere_mc",style="height: 602px;background-color:white;box-shadow: 4px 5px 11px grey;padding:17px;",
                                                reactableOutput('mc_filiere_reactable')
                                       ),


                                ),
                                column(width=9,style="margin-left: 0px;width: 74%;background-color:white;",
                                       fluidRow(id="rowselect_mc",style="height: 800px;",
                                                column(width=8,style="box-shadow:4px 5px 11px grey;background-color:white;overflow:auto;",
                                                       actionBttn(
                                                         inputId = "choose_leaflet_mc",
                                                         label = NULL,
                                                         style = "simple",
                                                         color = "primary",
                                                         icon = icon("th-list",lib = "glyphicon")
                                                       ),
                                                       div(id="display_when_hover_choose_leaflet_mc",

                                                           awesomeRadio(
                                                             inputId = "radio_choose_leaflet_mc",
                                                             label = "",
                                                             choices = c("Maps","Tableau","Tableau croisé dynamique"),
                                                             selected = "Maps"
                                                             #,color="default"
                                                           )

                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.radio_choose_leaflet_mc == 'Maps'",
                                                         selectInput("search", "Rechercher:",
                                                                     #textOutput("datamc_identification"),
                                                                     datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>%
                                                                       select(Identification) %>% .$Identification,
                                                                     #datamc$Identification,
                                                                     multiple = TRUE

                                                         ),
                                                         withSpinner(
                                                           leafletOutput("distPlot2_mc",height="800px")
                                                         ),
                                                         textOutput("nd_mc_maps")
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.radio_choose_leaflet_mc == 'Tableau'",
                                                         withSpinner(

                                                           reactableOutput("tableau_mc",height="800px")
                                                         )
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.radio_choose_leaflet_mc == 'Tableau croisé dynamique'",
                                                         withSpinner(
                                                           rpivotTableOutput("pivot_table_mc",height="800px")
                                                         )
                                                       )

                                                ),
                                                column(width=4,
                                                       style="height: 800px;background-color:white;box-shadow: 4px 5px 11px grey;width: 31%;margin-left: 29px;padding:17px;",
                                                       reactableOutput('mc_statut_reactable'),
                                                       #highchartOutput("mc_annee_debut"),
                                                       #textOutput("annee_na_count"),

                                                       reactableOutput('mc_wilaya_reactable',height="450px")

                                                )
                                       )
                                       #,
                                       #fluidRow(style="height: 250px;background-color:white;box-shadow: 4px 5px 11px grey;")
                                )
                              )
                            ),
                            pickerInput(
                              inputId = "select_arretee_datamc",
                              label = "",
                              #choices = sort(paste(c("Arrétée le :"),unique(moyens_realisation$Arretee)),TRUE),
                              choices = sort(paste(c("Arrétée le :"),unique(datamc$Arretee)),TRUE),

                              #choices = c('Arretee le : 2021-03-31','Arretee le : 2018-12-31')
                            )

                   ),
                   tabPanel("DTR",
                            fluidRow(
                              column(id="column1_pdf",width = 3,style="height: 800px;background: white;box-shadow: 4px 5px 11px grey;margin-right:8px;width:23%;overflow-y:scroll;font-size:16px;",
                                     treeviewInput(
                                       inputId = "tree",
                                       label = "",
                                       choices = make_tree(
                                         dtr_files, c("Niveau","fichier")
                                       ),
                                       multiple = FALSE,
                                       prevent_unselect = TRUE,
                                       width = "100%"
                                     )
                              ),
                              column(id="column2_pdf2",width=9,style="height: 800px;background: white;box-shadow: 4px 5px 11px grey;"
                                     ,
                                     #verbatimTextOutput(outputId = "tree_file")
                                     htmlOutput("tree_file")
                                     #includeHTML(paste0(getwd(),"/pdf_files_DGCMR/niveau3/BE11.html"))
                              )

                            )

                   ),
                   tabPanel("Agréments et certificats de qualification", #c'était sous le nom : Moyens de Realisation

                            bsModal("modal1_mapst", htmlOutput("tablededonnes1_mapst"), "tabmodalserie1_mapst", size = "large",excelOutput("excel_mr1")),
                            bsModal("modal2_mapst", htmlOutput("tablededonnes2_mapst"), "tabmodalserie2_mapst", size = "large",excelOutput("excel_mr2")),
                            bsModal("modal3_mapst", htmlOutput("tablededonnes3_mapst"), "tabmodalserie3_mapst", size = "large",excelOutput("excel_mr3")),
                            bsModal("modal4_mapst", htmlOutput("tablededonnes4_mapst"), "tabmodalserie4_mapst", size = "large",excelOutput("excel_mr4")),
                            bsModal("modal5_mapst", htmlOutput("tablededonnes5_mapst"), "tabmodalserie5_mapst", size = "large",excelOutput("excel_mr5")),



                            bsModal("modal_maps", htmlOutput("tablededonnes1_maps"), "tabmodalserie_maps", size = "large"
                                    ,
                                    div(
                                      actionBttn(
                                        inputId = "choose_leaflet_agrement",
                                        label = NULL,
                                        style = "simple",
                                        color = "primary",
                                        icon = icon("th-list",lib = "glyphicon")
                                      ),
                                      div(id="display_when_hover_choose_leaflet_agrement",

                                          awesomeRadio(
                                            inputId = "radio_choose_leaflet_agrement",
                                            label = "",
                                            choices = c("Entreprise qualifiees (Cat 1-9)",
                                                        "Entreprise qualifiees (Cat 1-4)",
                                                        "Entreprise qualifiees (Cat 5-9)",
                                                        "Entreprise qualifiees (Cat1)",
                                                        "Entreprise qualifiees (Cat2)",
                                                        "Entreprise qualifiees (Cat3)",
                                                        "Entreprise qualifiees (Cat4)",
                                                        "Entreprise qualifiees (Cat5)",
                                                        "Entreprise qualifiees (Cat6)",
                                                        "Entreprise qualifiees (Cat7)",
                                                        "Entreprise qualifiees (Cat8)",
                                                        "Entreprise qualifiees (Cat9)"

                                            ),
                                            selected = "Entreprise qualifiees (Cat 1-9)"
                                            #,color="default"
                                          )

                                      ),
                                      leafletOutput("distPlot2_agrementmaps",height=553)
                                    )
                            ),

                            ################ promoteur immobiliers maps

                            bsModal("modal_maps_prom", htmlOutput("tablededonnes1_maps_prom"), "tabmodalserie_maps_prom", size = "large"
                                    ,
                                    div(
                                      actionBttn(
                                        inputId = "choose_leaflet_agrement_prom",
                                        label = NULL,
                                        style = "simple",
                                        color = "primary",
                                        icon = icon("th-list",lib = "glyphicon")
                                      ),
                                      div(id="display_when_hover_choose_leaflet_agrement_prom",

                                          awesomeRadio(
                                            inputId = "radio_choose_leaflet_agrement_prom",
                                            label = "",
                                            choices = c("Nbre de dossiers déposés",
                                                        "dont Par l'application E-Certif",
                                                        "Nbre de dossiers éxaminés",
                                                        "dont par l'application E-Certif"


                                            ),
                                            selected = "Nbre de dossiers déposés"
                                            #,color="default"
                                          )

                                      ),
                                      leafletOutput("distPlot2_agrementmaps_prom",height=553)
                                    )
                            ),


                            ################ promoteur immobiliers maps fin



                            ################ agence immobiliers maps

                            bsModal("modal_maps_agence", htmlOutput("tablededonnes1_maps_agence"), "tabmodalserie_maps_agence", size = "large"
                                    ,
                                    div(
                                      actionBttn(
                                        inputId = "choose_leaflet_agrement_agence",
                                        label = NULL,
                                        style = "simple",
                                        color = "primary",
                                        icon = icon("th-list",lib = "glyphicon")
                                      ),
                                      div(id="display_when_hover_choose_leaflet_agrement_agence",

                                          awesomeRadio(
                                            inputId = "radio_choose_leaflet_agrement_agence",
                                            label = "",
                                            choices = c("Communes",
                                                        "Nbre de lots",
                                                        "Nbre de bénéficiaires de Lots",
                                                        "Nbre d'aides notifiées",
                                                        "Nbre de bénéficiaires d'aides"


                                            ),
                                            selected = "Nbre de lots"
                                            #,color="default"
                                          )

                                      ),
                                      leafletOutput("distPlot2_agrementmaps_agence",height=553)
                                    )
                            ),


                            ################ agences immobiliers maps fin

                            ################ ingenieurs maps debut

                            bsModal("modal_maps_ing", htmlOutput("tablededonnes1_maps_ing"), "tabmodalserie_maps_ing", size = "large"
                                    ,
                                    div(
                                      actionBttn(
                                        inputId = "choose_leaflet_agrement_ing",
                                        label = NULL,
                                        style = "simple",
                                        color = "primary",
                                        icon = icon("th-list",lib = "glyphicon")
                                      ),
                                      div(id="display_when_hover_choose_leaflet_agrement_ing",

                                          awesomeRadio(
                                            inputId = "radio_choose_leaflet_agrement_ing",
                                            label = "",
                                            choices = c("Nbre dossiers reçus_PI",
                                                        "Nbre dossiers éxaminés par la Commision",
                                                        "Nbre d'agréments établis",
                                                        "Nbre demande d'inscription au TNPI"
                                            ),
                                            selected = "Nbre dossiers reçus_PI"
                                            #,color="default"
                                          )

                                      ),
                                      leafletOutput("distPlot2_agrementmaps_ing",height=553)
                                    )
                            ),

                            ################ ingenieurs maps fin


                            ################ AI maps debut

                            bsModal("modal_maps_ai", htmlOutput("tablededonnes1_maps_ai"), "tabmodalserie_maps_ai", size = "large"
                                    ,leafletOutput("distPlot2_agrementmaps_ai",height=553)
                            ),
                            bsModal("modal1_search", htmlOutput("tablededonnes1_search"), "tabmodalserie1_search", size = "large",reactableOutput("reactable_mr1")),
                            #bsModal("modal1_mapst", htmlOutput("tablededonnes1_mapst"), "tabmodalserie1_mapst", size = "large",excelOutput("excel_mr1")),


                            # ,
                            # div(
                            #   actionBttn(
                            #     inputId = "choose_leaflet_agrement_ai",
                            #     label = NULL,
                            #     style = "simple",
                            #     color = "primary",
                            #     icon = icon("th-list",lib = "glyphicon")
                            #   ),
                            #   div(id="display_when_hover_choose_leaflet_agrement_ai",
                            #
                            #       awesomeRadio(
                            #         inputId = "radio_choose_leaflet_agrement_ai",
                            #         label = "",
                            #         choices = c("Nombre d'ingenieurs agrees",
                            #                     "Ingenieurs (25ans-40ans)",
                            #                     "Ingenieurs (41ans-60ans)",
                            #                     "Ingenieurs (>60ans)",
                            #                     "Ingenieurs (M)",
                            #                     "Ingenieurs (F)"
                            #
                            #
                            #
                            #
                            #         ),
                            #         selected = "Nombre d'ingenieurs agrees"
                            #         #,color="default"
                            #       )
                            #
                            #   ),
                            #   leafletOutput("distPlot2_agrementmaps_ai",height=553)
                            # )
                            #),


                            ################ AI maps fin








                            fluidRow(
                              id="tabfichewilaya_mr",
                              # prettySwitch(
                              #   inputId = "pourcentage_mr",
                              #   label = "Pourcentage",
                              #   status = "primary",
                              #   slim = TRUE,
                              #   value=FALSE
                              # ),
                              column(3,id="c1_fichewilaya_mr",
                                     reactableOutput("table2_fichewilaya_mr")
                              ),
                              column(9,id='c12_fichewilaya_mr',style="margin-left:155px;",
                                     actionBttn(
                                       inputId = "tabmodalserie_maps",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("globe", lib = "glyphicon")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie_maps_prom",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("globe", lib = "glyphicon")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie_maps_agence",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("globe", lib = "glyphicon")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie_maps_ing",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("globe", lib = "glyphicon")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie_maps_ai",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("globe", lib = "glyphicon")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie1_mapst",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("table")
                                     ),

                                     actionBttn(
                                       inputId = "tabmodalserie2_mapst",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("table")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie3_mapst",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("table")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie4_mapst",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("table")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie5_mapst",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("table")
                                     ),
                                     actionBttn(
                                       inputId = "tabmodalserie1_search",
                                       label = NULL,
                                       style = "bordered",
                                       color = "success",
                                       icon = icon("search")
                                     ),



                                     wellPanel(id="well_fichewilaya_mr",
                                               style="box-shadow:4px 5px 11px grey;background-color:white;",

                                               div(id = "box-score1_mr",
                                                   div(id = "box-score-title1_mr", "Entreprises Qualifiées au ",textOutput("boxscore1_arretee_mr"),textOutput("boxscore1_wilaya_mr")),
                                                   reactableOutput('gt1_mr',width = '1082px')
                                               ),

                                               div(id = "box-score2_mr",
                                                   div(id = "box-score-title2_mr",textOutput("boxscore2_arretee_mr"),textOutput("boxscore2_wilaya_mr")),
                                                   reactableOutput('gt2_mr',width = '922px')
                                               ),
                                               div(id = "box-score3_mr",
                                                   div(id = "box-score-title3_mr", "Lotissement Socioux au",textOutput("boxscore3_arretee_mr"),textOutput("boxscore3_wilaya_mr")),
                                                   reactableOutput('gt3_mr',width = '1023px')
                                               ),

                                               div(id = "box-score4_mr",
                                                   div(id = "box-score-title4_mr", "Agréments des Promoteurs Immobiliers au ",textOutput("boxscore4_arretee_mr"),textOutput("boxscore4_wilaya_mr")),
                                                   reactableOutput('gt4_mr',width = '1002px')
                                               ),
                                               div(id = "box-score5_mr",
                                                   div(id = "box-score-title5_mr", "Agréments des Agents Immobiliers au ",textOutput("boxscore5_arretee_mr"),textOutput("boxscore5_wilaya_mr")),
                                                   reactableOutput('gt5_mr',width = '1163px')
                                               )
                                               # ,
                                               # div(id = "box-score6_mr",
                                               #     div(id = "box-score-title6_mr", "Ingénieurs  au ",textOutput("boxscore6_arretee_mr"),textOutput("boxscore6_wilaya_mr")),
                                               #     reactableOutput('gt6_mr',width = '722px')
                                               # ),


                                     )
                              )
                            ),
                            pickerInput(
                              inputId = "select_arretee_fichewilaya_mr",
                              label = "",
                              choices = sort(paste(c("Arrétée le :"),unique(moyens_realisation2$Arretee)),TRUE),
                              #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
                            )
                   ),
                   tabPanel("Présentations",
                            fluidRow(
                              id="tabpresentations",
                              pickerInput(
                                inputId = "select_presentations",
                                label = "",
                                choices = c("Bilan et Actions 3T 2021","Synth?se du Bilan et Actions","Suivi de la mise en oeuvre du plan d'Action du Gouvernement")
                                #sort(paste(c("Arr?t?e le :"),unique(data_fiche_wilaya$arretee)),TRUE),
                                #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
                              ),
                              htmlOutput("presentation_pptx")
                            )
                   )
        )
        ################################### fin DGCMR

        #, navbarMenu("Données",
        #            tabPanel("Livraisons des logements ",
        #                   fluidRow(id='donnees1_liv',
        #                     fluidRow(id="rowdonnees1",style="padding-top: 25px",
        #                              column(2,offset = 2,style="padding-top: 22px;",
        #                                     pickerInput(
        #                                       inputId = "selectwilayas",
        #                                       label = "",
        #                                       choices = unique(livraison_wilayas$waw),
        #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
        #                                                      `selected-text-format`= "count>2",
        #                                                      `count-selected-text` = "{0} Wilayas séléctionnés",
        #                                                      `none-selected-text` ="Wilaya",`select-all-text`=
        #                                                        tags$div(
        #                                                          "Séléctionner",
        #                                                          tags$br(),
        #                                                          "Tout"
        #                                                        ),
        #                                                      `deselect-all-text`=tags$div(
        #                                                        "Deselectionner",
        #                                                        tags$br(),
        #                                                        "Tout"
        #                                                      )),
        #                                       multiple = TRUE
        #                                     )
        #
        #
        #                              ),
        #                              column(2,style="padding-top: 22px;",
        #                                     pickerInput(
        #                                       inputId = "selectsegments",
        #                                       label = "",
        #                                       choices = unique(livraison_wilayas$type_de_logement),
        #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
        #                                                      `selected-text-format`= "count>2",
        #                                                      `count-selected-text` = "{0} Segment séléctionnés",
        #                                                      `none-selected-text` ="Segment",`select-all-text`=
        #                                                        tags$div(
        #                                                          "Séléctionner",
        #                                                          tags$br(),
        #                                                          "Tout"
        #                                                        ),
        #                                                      `deselect-all-text`=tags$div(
        #                                                        "Deselectionner",
        #                                                        tags$br(),
        #                                                        "Tout"
        #                                                      )),
        #                                       multiple = TRUE
        #                                     )
        #                              ),
        #                              column(4,
        #                                     sliderInput("selectannees","",
        #                                                 min = min(unique(livraison_wilayas$annee)), max = max(unique(livraison_wilayas$annee)), value = c(min(unique(livraison_wilayas$annee)),max(unique(livraison_wilayas$annee))),sep = "")
        #
        #                              )
        #                     ),
        #                     fluidRow(id="rowdonnes2",
        #                              column(2,offset = 3,
        #                                     prettySwitch(
        #                                       inputId = "parwilayas",
        #                                       label = "Par Wilaya",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=FALSE
        #                                     )
        #
        #                              ),
        #                              column(2,style="padding-left:310px;",
        #                                     prettySwitch(
        #                                       inputId = "parsegments",
        #                                       label = "Par Segment",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=TRUE
        #                                     )
        #                              ),
        #                              column(2,style="padding-left:310px;",
        #                                     prettySwitch(
        #                                       inputId = "parperiode",
        #                                       label = "Par Année",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=FALSE
        #                                     )
        #                              )
        #                     ),
        #                     fluidRow(id="rowdonnes33",style="margin-top:35px;",
        #                              htmlOutput('rowdonnees33html')
        #                     ),
        #                     fluidRow(
        #
        #                       excelOutput("donnees_excel",height="100%")
        #
        #                     )
        #
        #            )),
        #            tabPanel("Lancements des logements ",
        #
        #                     fluidRow(id="rowdonnees1",style="padding-top: 25px",
        #                              column(2,offset = 2,style="padding-top: 22px;",
        #                                     pickerInput(
        #                                       inputId = "selectwilayas_lanc",
        #                                       label = "",
        #                                       choices = unique(livraison_wilayas$waw),
        #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
        #                                                      `selected-text-format`= "count>2",
        #                                                      `count-selected-text` = "{0} Wilayas séléctionnés",
        #                                                      `none-selected-text` ="Wilaya",`select-all-text`=
        #                                                        tags$div(
        #                                                          "Séléctionner",
        #                                                          tags$br(),
        #                                                          "Tout"
        #                                                        ),
        #                                                      `deselect-all-text`=tags$div(
        #                                                        "Deselectionner",
        #                                                        tags$br(),
        #                                                        "Tout"
        #                                                      )),
        #                                       multiple = TRUE
        #                                     )
        #
        #
        #                              ),
        #                              column(2,style="padding-top: 22px;",
        #                                     pickerInput(
        #                                       inputId = "selectsegments_lanc",
        #                                       label = "",
        #                                       choices = unique(livraison_wilayas$type_de_logement),
        #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
        #                                                      `selected-text-format`= "count>2",
        #                                                      `count-selected-text` = "{0} Segment séléctionnés",
        #                                                      `none-selected-text` ="Segment",`select-all-text`=
        #                                                        tags$div(
        #                                                          "Séléctionner",
        #                                                          tags$br(),
        #                                                          "Tout"
        #                                                        ),
        #                                                      `deselect-all-text`=tags$div(
        #                                                        "Deselectionner",
        #                                                        tags$br(),
        #                                                        "Tout"
        #                                                      )),
        #                                       multiple = TRUE
        #                                     )
        #                              ),
        #                              column(4,
        #                                     sliderInput("selectannees_lanc","",
        #                                                 min = min(unique(lancement_wilayas$Annee)), max = max(unique(lancement_wilayas$Annee)), value = c(min(unique(lancement_wilayas$Annee)),max(unique(lancement_wilayas$Annee))),sep = "")
        #
        #                              )
        #                     ),
        #                     fluidRow(id="rowdonnes2",
        #                              column(2,offset = 3,
        #                                     prettySwitch(
        #                                       inputId = "parwilayas_lanc",
        #                                       label = "Par Wilaya",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=FALSE
        #                                     )
        #
        #                              ),
        #                              column(2,style="padding-left:310px;",
        #                                     prettySwitch(
        #                                       inputId = "parsegments_lanc",
        #                                       label = "Par Segment",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=TRUE
        #                                     )
        #                              ),
        #                              column(2,style="padding-left:310px;",
        #                                     prettySwitch(
        #                                       inputId = "parperiode_lanc",
        #                                       label = "Par Année",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=FALSE
        #                                     )
        #                              )
        #                     ),
        #                     fluidRow(id="rowdonnes33",style="margin-top:35px;",
        #                              htmlOutput('rowdonnees33html_lanc')
        #                     ),
        #                     fluidRow(
        #
        #                       excelOutput("donnees_excel_lanc",height="100%")
        #
        #                     )
        #
        #            ),
        #
        #
        #            tabPanel("Données des Wilaya",
        #
        #                     fluidRow(id="rowdonnees1",style="padding-top: 25px",
        #                              column(2,offset = 3,style="padding-top: 22px;",
        #                                     pickerInput(
        #                                       inputId = "selectwilayas_wilaya",
        #                                       label = "",
        #                                       choices = unique(livraison_wilayas$waw),
        #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
        #                                                      `selected-text-format`= "count>2",
        #                                                      `count-selected-text` = "{0} Wilayas séléctionnés",
        #                                                      `none-selected-text` ="Wilaya",`select-all-text`=
        #                                                        tags$div(
        #                                                          "Séléctionner",
        #                                                          tags$br(),
        #                                                          "Tout"
        #                                                        ),
        #                                                      `deselect-all-text`=tags$div(
        #                                                        "Deselectionner",
        #                                                        tags$br(),
        #                                                        "Tout"
        #                                                      )),
        #                                       multiple = TRUE
        #                                     )
        #
        #
        #                              ),
        #
        #                              column(4,
        #                                     sliderInput("selectannees_wilaya","",
        #                                                 min = min(unique(estimation_tolpopparc$Annee)), max = max(unique(estimation_tolpopparc$Annee)), value = c(min(unique(estimation_tolpopparc$Annee)),max(unique(estimation_tolpopparc$Annee))),sep = "")
        #
        #                              )
        #                     ),
        #                     fluidRow(id="rowdonnes2",style="margin-left: -6px;margin-bottom:30px; ",
        #                              column(2,offset = 5,
        #                                     prettySwitch(
        #                                       inputId = "parwilayas_wilaya",
        #                                       label = "Par Wilayas",
        #                                       status = "primary",
        #                                       slim = TRUE,
        #                                       value=FALSE
        #                                     )
        #
        #                              )),
        #                     fluidRow(id="rowdonnes332",style="margin-top:35px;",
        #                              htmlOutput('rowdonnees33html2')
        #                     ),
        #                     fluidRow(
        #
        #                       excelOutput("donnees_excel_wilaya",height="100%")
        #
        #                     )
        #
        #            ))

      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom shinyjs extendShinyjs useShinyjs
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom fresh use_theme create_theme bs_vars_wells
#' @importFrom shinyWidgets setBackgroundColor
#' @noRd
golem_add_external_resources <- function(){



  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'go5'
    ),

    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),

    #includeScript("www/check.js"),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode1, functions = c("opac1")),
    extendShinyjs(text = jsCode2, functions = c("opac2")),
    extendShinyjs(text = jsCode3, functions = c("opac3")),
    extendShinyjs(text = jsCode4, functions = c("opac4")),
    extendShinyjs(text = jsCode5, functions = c("opac5")),
    extendShinyjs(text = jsCode6, functions = c("opac6")),
    #extendShinyjs(text = jsCode77, functions = c("opac77")),
    extendShinyjs(text = jsCode88, functions = c("opac88")),

    extendShinyjs(text = jsCode99, functions = c("opac99")),
    #extendShinyjs(text = jsCode1010, functions = c("opac1010")),
    #extendShinyjs(text = jsCode1010, functions = c("opac1111")),



    use_theme(create_theme(theme="united",bs_vars_wells(bg="#fff"))),
    setBackgroundColor(
      color = c("#f2f2f2", "#f2f2f2"),
      gradient = "radial",
      direction = c("top", "left")
    )

    )
}

