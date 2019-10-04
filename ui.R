#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(shinycssloaders) 

header <- dashboardHeader(title = "Klasifikavimo algoritmai",titleWidth = 250)

sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "menu",
              menuItem("About", icon = icon("th"), tabName = "ML")),
  
  hr(),
  
  sidebarMenu(id = "menu1",
              menuItem("Klasifikavimo algoritmai", icon = icon("th"), tabName = "CL",
                       menuSubItem("Sprendimu medzio klasifikatorius", tabName = "DT", icon = icon("dashboard")),
                       menuSubItem("Atsitiktinis misko klasifikatorius", tabName = "RF", icon = icon("dashboard")),
                       menuSubItem("k-Artimiausiu Kaimynu klasifikatorius", tabName = "KNN", icon = icon("dashboard")),
                       menuSubItem("Logistine regresija", tabName = "LOGR", icon = icon("dashboard")),
                       menuSubItem("Atraminiu vektoriu klasifikatorius", tabName = "SVM", icon = icon("dashboard")),
                       menuSubItem("Rezultatai", tabName = "rezultat", icon = icon("dashboard"))
                       # menuSubItem("Neural Network", tabName = "NN", icon = icon("dashboard"))
                       
              )
  ),
  
  width = 270
  
)

# Body ----  
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "ML",
            fluidPage(
              titlePanel("TalkingData AdTracking Fraud Detection"),
              p("Darbe bus pateikiamos klasifikatoriu prognozes keiciant klasifikatoriu parametrus."),
              p("Visa programini koda galima rasti https://github.com/donkratas/Rshiny,
                taip pat, neur.R faila, kuriame pateiktas ir neuroniniu tinklu klasifikatorius."),
              p("Cia pateikiamas tik aplikacijos prototipas."),
              p("TalkingData nepriklausoma didelių duomenų įmonė, kuri apdoroja daugiau nei 70% visų telefonų
                paspaudimų Kinijoje. Jų didžiausias tikslas nustatyti ar paspaudimas buvo sukčiavimo atvejis ar ne
                naudojant informaciją apie atsisiųstų programų kiekį. Nustačius, kad tai buvo sukčiavimo atvejis IP
                adresą pridėti į juodąjį sąrašą")
              )
              ),
    # Desision Trees ----       
    tabItem(tabName = "DT",
            fluidRow(
              column(width = 12, 
                     tabBox( width = NULL,
                             tabPanel(h5("Apie"),
                                      p("Sprendimu medzio mokymosi metodas yra dazniausiai naudojamas metodas duomenu isgavime.[1] Metodo tikslas - sukurti modeli, kuris prognozuoja priklausomo kintamojo verte remdamasis keliais nepriklausomais kintamaisiais. Desineje schemoje galite pamatyti pavyzdi. Kiekvienas vidinis mazgas atitinka viena is nepriklausomu kintamuju;  tieses keliauja iki posakiu kiekvienam imanomam nepriklausomo kintamojo variantui. Kiekvienas lapas - tai priklausomo kintamojo reiksme atsizvelgiant i nepriklausomus kintamuosius, kurie yra pavaizduoti kelyje nuo medzio kamieno iki lapo.
                                        Sprendimu medzio metodas - paprastas klasiu pavaizdavimo pavyzdys. Siame skirsnyje, tarkime, kad visos ypatybes  bus imamos is baigtines bei diskrecios srities, o isskirtine norima ypatybe bus vadinama  klasifikacija. Kiekvienas klasifikacijos srities elementas bus vadinamas klase.  Sprendimu medis (arba klasifikavimo medis) - medis, kuriame kiekvienas vidinis (belapis) mazgas pazenklinti tam tikra ivesties ypatybe. Lankai, einantys is mazgo, pazymeto ta ypatybe, arba zymi  visas gautas galimas ypatybes, arba  veda prie antraeilio sprendimu mazgo kitai ivesties ypatybei. Visi medzio lapai pazymeti savo klase ar tikimybiniu klasiu skirstiniu.")),
                             tabPanel(h5("Duomenys"),
                                      withSpinner(DT::dataTableOutput(outputId="table_for_dt"))),
                             tabPanel(h5("Prognozavimas"),
                                      fluidRow(
                                        box(
                                          title = "Sprendimu medzio klasifikatoriaus parametrai",
                                          # background = "light-blue",
                                          status = "success",
                                          solidHeader = TRUE,
                                          width = 3,
                                          collapsible = TRUE,
                                          uiOutput("classvar"),
                                          tags$hr(),
                                          uiOutput("depth"),
                                          uiOutput("split"),
                                          uiOutput("number_of_folds_dt"),
                                          uiOutput("cv_repeats_dt"),
                                          tags$hr(),
                                          actionButton("action", "Sukurti/atnaujinti modeli")
                                        ),
                                        box(
                                          title = "Prognozes tikslumas ant testavimo aibes",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 4,
                                          collapsible = TRUE,
                                          verbatimTextOutput("pred"),
                                          tags$head(tags$style("#pred{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("accur"),
                                          tags$head(tags$style("#accur{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("AUC_dt"),
                                          tags$head(tags$style("#AUC_dt{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}"))
                                          )
                                          ),
                                      fluidRow(
                                        box(
                                          title = "Sprendimu medis",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          collapsible = TRUE,
                                          plotOutput("plot",height="500px")
                                        )
                                      )
                                          ),
                             tabPanel(h5("Prognozuoti duomenys"),
                                      
                                      DT::dataTableOutput(outputId="table2")
                                      
                             )
              )
    )
                             )
              ),
    tabItem(tabName = "RF",
            fluidRow(
              column(width = 12,
                     tabBox( width = NULL,
                             tabPanel(h5("Apie"),
                                      p("Atsiktinio misko (angl. Random forest) algoritmas apmoko keleta sprendimo medziu
                                        . Naudojant atsitiktini miska pradinis duomenu rinkinys yra atsitiktinai
                                        padalijama i keleta to paties dydzio mazesniu rinkiniu. Sprendimu medis yra ismokomas
                                        kiekvienam padalintam mazesniam rinkiniui. Kiekvienas gautas mazesnis bruozu
                                        rinkinys yra parenkamas kiekvienam sprendimo medzio mokymuisi. Vykstant
                                        klasifikacijai visi sprendimu medziai yra pereinami ir apskaiciuojama balsu dauguma
                                        (angl. majority vote) is visu suklasifikavimu.")
                                      ),
                             tabPanel(h5("Duomenys"),
                                      p(""),
                                      p(""),
                                      withSpinner(DT::dataTableOutput(outputId="table_for_rf"))),
                             tabPanel(h5("Prognozavimas"),
                                      fluidRow(
                                        box(
                                          title = "Atsitiktinio misko klasifikatoriaus parametrai",
                                          # background = "light-blue",
                                          status = "success",
                                          solidHeader = TRUE,
                                          width = 3,
                                          collapsible = TRUE,
                                          uiOutput("classvara"),
                                          tags$hr(),
                                          uiOutput("ntree"),
                                          # uiOutput("tuneLength"),
                                          uiOutput("number_of_folds"),
                                          uiOutput("cv_repeats"),
                                          actionButton("action_rf", "Sukurti/atnaujinti modeli")
                                        ),
                                        box(
                                          title = "Prognozes tikslumas ant testavimo aibes",
                                          #background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 4,
                                          collapsible = TRUE,
                                          verbatimTextOutput("pred1"),
                                          tags$head(tags$style("#pred1{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("accur1"),
                                          tags$head(tags$style("#accur1{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("AUC_rf"),
                                          tags$head(tags$style("#AUC_rf{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}"))
                                          ),
                                        fluidRow(
                                          box(
                                            title = "Rezultatai ant mokymosi duomenu",
                                            # background = "navy",
                                            status = "primary",
                                            solidHeader = TRUE,
                                            width = 12,
                                            collapsible = TRUE,
                                            withSpinner(plotOutput("plot_rf",height="500px"))
                                          )
                                        )
                                          )),
                             tabPanel(h5("Prognozuoti duomenys"),
                                      DT::dataTableOutput(outputId="table21"))
                             
                             )
            )
              )
                             ),
    tabItem(tabName = "KNN",
            fluidRow(
              column(width = 12, 
                     tabBox( width = NULL,
                             tabPanel(h5("Apie"),
                                      p("K artimiausio kaimyno klasifikatorius yra vienas is paprasciausiu naudojamu algoritmu. Algoritmo veikimas pagristas
                                        atstumo nustatymu nuo tiriamo tasko iki apmokymo duomenu. Standartiskai atstumas
                                        skaiciuojamas pasitelkiant Euklido atstumo lygti.
                                        Vykdant artimiausio kaimyno metodo apmokyma, pateikiami apmokymo duomenis su
                                        priskirtomis klasemis. Jei artimiausiu kaimynu skaicius K=1, tiriamam taskui priskiriama
                                        kaimyno, iki kurio atstumas yra maziausias, klase.")
                                      ),
                             tabPanel(h5("Duomenys"),
                                      withSpinner(DT::dataTableOutput(outputId="qqq"))),
                             tabPanel(h5("Prognozavimas"),
                                      fluidRow(
                                        box(
                                          title = "k-Artimiausiu kaimynu klasifikatoriaus parametrai",
                                          # background = "light-blue",
                                          status = "success",
                                          solidHeader = TRUE,
                                          width = 3,
                                          collapsible = TRUE,
                                          tags$hr(),
                                          uiOutput("k_number"),
                                          uiOutput("number_of_folds_knn"),
                                          uiOutput("cv_repeats_knn"),
                                          tags$hr(),
                                          actionButton("action_knn", "Sukurti/atnaujinti modeli")
                                        ),
                                        box(
                                          title = "Prognozes tikslumas ant testavimo aibes",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 4,
                                          collapsible = TRUE,
                                          verbatimTextOutput("pred_knn"),
                                          tags$head(tags$style("#pred_knn{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("accur_knn"),
                                          tags$head(tags$style("#accur_knn{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("ROC_knn"),
                                          tags$head(tags$style("#ROC_knn{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}"))
                                          )
                                          ),
                                      fluidRow(
                                        box(
                                          title = "Rezultatai ant mokymosi duomenu",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          collapsible = TRUE,
                                          plotOutput("k_neighbours",height="500px")
                                        )
                                      ))
                             ,
                             tabPanel(h5("Prognozuoti duomenys"),
                                      DT::dataTableOutput(outputId="table_knn")
                                      
                             )
                                        )
            )
            )
                                      ),
    tabItem(tabName = "SVM",
            fluidRow(
              column(width = 12,
                     tabBox( width = NULL,
                             tabPanel(h5("Apie"),
                                      p("Atraminiu vektoriu masina- tai statistiniu pozymiu klasifikavimo metodas, istobulintas V.
                                        Vapnicko 1995 metais. Pagrindine sio metodo ideja- apmokymo duomenu projektavimas i
                                        bendra pozymiu erdve ir sukurimas hiperplokstumos, atskiriancios skirtingas pozymiu klases.
                                        Atraminiu vektoriu masinos metodas yra vienas is populiariausiu metodu del galimybes spresti
                                        sudetingus klasifikavimo uzdavinius. Kitas sio metodo privalumas yra jo greitaveika lyginant su
                                        kitais dirbtinio intelekto metodais naudojamais klasifikavimui")),
                             tabPanel(h5("Duomenys"),
                                      p(""),
                                      p(""),
                                      withSpinner(DT::dataTableOutput(outputId="table_for_svm"))),
                             tabPanel(h5("Prognozavimas"),
                                      fluidRow(
                                        box(
                                          title = "Atraminiu vektoriu klasifikatoriaus parametrai:",
                                          # background = "light-blue",
                                          status = "success",
                                          solidHeader = TRUE,
                                          width = 3,
                                          collapsible = TRUE,
                                          tags$hr(),
                                          uiOutput("number_of_folds_svm"),
                                          uiOutput("cv_repeats_svm"),
                                          uiOutput("sigma1"),
                                          tags$hr(),
                                          actionButton("action_svm", "Sukurti/atnaujinti modeli")
                                        ),
                                        box(
                                          title = "Prognozes tikslumas ant testavimo aibes",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 4,
                                          collapsible = TRUE,
                                          verbatimTextOutput("pred_svm"),
                                          tags$head(tags$style("#pred_svm{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("accur_svm"),
                                          tags$head(tags$style("#accur_svm{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("ROC_svm"),
                                          tags$head(tags$style("#ROC_svm{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}"))
                                          )
                                          ),
                                      fluidRow(
                                        box(
                                          title = "Prognozes tikslumas ant testavimo aibes",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 12,
                                          collapsible = TRUE,
                                          withSpinner(plotOutput("plot_svm",height="500px"), type = 8)
                                        ))),
                             tabPanel(h5("Prognozuoti duomenys"),
                                      withSpinner(DT::dataTableOutput(outputId="table_svm")))
                             
                                        )))),
    tabItem(tabName = "LOGR",
            fluidRow(
              column(width = 12,
                     tabBox( width = NULL,
                             tabPanel(h5("Apie"),
                                      p("Logistine regresija (angl. Logistic regression) yra optimizacijos algoritmas (angl.
                                        optimization algorithm - procedura, kuri yra kartojanciai paleidziama lyginant ivairius
                                        sprendimus iki tol kol geriausias ar patenkinamas sprendimas yra atrastas). Dirbant su
                                        logistines regresijos algoritmu ir turint tam tikrus duomenis su jais bandoma sukurti lygti
                                        (angl. equation), kuri atliktu klasifikavima")
                             ),
                             tabPanel(h5("Duomenys"),
                                      withSpinner(DT::dataTableOutput(outputId="table_for_log"))),
                             tabPanel(h5("Prognozavimas"),
                                      fluidRow(
                                        box(
                                          title = "Logistines regresijos parametrai:",
                                          # background = "light-blue",
                                          status = "success",
                                          solidHeader = TRUE,
                                          width = 3,
                                          collapsible = TRUE,
                                          tags$hr(),
                                          uiOutput("number_of_folds_log"),
                                          uiOutput("cv_repeats_log"),
                                          tags$hr(),
                                          actionButton("action_log", "Sukurti/atnaujinti modeli")
                                        ),
                                        box(
                                          title = "Prognozes tikslumas ant testavimo aibes",
                                          # background = "navy",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 4,
                                          collapsible = TRUE,
                                          verbatimTextOutput("pred_log"),
                                          tags$head(tags$style("#pred_log{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("accur_log"),
                                          tags$head(tags$style("#accur_log{color:black; font-size:16px; font-style:initial;
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}")),
                                          verbatimTextOutput("ROC_log"),
                                          tags$head(tags$style("#ROC_log{color:black; font-size:16px; font-style:initial; 
                                                               max-height: 500px; max-width: 500px; background: ghostwhite;}"))
                                          )
                                          )
                                      # fluidRow(
                                      #   box(
                                      #     title = "tune",
                                      #     # background = "navy",
                                      #     status = "primary",
                                      #     solidHeader = TRUE,
                                      #     width = 12,
                                      #     collapsible = TRUE,
                                      #     withSpinner(plotOutput("plot_log",height="500px"), type = 8)
                                      #   ))
                                          ),
                             tabPanel(h5("Prognozuoti duomenys"),
                                      withSpinner(DT::dataTableOutput(outputId="table_log")))
              )))),
    tabItem(tabName = "rezultat",
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Atraminiu vektoriu klasifikatoriaus rezultatai",
                  # background = "navy",
                  status = "success",
                  solidHeader = F,
                  width = 19,
                  collapsible = F,
                  verbatimTextOutput("pred_svm_rez"),
                  tags$head(tags$style("#pred_svm_rez{color:black; font-size:16px; font-style:initial;
                                       max-height: 400px; max-width: 400px; background: ghostwhite;}")),
                  verbatimTextOutput("ROC_svm_rez"),
                  tags$head(tags$style("#ROC_svm_rez{color:black; font-size:16px; font-style:initial; 
                                       max-height: 400px; max-width: 400px; background: ghostwhite;}"))
                  )),
              column(4,
                     box(title = "Logistines regresijos rezultatai", #weights
                         # background = "navy",
                         status = "success",
                         solidHeader = F,
                         width = 19,
                         collapsible = F,
                         verbatimTextOutput("pred_log_rez"),
                         tags$head(tags$style("#pred_log_rez{color:black; font-size:16px; font-style:initial;
                                              max-height: 400px; max-width: 400px; background: ghostwhite;}")),
                         verbatimTextOutput("ROC_log_rez"),
                         tags$head(tags$style("#ROC_log_rez{color:black; font-size:16px; font-style:initial; 
                                              max-height: 400px; max-width: 400px; background: ghostwhite;}")))),
              column(4,
                     box(title = "k-Artimiausiu Kaimynu klasifikatoriaus rezultatai",
                         # background = "navy",
                         status = "success",
                         solidHeader = F,
                         width = 19,
                         collapsible = F,
                         verbatimTextOutput("pred_knn_rez"),
                         tags$head(tags$style("#pred_knn_rez{color:black; font-size:16px; font-style:initial;
                                              max-height: 400px; max-width: 400px; background: ghostwhite;}")),
                         verbatimTextOutput("ROC_knn_rez"),
                         tags$head(tags$style("#ROC_knn_rez{color:black; font-size:16px; font-style:initial; 
                                              max-height: 400px; max-width: 400px; background: ghostwhite;}")))
                     
                         ),
              column(4, #lenteles plotis 
                     box(
                       title = "Sprendimu medzio klasifikatoriaus rezultatai",
                       # background = "navy",
                       status = "success",
                       solidHeader = F,
                       width = 19,
                       collapsible = F,
                       verbatimTextOutput("pred_dt_rez"),
                       tags$head(tags$style("#pred_dt_rez{color:black; font-size:16px; font-style:initial;
                                            max-height: 400px; max-width: 400px; background: ghostwhite;}")),
                       verbatimTextOutput("ROC_dt_rez"),
                       tags$head(tags$style("#ROC_dt_rez{color:black; font-size:16px; font-style:initial; 
                                            max-height: 400px; max-width: 400px; background: ghostwhite;}")))
                       ),
              column(4,
                     box(
                       title = "Atsitiktinio misko klasifikatoriaus rezultatai",
                       # background = "navy",
                       status = "success",
                       solidHeader = F,
                       width = 19,
                       collapsible = F,
                       verbatimTextOutput("pred_rf_rez"),
                       tags$head(tags$style("#pred_rf_rez{color:black; font-size:16px; font-style:initial;
                                            max-height: 400px; max-width: 400px; background: ghostwhite;}")),
                       verbatimTextOutput("ROC_rf_rez"),
                       tags$head(tags$style("#ROC_rf_rez{color:black; font-size:16px; font-style:initial; 
                                            max-height: 400px; max-width: 400px; background: ghostwhite;}")))
                       )
              
              
              
                       )
            
            
                     )
    
                         )
  
  
  
              )




dashboardPage(header, sidebar, body)

