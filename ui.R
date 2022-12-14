library(shiny)
library(knitr)
library(fmsb)
library(plotly)
library(gganimate)
library(tidyverse)

raw_db <- read_csv("raw_db.csv")

shinyUI(fluidPage(
  # Application title
  tabsetPanel(
    tabPanel("Accueil",
              fluidRow(column(width = 6, align="center", imageOutput("logo_sfds", height = "280px")),
                       column(width = 6, align="center", imageOutput("logo_insep", height = "280px"))),
               fluidRow(column(width = 12),
                h2("Bienvenue dans cette application dédiée à la data visualisation", align="center"),
                h4("Onglet 1:", align="center"),
                p("Représenter les variables clefs de séances d'entrainement complètes à l'aide d'une heatmap", align="center"),
                h4("Onglet 2:", align="center"),
                p("Visualiser la distance parcourue à différentes intensités pour chaque exercice d'une séance d'entrainement", align="center"),
                h4("Onglet 3: ", align="center"),
                p("Suivre l'évolution de variables clefs au cours du temps à travers les entrainemetns successifs", align="center"),
                h4("Onglet 4: ", align="center"),
                p("Se concentrer sur un petit nombre de variables pour mieux analyser l'évolution de leur relation au fil des entrainements", align="center")
               )
    ),
    tabPanel("Résumé séance",
             titlePanel("Résumé de séance et comparaison"),sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "select_train", "Séance d'entrainement",
                             unique(raw_db$Entrainement), multiple = TRUE,
                             selected = unique(raw_db$Entrainement)[1]),
                 selectInput(inputId = "select_exer", "Ignorer un exercice",
                             unique(raw_db$Exercice), multiple = TRUE,
                             selected = 'Session'),
                 textInput("name_export1", 'Nom du fichier', 'my_plot'),
                 radioButtons("format_export1","Format du fichier:",
                              choices=c("pdf","png")),
                 downloadButton(outputId = 'export1')
                ),
               mainPanel(
                 plotOutput("heatmap", width = "100%")
               )
          )
    ),
    tabPanel("Séries temporelles par séance",
             titlePanel("Séries temporelles par séance"),sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "select_exer2", "Exercice",
                             unique(raw_db$Exercice), multiple = FALSE,
                             selected = 'Session'),
                 selectInput(inputId = "select_var2", "Variables",
                             c( "Temps_total" , 'Vit_max', 'Acc_max',
                                'Acc_sup_3ms2','Nb_sprints_sup_90_max',
                                "Dist_sup_6km_h", "Dist_70_80_max",
                                "Dist_80_90_max", "Dist_sup_90_max"),
                             multiple = TRUE,
                             selected = c('Vit_max', 'Acc_max', 'Acc_sup_3ms2',
                                          'Nb_sprints_sup_90_max')),
                 textInput("name_export2", 'Nom du fichier', 'my_plot'),
                 radioButtons("format_export2","Format du fichier:",
                              choices=c("pdf","png")),
                 downloadButton(outputId = 'export2')
               ),
               mainPanel(
                 plotOutput("timeseries", width = "100%")
               )
             )
    ),
    tabPanel("Evolution cumulée par exercice",
             titlePanel("Evolution cumulée par exercice"), sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "select_train3", "Séance d'entrainement",
                             unique(raw_db$Entrainement), multiple = FALSE,
                             selected = unique(raw_db$Entrainement)[1]),
                 selectInput(inputId = "select_exer3", "Ignorer un exercice",
                             unique(raw_db$Exercice), multiple = TRUE,
                             selected = 'Session'),
                 selectInput(inputId = "select_var3", "Variables",
                             c( "Temps_total" , 'Vit_max', 'Acc_max',
                                'Acc_sup_3ms2','Nb_sprints_sup_90_max',
                                "Dist_sup_6km_h", "Dist_70_80_max",
                                "Dist_80_90_max", "Dist_sup_90_max"),
                             multiple = TRUE,
                             selected = c("Dist_sup_6km_h", "Dist_70_80_max",
                                          "Dist_80_90_max", "Dist_sup_90_max")),
                 textInput("name_export3", 'Nom du fichier', 'my_plot'),
                 radioButtons("format_export3","Format du fichier:",
                              choices=c("pdf","png")),
                 downloadButton(outputId = 'export3')
               ),
               mainPanel(
                 plotOutput("cumule", width = "100%")
               )
             )
    ),
    tabPanel("Nuage de points par séance",
             titlePanel("Nuage de points par séance"),sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "select_exer4", "Exercice",
                             unique(raw_db$Exercice), multiple = TRUE,
                             selected = 'Session'),
                 selectInput(inputId = "select_var4_1", "Variable en abscisse",
                             c( "Temps_total" , 'Vit_max', 'Acc_max',
                                'Acc_sup_3ms2','Nb_sprints_sup_90_max',
                                "Dist_sup_6km_h", "Dist_70_80_max",
                                "Dist_80_90_max", "Dist_sup_90_max"),
                             multiple = FALSE,
                             selected = 'Vit_max'),
                 selectInput(inputId = "select_var4_2", "Variable en ordonnée",
                             c( "Temps_total" , 'Vit_max', 'Acc_max',
                                'Acc_sup_3ms2','Nb_sprints_sup_90_max',
                                "Dist_sup_6km_h", "Dist_70_80_max",
                                "Dist_80_90_max", "Dist_sup_90_max"),
                             multiple = FALSE,
                             selected = 'Acc_max'),
                 selectInput(inputId = "select_var4_3", "Taille des points",
                             c( "Temps_total" , 'Vit_max', 'Acc_max',
                                'Acc_sup_3ms2','Nb_sprints_sup_90_max',
                                "Dist_sup_6km_h", "Dist_70_80_max",
                                "Dist_80_90_max", "Dist_sup_90_max"),
                             multiple = FALSE,
                             selected = 'Temps_total'),
                 textInput("name_export4", 'Nom du fichier', 'my_plot'),
                 radioButtons("format_export4","Format du fichier:",
                              choices=c("pdf","png")),
                 downloadButton(outputId = 'export4')
               ),
               mainPanel(
                 plotOutput("nuage", width = "100%")
               )
             )
    ),
    tabPanel("Données brutes",
        dataTableOutput('raw')
    )
  )
))
