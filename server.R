library(shiny)
library(knitr)
library(fmsb)
library(plotly)
library(gganimate)
library(png)
library(tidyverse)

raw_db <- read_csv("raw_db.csv")

###### Create formated database ######
db = raw_db %>%
  pivot_longer(cols = c(Temps_total, Vit_max, Acc_max, Dist_sup_6km_h,
                        Dist_70_80_max, Dist_80_90_max, Dist_sup_90_max,
                        Acc_sup_3ms2, Nb_sprints_sup_90_max),
               names_to = "Variable", values_to = 'Valeur') %>%
  mutate(Valeur = round(Valeur, 2))

db_centree = raw_db %>%
  dplyr::mutate(dplyr::across(Temps_total:Nb_sprints_sup_90_max,
                ~ (.x - mean(.x)) / sd(.x))) %>%
  pivot_longer(cols = c(Temps_total, Vit_max, Acc_max, Dist_sup_6km_h,
                        Dist_70_80_max, Dist_80_90_max, Dist_sup_90_max,
                        Acc_sup_3ms2, Nb_sprints_sup_90_max),
               names_to = "Variable", values_to = 'Valeur') %>%
  mutate(Valeur_scaled = round(Valeur, 2)) %>%
  select(- Valeur)

##### Shiny Server #####
shinyServer(function(input, output) {

##### Plot graphs ######

##### Reactive UI ######
  select_train = reactive(input$select_train)
  select_exer = reactive(input$select_exer)

  gg1 = reactive(db %>%
    left_join(db_centree) %>%
    group_by(Entrainement, Exercice , Variable) %>%
    filter(Entrainement %in% select_train()) %>%
    filter(!Exercice %in% select_exer()) %>%
    ggplot() +
    geom_tile(aes(x = Variable, y = Exercice, fill = Valeur_scaled)) +
    scale_fill_gradient(low = "red", high = "green")+
    geom_text(aes(x = Variable, y = Exercice, label = Valeur)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(Entrainement ~., scales = 'free_y', space='free'))


  select_exer2 = reactive(input$select_exer2)
  select_var2 = reactive(input$select_var2)

  gg2 =  reactive(db %>%
    filter(Exercice == select_exer2()) %>%
    filter(Variable %in% select_var2()) %>%
    pivot_wider(names_from = Variable, values_from = Valeur) %>%
    mutate(Time = 1:nrow(.)) %>%
    mutate(Entrainement = fct_reorder(as.factor(Entrainement),Time)) %>%
    pivot_longer(select_var2(),
                 names_to = 'Variable', values_to = 'Valeur') %>%
    ggplot() +
    geom_line(aes(x = Entrainement, y = Valeur, group = Variable, color = Variable)) +
    geom_point(aes(x = Entrainement, y = Valeur, group = Variable, color = Variable)) +
    theme_classic() +
    ggtitle("Evolution des sessions au cours des entrainements successifs") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

  select_train3 = reactive(input$select_train3)
  select_exer3 = reactive(input$select_exer3)
  select_var3 = reactive(input$select_var3)

  gg3 = reactive(db %>%
    filter(Entrainement == select_train3())  %>%
    filter(!Exercice %in% select_exer3()) %>%
    filter(Variable %in% select_var3()) %>%
    mutate(Exercice = fct_reorder(as.factor(Exercice),Numero_exercice)) %>%
    ggplot() +
    geom_bar(aes(x = Variable, y = Valeur, fill = Exercice),
             position="stack", stat="identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle("Evolution de la session au cours des exercices successifs"))

  select_exer4 = reactive(input$select_exer4)
  select_var4_1 = reactive(input$select_var4_1)
  select_var4_2 = reactive(input$select_var4_2)
  select_var4_3 = reactive(input$select_var4_3)

  gg4 = reactive(db %>%
    filter(Exercice %in% select_exer4()) %>%
    filter(Variable %in% c(select_var4_1(), select_var4_2(), select_var4_3())) %>%
    pivot_wider(names_from = Variable, values_from = Valeur) %>%
    ggplot() +
    geom_point(aes_string(x = select_var4_1(), y = select_var4_2(),
                   size = select_var4_3(), color = 'Entrainement')) +
    theme_classic() +
    ggtitle('Evolution de 3 variables au cours des séances'))

##### Display graphs ######
  output$logo_sfds<-renderImage({
    return(list(
      src = "logo_sfds2.png",
      width = 400,
      height = 300
    ))}, deleteFile = FALSE)

    output$logo_insep<-renderImage({
    return(list(
      src = "logo_insep.png",
      width = 350,
      height = 200
    ))}, deleteFile = FALSE)

  output$heatmap <- renderPlot({gg1()}, width = 750, height = 600, res = 80)

  output$timeseries <- renderPlot({gg2()}, width = 750, height = 600, res = 80)

  output$cumule <- renderPlot({gg3()}, width = 750, height = 600, res = 80)

  output$nuage <- renderPlot({gg4()}, width = 750, height = 600, res = 80)

##### Display tables ######
    output$raw <- renderDataTable(raw_db)

##### Settings for the PDF exports ######

    output$export1 = downloadHandler(
      filename = function() {paste0(input$name_export1, ".",
                                    input$format_export1)},
      content = function(file) {

        if(input$format_export1 == 'pdf'){
          pdf(file, onefile = TRUE, width = 10, height = 6)
        } else {
          png(file, width = 900, height = 600, res = 80)
        }
        gg1() %>% print()
        dev.off()
      }
    )

    output$export2 = downloadHandler(
      filename = function() {paste0(input$name_export2, ".",
                                    input$format_export2)},
      content = function(file) {

        if(input$format_export2 == 'pdf'){
          pdf(file, onefile = TRUE, width = 10, height = 6)
        } else {
          png(file, width = 900, height = 600, res = 80)
        }
        gg2() %>% print()
        dev.off()
      }
    )

    output$export3 = downloadHandler(
      filename = function() {paste0(input$name_export3, ".",
                                    input$format_export3)},
      content = function(file) {

        if(input$format_export3 == 'pdf'){
          pdf(file, onefile = TRUE, width = 10, height = 6)
        } else {
          png(file, width = 900, height = 600, res = 80)
        }
        gg3() %>% print()
        dev.off()
      }
    )

    output$export4 = downloadHandler(
      filename = function() {paste0(input$name_export4, ".",
                                    input$format_export4)},
      content = function(file) {

        if(input$format_export4 == 'pdf'){
          pdf(file, onefile = TRUE, width = 10, height = 6)
        } else {
          png(file, width = 900, height = 600, res = 80)
        }
        gg4() %>% print()
        dev.off()
      }
    )
})
