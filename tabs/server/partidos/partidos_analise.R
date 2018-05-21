##-- + Atualizações dos filtros ----
##-- ++ Atualizações dos cargos ----
observeEvent(input$partido_ano,{
  ano <- isolate(input$partido_ano)
  
  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano) %>%
      distinct(CODIGO_CARGO, DESCRICAO_CARGO)
    
    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$CODIGO_CARGO)
    ##-- Arrumar aqui
    cargos <- cargos[!(cargos %in% c("GOVERNADOR", "PRESIDENTE", "PREFEITO"))]
    cargo_default <- input$eleicoes_cargo_uf
    
    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }
    
    cargos_list <- setNames(as.list(chaves_sub$CODIGO_CARGO), chaves_sub$DESCRICAO_CARGO)
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "partido_cargo",
                      label = "Cargo", 
                      choices = cargos_list, 
                      selected = cargo_default)
  }
  
}, priority = 1)
##-- ++ Atualizações dos partidos mapa ----
# observeEvent(c(input$partido_ano,
#                input$partido_cargo),{
#                  
#                  ano <- isolate(input$partido_ano)
#                  cargo <- isolate(input$partido_cargo)
#                  
#                  if(!is.null(cargo)){
#                    chaves_sub <- chaves %>%
#                      filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo) %>%
#                      collect()
#                    
#                    ##-- Setando o cargo default
#                    partidos <- levels(factor(x = sort(unique(chaves_sub$SIGLA_PARTIDO)),
#                                              levels = sort(unique(chaves_sub$SIGLA_PARTIDO))))
#                    partido_default <- input$partido_partido_mapa
#                    
#                    if(!(partido_default %in% partidos)){
#                      partido_default <- partidos[1]
#                    }
#                    
#                    ##-- Atualizando os partidos ----
#                    updatePickerInput(session = session,
#                                      inputId = "partido_partido_mapa",
#                                      label = "Partido",
#                                      choices = partidos,
#                                      selected = partido_default)
#                  }
#                  
#                }, priority = 2)
##-- ++ Atualizações dos partidos donuts ----
observeEvent(c(input$partido_ano,
               input$partido_cargo),{
                 
                 ano <- isolate(input$partido_ano)
                 cargo <- isolate(input$partido_cargo)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo) %>%
                     collect()
                   
                   ##-- Setando o cargo default
                   partidos <- levels(factor(x = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO))),
                                             levels = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO)))))
                   partido_default <- input$partido_partido_donuts
                   
                   if(!(partido_default %in% partidos)){
                     partido_default <- "Todos os partidos"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "partido_partido_donuts",
                                     label = "Partido",
                                     choices = partidos,
                                     selected = partido_default)
                 }
                 
               }, priority = 3)
##-- + Dados de npumero de candidadtos ----
# base_ncand <- eventReactive(c(input$partido_partido_mapa, input$partidos_gerar_visualizacoes), {
#   
#   ano <- input$partido_ano
#   cargo <- input$partido_cargo
#   partido <- input$partido_partido_mapa
#   
#   if(partido == "Todos os partidos"){
#     partido <- partidos
#   }
# 
#   data("regUF")
#   
#   n_cand <- dados_gerais %>%
#     filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo) %>% 
#     mutate(DESCRICAO_CARGO = toupper(DESCRICAO_CARGO)) %>%
#     select(ANO_ELEICAO, UF, SIGLA_PARTIDO, DESCRICAO_CARGO, DESC_SIT_TOT_TURNO, NUM_TURNO) %>%
#     distinct() %>%
#     collect() %>%
#     spread(key = NUM_TURNO, value = DESC_SIT_TOT_TURNO)
#   
#   if(is.null(n_cand$`2`)){
#     
#     n_cand <- n_cand %>%
#       group_by(SIGLA_PARTIDO, DESCRICAO_CARGO, UF, ANO_ELEICAO) %>%
#       summarise(n_partido = n(),
#                 n_eleito_partido = sum(`1` == 'ELEITO')) %>%
#       ungroup() %>%
#       group_by(UF) %>%
#       mutate(cand_state = sum(n_partido),
#              prop_cand = n_partido/cand_state) %>%
#       filter(SIGLA_PARTIDO %in% partido)
#     
#   } else{
#     
#     n_cand <- n_cand %>%
#       mutate(SITUACAO_TURNO = case_when(`1` == "ELEITO" ~ "ELEITO",
#                                         `1` == "NÃO ELEITO" ~ "NÃO ELEITO",
#                                         `2` == "ELEITO" ~ "ELEITO",
#                                         `2` == "NÃO ELEITO" ~ "NÃO ELEITO")) %>%
#       group_by(SIGLA_PARTIDO, DESCRICAO_CARGO, UF, ANO_ELEICAO) %>%
#       summarise(n_partido = n(),
#                 n_eleito_partido = sum(SITUACAO_TURNO == 'ELEITO')) %>%
#       ungroup() %>%
#       group_by(UF) %>%
#       mutate(cand_state = sum(n_partido),
#              prop_cand = n_partido/cand_state) %>%
#       filter(SIGLA_PARTIDO %in% partido)
#     
#   }
#   
#   base <- merge(regUF, n_cand, by.x = "COD", by.y = "UF")
#   
#   return(base)
# })
##-- + Dados necessários para o donnut ----
donut <- reactive({
  
  ano <- input$partido_ano
  cargo <- input$partido_cargo
  
  dados_cand_part <- dados_gerais %>%
    filter(CODIGO_CARGO == cargo & ANO_ELEICAO == ano) %>%
    collect()
  
  return(dados_cand_part)
  
})
##-- + Gráficos ----
donuts_tabelas <- eventReactive(c(input$partido_partido_donuts, input$partidos_gerar_visualizacoes), {
  
  dados_cand_part <- donut()
  
  ##-- Donnuts ----
  partido_sel <- isolate(input$partido_partido_donuts)
  
  ##-- Donnut sexo ----
  
  sex_cand_par <- dados_cand_part %>%
    select(DESCRICAO_SEXO, SIGLA_PARTIDO, DESC_SIT_TOT_TURNO, CPF_CANDIDATO) %>%
    distinct()
  
  if(partido_sel == "Todos os partidos"){
    sex_cand_par <- sex_cand_par  %>%
      group_by(DESCRICAO_SEXO) %>%
      summarise(n_partido = n_distinct(CPF_CANDIDATO),
                n_eleito_partido = sum(DESC_SIT_TOT_TURNO %in% c('ELEITO', 'ELEITO POR QP', 'ELEITO POR MÉDIA'))) %>%
      arrange(desc(n_partido)) %>%
      mutate(percn = n_partido/sum(n_partido),
             percn_acum = cumsum(percn),
             percn_label = paste0(100*round(percn, digits = 3), " %"),
             percel = n_eleito_partido/sum(n_eleito_partido),
             percel_acum = cumsum(percel),
             percel_label = paste0(100*round(percel, digits = 3), " %")) %>%
      as.data.frame() 
  } else{
    sex_cand_par <- sex_cand_par %>%
      group_by(DESCRICAO_SEXO, SIGLA_PARTIDO) %>%
      summarise(n_partido = n_distinct(CPF_CANDIDATO),
                n_eleito_partido = sum(DESC_SIT_TOT_TURNO %in% c('ELEITO', 'ELEITO POR QP', 'ELEITO POR MÉDIA'))) %>%
      ungroup() %>%
      arrange(desc(n_partido)) %>%
      filter(SIGLA_PARTIDO == partido_sel) %>%
      mutate(percn = n_partido/sum(n_partido),
             percn_acum = cumsum(percn),
             percn_label = paste0(100*round(percn, digits = 3), " %"),
             percel = n_eleito_partido/sum(n_eleito_partido),
             percel_acum = cumsum(percel),
             percel_label = paste0(100*round(percel, digits = 3), " %")) %>%
      as.data.frame() 
  }
  
  tooltip_sexo <- paste0("<b>", sex_cand_par$DESCRICAO_SEXO, "</b><br>",
                         "Nº Candidatos: ", sex_cand_par$n_partido, "<br>",
                         "Nº Candidatos Eleitos:", sex_cand_par$n_eleito_partido, "<br>",
                         "Percentual de Candidatos: ", sex_cand_par$percn_label, "<br>")
  
  cores <- tableau_color_pal(palette = "tableau10medium")(n_distinct(sex_cand_par$DESCRICAO_SEXO))
  
  donut_sexo <- plot_ly(data = sex_cand_par, 
          labels = ~DESCRICAO_SEXO, 
          values = ~n_partido,
          hovertext = tooltip_sexo, 
          marker = list(colors = cores, line = list(color = '#FFFFFF', width = 2)), 
          hoverinfo = "text") %>%
    add_pie(hole = 0.6) %>%
    layout(showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
  
  ##-- Donnut raça ----

  raca_cand_par <- dados_cand_part %>%
    select(DESCRICAO_COR_RACA, SIGLA_PARTIDO, DESC_SIT_TOT_TURNO, CPF_CANDIDATO) %>%
    distinct()
  
  if(partido_sel == "Todos os partidos"){
    raca_cand_par <- raca_cand_par  %>%
      group_by(DESCRICAO_COR_RACA) %>%
      summarise(n_partido = n_distinct(CPF_CANDIDATO),
                n_eleito_partido = sum(DESC_SIT_TOT_TURNO %in% c('ELEITO', 'ELEITO POR QP', 'ELEITO POR MÉDIA'))) %>%
      arrange(desc(n_partido)) %>%
      mutate(percn = n_partido/sum(n_partido),
             percn_acum = cumsum(percn),
             percn_label = paste0(100*round(percn, digits = 3), " %"),
             percel = n_eleito_partido/sum(n_eleito_partido),
             percel_acum = cumsum(percel),
             percel_label = paste0(100*round(percel, digits = 3), " %")) %>%
      as.data.frame() 
  } else{
    raca_cand_par <- raca_cand_par %>%
      group_by(DESCRICAO_COR_RACA, SIGLA_PARTIDO) %>%
      summarise(n_partido = n_distinct(CPF_CANDIDATO),
                n_eleito_partido = sum(DESC_SIT_TOT_TURNO %in% c('ELEITO', 'ELEITO POR QP', 'ELEITO POR MÉDIA'))) %>%
      ungroup() %>%
      arrange(desc(n_partido)) %>%
      filter(SIGLA_PARTIDO == partido_sel) %>%
      mutate(percn = n_partido/sum(n_partido),
             percn_acum = cumsum(percn),
             percn_label = paste0(100*round(percn, digits = 3), " %"),
             percel = n_eleito_partido/sum(n_eleito_partido),
             percel_acum = cumsum(percel),
             percel_label = paste0(100*round(percel, digits = 3), " %")) %>%
      as.data.frame() 
  }
  
  tooltip_raca <- paste0("<b>", raca_cand_par$DESCRICAO_COR_RACA, "</b><br>",
                         "Nº Candidatos: ", raca_cand_par$n_partido, "<br>",
                         "Nº Candidatos Eleitos:", raca_cand_par$n_eleito_partido, "<br>",
                         "Percentual de Candidatos: ", raca_cand_par$percn_label, "<br>")
  
  cores <- tableau_color_pal(palette = "tableau10medium")(n_distinct(raca_cand_par$DESCRICAO_COR_RACA))
  
  donut_raca <- plot_ly(data = raca_cand_par,
                        labels = ~DESCRICAO_COR_RACA,
                        values = ~n_partido,
                        hovertext = tooltip_raca,
                        marker = list(colors = cores, line = list(color = '#FFFFFF', width = 2)),
                        hoverinfo = "text") %>%
    add_pie(hole = 0.6) %>%
    layout(showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  ##-- Tabela escolaridade ----
  
  inst_cand_par <- dados_cand_part %>%
    select(DESCRICAO_GRAU_INSTRUCAO, SIGLA_PARTIDO, DESC_SIT_TOT_TURNO, CPF_CANDIDATO) %>%
    distinct()
  
  if(partido_sel == "Todos os partidos"){
    inst_cand_par <- inst_cand_par  %>%
      group_by(DESCRICAO_GRAU_INSTRUCAO) %>%
      summarise(n_partido = n_distinct(CPF_CANDIDATO),
                n_eleito_partido = sum(DESC_SIT_TOT_TURNO %in% c('ELEITO', 'ELEITO POR QP', 'ELEITO POR MÉDIA'))) %>%
      arrange(desc(n_partido)) %>%
      mutate(percn = n_partido/sum(n_partido),
             percn_acum = cumsum(percn),
             percn_label = paste0(100*round(percn, digits = 3), " %"),
             percel = n_eleito_partido/sum(n_eleito_partido),
             percel_acum = cumsum(percel),
             percel_label = paste0(100*round(percel, digits = 3), " %")) %>%
      ungroup() %>%
      mutate(percn_pad = scale(percn)) %>%
      as.data.frame() 
  } else{
    inst_cand_par <- inst_cand_par %>%
      group_by(DESCRICAO_GRAU_INSTRUCAO, SIGLA_PARTIDO) %>%
      summarise(n_partido = n_distinct(CPF_CANDIDATO),
                n_eleito_partido = sum(DESC_SIT_TOT_TURNO %in% c('ELEITO', 'ELEITO POR QP', 'ELEITO POR MÉDIA'))) %>%
      ungroup() %>%
      arrange(desc(n_partido)) %>%
      filter(SIGLA_PARTIDO == partido_sel) %>%
      mutate(percn = n_partido/sum(n_partido),
             percn_acum = cumsum(percn),
             percn_label = paste0(100*round(percn, digits = 3), " %"),
             percel = n_eleito_partido/sum(n_eleito_partido),
             percel_acum = cumsum(percel),
             percel_label = paste0(100*round(percel, digits = 3), " %")) %>%
      mutate(percn_pad = scale(percn)) %>%
      as.data.frame() 
  }
  
  if(partido_sel != "Todos os partidos"){
    inst_cand_par_tab <- inst_cand_par %>%
      select(SIGLA_PARTIDO, DESCRICAO_GRAU_INSTRUCAO, n_partido, percn_label, n_eleito_partido)
    
    colunas <- c("Partido", "Instrução", "Nº Cand", "Prop. Cand", "Nº Eleitos")
    
  } else{
    inst_cand_par_tab <- inst_cand_par %>%
      select(DESCRICAO_GRAU_INSTRUCAO, n_partido, percn_label, n_eleito_partido)
    
    colunas<- c("Instrução", "Nº Cand", "Prop. Cand", "Nº Eleitos")
  }
  
  
  tabela_escol <- datatable(inst_cand_par_tab, 
            colnames = colunas,
            options = list(pageLength = 7,
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#4d4d4d', 'color': '#ffffff'});",
                             "}")
            ))  
  ##-- Retornando os resultados ----
  return(list(donut_sexo = donut_sexo, donut_raca = donut_raca, 
              tabela_escol = tabela_escol))
})
##-- ++ Candidatos por partido e UF ----
output$mapa_cand <- renderLeaflet({

    base <- base_ncand()
  ##-- Mapa dos candidatos ----
  pal <- colorBin("YlOrRd", domain = base$n, bins = 10)
  
  pop_up_text <- sprintf("<strong>%s - %s</strong><br>
                         <br>Ano: %s <br>
                         <br>Cargo: %s <br>
                         <br>Total Candidatos: %s <br>
                         <br>Candidatos pelo partido: %s <br>
                         <br>Nº Eleitos: %s",
                         base$SIGLA_PARTIDO,
                         base$COD, 
                         base$ANO_ELEICAO,
                         base$DESCRICAO_CARGO,
                         base$cand_state,
                         base$n_partido,
                         base$n_eleito_partido)
  
  leaflet(base) %>%
    addProviderTiles(providers$CartoDB.Positron)%>%
    addPolygons(color = "#444444", 
                weight = 0.5,
                popup = pop_up_text,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = ~colorNumeric("YlOrRd", prop_cand)(prop_cand),
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE))   
})
##-- ++ Donnut por sexo ----
output$donut_sexo <- renderPlotly({
  donuts_tabelas()$donut_sexo
})
##-- ++ Donnut por raça ----
output$donut_raca <- renderPlotly({
  donuts_tabelas()$donut_raca
})
##-- ++ Tabela de escolaridade ----
output$tabela <- renderDataTable({
  donuts_tabelas()$tabela_escol
})