##-- Atualizações dos filtros ----
##-- + Atualizações os anos ----
observeEvent(input$eleicoes_cargo_br,{
  cargo <- isolate(input$eleicoes_cargo_br)
  
  if(!is.null(cargo)){
    chaves_sub <- chaves %>%
      filter(CODIGO_CARGO == cargo) 
    
    ##-- Setando o cargo default
    anos <- sort(unique(chaves_sub$ANO_ELEICAO))
    ano_default <- input$eleicoes_ano_br
    
    if(!(ano_default %in% anos)){
      ano_default <- anos[1]
    }
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "eleicoes_ano_br",
                      label = "Ano", 
                      choices = anos, 
                      selected = ano_default)
    
  }
  
}, priority = 1)
##-- + Atualizações dos turnos ----
observeEvent(c(input$eleicoes_ano_br, 
               input$eleicoes_cargo_br),{
                 
                 ano <- isolate(input$eleicoes_ano_br)
                 cargo <- isolate(input$eleicoes_cargo_br)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo)
                   
                   ##-- Setando o cargo default
                   turnos <- unique(chaves_sub$NUM_TURNO)
                   turno_default <- input$eleicoes_turno_br
                   
                   if(!(turno_default %in% paste0(turnos, "º turno")) | length(turnos) == 0){
                     turno_default <- "1º turno"
                   }
                   
                   if(length(turnos) == 0){
                     turnos <- ""
                   } else{
                     turnos <- paste0(turnos, "º turno")
                   }
                   
                   ##-- Atualizando os turnos ----
                   updatePickerInput(sessio = session,
                                     inputId = "eleicoes_turno_br", 
                                     label = "Turno", 
                                     choices = turnos, 
                                     selected = turno_default)
                 }
                 
               }, priority = 2)
##-- + Atualizações dos estados ----
observeEvent(c(input$eleicoes_ano_br, 
               input$eleicoes_cargo_br, 
               input$eleicoes_turno_br),{
                 
                 ano <- isolate(input$eleicoes_ano_br)
                 cargo <- isolate(input$eleicoes_cargo_br)
                 turno <- isolate(input$eleicoes_turno_br)
                 turno <- ifelse(turno != "1º turno", "2", "1")
                 
                 if(!is.null(turno)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & NUM_TURNO == turno)
                   ##-- Setando o estado default
                   estados <- levels(factor(x = c("Todos os estados", sort(unique(chaves_sub$UF))),
                                             levels = c("Todos os estados", sort(unique(chaves_sub$UF)))))
                   estado_default <- input$eleicoes_estado_br
                   
                   if(!(estado_default %in% estados)){
                     estado_default <- "Todos os estados"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "eleicoes_estado_br",
                                     label = "Estado", 
                                     choices = estados, 
                                     selected = estado_default)  
                 }
                 
               }, priority = 3)
##-- Reactive para os dados ----
dados_eleicao_geral_br <- reactive({
  ##-- + Inputs ----
  ano <- input$eleicoes_ano_br
  cargo <- input$eleicoes_cargo_br
  turno <- input$eleicoes_turno_br
  turno <- ifelse(turno != "1º turno", "2", "1")
  
  ##-- + Selecionando os dados ----
  dados <- dados_gerais %>% filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo & NUM_TURNO == turno) %>% collect()
  
  return(dados)
})
##-- Reactive para gerar as visualizações ----
graficos_eleicao_geral_br <- eventReactive(input$eleicoes_gerar_visualizacoes_br, {
  dados <- dados_eleicao_geral_br()
  
  cod_uf <- input$eleicoes_estado_br
  if(cod_uf == "Todos os estados") cod_uf <- NULL
  
  registerDoMC(cores = 3)
  graficos <- foreach(i = 1:3, .packages = c("leaflet", "plotly", "dplyr")) %dopar% {
    if(i == 1){
      ##-- + Mapa ----
      names(regUF)[c(1, 3)] <- c("UF", "REG")
      g <- mapa_uf(data = dados, poly = regUF)
    }
    
    if(i == 2){
      g <- mapa_mun(data = dados, poly = regMun, uf = cod_uf)
    }
    
    if(i == 3){
      g <- bar_plot(data = dados, uf = cod_uf, value_var = "QTDE_VOTOS_TOT", group_var = "NOME_URNA_CANDIDATO")
    }
    
    return(g)
  }
  
  names(graficos) <- c("mapa_uf_br", "mapa_mun_br", "bar_plot_br")
  
  return(graficos)
  
})
##-- Mapa dos candidatos à presidência por estados ----
output$mapa_uf_geral_br <- renderLeaflet({
  graficos_eleicao_geral_br()$mapa_uf_br
})
##-- Mapa dos candidatos municipais ----
output$mapa_mun_geral_br <- renderLeaflet({
  graficos_eleicao_geral_br()$mapa_mun_br
})
##-- Gráfico de barras com o percentual de votos por candidato ----
output$barras_geral_br <- renderPlotly({
  graficos_eleicao_geral_br()$bar_plot_br
})