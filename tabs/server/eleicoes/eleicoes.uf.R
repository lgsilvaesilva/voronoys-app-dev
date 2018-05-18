##-- Atualizações dos filtros ----
##-- + Atualizações dos cargos ----
observeEvent(input$eleicoes_ano_uf,{
  ano <- isolate(input$eleicoes_ano_uf)
  
  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano & !(DESCRICAO_CARGO %in% c("PREFEITO", "VEREADOR", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "DEPUTADO DISTRITAL"))) %>%
      distinct(CODIGO_CARGO, DESCRICAO_CARGO)

    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$CODIGO_CARGO)
    cargo_default <- input$eleicoes_cargo_uf
    
    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }
    
    cargos_list <- setNames(as.list(chaves_sub$CODIGO_CARGO), chaves_sub$DESCRICAO_CARGO)
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "eleicoes_cargo_uf",
                      label = "Cargo", 
                      choices = cargos_list, 
                      selected = cargo_default)
    
  }
  
}, priority = 1)
##-- + Atualizações dos turnos ----
observeEvent(c(input$eleicoes_ano_uf, 
               input$eleicoes_cargo_uf),{
                 
                 ano <- isolate(input$eleicoes_ano_uf)
                 cargo <- isolate(input$eleicoes_cargo_uf)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo)
                   
                   ##-- Setando o cargo default
                   turnos <- unique(chaves_sub$NUM_TURNO)
                   turno_default <- input$eleicoes_turno_uf
                   
                   if(!(turno_default %in% turnos) | length(turnos) == 0){
                     turno_default <- "1º turno"
                   }
                   
                   if(length(turnos) == 0){
                     turnos <- ""
                   } else{
                     turnos <- paste0(turnos, "º turno")
                   }
                   
                   ##-- Atualizando os turnos ----
                   updatePickerInput(sessio = session,
                                     inputId = "eleicoes_turno_uf", 
                                     label = "Turno", 
                                     choices = turnos, 
                                     selected = turno_default)
                 }
                 
               }, priority = 2)
##-- + Atualizações dos estados ----
observeEvent(c(input$eleicoes_ano_uf, 
               input$eleicoes_cargo_uf, 
               input$eleicoes_turno_uf),{
                 
                 ano <- isolate(input$eleicoes_ano_uf)
                 cargo <- isolate(input$eleicoes_cargo_uf)
                 turno <- isolate(input$eleicoes_turno_uf)
                 turno <- ifelse(turno != "1º turno", "2", "1")
                 
                 if(!is.null(turno)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & NUM_TURNO == turno)
                   ##-- Setando o estado default
                   estados <- levels(factor(x = c("Todos os estados", sort(unique(chaves_sub$UF))),
                                            levels = c("Todos os estados", sort(unique(chaves_sub$UF)))))
                   estado_default <- input$eleicoes_estado_uf
                   
                   if(!(estado_default %in% estados)){
                     estado_default <- "Todos os estados"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "eleicoes_estado_uf",
                                     label = "Estado", 
                                     choices = estados, 
                                     selected = estado_default)  
                 }
                 
               }, priority = 3)
##-- Reactive para os dados ----
dados_eleicao_geral_uf <- reactive({
  ##-- + Inputs ----
  ano <- input$eleicoes_ano_uf
  cargo <- input$eleicoes_cargo_uf
  turno <- input$eleicoes_turno_uf
  turno <- ifelse(turno != "1º turno", "2", "1")
  
  ##-- + Selecionando os dados ----
  dados <- dados_gerais %>% filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo & NUM_TURNO == turno) %>% collect()
  
  return(dados)
})
##-- Reactive para gerar as visualizações ----
graficos_eleicao_geral_uf <- eventReactive(input$eleicoes_gerar_visualizacoes_uf, {
  dados <- dados_eleicao_geral_uf()
  
  cod_uf <- input$eleicoes_estado_uf
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
  
  names(graficos) <- c("mapa_uf_uf", "mapa_mun_uf", "bar_plot_uf")
  
  return(graficos)
  
})
##-- Mapa dos candidatos à presidência por estados ----
output$mapa_uf_geral_uf <- renderLeaflet({
  graficos_eleicao_geral_uf()$mapa_uf_uf
})
##-- Mapa dos candidatos municipais ----
output$mapa_mun_geral_uf <- renderLeaflet({
  graficos_eleicao_geral_uf()$mapa_mun_uf
})
##-- Gráfico de barras com o percentual de votos por candidato ----
output$barras_geral_uf <- renderPlotly({
  graficos_eleicao_geral_uf()$bar_plot_uf
})