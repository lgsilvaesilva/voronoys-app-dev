##-- Base treemap despesas ----
base_treemap_desp <- reactive({
  
  despesa_part_desp <- dados_despesas %>%
    group_by(SIGLA_PARTIDO, TIPO_DESPESA) %>%
    summarise(total_despesa_partido = sum(VALOR_DESPESA)) %>%
    ungroup() %>%
    collect() %>%
    group_by(SIGLA_PARTIDO) %>%
    mutate(total_partido = sum(total_despesa_partido),
           perc_despesa = total_despesa_partido/total_partido) %>%
    ungroup() %>%
    mutate(perc_partido = total_partido/sum(total_despesa_partido)) %>%
    as.data.frame()
  
  numeric_columns2 <- sapply(despesa_part_desp, mode) == 'numeric'
  
  despesa_part_desp[numeric_columns2] <-  round(despesa_part_desp[numeric_columns2], digits = 3)
  
  names(despesa_part_desp)[1] <- 'partido'
  
  despesa_part_desp$partido <- as.factor(despesa_part_desp$partido)
  
  return(list(despesas = despesa_part_desp))
})

##-- Gráfico despesas ----
output$treemap_desp <- renderHighchart({
  
  despesas <- base_treemap_desp()
  despesa_part_desp <- despesas$despesas
  
  hctreemap2(despesa_part_desp,
             group_vars = c('partido', 'TIPO_DESPESA'),
             size_var = 'total_despesa_partido',
             color_var = 'total_despesa_partido',
             layoutAlgorithm = "squarified",
             levelIsConstant = F,
             allowDrillToNode = T,
             animationLimiti = 1000,
             dataLabels = list(enabled = F, color = 'black', shadow = F),
             levels = list(list(level = 1,
                                dataLabels = list(enabled = T))))
})

##-- Dados de receita por setor ----
base_rank_setor <- eventReactive(input$partido_rec_desp_partido, {
  
  partido_2014 <- input$partido_rec_desp_partido
  
  if(!is.null(partido_2014)){
    
    doadores_setor <- dados_receitas %>%
      group_by(SIGLA_PARTIDO, SETOR_ECON_DOADOR) %>%
      summarise(TOTAL_SETOR_PARTIDO = sum(VALOR_RECEITA)) %>%
      arrange(SIGLA_PARTIDO, desc(TOTAL_SETOR_PARTIDO)) %>%
      ungroup() %>%
      group_by(SIGLA_PARTIDO) %>%
      collect() %>%
      mutate(PERC_SETOR_PARTIDO = TOTAL_SETOR_PARTIDO/sum(TOTAL_SETOR_PARTIDO),
             PERC_ACUM_SETOR_PARTIDO = cumsum(PERC_SETOR_PARTIDO)) %>%
      filter(SIGLA_PARTIDO == partido_2014) %>%
      as.data.frame() %>%
      slice(1:10)
    
    doadores_setor$SETOR_ECON_DOADOR <- factor(doadores_setor$SETOR_ECON_DOADOR, 
                                               levels = unique(doadores_setor$SETOR_ECON_DOADOR)[order(doadores_setor$TOTAL_SETOR_PARTIDO, decreasing = F)])
    levels(doadores_setor$SETOR_ECON_DOADOR)[levels(doadores_setor$SETOR_ECON_DOADOR) == "#NULO"] <- "Não Classificado"
    
    return(doadores_setor)
    
  }
})

##-- Gráfico das doações por setor ----
output$bar_doa <- renderPlotly({

  doadores_setor <- base_rank_setor()
  
  tooltip <- paste0("<b>", doadores_setor$SIGLA_PARTIDO[1:10],"</b><br>",
                    "Setor: ", doadores_setor$SETOR_ECON_DOADOR, "<br>",
                    "R$ ", formatC(doadores_setor$TOTAL_SETOR_PARTIDO, big.mark = ".", decimal.mark = ",", format = "f", digits = 2), "<br>")
  
  
  paleta <- tableau_color_pal("tableau20")(20)
  
  plot_ly(doadores_setor,
          x = ~TOTAL_SETOR_PARTIDO,
          y = ~SETOR_ECON_DOADOR,
          color = ~as.character(SETOR_ECON_DOADOR),
          colors = paleta, 
          type = 'bar',
          orientation = 'h',
          text = tooltip,
          hoverinfo = "text")%>%
    layout(xaxis = list(title = "Valor total de Doação"),
           yaxis = list(title = "Setor (passe o mouse nas barras)", showticklabels = F),
           barmode = 'group',
           #margin = list(l = 10),
           showlegend = FALSE)
  
})