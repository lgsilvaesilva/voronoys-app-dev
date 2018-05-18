eleicoes_brasil <- tabPanel(title = "Nível federal", 
                            value = "brasil",
                            br(), hr(),
                            ##-- Botões ----
                            column(width = 8,
                                   ##-- + Ano ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_ano_br", 
                                                      label = "Ano", 
                                                      choices = sort(anos, decreasing = T), 
                                                      selected = 2014, 
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Cargo ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_cargo_br", 
                                                      label = "Cargo", 
                                                      choices = list("PRESIDENTE" = 1), 
                                                      selected = 1,
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Turno ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_turno_br", 
                                                      label = "Turno", 
                                                      choices = c("1º turno", "2º turno"), 
                                                      selected = "1º turno",
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Estado ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_estado_br", 
                                                      label = "Estado", 
                                                      choices = levels(factor(x = c("Todos os estados", estados),
                                                                              levels = c("Todos os estados", estados))), 
                                                      selected = "Todos os estados",
                                                      options = list(`live-search` = TRUE,
                                                                     `none-selected-text` = "Nenhum estado selecionado"))
                                   )
                            ), 
                            ##-- Visualizar ----
                            column(width = 4,
                                   column(width = 12,
                                          br(),
                                          actionBttn(inputId = "eleicoes_gerar_visualizacoes_br", 
                                                     label = "Selecionar", 
                                                     style = "fill", 
                                                     color = "success", 
                                                     icon = icon("check")) 
                                   )
                            ),
                            ##-- Outputs ----
                            column(width = 12,
                                   ##-- + Mapa do Brasil ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                                           HTML("<center><h1>MAPA DAS ELEIÇÕES POR ESTADO</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_uf_geral_br"), type = 6)
                                          )
                                   ),
                                   ##-- + Mapa dos municípios ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                                           HTML("<center><h1>MAPA DAS ELEIÇÕES POR MUNICÍPIO</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_mun_geral_br"), type = 6)
                                                           
                                          )
                                   ),
                                   ##-- + Gráfico de barras ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                                           HTML("<center><h1>GRÁFICO DE BARRAS DO % DE VOTOS</h1></center>"),
                                                           br(),
                                                           withSpinner(plotlyOutput("barras_geral_br"), type = 6)
                                                           
                                          )
                                   )
                            )
)