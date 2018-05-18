eleicoes_uf <- tabPanel(title = "Nível estadual", 
                            value = "uf",
                            br(), hr(),
                            ##-- Botões ----
                            column(width = 8,
                                   ##-- + Ano ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_ano_uf", 
                                                      label = "Ano", 
                                                      choices = sort(anos, decreasing = T), 
                                                      selected = 2014, 
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Cargo ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_cargo_uf", 
                                                      label = "Cargo", 
                                                      choices = cargos, 
                                                      selected = "PRESIDENTE",
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Turno ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_turno_uf", 
                                                      label = "Turno", 
                                                      choices = c("1º turno", "2º turno"), 
                                                      selected = "1º turno",
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   ##-- + Estado ----
                                   column(width = 3,
                                          pickerInput(inputId = "eleicoes_estado_uf", 
                                                      label = "Estado", 
                                                      choices = levels(factor(x = estados,
                                                                              levels = estados)), 
                                                      selected = "AC",
                                                      options = list(`live-search` = TRUE,
                                                                     `none-selected-text` = "Nenhum estado selecionado"))
                                   )
                            ), 
                            ##-- Visualizar ----
                            column(width = 4,
                                   column(width = 12,
                                          br(),
                                          actionBttn(inputId = "eleicoes_gerar_visualizacoes_uf", 
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
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                           HTML("<center><h1>MAIS VOTADOS POR ESTADO</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_uf_geral_uf"), type = 6)
                                          )
                                   ),
                                   ##-- + Mapa dos municípios ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                           HTML("<center><h1>MAPA DAS ELEIÇÕES POR MUNICÍPIO</h1></center>"),
                                                           br(),
                                                           withSpinner(leafletOutput("mapa_mun_geral_uf"), type = 6)
                                                           
                                          )
                                   ),
                                   ##-- + Gráfico de barras ----
                                   column(width = 4,
                                          conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                           HTML("<center><h1>GRÁFICO DE BARRAS DO % DE VOTOS</h1></center>"),
                                                           br(),
                                                           withSpinner(plotlyOutput("barras_geral_uf"), type = 6)
                                                           
                                          )
                                   )
                            )
)