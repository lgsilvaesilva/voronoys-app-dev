partido_analise <- tabPanel(title = "Análise Partidária", 
                            value = "partidos",
                            br(), hr(),
                            column(width = 4,
                                   column(width = 6,
                                          pickerInput(inputId = "partido_ano", 
                                                      label = "Ano", 
                                                      choices = anos, 
                                                      selected = 2014, 
                                                      options = list(`live-search` = TRUE))
                                   ),
                                   column(width = 6,
                                          pickerInput(inputId = "partido_cargo", 
                                                      label = "Cargo", 
                                                      choices = cargos, 
                                                      selected = 1,
                                                      options = list(`live-search` = TRUE))
                                   )
                            ), 
                            column(width = 8,
                                   br(), 
                                   actionBttn(inputId = "partidos_gerar_visualizacoes", 
                                              label = "Selecionar", 
                                              style = "fill", 
                                              color = "success", 
                                              icon = icon("check")) 
                            ),
                            ##-- Outputs ----
                            column(width = 12,
                                   conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                    hr(),
                                                    HTML("<center>"),
                                                    pickerInput(inputId = "partido_partido_donuts", 
                                                                label = "Partido", 
                                                                choices = levels(factor(x = c("Todos os partidos", partidos),
                                                                                        levels = c("Todos os partidos", partidos))), 
                                                                selected = "Todos os partidos",
                                                                options = list(`live-search` = TRUE,
                                                                               `none-selected-text` = "Nenhum partido selecionado")),
                                                    HTML("</center>")
                                   ),
                                   column(width = 4,
                                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                           HTML("<center><h1>Proporção de gênero</h1></center>"),
                                                           column(width = 12,
                                                                  withSpinner(plotlyOutput("donut_sexo"), type = 6)
                                                           )           
                                          )
                                   ),
                                   column(width = 4,
                                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                           HTML("<center><h1>Proporção de raça</h1></center>"),
                                                           column(width = 12,
                                                                  withSpinner(plotlyOutput("donut_raca"), type = 6)
                                                           )           
                                          )
                                   ),
                                   column(width = 4,
                                          conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                           HTML("<center><h1>Grau de instrução</h1></center>"),
                                                           column(width = 12,
                                                                  withSpinner(dataTableOutput("tabela"), type = 6)
                                                           )           
                                          )
                                   )
                            ),
                            column(width = 4, offset = 4,
                                   conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                                    hr(),
                                                    HTML("<center>"),
                                                    pickerInput(inputId = "partido_partido_mapa", 
                                                                label = "Partido", 
                                                                choices = levels(factor(x = c("Todos os partidos", partidos),
                                                                                        levels = c("Todos os partidos", partidos))), 
                                                                selected = "Todos os partidos",
                                                                options = list(`live-search` = TRUE,
                                                                               `none-selected-text` = "Nenhum partido selecionado")),
                                                    HTML("</center>"),
                                                    HTML("<center><h1>Mapa de Candidatos</h1></center>"),
                                                    column(width = 12,
                                                           withSpinner(leafletOutput("mapa_cand"), type = 6)
                                                    ) 
                                   )
                            )
)