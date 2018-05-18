partido_geral <- tabPanel(title = "Análise geral", 
                          value = "partidos_geral",
                          br(), hr(),
                          column(width = 4,
                                 column(width = 6,
                                        pickerInput(inputId = "partido_geral_ano", 
                                                    label = "Ano", 
                                                    choices = anos, 
                                                    selected = 2014, 
                                                    options = list(`live-search` = TRUE))
                                 ),
                                 column(width = 6,
                                        pickerInput(inputId = "partido_geral_cargo", 
                                                    label = "Cargo", 
                                                    choices = cargos, 
                                                    selected = "GOVERNADOR", 
                                                    options = list(`live-search` = TRUE))
                                 )
                          ),
                          column(width = 8,
                                 column(width = 4,
                                        br(),
                                        actionBttn(inputId = "partidos_gerar_visualizacoes1", 
                                                   label = "Selecionar", 
                                                   style = "fill", 
                                                   color = "success", 
                                                   icon = icon("check")) 
                                 )
                          ),
                          column(width = 12,
                                 column(width = 6,
                                        conditionalPanel(condition = "input.partidos_gerar_visualizacoes1 > 0",
                                                         br(), hr(), br(),
                                                         HTML("<center><h1>HEATMAP DAS COLIGAÇÕES</h1></center>"),
                                                         column(width = 12,
                                                                withSpinner(plotlyOutput("heatmap_coligacoes"), 6)
                                                         )           
                                        )
                                 ),
                                 column(width = 6,
                                        conditionalPanel(condition = "input.partidos_gerar_visualizacoes1 > 0 & input.partido_geral_cargo == 11",
                                                         br(), hr(), br(),
                                                         HTML("<center><h1>MAPA DOS PARTIDOS VENCEDORES</h1></center>"),
                                                         column(width = 12,
                                                                withSpinner(leafletOutput("mapa_partidos_cid"), type = 6)
                                                         )           
                                        )
                                 )
                          )
)