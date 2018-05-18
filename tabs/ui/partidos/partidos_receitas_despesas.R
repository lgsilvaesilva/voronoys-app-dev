partido_receitas_despesas <- tabPanel(title = "Receitas e despesas (2014)", 
                                      value = "partido_receitas_despesas",
                                      br(), hr(),
                                      column(width = 6,
                                             HTML("<center><h1>TREEMAP DAS DOAÇÕES POR PARTIDO (2014)</h1></center>"),
                                             column(width = 12,
                                                    HTML("<center>"),
                                                    pickerInput(inputId = "partido_rec_desp_partido", 
                                                                label = "Selecione um partido", 
                                                                choices = levels(factor(x = sort(partidos_2014),
                                                                                        levels = sort(partidos_2014))), 
                                                                options = list(`live-search` = TRUE,
                                                                               `none-selected-text` = "Nenhum partido selecionado")),
                                                    HTML("</center>"),
                                                    withSpinner(plotlyOutput(outputId = "bar_doa"), type = 6)
                                             )           
                                      ),
                                      column(width = 6,
                                             HTML("<center><h1>DESPESAS POR PARTIDO (2014)</h1></center>"),
                                             column(width = 12,
                                                    withSpinner(highchartOutput(outputId = "treemap_desp", height = "550px"), type = 6)
                                             )           
                                      )
)