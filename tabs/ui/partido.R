tab_files <- list.files(path = "tabs/ui/partidos", full.names = T)
suppressMessages(lapply(tab_files, source))

partido <- tabPanel(title = "Partidos", 
                     value = "partidos",
                    hr(),
                     tabsetPanel(
                       partido_geral,
                       partido_analise,
                       partido_receitas_despesas
                     )
)