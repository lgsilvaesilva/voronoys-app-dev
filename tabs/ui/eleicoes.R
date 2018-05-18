tab_files <- list.files(path = "tabs/ui/eleicoes", full.names = T)
suppressMessages(lapply(tab_files, source))

eleicoes <- tabPanel(title = "Eleições", 
                     value = "eleicoes",
                     hr(),
                     tabsetPanel(
                       eleicoes_brasil,
                       eleicoes_uf
                     )
)