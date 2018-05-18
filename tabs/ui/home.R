home <- tabPanel(title = "Home", 
                 value = "home",
                 hr(),
                 br(), br(),
                 HTML("<h1><center>BEM VINDOS À PÁGINA DO <b>VORONOYS</b>. CONHEÇA O NOSSO TRABALHO...</center></h1>"),
                 br(), br(), br(), br(),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "Análises gerais", cor = cores[1], icon = "brasil.png", id = "analise_geral")
                        ),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "Análises por partidos", cor = cores[2], icon = "flag.png", id = "analise_partidos")
                 ),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "Análises por candidatos", cor = cores[3], icon = "person.png", id = "analise_candidatos")
                 ),
                 column(width = 3, align = "center",
                        tab_voronoys(texto = "Sobre a equipe", cor = cores[4], icon = "sobre.png", id = "sobre")
                 ),
                 column(width = 12,
                        br(), br(), br(), br(),
                        wellPanel(
                          #HTML("<h2>O <b>VORONOYS</b> é um grupo formado por 4 estatísticos e um cientista político. Sua criação foi motivada pelo 1º Desafio do CEPESP Data da FGV, no qual o objetivo é decifrar as eleições no Brasil através de ferramentas de análise, visualização e busca em dados. O nome Voronoys é oriundo de uma técnica matemática chamada Tesselação de Voronoi que foi utilizada nas análises propostas pelo grupo.</h2>"),
                          HTML("<h2>VORONOYS é um projeto que traz o universo da ciência de dados aplicado às eleições brasileiras. Nossa visão é transmitir, de forma clara e objetiva, informações relevantes sobre resultados eleitorais. Nossas soluções contemplam assuntos como visualização de dados e elucidação do perfil socioeconômica dos votantes de cada partido.</h2>")
                        )
                 )
)
