##-- Pacotes ----
require(RMySQL)
require(yaml)
require(dplyr)
require(data.table)
##-- Criando as tabelas ----
config <- read_yaml(file = "~/Documents/git/github/voronoys/voronoys-app-dev/config.yml")

conexao <- dbConnect(drv = MySQL(), 
                     host = config$mysql$host, port = config$mysql$port, 
                     user = config$mysql$user, password = config$mysql$password, 
                     dbname = config$mysql$databaseName)

##-- Base de resultados gerais ----
setwd("~/Documents/git/github/voronoys/voronoys-app-dev/dados/data/geral/")
db_existe <- dbExistsTable(conn = conexao, name = "resultados_gerais")

if(!db_existe){
  presidente <- readRDS(file = "presidente.RDS")
  var_tabela <- names(presidente)
  classe_tabela <- c(rep("varchar(100)", length(var_tabela) - 1), "int")
  
  nome_classe <- paste(var_tabela, classe_tabela, collapse = ", ")
  chaves <- "CONSTRAINT PK_ELEICAO PRIMARY KEY (ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO)"
  
  query <- sprintf("CREATE TABLE resultados_gerais (%s, %s)", nome_classe, chaves)
  dbSendQuery(conn = conexao, statement = query)
  
  ##-- Presidente ----
  presidente <- readRDS(file = "presidente.RDS")
  presidente <- presidente %>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = presidente, append = T, row.names = F)
  rm(presidente)
  ##-- Senador ----
  senador <- readRDS(file = "senador.RDS")
  senador <- senador %>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = senador, append = T, row.names = F)
  rm(senador)
  ##-- dep_federal ----
  dep_federal <- readRDS(file = "dep_federal.RDS")
  dep_federal <- dep_federal %>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = dep_federal, append = T, row.names = F)
  rm(dep_federal)
  ##-- dep_estadual ----
  dep_estadual <- readRDS(file = "dep_estadual.RDS")
  dep_estadual <- dep_estadual %>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = dep_estadual, append = T, row.names = F)
  rm(dep_estadual)
  ##-- governador ----
  governador <- readRDS(file = "governador.RDS")
  governador <- governador%>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = governador, append = T, row.names = F)
  rm(governador)
  ##-- prefeito ----
  prefeito <- readRDS(file = "prefeito.RDS")
  prefeito <- prefeito%>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = prefeito, append = T, row.names = F)
  rm(prefeito)
  ##-- vereador ----
  vereador <- readRDS(file = "vereador.RDS")
  vereador <- vereador%>% 
    mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
    group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO) %>%
    slice(1)
  
  dbWriteTable(conn = conexao, name = "resultados_gerais", value = vereador, append = T, row.names = F)
  rm(vereador)
  ##-- Chaves ----
  chaves <- readRDS(file = "chaves.RDS")
  dbWriteTable(conn = conexao, name = "chaves", value = chaves)
  rm(chaves)
}

##-- Bases de receitas e despesas ----
setwd("~/Documents/git/github/voronoys/voronoys-app-dev/dados/data/receitas_despesas/")
##-- Bases de receitas ----
db_existe <- dbExistsTable(conn = conexao, name = "receitas_partidos")

if(!db_existe){
  receita <- readRDS(file = "receitas_partidos.RDS")
  vars_classes <- data.frame(var = c("COD_ELEICAO", "DESC_ELEICAO", "DATA_HORA", "CNPJ_PRESTADOR", "CANDIDATO", "UF", "SIGLA_PARTIDO", "NUMERO_CANDIDATO", 
                                     "CARGO", "NOME_CANDIDATO", "CPF_CADIDATO", "NUMERO_RECIBO_ELEITORAL", "NUMERO_DOCUMENTO", "CPF_CNPJ_DOADOR", "NOME_DOADOR", 
                                     "NOME_DOADOR_RECEITA", "SIGLA_UE_DOADOR", "NUMERO_PARTIDO_DOADOR", "NUMERO_CANDIDATO_DOADOR", "COD_SETOR_ECON_DOADOR", 
                                     "SETOR_ECON_DOADOR", "DATA_RECEITA", "VALOR_RECEITA", "TIPO_RECEITA", "FONTE_RECURSO", "ESPECIE_RECURSO", "DESCRICAO_RECEITA",                       
                                     "CPF_CNPJ_DOADOR_ORIGINARIO", "NOME_DOADOR_ORIGINARIO", "TIPO_DOADOR_ORIGINARIO", "SETOR_ECON_DOADOR_ORIGINARIO",
                                     "NOME_DOADOR_ORIGINARIO_RECEITA"),
                             classe = c("varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)",
                                        "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)",
                                        "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)",
                                        "varchar(100)", "varchar(100)",  "float", "varchar(100)","varchar(100)", "varchar(100)", "varchar(100)",
                                        "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", 
                                        "varchar(100)"), stringsAsFactors = F)
  var_tabela <- vars_classes$var
  classe_tabela <- vars_classes$classe
  
  names(receita) <- var_tabela
  
  nome_classe <- paste(var_tabela, classe_tabela, collapse = ", ")
  #chaves <- "CONSTRAINT PK_ELEICAO PRIMARY KEY (ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO)"
  
  query <- sprintf("CREATE TABLE receitas_partidos (%s)", nome_classe)
  dbSendQuery(conn = conexao, statement = query)
  
  dbWriteTable(conn = conexao, name = "receitas_partidos", value = receita, append = T, row.names = F)
  rm(receita)
}

##-- Base de despesas ----
db_existe <- dbExistsTable(conn = conexao, name = "despesas_partidos")

if(!db_existe){
  despesas <- readRDS(file = "despesa_partidos.RDS")
  vars_classes <- data.frame(var = c("COD_ELEICAO", "DESC_ELEICAO", "DATA_HORA", "CNPJ_PRESTADOR", "CANDIDATO", "UF", "SIGLA_PARTIDO", "NUMERO_CANDIDATO", 
                                     "CARGO", "NOME_CANDIDATO", "CPF_CADIDATO", "TIPO_DOCUMENTO", "NUMERO_DOCUMENTO", "CPF_CNPJ_FORNECEDOR", "NOME_FORNECEDOR", 
                                     "NOME_FORNECEDOR_RECEITA", "COD_SETOR_ECON_FORNECEDOR", "SETOR_ECON_FORNECEDOR", "DATA_DESPESA", "VALOR_DESPESA", "TIPO_DESPESA", 
                                     "DESCRICAO_DESPESA"),
                             classe = c("varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)",
                                        "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)",
                                        "varchar(100)", "varchar(100)", "varchar(100)", "varchar(100)", "float", "varchar(100)","varchar(100)"), 
                             stringsAsFactors = F)
  
  var_tabela <- vars_classes$var
  classe_tabela <- vars_classes$classe
  
  names(despesas) <- var_tabela
  
  nome_classe <- paste(var_tabela, classe_tabela, collapse = ", ")
  #chaves <- "CONSTRAINT PK_ELEICAO PRIMARY KEY (ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, COD_MUN_IBGE, DESCRICAO_ELEICAO, NUMERO_CANDIDATO)"
  
  query <- sprintf("CREATE TABLE despesas_partidos (%s)", nome_classe)
  dbSendQuery(conn = conexao, statement = query)
  
  dbWriteTable(conn = conexao, name = "despesas_partidos", value = despesas, append = T, row.names = F)
  rm(despesas)
}