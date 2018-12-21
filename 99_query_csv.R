library(dbConnect)
library(RMariaDB)
library(purrr)
library(readr)
library(tidyverse)


# Corrija aqui o caminho que será salvo os dados
pasta <- "C:/Users/m7531296/OneDrive/Nucleo SI/Base de Dados/monitoramento-si/"

# Reading -----------------------------------------------------------------

#Conversão da data no R para a data em formato numérico do Excel (contagem em dias):
data_hoje <- Sys.Date()
data_last7 <- (Sys.Date() - 7)

# Conectando com o MySQL:
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "db_ngi",
                 host = "10.23.185.10",
                 port = 3306,
                 user = "arthur_cheib",
                 password = rstudioapi::askForPassword("Database password"))

#### Query para df matrícula e enturmação:
qry_01 <- paste0("SELECT SRE, COD_ESCOLA, ESCOLA, NIVEL, QT_ALUNO_MATRICULADO, QT_ALUNO_ENTURMADO, DATA ", 
                 "FROM TBL_MATRICULA ",
                 "WHERE data ",
                 "BETWEEN ", "'",data_last7, "'", " AND ", "'", data_hoje, "'", ";")

data_mt <- dbSendQuery(con, qry_01)
bd_matricula <- dbFetch(data_mt)
dbClearResult(data_mt)
rm(data_mt)

#### Query para df criação de turmas:
qry_02 <- paste0("SELECT SRE, COD_ESCOLA, ESCOLA, NIVEL, QT_TURMA_PA, QT_TURMA_CRIADA, QT_TURMA_AUTORIZADA, DATA ", 
                 "FROM TBL_CRIACAO ",
                 "WHERE data ",
                 "BETWEEN ", "'",data_last7, "'", " AND ", "'", data_hoje, "'", ";")

data_cr <- dbSendQuery(con, qry_02)
bd_criacao <- dbFetch(data_cr)
dbClearResult(data_cr)
rm(data_cr)

#### Query para df encerramento:
qry_03 <- paste0("SELECT SRE, COD_ESCOLA, ESCOLA, NIVEL, ETAPA, QT_ALUNO_ENTURMADO_ATIVO, QT_ALUNO_ENCERRADO, DATA ", 
                 "FROM TBL_ENCERRAMENTO ",
                 "WHERE data ",
                 "BETWEEN ", "'",data_last7, "'", " AND ", "'", data_hoje, "'", ";")

data_enc <- dbSendQuery(con, qry_03)
bd_encerramento <- dbFetch(data_enc)
dbClearResult(data_enc)
rm(data_enc)

# Wrangling ---------------------------------------------------------------

bd_criacao <- bd_criacao %>% 
  group_by(SRE, DATA) %>% 
  summarise(TOTAL_TURMA_PA = sum(QT_TURMA_PA, na.rm = TRUE),
            TOTAL_TURMA_CRIADA = sum(QT_TURMA_CRIADA, na.rm = TRUE),
            TOTAL_TURMA_AUTORIZADA = sum(QT_TURMA_AUTORIZADA, na.rm = TRUE)) %>% 
  mutate(TX_CRIACAO = TOTAL_TURMA_CRIADA / TOTAL_TURMA_PA,
         TX_AUTORIZACAO = TOTAL_TURMA_AUTORIZADA / TOTAL_TURMA_CRIADA) %>% 
  select(-starts_with("TOTAL")) 

bd_encerramento <- bd_encerramento %>% 
  group_by(SRE, DATA) %>% 
  summarise(TOTAL_ALUNO_ENTURMADO  = sum(QT_ALUNO_ENTURMADO_ATIVO, na.rm = TRUE),
            TOTAL_ALUNO_ENCERRADO = sum(QT_ALUNO_ENCERRADO, na.rm = TRUE)) %>% 
  mutate(TX_ENCERRAMENTO = TOTAL_ALUNO_ENCERRADO / TOTAL_ALUNO_ENTURMADO) %>% 
  select(-starts_with("TOTAL"))

bd_matricula <- bd_matricula %>% 
  filter(!(NIVEL %in% c("SEMI PRESENCIAL - ENSINO FUNDAMENTAL", "SEMI PRESENCIAL - ENSINO MÉDIO"))) %>% 
  group_by(SRE, DATA) %>% 
  summarise(TOTAL_ALUNO_MATRICULADO  = sum(QT_ALUNO_MATRICULADO, na.rm = TRUE),
            TOTAL_ALUNO_ENTURMADO = sum(QT_ALUNO_ENTURMADO, na.rm = TRUE)) %>% 
  mutate(TX_ENTURMACAO = TOTAL_ALUNO_ENTURMADO / TOTAL_ALUNO_MATRICULADO) %>% 
  select(-starts_with("TOTAL"))


# Writing -----------------------------------------------------------------

write_csv(bd_criacao, path = paste0(pasta, "bd_criacao.csv"))
write_csv(bd_encerramento, path = paste0(pasta, "bd_encerramento.csv"))
write_csv(bd_matricula, path = paste0(pasta, "bd_matricula.csv"))

dbDisconnect(con)
