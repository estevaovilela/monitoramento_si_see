library(dbConnect)
library(RMariaDB)

#Insira suas credenciais para obter acesso ao Banco NGI:
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "db_ngi",
                 host = "10.23.185.10",
                 port = 3306,
<<<<<<< HEAD
                 user = "arthur_cheib",
                 password = rstudioapi::askForPassword("Database password"))
=======
                 user = "xxx",
                 password = "xxx")
>>>>>>> e2bb7c69a23d09c7a066865692b3b36c674a941a

#Listagem de todas as tabelas que existem no banco NGI:
tabelas_banco <- dbListTables(con)

# Vetor que contém os tipos das colunas das tabelas do banco na mesma ordem do vetor tabelas_banco:
lista_variaveis_banco <- list(CRIACAO = c(rep("TEXT", 10), rep("DOUBLE", 5), "DATE"),
                             ENCERRAMENTO = c(rep("TEXT", 12), rep("DOUBLE", 3), "DATE"),
                              MATRICULA = c(rep("TEXT", 10), rep("DOUBLE", 2), "DATE"))

#Inicio da Função de Teste para checagem e inserção dos dados
checkVars_insertData <- function(lista_bases, lista_variaveis_banco, tabelas_banco) {
  
  if ((sum(dbDataType(con, as.data.frame(lista_bases)) != lista_variaveis_banco)) != 0) {
    
  print("A quantidade de colunas da data.frame a ser inserida ou não é igual a do banco \n
        ou são variáveis de tipos - class - diferentes as variáveis do banco")
    
  stop()
    
  } else {
  
  dbWriteTable(conn = con, tabelas_banco, lista_bases, append = TRUE, row.names = FALSE)
  
  }    
    
}

# Rodar a função de inserção e checagem sobre as listas de data.frames
pwalk(list(lista_data_frames, lista_variaveis_banco, tabelas_banco), checkVars_insertData)

dbDisconnect(con)