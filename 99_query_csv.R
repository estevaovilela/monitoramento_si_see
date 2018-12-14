library(dbConnect)
library(RMariaDB)
library(purrr)
library(readr)

# Insira suas credenciais para obter acesso ao Banco NGI:
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "db_ngi",
                 host = "10.23.185.10",
                 port = 3306,
                 user = "xxxx",
                 password = "xxxx")

# Listagem de todas as tabelas que existem no banco NGI:
tabelas_banco <- dbListTables(con)

criacao <- RMariaDB::dbReadTable(con, tabelas_banco[1])
encerramento <- RMariaDB::dbReadTable(con, tabelas_banco[2])
matricula <- RMariaDB::dbReadTable(con, tabelas_banco[3])

#walk(list(criacao, encerramento, matricula), function(x) write.csv(x, file = c("criacao.csv",
#                                                                                  "encerramento.csv",
#                                                                                  "matricula.csv")))

write_csv(criacao, "criacao.csv")
write_csv(encerramento, "encerramento.csv")
write_csv(matricula, "matricula.csv")

dbDisconnect(con)