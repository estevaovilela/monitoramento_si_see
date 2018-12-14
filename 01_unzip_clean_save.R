library(readxl)
library(tidyverse)

dia_atual <- paste0(rev(str_split(Sys.Date(), pattern = "-")[[1]]), collapse = "")

# Mude seu working directory para a pasta que contém os arquivos enviados pela PRODEMGE atualizados: CTRL+H
arquivos_originais <- paste0(getwd(),
                             "/",
                             sort(list.files(path = paste0(getwd(), "/"),
                                             pattern = paste0(dia_atual, ".zip$"))))

arquivos_finais <- paste0(getwd(), c("/matricula.zip",
                                     "/encerramento.zip",
                                     "/criacao.zip"))


# Tornando o vetor de caracteres em uma lista
list(arquivos_originais) %>% 
  walk(function(x) file.rename(x, arquivos_finais))

walk(arquivos_finais, unzip)

# Renomeando os arquivos .xsls para formato desejado
walk(list(list.files(pattern = ".xlsx$")),
     file.rename,
     to = c("matricula.xlsx",
            "encerramento.xlsx",
            "criacao.xlsx"))

# Lendo todos arquivos .xlsx na ordem certa e agrupando todos eles numa lista após renomeação das colunas:
lista_arquivos <- as.list(list.files(pattern = ".xlsx$"))
lista_skips <- list(5,8,4)
lista_names <- list(CRIACAO = c("SRE", "COD_MUNICIPIO", "MUNICIPIO", "COD_ESCOLA",
                                "ESCOLA", "ENDERECO", "NIVEL", "ETAPA", "TIPO_TURMA",
                                "TURNO", "QT_TURMA_PA", "QT_ALUNO_PA", "QT_TURMA_CRIADA",
                                "QT_TURMA_AUTORIZADA", "QT_ALUNO_ENTURMADO"),
                    ENCERRAMENTO = c("SRE", "COD_MUNICIPIO", "MUNICIPIO", "COD_ESCOLA",
                                     "ESCOLA", "NIVEL", "ETAPA", "TURMA", "TIPO_TURMA",
                                     "PERIODO", "DT_INICIO", "DT_TERMINO", "QT_ALUNO_ENTURMADO_ATIVO",
                                     "QT_ALUNO_ENCERRADO", "PERCENTUAL"),
                    MATRICULA = c("SRE", "COD_MUNICIPIO", "MUNICIPIO", "COD_ESCOLA",
                                  "ESCOLA", "ENDERECO", "NIVEL", "ETAPA", "TIPO_TURMA",
                                  "TURNO", "QT_ALUNO_MATRICULADO", "QT_ALUNO_ENTURMADO"))

# Função que lê e renomeia os arquivos .xslx e os coloca em uma lista:
ler_renomearVars <- function(arquivo, linhas_skip, name_colunas) {
  
  read_excel(path = arquivo, skip = linhas_skip) %>% 
    select(-starts_with("X__")) %>%
    setNames(name_colunas) %>% 
    mutate(DATA = Sys.Date())
  
}

lista_data_frames <- pmap(list(lista_arquivos, lista_skips, lista_names), ler_renomearVars)

# Preenchendo as colunas vazias devido a mesclagem da data.frame de encerramento:
parse_column <- function(x){
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      temp <- x[i]
    }else{
      x[i] <- temp
    }
  }
  x
}

lista_data_frames[[2]] <- map_df(lista_data_frames[[2]], ~ parse_column(.))

removerArquivos <- function(lista_arquivos_01, lista_arquivos_02) {
  
  if (sum(file.exists(lista_arquivos_01, lista_arquivos_02)) != 0) {
    file.remove(lista_arquivos_01, lista_arquivos_02)
    
  } else  {
    print("Arquivos j� foram deletados ou tiveram seus nomes alterados. Cheque seu working directory")
  }
}

walk2(lista_arquivos, as.list(arquivos_finais), removerArquivos)