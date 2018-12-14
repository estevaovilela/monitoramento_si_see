library(readr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)
library(DT)
library(data.table)
library(lubridate)
library(leaflet)
library(sp)

theme_set(theme_bw())


# Reading - SQL -----------------------------------------------------------

#Insira suas credenciais para obter acesso ao Banco NGI:
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "db_ngi",
                 host = "10.23.185.10",
                 port = 3306,
                 user = "arthur_cheib",
                 password = rstudioapi::askForPassword("Database password"))

#Listagem de todas as tabelas que existem no banco NGI:
tabelas_banco <- dbListTables(con)

bd_criacao <- RMariaDB::dbReadTable(con, tabelas_banco[1])
bd_encerramento <- RMariaDB::dbReadTable(con, tabelas_banco[2])
bd_matricula <- RMariaDB::dbReadTable(con, tabelas_banco[3])


# Reading - Geo - SRE -----------------------------------------------------

sre_geo <- rgdal::readOGR(dsn = path.expand("C:/Users/m7531296/OneDrive/Estevao/Profissional/SEE/projects/localizacao-escolas/shp/Contorno SREs.shp"),
                          layer = "Contorno SREs")

sre_geo@data$SRE <- str_to_upper(sre_geo@data$Nome.SRE)
sre_geo@data$SRE[c(7,12,15,24,26:28,34)] <- c("SAO JOAO DEL REI", "CONSELHEIRO LAFAIETE", "SAO SEBASTIAO DO PARAISO",
                                            "CORONEL FABRICIANO", "METROPOLITANA A", "METROPOLITANA B",
                                            "METROPOLITANA C", "GOVERNADOR VALADARES")

# Reading - csv-----------------------------------------------------------------

#bd_criacao <- data.table::fread(input = "criacao.csv",
#                          sep = ",",
#                          header = TRUE,
#                          stringsAsFactors = FALSE,
#                          encoding = "UTF-8")

#bd_encerramento <- data.table::fread(input = "encerramento.csv",
#                                sep = ",",
#                                header = TRUE,
#                                stringsAsFactors = FALSE,
#                                encoding = "UTF-8")

#bd_matricula <- data.table::fread(input = "matricula.csv",
#                                sep = ",",
#                                header = TRUE,
#                                stringsAsFactors = FALSE,
#                                encoding = "UTF-8")

# Wrangling ---------------------------------------------------------------

bd_criacao <- bd_criacao %>% 
  group_by(SRE, DATA) %>% 
  summarise(TOTAL_TURMA_PA = sum(QT_TURMA_PA, na.rm = TRUE),
            TOTAL_TURMA_CRIADA = sum(QT_TURMA_CRIADA, na.rm = TRUE),
            TOTAL_TURMA_AUTORIZADA = sum(QT_TURMA_AUTORIZADA, na.rm = TRUE)) %>% 
  mutate(TX_CRIACAO = TOTAL_TURMA_CRIADA / TOTAL_TURMA_PA,
         TX_AUTORIZACAO = TOTAL_TURMA_AUTORIZADA / TOTAL_TURMA_PA) %>% 
  select(-starts_with("TOTAL"))

bd_encerramento <- bd_encerramento %>% 
  group_by(SRE, DATA) %>% 
  summarise(TOTAL_ALUNO_ENTURMADO  = sum(QT_ALUNO_ENTURMADO_ATIVO, na.rm = TRUE),
            TOTAL_ALUNO_ENCERRADO = sum(QT_ALUNO_ENCERRADO, na.rm = TRUE)) %>% 
  mutate(PRIMEIRO = first(TOTAL_ALUNO_ENTURMADO)) %>% 
  mutate(TX_ENCERRAMENTO = TOTAL_ALUNO_ENCERRADO / PRIMEIRO) %>% 
  select(-starts_with("TOTAL"), -PRIMEIRO)

bd_matricula <- bd_matricula %>% 
  group_by(SRE, DATA) %>% 
  summarise(TOTAL_ALUNO_MATRICULADO  = sum(QT_ALUNO_MATRICULADO, na.rm = TRUE),
            TOTAL_ALUNO_ENTURMADO = sum(QT_ALUNO_ENTURMADO, na.rm = TRUE)) %>% 
  mutate(TX_ENTURMACAO = TOTAL_ALUNO_ENTURMADO / TOTAL_ALUNO_MATRICULADO) %>% 
  select(-starts_with("TOTAL"))

min_date <- min(bd_criacao$DATA)
max_date <- max(bd_criacao$DATA)

# Tabela agrupada
base_tabela <- bd_criacao %>% 
  group_by(SRE) %>% 
  top_n(1, wt = DATA) %>%
  left_join(bd_encerramento, by = c("SRE", "DATA")) %>% 
  left_join(bd_matricula, by = c("SRE", "DATA")) %>% 
  arrange(TX_ENCERRAMENTO)

# Leaflet -----------------------------------------------------------------

sre_geo@data <- sre_geo@data %>% 
  left_join(base_tabela, by = c("SRE"))

pal <- colorNumeric(palette = "Blues",
                    domain = sre_geo@data$TX_ENCERRAMENTO)

content <- paste(sep = "<br>",
                str_to_title(sre_geo@data$SRE),
                paste(as.character(round(sre_geo@data$TX_ENCERRAMENTO*100, digits = 2)), "%"))

leaflet::leaflet(sre_geo) %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
  leaflet::addPolygons(data = sre_geo, # LAD polygon data from geojson
                       fillColor = ~pal(TX_ENCERRAMENTO),
                       fillOpacity = 1,
                       weight = 1,  # line thickness
                       opacity = 1,  # line transparency
                       color = "black", # line colour
                       popup = content) 
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("readable"),
  
  # Titulo Aplicativo
  titlePanel("Acompanhamento SI - 2018", 
             windowTitle = "Acompanhamento 2018"),
  
  navbarPage(title = "", 
             tabPanel(title = "Tabela", 
                      DT::DTOutput(outputId = "table_sre")),
             tabPanel(title = "Evolucao",
                      sidebarLayout(
                        sidebarPanel(
                          # Instrucoes
                          tags$p("Selecione as Superintendencias que deseja vizualizar.",
                                 " Em seguida selecione o periodo de tempo."),
                          # Escolher SRE
                          selectInput(inputId = "sre",
                                      label = "SRE",
                                      choices = unique(bd_criacao$SRE),
                                      multiple = TRUE,
                                      selected = "ALMENARA"),
                          # Date input
                          dateRangeInput(inputId = "date",
                                         label = "Selecione o periodo",
                                         start = "2018-01-01",
                                         end = "2019-12-31",
                                         min = min_date, max = max_date,
                                         startview = "year")
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Criacao de Turmas", plotOutput("plot_criacao")),
                                      tabPanel("Encerramento", plotOutput("plot_encerramento")),
                                      tabPanel("Matricula e Enturmacao", plotOutput("plot_matricula")))
                          )
                      )
             ),
             tabPanel(title = "Mapa",
                      sidebarLayout(
                        sidebarPanel(
                          # Instrucoes
                          tags$p("Selecione qual processo deseja visualizar."),
                          # Escolher Processo
                          selectInput(inputId = "processo",
                                      label = "Processo",
                                      choices = c("Criacao de turmas" = "criacao",
                                                  "Autorizacao de turmas" = "autorizacao",
                                                  "Encerramento" = "encerramento",
                                                  "Enturmacao" = "enturmacao"),
                                      multiple = FALSE,
                                      selected = "criacao")
                        ),
                        mainPanel(
                          plotOutput("plot_map")
                        )
                      )
             ),
             tabPanel(title = "Sobre",
                      tags$p("Dashboard desenvolvido para uso da",
                             " Subsecretaria de Informacoes e Tecnologias",
                             " Educacionais da Secretaria de Estado de Educacao",
                             " para monitoramento de processos administrativos escolares.")
                      )
             )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  output$table_sre <- DT::renderDT({
    datatable(base_tabela,
              selection = "none",
              rownames = FALSE,
              colnames = c("SRE", "Dia", "Taxa de Criacao de Turmas", "Taxa de Autorizacao",
                           "Taxa de Encerramento", "Taxa de Enturmacao")) %>% 
      formatPercentage(columns = c("TX_CRIACAO",
                                   "TX_AUTORIZACAO", "TX_ENCERRAMENTO",
                                   "TX_ENTURMACAO"), 2)
  })
  
  # Grafico Criacao de Turmas
  sre_selected_criacao <- reactive({
    req(input$sre)
    bd_criacao %>% 
      filter(SRE %in% input$sre) %>% 
      filter(DIA >= input$date[1] & DIA <= input$date[2])
  })
  
  output$plot_criacao <- renderPlot({
    ggplot(data = sre_selected_criacao(), aes(x = DIA, y = TX_CRIACAO, fill = SRE, group = SRE)) +
      #geom_line(size = 2, linetype = "dashed") +
      geom_col(alpha = 0.75, position = "dodge") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0("Evolucao da Enturmacao\n",
                          "Superintendencias Regionais de Ensino."),
           x = "Dia",
           y = "Percentual de Enturmacao",
           color = "SRE",
           caption = paste0("SIMADE: ", Sys.Date())) +
      theme(axis.text.x = element_text(face = "bold", angle = 90, size = 17.5),
            axis.text.y = element_text(face = "bold", size = 17.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 17.5),
            plot.caption = element_text(face = "bold", size = 12.5),
            plot.title = element_text(face = "bold", size = 20),
            legend.text = element_text(size = 12.5))
  })
  
  # Grafico Encerramento
  sre_selected_encerramento <- reactive({
    req(input$sre)
    bd_encerramento %>% 
      filter(SRE %in% input$sre) %>% 
      filter(DIA >= input$date[1] & DIA <= input$date[2])
  })
  
  output$plot_encerramento <- renderPlot({
    ggplot(data = sre_selected_encerramento(), aes(x = DIA, y = TX_ENCERRAMENTO, fill = SRE, group = SRE)) +
      #geom_line(size = 2, linetype = "dashed") +
      geom_col(alpha = 0.75, position = "dodge") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0("Evolucao do Encerramento\n",
                          "Superintendencias Regionais de Ensino."),
           x = "Dia",
           y = "Percentual de Encerramento",
           color = "SRE",
           caption = paste0("SIMADE: ", Sys.Date())) +
      theme(axis.text.x = element_text(face = "bold", angle = 90, size = 17.5),
            axis.text.y = element_text(face = "bold", size = 17.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 17.5),
            plot.caption = element_text(face = "bold", size = 12.5),
            plot.title = element_text(face = "bold", size = 20),
            legend.text = element_text(size = 12.5))
  })
  
  # Grafico Matricula e Enturmacao
  sre_selected_matricula <- reactive({
    req(input$sre)
    bd_matricula %>% 
      filter(SRE %in% input$sre) %>% 
      filter(DIA >= input$date[1] & DIA <= input$date[2])
  })
  
  output$plot_matricula <- renderPlot({
    ggplot(data = sre_selected_matricula(), aes(x = DIA, y = TX_ENTURMACAO, fill = SRE, group = SRE)) +
      #geom_line(size = 2, linetype = "dashed") +
      geom_col(alpha = 0.75, position = "dodge") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = paste0("Evolucao da Enturmacao\n",
                          "Superintendencias Regionais de Ensino."),
           x = "Dia",
           y = "Percentual de Enturmacao",
           color = "SRE",
           caption = paste0("SIMADE: ", Sys.Date())) +
      theme(axis.text.x = element_text(face = "bold", angle = 90, size = 17.5),
            axis.text.y = element_text(face = "bold", size = 17.5),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 17.5),
            plot.caption = element_text(face = "bold", size = 12.5),
            plot.title = element_text(face = "bold", size = 20),
            legend.text = element_text(size = 12.5))
  })

}

# APP ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)


