library(readr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)
library(DT)
library(data.table)
library(lubridate)

theme_set(theme_bw())

# Reading -----------------------------------------------------------------

bd_criacao <- data.table::fread(input = "criacao.csv",
                          sep = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          encoding = "UTF-8")

bd_encerramento <- data.table::fread(input = "encerramento.csv",
                                sep = ",",
                                header = TRUE,
                                stringsAsFactors = FALSE,
                                encoding = "UTF-8")

bd_matricula <- data.table::fread(input = "matricula.csv",
                                sep = ",",
                                header = TRUE,
                                stringsAsFactors = FALSE,
                                encoding = "UTF-8")


# Wrangling ---------------------------------------------------------------

bd_criacao <- bd_criacao %>% 
  mutate(DIA = dmy("12-12-2018")) %>% 
  group_by(SRE, DIA) %>% 
  summarise(TOTAL_TURMA_PA = sum(QT_TURMA_PA, na.rm = TRUE),
            TOTAL_ALUNO_PA = sum(QT_ALUNO_PA, na.rm = TRUE),
            TOTAL_TURMA_CRIADA = sum(QT_TURMA_CRIADA, na.rm = TRUE),
            TOTAL_TURMA_AUTORIZADA = sum(QT_TURMA_AUTORIZADA, na.rm = TRUE),
            TOTAL_ALUNO_ENTURMADO = sum(QT_ALUNO_ENTURMADO, na.rm = TRUE)) %>% 
  #mutate(TX_ENTURMACAO = TOTAL_ALUNO_ENTURMADO / TOTAL_ALUNO_PA,
  #       TX_CRIACAO = TOTAL_TURMA_CRIADA / TOTAL_TURMA_PA,
  #       TX_AUTORIZACAO = TOTAL_TURMA_AUTORIZADA / TOTAL_TURMA_PA) %>% 
  mutate(TX_CRIACAO = TOTAL_TURMA_CRIADA / TOTAL_TURMA_PA,
         TX_AUTORIZACAO = TOTAL_TURMA_AUTORIZADA / TOTAL_TURMA_PA) %>% 
  select(-starts_with("TOTAL"))

bd_encerramento <- bd_encerramento %>% 
  mutate(DIA = dmy("12-12-2018")) %>% 
  group_by(SRE, DIA) %>% 
  summarise(TOTAL_ALUNO_ENTURMADO  = sum(QT_ALUNO_ENTURMADO_ATIVO, na.rm = TRUE),
            TOTAL_ALUNO_ENCERRADO = sum(QT_ALUNO_ENCERRADO, na.rm = TRUE)) %>% 
  mutate(PRIMEIRO = first(TOTAL_ALUNO_ENTURMADO)) %>% 
  mutate(TX_ENCERRAMENTO = TOTAL_ALUNO_ENCERRADO / PRIMEIRO) %>% 
  select(-starts_with("TOTAL"))

bd_matricula <- bd_matricula %>% 
  mutate(DIA = dmy("12-12-2018")) %>% 
  group_by(SRE, DIA) %>% 
  summarise(TOTAL_ALUNO_MATRICULADO  = sum(QT_ALUNO_MATRICULADO, na.rm = TRUE),
            TOTAL_ALUNO_ENTURMADO = sum(QT_ALUNO_ENTURMADO, na.rm = TRUE)) %>% 
  mutate(TX_ENTURMACAO = TOTAL_ALUNO_ENTURMADO / TOTAL_ALUNO_MATRICULADO) %>% 
  select(-starts_with("TOTAL"))

min_date <- min(bd_criacao$DIA)
max_date <- max(bd_criacao$DIA)

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
  
  # Tabela
  base_tabela <- bd_criacao %>% 
    group_by(SRE) %>% 
    top_n(1, wt = DIA) %>%
    left_join(bd_encerramento, by = c("SRE", "DIA")) %>% 
    left_join(bd_matricula, by = c("SRE", "DIA")) %>% 
    select(-PRIMEIRO) %>% 
    arrange(TX_ENCERRAMENTO)
  
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


