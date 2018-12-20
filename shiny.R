library(readr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)
library(DT)
library(data.table)
library(lubridate)

theme_set(theme_bw())

# Corrija aqui o caminho que será salvo os dados
pasta <- "C:/Users/m7531296/OneDrive/Núcleo SI/Base de Dados/monitoramento-si/"

# Reading - .RData-----------------------------------------------------------------

load(paste0(pasta, "bd_criacao.Rdata"))
load(paste0(pasta, "bd_encerramento.Rdata"))
load(paste0(pasta, "bd_matricula.Rdata"))

# Wrangling ---------------------------------------------------------------

min_date <- min(bd_criacao$DATA)
max_date <- max(bd_criacao$DATA)

# Tabela agrupada
base_tabela <- bd_criacao %>% 
  group_by(SRE) %>% 
  top_n(1, wt = DATA) %>%
  left_join(bd_encerramento, by = c("SRE", "DATA")) %>% 
  left_join(bd_matricula, by = c("SRE", "DATA")) %>% 
  arrange(TX_ENCERRAMENTO)

# A tabela de criação de turmas tem tratamento a parte
bd_criacao <- bd_criacao %>%  
  setNames(c("SRE", "DATA", "CRIACAO", "AUTORIZACAO")) %>% 
  gather(key = TIPO, value = TAXA, -SRE, -DATA)

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
                                         startview = "year"),
                          width = 3
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Criacao de Turmas", plotOutput("plot_criacao")),
                                      tabPanel("Encerramento", plotOutput("plot_encerramento")),
                                      tabPanel("Matricula e Enturmacao", plotOutput("plot_matricula"))),
                          width = 9
                          )
                      )
             ),
             tabPanel(title = "Sobre",
                      tags$p("Dashboard pelo Nucleo de Gestao da Informacao da",
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
      formatPercentage(columns = c("TX_CRIACAO", "TX_AUTORIZACAO", 
                                   "TX_ENCERRAMENTO", "TX_ENTURMACAO"), 2)
  })
  
  # Grafico Criacao de Turmas
  sre_selected_criacao <- reactive({
    req(input$sre)
    bd_criacao %>% 
      filter(SRE %in% input$sre) %>% 
      filter(DATA >= input$date[1] & DATA <= input$date[2])
  })
  
  output$plot_criacao <- renderPlot({
    ggplot(data = sre_selected_criacao(), aes(x = DATA, y = TAXA, color = SRE, group = SRE)) +
      geom_line(size = 1.5) +
      scale_y_continuous(labels = scales::percent_format()) +
      #scale_y_continuous(limits = c(0,1.1)) +
      facet_grid(. ~ TIPO)+
      labs(title = paste0("Evolucao da Enturmacao\n",
                          "Superintendencias Regionais de Ensino."),
           x = "Dia",
           y = "Percentual de Criacao e Autorizacao de Turmas",
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
      filter(DATA >= input$date[1] & DATA <= input$date[2])
  })
  
  output$plot_encerramento <- renderPlot({
    ggplot(data = sre_selected_encerramento(), aes(x = DATA, y = TX_ENCERRAMENTO, color = SRE, group = SRE)) +
      geom_line(size = 1.5) +
      scale_y_continuous(labels = scales::percent_format()) +
      #scale_y_continuous(limits = c(0,1.1)) +
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
      filter(DATA >= input$date[1] & DATA <= input$date[2])
  })
  
  output$plot_matricula <- renderPlot({
    ggplot(data = sre_selected_matricula(), aes(x = DATA, y = TX_ENTURMACAO, color = SRE, group = SRE)) +
      geom_line(size = 1.5) +
      scale_y_continuous(labels = scales::percent_format()) +
      #scale_y_continuous(limits = c(0,1.1)) +
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


