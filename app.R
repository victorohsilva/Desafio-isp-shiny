# ==============================================================================
# PROJETO: Dashboard de Inteligência Estratégica - Segurança Pública RJ (ISP)
# OBJETIVO: Análise temporal e espacial de delitos para apoio à decisão.
# ==============================================================================
# ==============================
# BIBLIOTECAS
# ==============================

library(shiny)           # Framework para construção de aplicações web interativas
library(shinydashboard)  # Estrutura visual de dashboard profissional
library(tidyverse)      # Ecossistema para manipulação (dplyr) e visualização (ggplot2)
library(stringr)        # Manipulação de strings e limpeza de textos
library(DT)             # Renderização de tabelas interativas (DataTables)
library(sf)             # Tratamento de dados espaciais (Simple Features)
library(grid)            # Controle de baixo nível para layouts e unidades gráfica
library(viridis)        # Escalas de cores otimizadas para mapas e gráficos
# ==============================
# 2. CARREGAMENTO
# ------------------------------------------------------------------------------
# Optou-se pelo uso de arquivos .rds pré-processados para otimizar a performance,
# garantindo o carregamento instantâneo de bases com mais de 1 milhão de registros.
dados_completos <- readRDS("base_agregada.rds")
mapa_municipios <- readRDS("mapa_rj_simplificado.rds")

# Extração dinâmica de categorias para o filtro global
escolhas_crimes <- c("Todos", sort(unique(dados_completos$DESCRITIVO)))
# 3. INTERFACE DO USUÁRIO (UI)
# ==============================

ui <- dashboardPage(
  
  dashboardHeader(title = "Análise Criminal ISP"),
  
  dashboardSidebar(
    sidebarMenu(
      
      selectInput("escolha_crime_global",
                  "Selecione o Delito:",
                  choices = escolhas_crimes),
      
      hr(),
      
      menuItem("Séries Temporais", tabName = "temporal", icon = icon("chart-line")),
      menuItem("Análise Espaço-Temporal", tabName = "espacial", icon = icon("th")),
      menuItem("Rankings e Foco", tabName = "ranking", icon = icon("list-ol")),
      menuItem("Exportar Dados", tabName = "exportar", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ==============================
      # # ABA 1: SÉRIES TEMPORAIS - Foco em tendências e picos históricos
      # ==============================
      
      tabItem(tabName = "temporal",
              
              fluidRow(
                box(title = "Parâmetros de Comparação",
                    width = 12, status = "primary", solidHeader = TRUE,
                    
                    selectInput("comparar_crimes",
                                "Selecione Delitos para Comparar:",
                                choices = sort(unique(dados_completos$DESCRITIVO)),
                                multiple = TRUE,
                                selected = sort(unique(dados_completos$DESCRITIVO))[1:2])
                )
              ),
              
              fluidRow(
                box(title = "Evolução Histórica e Comparativa",
                    width = 12,
                    plotOutput("grafico_linha", height = 550),
                    br(),
                    downloadButton("download_grafico",
                                   "Download Gráfico (PNG)",
                                   class = "btn-success"))
              )
      ),
      
      # ==============================
      # ABA 2: ANÁLISE ESPAÇO-TEMPORAL - Foco em Mancha Criminal Mensal
      # ==============================
      
      tabItem(tabName = "espacial",
              
              # Linha 1: Controle de Tempo (Slider)
              fluidRow(
                box(title = "Controle de Tempo (Mês de Referência)", width = 12, status = "primary",
                    sliderInput("mes_slider", "Selecione o Mês para análise no Mapa e Ranking:", 
                                min = min(dados_completos$data_grafico), 
                                max = max(dados_completos$data_grafico), 
                                value = max(dados_completos$data_grafico),
                                timeFormat = "%m/%Y", width = "100%"))
              ),
              
              # Linha 2: Mapa e Ranking Dinâmico lado a lado
              fluidRow(
                # Mapa (Coluna de largura 8)
                box(title = "Distribuição Geográfica no Mês", width = 8, status = "primary",
                    plotOutput("mapa_estado_plot", height = 600),
                    br(),
                    downloadButton("downloadMapa", "Baixar Mapa (PNG)")),
                
                # Ranking Dinâmico (Coluna de largura 4)
                box(title = "Top 10 Delegacias no Mês", width = 4, status = "warning",
                    tableOutput("ranking_dinamico_mes"),
                    br(),
                    helpText("Este ranking muda conforme você move o slider acima."))
              )
      ),
      
      # ==============================
      # ABA 3: RANKING ESTRATÉGICO - Visão acumulada por unidade territorial      
      # ==============================
      
      tabItem(tabName = "ranking",
              
              fluidRow(
                box(title = "Ranking Acumulado por Delegacia (CISP)",
                    width = 12,
                    
                    h4(textOutput("titulo_ranking")),
                    br(),
                    
                    tableOutput("tabela_ranking"),
                    br(),
                    
                    downloadButton("downloadRanking",
                                   "Baixar Ranking (CSV)",
                                   class = "btn-success")
                )
              )
      ),
      
      # ==============================
      # ABA 4: EXPORTAÇÃO - Transparência e acesso aos dados brutos tratados
      # ==============================
      
      tabItem(tabName = "exportar",
              
              box(title = "Extração de Dados Tratados",
                  width = 6,
                  downloadButton("downloadData",
                                 "Baixar Base Completa (CSV)"),
                  br(), br(),
                  DTOutput("tabela_bruta"))
      )
    )
  )
)
#
# 4. LÓGICA DO SERVIDOR (SERVER)
# ------------------------------------------------------------------------------

server <- function(input, output) {
  
  # ------------------------------
  # FILTRO GLOBAL REATIVO
  # ------------------------------
  
  dados_filtrados <- reactive({
    
    if (input$escolha_crime_global == "Todos") {
      dados_completos
    } else {
      dados_completos %>%
        filter(DESCRITIVO == input$escolha_crime_global)
    }
  })
  # ADICIONE ESTE NOVO (Ele servirá especificamente para o Mapa e Ranking do Mês)
  # Substitua o seu reativo por este:
  dados_mes_selecionado <- reactive({
    req(input$mes_slider, input$escolha_crime_global)
    
    # Forçamos o input a ser o primeiro dia do mês em formato texto
    data_alvo_txt <- as.character(lubridate::floor_date(as.Date(input$mes_slider), "month"))
    
    # Filtramos a base comparando texto com texto
    df <- dados_completos %>%
      filter(as.character(data_grafico) == data_alvo_txt)
    
    if (input$escolha_crime_global != "Todos") {
      df <- df %>% filter(DESCRITIVO == input$escolha_crime_global)
    }
    
    return(df)
  })
  # ==============================
  # 4.1 Gráfico de Séries Temporais (ggplot2)
  # ==============================
  
  output$grafico_linha <- renderPlot({
    
    req(input$comparar_crimes)
    
    df <- dados_completos %>%
      filter(DESCRITIVO %in% input$comparar_crimes) %>%
      group_by(data_grafico, DESCRITIVO) %>%
      summarise(total = sum(n), .groups = "drop")
    
    ggplot(df,
           aes(x = data_grafico,
               y = total,
               color = DESCRITIVO)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      scale_color_discrete(labels = function(x) str_wrap(x, 50)) +
      labs(title = "Análise Comparativa Temporal de Delitos",
           x = "Período",
           y = "Ocorrências",
           color = "Delito") +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
  })
  
  
  output$download_grafico <- downloadHandler(
    
    filename = function() {
      paste0("serie_temporal_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      
      df <- dados_completos %>%
        filter(DESCRITIVO %in% input$comparar_crimes) %>%
        group_by(data_grafico, DESCRITIVO) %>%
        summarise(total = sum(n), .groups = "drop")
      
      g <- ggplot(df,
                  aes(x = data_grafico,
                      y = total,
                      color = DESCRITIVO)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        theme_minimal()
      
      ggsave(file, plot = g, width = 10, height = 6)
    }
  )
  
  
  # ==============================
  # 2) HEATMAP ESPAÇO-TEMPORAL
  # ==============================
  
  output$heatmap_temporal <- renderPlot({
    
    dados_heatmap <- dados_filtrados() %>%
      group_by(`Unidade Territorial`, data_grafico) %>%
      summarise(total = sum(n), .groups = "drop")
    
    ordem <- dados_heatmap %>%
      group_by(`Unidade Territorial`) %>%
      summarise(total_geral = sum(total), .groups = "drop") %>%
      arrange(desc(total_geral)) %>%
      pull(`Unidade Territorial`)
    
    ggplot(dados_heatmap,
           aes(x = data_grafico,
               y = factor(`Unidade Territorial`, levels = ordem),
               fill = total)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C", direction = -1) +
      theme_minimal() +
      labs(title = "Intensidade Criminal por Área ao Longo do Tempo",
           subtitle = paste("Filtro aplicado:", input$escolha_crime_global),
           x = "Tempo",
           y = "Delegacia (CISP)",
           fill = "Ocorrências") +
      theme(
        axis.text.y = element_text(size = 6)
      )
  })
  
  
  # ==============================
  # 4.2 Mapa Coroplético Municipal (Geoprocessamento)  
  # ==============================
  
  output$mapa_estado_plot <- renderPlot({
    df <- dados_mes_selecionado()
    
    # Se o filtro retornar vazio, o Shiny avisa amigavelmente
    validate(
      need(nrow(df) > 0, "Sem ocorrências para este crime no mês selecionado.")
    )
    
    dados_municipio <- df %>%
      group_by(Município) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      mutate(Município = toupper(Município))
    
    mapa_final <- mapa_municipios %>%
      mutate(name_muni = toupper(name_muni)) %>%
      left_join(dados_municipio, by = c("name_muni" = "Município"))
    
    ggplot(mapa_final) +
      geom_sf(aes(fill = total), color = "white", size = 0.2) +
      scale_fill_viridis_c(option = "C", direction = -1, na.value = "grey95") +
      theme_minimal() +
      labs(title = paste("Ocorrências em", format(input$mes_slider, "%m/%Y")),
           fill = "Qtd")
  })
  
  
  output$downloadMapa <- downloadHandler(
    filename = function() {
      paste("mapa_ocorrencias_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      
      df <- dados_filtrados()
      
      dados_municipio <- df %>%
        group_by(Município) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(Municipio_join = str_squish(str_to_upper(Município)))
      
      mapa_final <- mapa_municipios %>%
        mutate(Municipio_join = str_squish(str_to_upper(name_muni))) %>%
        left_join(dados_municipio, by = "Municipio_join")
      
      g <- ggplot(mapa_final) +
        geom_sf(aes(fill = total), color = "white", size = 0.2) +
        scale_fill_viridis_c(option = "C", direction = -1, na.value = "grey90") +
        theme_minimal()
      
      ggsave(file, plot = g, width = 10, height = 6)
    }
  )

  # ==============================
  # 4.3 Tabela Interativa de Dados Tratados 
  # ==============================  
  output$ranking_dinamico_mes <- renderTable({
    df <- dados_mes_selecionado()
    req(nrow(df) > 0)
    
    df %>%
      group_by(`Unidade Territorial`) %>%
      summarise(Ocorrências = sum(n), .groups = "drop") %>%
      arrange(desc(Ocorrências)) %>%
      head(10) %>%
      rename(Delegacia = `Unidade Territorial`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ==============================
  # 4) RANKING
  # ==============================
  
  ranking_dados <- reactive({
    
    dados_filtrados() %>%
      group_by(`Unidade Territorial`) %>%
      summarise(Total_Acumulado = sum(n), .groups = "drop") %>%
      arrange(desc(Total_Acumulado)) %>%
      head(10) %>%
      rename(Delegacia = `Unidade Territorial`)
  })
  
  
  output$tabela_ranking <- renderTable({
    ranking_dados()
  })
  
  
  output$titulo_ranking <- renderText({
    
    paste0(
      "Delito: ", input$escolha_crime_global,
      " | Período: ",
      format(min(dados_filtrados()$data_grafico), "%m/%Y"),
      " até ",
      format(max(dados_filtrados()$data_grafico), "%m/%Y")
    )
  })
  
  
  output$downloadRanking <- downloadHandler(
    
    filename = function() {
      paste0("ranking_",
             gsub(" ", "_", input$escolha_crime_global),
             ".csv")
    },
    
    content = function(file) {
      write.csv(ranking_dados(),
                file,
                row.names = FALSE,
                fileEncoding = "UTF-8")
    }
  )
  
  
  # ==============================
  # 5) EXPORTAÇÃO
  # ==============================
  
  output$tabela_bruta <- renderDT({
    datatable(dados_completos, options = list(pageLength = 5))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("extracao_isp_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dados_completos, file, row.names = FALSE)
    }
  )
}
# ==============================
# RODAR APP
# ==============================

shinyApp(ui, server)
