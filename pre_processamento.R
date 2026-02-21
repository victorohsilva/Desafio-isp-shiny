# ==============================================================================
# SCRIPT DE PRÉ-PROCESSAMENTO E ETL (Extração, Transformação e Carga)
# PROJETO: Dashboard Criminal ISP/RJ
# OBJETIVO: Tratar bases brutas e gerar dados agregados de alta performance.
# ==============================================================================

# 1. BIBLIOTECAS NECESSÁRIAS
# ------------------------------------------------------------------------------
library(tidyverse) # Manipulação de dados (dplyr, tidyr)
library(readxl)    # Leitura de arquivos Excel (.xlsx)
library(geobr)     # Acesso à malha geográfica oficial do RJ

# 2. LEITURA DAS FONTES DE DADOS ORIGINAIS
# ------------------------------------------------------------------------------
# Importante: Estes arquivos devem estar na sua pasta de trabalho atual.
dados <- read.csv("base.csv", sep = ",", fileEncoding = "UTF-16")
tab_titulo <- read_excel("de_para.xlsx", sheet = "título")
tab_cisp   <- read_excel("de_para.xlsx", sheet = "cisp_aisp_risp")
tab_municipios <- read_excel("de_para.xlsx", sheet = "municípios")

# 3. LIMPEZA E INTEGRAÇÃO (JOINS)
# ------------------------------------------------------------------------------
# 3.1 Tratamento da tabela de Delegacias (CISP) para evitar duplicidade
tab_cisp_limpa <- tab_cisp %>% 
  distinct(CISP, .keep_all = TRUE)

# 3.2 Unificação das bases (Join) para criar a base completa
dados_completos <- dados %>%
  left_join(tab_titulo, by = c("titulo" = "Código")) %>%
  left_join(tab_cisp_limpa, by = c("cisp" = "CISP")) %>%
  left_join(tab_municipios %>% mutate(código = as.integer(código)), 
            by = c("municipio_fato" = "código"))

# 3.3 Tratamento de datas para o slider temporal do Shiny
dados_completos <- dados_completos %>%
  mutate(data_grafico = as.Date(paste(ano, mes, "01", sep = "-")))

# 4. AGREGAÇÃO ESTRATÉGICA (Otimização para o GitHub e App)
# utilizando apenas variáveis que irei usar no projeto
# ------------------------------------------------------------------------------

dados_agregados <- dados_completos %>%
  count(
    data_grafico,
    DESCRITIVO,
    `Unidade Territorial`,
    Município
  )

# 5. DOWNLOAD E SIMPLIFICAÇÃO DA MALHA GEOGRÁFICA
# ------------------------------------------------------------------------------
# Baixa os polígonos dos municípios do RJ(para fazer o mapa da aba 2)
mapa_municipios <- geobr::read_municipality(code_muni = "RJ", year = 2020)

# 6. EXPORTAÇÃO FINAL (SERIALIZAÇÃO EM RDS)
# ------------------------------------------------------------------------------
# Estes são os dois arquivos que vai ser usado no app.R
saveRDS(dados_agregados, "base_agregada.rds")
saveRDS(mapa_municipios, "mapa_rj_simplificado.rds")

m