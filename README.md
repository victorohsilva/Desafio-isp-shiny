# Desafio-isp-shiny
Dashboard de Inteligência em Segurança Pública (ISP/RJ)
Este projeto consiste em um dashboard interativo desenvolvido em R/Shiny para a análise estratégica de dados criminais do Estado do Rio de Janeiro. O objetivo é transformar dados brutos do Instituto de Segurança Pública (ISP) em insights acionáveis para o monitoramento de manchas criminais e tendências temporais.

Objetivos do Projeto:
Conforme as premissas do desafio, a ferramenta foca em:

Monitoramento de Manchas Criminais: Visualização espacial por município para identificação de áreas críticas.

Análise de Séries Temporais: Comparativo histórico entre diferentes tipos de delitos para identificar sazonalidade e picos.

Rankings Estratégicos: Identificação das Unidades Territoriais (CISP) com maior incidência criminal.

Arquitetura e Performance:
Para garantir uma experiência de usuário fluida e eficiente, o projeto foi estruturado em duas etapas:

Processamento de Dados (pre_processamento.R):

Realiza o carregamento e limpeza de mais de 1 milhão de registros.

Executa o cruzamento (Join) entre a base de ocorrências e tabelas auxiliares de municípios e delegacias.

Otimização: Os dados são agregados e salvos em formato .rds (base_agregada.rds), reduzindo o tempo de resposta do dashboard e otimizando o consumo de memória.

Visualização Interativa (aplicativo.R):

Interface construída com shinydashboard para ambiente profissional.

Uso de sf e geobr para renderização precisa de mapas geográficos.

Implementação de filtros globais e reativos para análise dinâmica por tipo de crime e período.

Bibliotecas Utilizadas:
Manipulação: tidyverse, lubridate, stringr

Interface: shiny, shinydashboard, DT

Geoprocessamento: sf, geobr, viridis, grid

Como Executar:
Certifique-se de que todos os arquivos (aplicativo.R, base_agregada.rds e mapa_rj_simplificado.rds) estão no mesmo diretório.

Abra o arquivo aplicativo.R no RStudio.

Clique em Run App.

Abra o arquivo aplicativo.R no RStudio.

Clique em Run App.
