# Carregar bibliotecas
# Carregar bibliotecas necessárias
install.packages("janitor")
install.packages("gt")
install.packages("forcats")
install.packages("dplyr")  # Instala, se necessário
library(readr)      # Para leitura de arquivos CSV
library(dplyr)      # Para manipulação de dados
library(stringr)    # Para operações com strings
library(ggplot2)    # Para visualizações futuras (opcional)
library(janitor)
library(scales)
library(viridis)
library(gt)
library(viridis)  # Paleta moderna e acessível
library(ggthemes) # Tema mais elegante
library(tidyverse)

# Definir caminho base onde estão os arquivos
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"

# Carregar os dados principais
dados <- read_csv(file.path(caminho_base, "dados_tratados.csv"))

# Distribuição por ano de ingresso
dados %>%
  count(ano_ingresso) %>%
  ggplot(aes(x = ano_ingresso, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Distribuição de Alunos por Ano de Ingresso",
       x = "Ano de Ingresso", y = "Número de Alunos")

# Alunos evadidos x não evadidos
dados %>%
  count(evadido) %>%
  ggplot(aes(x = evadido, y = n, fill = evadido)) +
  geom_col() +
  labs(title = "Situação Final dos Alunos", x = "Evadido", y = "Quantidade")
