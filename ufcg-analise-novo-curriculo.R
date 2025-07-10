# Instalar pacotes necessários (se ainda não estiverem instalados)
install.packages("janitor")
install.packages("gt")
install.packages("forcats")
install.packages("dplyr")
install.packages("ggthemes")
install.packages("viridis")

# Carregar bibliotecas
library(readr)       # Leitura de arquivos CSV
library(dplyr)       # Manipulação de dados
library(stringr)     # Operações com strings
library(ggplot2)     # Visualizações
library(janitor)     # Limpeza de dados
library(scales)      # Formatação de escalas
library(viridis)     # Paleta de cores acessível
library(gt)          # Tabelas estilizadas
library(ggthemes)    # Temas visuais aprimorados
library(tidyverse)   # Conjunto completo de pacotes

# Caminho correto para a pasta
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"

# Agora sim, concatenando com o nome do arquivo
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

# Carregamento
dados <- read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Visualizar a estrutura geral do dataframe

head(dados, 10)  # Mostra as 10 primeiras linhas da tabela
str(dados)  # Mostra os nomes das variáveis e seus tipos
summary(dados)  # Fornece estatísticas básicas para cada coluna
colnames(dados)
View(dados)  # Abre uma aba de visualização em estilo de planilha

###

























# Distribuição por ano de ingresso
dados %>%
  count(ano_ingresso) %>%
  ggplot(aes(x = ano_ingresso, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Distribuição de Alunos por Ano de Ingresso",
       x = "Ano de Ingresso", y = "Número de Alunos") +
  theme_minimal()

# Alunos evadidos x não evadidos
dados %>%
  count(evadido) %>%
  ggplot(aes(x = evadido, y = n, fill = evadido)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Situação Final dos Alunos", x = "Evadido", y = "Quantidade") +
  theme_economist()

