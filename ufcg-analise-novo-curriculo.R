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

# Filtro de estudantes ingressantes entre 2011 e 2023

# Converter o período de ingresso para numérico, se necessário
dados$`Período de Ingresso` <- as.numeric(dados$`Período de Ingresso`)

# Filtrar os dados entre 2011 e 2023
dados_filtrados <- dados %>%
  filter(`Período de Ingresso` >= 2011 & `Período de Ingresso` <= 2023)

nrow(dados_filtrados)  # Quantidade de registros após o filtro

### 

# Comparação de Tamanho do Conjunto de Dados
# Total de registros antes do filtro
n_total <- nrow(dados)


# Total de registros após o filtro (2011–2023)
n_filtrado <- nrow(dados_filtrados)

cat("Total antes do filtro:", n_total, "\n")
cat("Total após o filtro (2011 a 2023):", n_filtrado, "\n")

###

# Distribuição por Período de Ingresso (Antes e Depois)
# Antes
table(dados$`Período de Ingresso`)

# Depois
table(dados_filtrados$`Período de Ingresso`)

### Distribuição por Currículo (Antes e Depois)

table(dados$Currículo)
table(dados_filtrados$Currículo)

### Distribuição por Status (Antes e Depois)

table(dados$Status)
table(dados_filtrados$Status)

###

# Tratamento de Valores Ausentes

# 1. Verificar a presença de valores ausentes em cada coluna
colSums(is.na(dados_filtrados))

# Visualizar colunas com maior incidência de NA

# Visualizar apenas as colunas que possuem NAs
na_por_coluna <- colSums(is.na(dados_filtrados))
na_por_coluna[na_por_coluna > 0]

# Tratar valores ausentes (algumas opções)
dados_tratados <- dados_filtrados %>%
  filter(!is.na(Status) & !is.na(Currículo) & !is.na(`Período de Ingresso`))

# Substituir valores ausentes em variáveis categóricas por "Não informado":

dados_tratados$`Forma de Ingresso`[is.na(dados_tratados$`Forma de Ingresso`)] <- "Não informado"
dados_tratados$`Cor`[is.na(dados_tratados$`Cor`)] <- "Não informado"
dados_tratados$`Estado Civil`[is.na(dados_tratados$`Estado Civil`)] <- "Não informado"

# Verificar distribuição antes e depois da substituição (opcional):

table(dados_filtrados$`Cor`, useNA = "ifany")
table(dados_tratados$`Cor`)

###

# 4.2.2.2  Detecção e Tratamento de Outliers

# Visualizar a distribuição de idade
library(ggplot2)

# Histograma da idade no ingresso
ggplot(dados_filtrados, aes(x = `Idade Aproximada no Ingresso`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribuição da Idade no Ingresso", x = "Idade", y = "Frequência")

# Verificar estatísticas básicas
summary(dados_filtrados$`Idade Aproximada no Ingresso`)

# Detectar outliers usando IQR
# Calcular limites de outliers pelo método do IQR
Q1 <- quantile(dados_filtrados$`Idade Aproximada no Ingresso`, 0.25)
Q3 <- quantile(dados_filtrados$`Idade Aproximada no Ingresso`, 0.75)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Visualizar os limites
cat("Limite inferior:", limite_inferior, "\n")
cat("Limite superior:", limite_superior, "\n")

# Identificar outliers
outliers <- dados_filtrados %>%
  filter(`Idade Aproximada no Ingresso` < limite_inferior |
           `Idade Aproximada no Ingresso` > limite_superior)

# Verificar quantidade
nrow(outliers)

# Excluir ou marcar outliers
# Remover outliers (se for decisão metodológica)
dados_sem_outliers <- dados_filtrados %>%
  filter(`Idade Aproximada no Ingresso` >= limite_inferior &
           `Idade Aproximada no Ingresso` <= limite_superior)

# Verificar quantidade
nrow(outliers)

###

# Transformações Realizadas nas Variáveis

library(dplyr)

# Criar a variável "Situacao_Final" com base em "Status" e "Tipo de Evasao"
dados_tratados <- dados_filtrados %>%
  mutate(
    Situacao_Final = case_when(
      Status == "ATIVO" ~ "Ativo",
      Status == "INATIVO" & Tipo_de_Evasao == "GRADUADO" ~ "Graduado",
      Status == "INATIVO" & Tipo_de_Evasao %in% c("CANCELAMENTO POR ABANDONO", "CANCELAMENTO P SOLICITACAO ALUNO") ~ "Evadido",
      TRUE ~ "Outro"  # Caso existam outras combinações
    )
  )

# Exemplo de padronização de categorias (ajuste conforme necessário)
dados_tratados <- dados_tratados %>%
  mutate(
    Forma_de_Ingresso = case_when(
      Forma_de_Ingresso %in% c("Vestibular", "vestibular") ~ "Vestibular",
      Forma_de_Ingresso %in% c("SISU", "sisu") ~ "SISU",
      TRUE ~ Forma_de_Ingresso
    ),
    Cor = case_when(
      Cor %in% c("Parda", "parda") ~ "Parda",
      Cor %in% c("Branca", "branca") ~ "Branca",
      TRUE ~ Cor
    ),
    Estado_Civil = case_when(
      Estado_Civil %in% c("Solteiro", "solteiro") ~ "Solteiro",
      Estado_Civil %in% c("Casado", "casado") ~ "Casado",
      TRUE ~ Estado_Civil
    )
  )

# Visualizar resumo da nova variável criada
table(dados_tratados$Situacao_Final)








