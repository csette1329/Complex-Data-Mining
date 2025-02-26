library(ggplot2)
library(ggcorrplot)
library(hrbrthemes)
library(dplyr)

#                                                                                  #
#                                                                                  #
#                                  FUNÇÕES                                         #
#                                                                                  #
#                                                                                  #


# Criar função para remover dados duplicados
consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  return(result)
}


#                                                                                  #
#                                                                                  #
#                             CARREGANDO OS DADOS                                  #
#                                                                                  #
#                                                                                  #

# Ler dados da URL e atribuir nomes às colunas
# Preencher dados faltantes com NA
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names, fill = TRUE)
head(cepagri)

# SETAR O WORKING DIRECTORY
setwd("C:/Users/czset/OneDrive/Documentos/Mineração de Dados Complexos-CarlosSette/INF-612 Análise de dados/trabalhos/trabalho 2")



#                                                                                  #
#                                                                                  #
#                            TRATAMENTO DE DADOS                                   #
#                                                                                  #
#                                                                                  #


# Checar o tipo de dado de cada coluna.
for (col in cepagri){
  print(class(col))
}

# Aqui, notamos que há valores não numéricos na coluna de temperatura
# precisamos tratar isso.
unique(cepagri$temp)
# Aqui vemos que existem colunas com o valor " [ERRO]" e também um valor absurdo de "-7999.0"
# Vamos remover ambos.

cepagri <- cepagri[cepagri$temp != " [ERRO]", ]
cepagri <- cepagri[cepagri$temp != "-7999.0", ]
# É necessário converter a coluna temp para numerico pois haviam strings nela.
cepagri$temp <- as.numeric(cepagri$temp)

# vamos verificar novamente se esses valores foram removidos
for (col in cepagri){
  print(class(col))
}

# A coluna de horário está como caractere. 
# vamos converter para horário.
cepagri$horario <- as.POSIXct(cepagri$horario, format = '%d/%m/%Y-%H:%M', tz = "America/Sao_Paulo")
class(cepagri$horario)

# criando colunas de ano e mes
cepagri$horario <- as.POSIXlt(cepagri$horario)
cepagri$ano <- unclass(cepagri$horario)$year + 1900
cepagri$mes <- unclass(cepagri$horario)$mon + 1


# Aqui, vemos que há valores extremos para sensaçao térmica (99.9 e -8.20)

cepagri[!is.na(cepagri$sensa) & (cepagri$sensa < 0), ]
cepagri[!is.na(cepagri$sensa) & (cepagri$sensa == 99.9), ]

# Vamos remover o 99.9 e colocar NA no lugar.
cepagri[!is.na(cepagri$sensa) & (cepagri$sensa == 99.9), 5] <- NA
summary(cepagri$sensa)

# checando se os dados de temperatura foram duplicados no intervalo de 1 dia
any(consecutive(cepagri$temp, 144))

# Criar um filtro. Será usado posteriormente para remover os dados duplicados.
filtro <- consecutive(cepagri$temp,144)
sum(filtro)
# Remover linhas com valores repetidos
cepagri <- cepagri[!filtro, ]
any(consecutive(cepagri$temp, 144)) # Deve ser FALSE

# checando se os dados de umidade foram duplicados no intervalo de 1 dia
any(consecutive(cepagri$umid, 144))
filtro <- consecutive(cepagri$umid,144)
sum(filtro)
cepagri <- cepagri[!filtro, ]


# Filtrar datas 
cepagri <- cepagri[(cepagri$ano >= 2015 & cepagri$ano<2025), ]


#                                                                                  #
#                                                                                  #
#                             ANÁLISE DE DADOS                                     #
#                                                                                  #
#                                                                                  #


# Verificar o summary de todas as colunas
summary(cepagri)

# Histograma das variáveis
hist(cepagri$vento)
hist(cepagri$temp)
hist(cepagri$umid)
hist(cepagri$sensa)


# Temperaturas médias e extremos, ano a ano
dados <- aggregate(temp ~ ano, data = cepagri, summary)
dados$temp_min <- dados$temp[, 1]
dados$temp_max <- dados$temp[, 6]
dados$temp_media <- dados$temp[, 4]
ggplot(dados, aes(x=ano)) +
  geom_line(aes(y=temp_min, color="temp Min"), size=1) +
  geom_line(aes(y=temp_max, color="temp Max"), size=1) +
  geom_line(aes(y=temp_media, color="temp Media"), size=1) +
  geom_point(aes(y=temp_min, color="temp Min"), size=2) +
  geom_point(aes(y=temp_max, color="temp Max"), size=2) +
  geom_point(aes(y=temp_media, color="temp Media"), size=2) +
  geom_smooth(aes(y=temp_min, color = "temp Min"),method = "lm", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=temp_max, color = "temp Max"),method = "lm", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=temp_media, color = "temp Media"),method = "lm", se=FALSE, linetype = "dashed") +
  labs(x="Ano", y="temp", title="Extremos e média de temp (2015-2024)") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1), labels = as.integer) +
  scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  scale_color_manual(values=c("temp Min"="blue", "temp Max"="red", "temp Media" = "black"), name="Legenda") +
  theme_minimal()


# vento máxima e mínima, ano a ano
dados <- aggregate(vento ~ ano, data = cepagri, summary)
dados$vento_min <- dados$vento[, 1]
dados$vento_max <- dados$vento[, 6]
dados$vento_media <- dados$vento[, 4]
ggplot(dados, aes(x=ano)) +
  geom_line(aes(y=vento_min, color="vento Min"), size=1) +
  geom_line(aes(y=vento_max, color="vento Max"), size=1) +
  geom_line(aes(y=vento_media, color="vento Media"), size=1) +
  geom_point(aes(y=vento_min, color="vento Min"), size=2) +
  geom_point(aes(y=vento_max, color="vento Max"), size=2) +
  geom_point(aes(y=vento_media, color="vento Media"), size=2) +
  geom_smooth(aes(y=vento_min, color = "vento Min"),method = "lm", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=vento_max, color = "vento Max"),method = "lm", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=vento_media, color = "vento Media"),method = "lm", se=FALSE, linetype = "dashed") +
  labs(x="Ano", y="vento", title="Extremos e média de vento (2015-2024)") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1), labels = as.integer) +
  scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  scale_color_manual(values=c("vento Min"="blue", "vento Max"="red", "vento Media" = "black"), name="Legenda") +
  theme_minimal()


# Umidade máxima e mínima, ano a ano
dados <- aggregate(umid ~ ano, data = cepagri, summary)
dados$umid_min <- dados$umid[, 1]
dados$umid_max <- dados$umid[, 6]
dados$umid_media <- dados$umid[, 4]
ggplot(dados, aes(x=ano)) +
  geom_line(aes(y=umid_min, color="umid Min"), size=1) +
  geom_line(aes(y=umid_max, color="umid Max"), size=1) +
  geom_line(aes(y=umid_media, color="umid Media"), size=1) +
  geom_point(aes(y=umid_min, color="umid Min"), size=2) +
  geom_point(aes(y=umid_max, color="umid Max"), size=2) +
  geom_point(aes(y=umid_media, color="umid Media"), size=2) +
  geom_smooth(aes(y=umid_min, color = "umid Min"),method = "lm", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=umid_max, color = "umid Max"),method = "lm", se=FALSE, linetype = "dashed") +
  geom_smooth(aes(y=umid_media, color = "umid Media"),method = "lm", se=FALSE, linetype = "dashed") +
  labs(x="Ano", y="umidade", title="Extremos e média de Umidade (2015-2024)") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1), labels = as.integer) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values=c("umid Min"="blue", "umid Max"="red", "umid Media" = "black"), name="Legenda") +
  theme_minimal()


# Analise de desvio padrao da temperatura
dados <- aggregate(temp ~ ano, data = cepagri, sd)
ggplot(dados, aes(x=ano)) +
  geom_line(aes(y=temp, color="temp"), size=1) +
  geom_point(aes(y=temp, color="temp"), size=2) +
  geom_smooth(aes(y=temp, color = "temp"),method = "lm", se=FALSE, linetype = "dashed") +
  labs(x="Ano", y="temp", title="Desvio padrão da temperatura (2015-2024)") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1), labels = as.integer) +
  scale_color_manual(values=c("temp"="black"), name="Legenda") +
  theme_minimal()

# Analise de desvio padrao da umidade
dados <- aggregate(umid ~ ano, data = cepagri, sd)
ggplot(dados, aes(x=ano)) +
  geom_line(aes(y=umid, color="umid"), size=1) +
  geom_point(aes(y=umid, color="umid"), size=2) +
  geom_smooth(aes(y=umid, color = "umid"),method = "lm", se=FALSE, linetype = "dashed") +
  labs(x="Ano", y="umid", title="Desvio padrão da umidade (2015-2024)") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1), labels = as.integer) +
  scale_color_manual(values=c("umid"="black"), name="Legenda") +
  theme_minimal()


# Analise de desvio padrao do vento
dados <- aggregate(vento ~ ano, data = cepagri, sd)
ggplot(dados, aes(x=ano)) +
  geom_line(aes(y=vento, color="vento"), size=1) +
  geom_point(aes(y=vento, color="vento"), size=2) +
  geom_smooth(aes(y=vento, color = "vento"),method = "lm", se=FALSE, linetype = "dashed") +
  labs(x="Ano", y="vento", title="Desvio padrão do vento (2015-2024)") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1), labels = as.integer) +
  scale_color_manual(values=c("vento"="black"), name="Legenda") +
  theme_minimal()



# ANALISE DE CORRELAÇÃO ENTRE AS VARIÁVEIS
ggcorrplot(cor(cepagri[c(2:5)]), 
           outline.color = "white",
           ggtheme = theme_bw(),
           colors = c("#F8696B", "#FFEB84", "#63BE7B"),
           legend.title = "Correlation",
           lab = TRUE,
           lab_size = 3,
           tl.cex = 8,
           tl.srt = 0,
           title = "Correlação entre as variáveis") +
  theme(plot.title = element_text(hjust = 0.5, size=10), legend.title = element_text(size = 10))



# ANALISE DE ESTAÇOES DO ANO

# Criar vetor de estacoes
estacoes <- c("Outono", "Inverno", "Primavera", "Verão")
# Colocar tudo como verão
cepagri$estacao <- estacoes[4]
# Defininindo as estaçoes de acordo com as datas de inicio e fim
cepagri[
  (cepagri$horario >= "2015-03-20 00:00:00" & cepagri$horario < "2015-06-20 00:00:00") |
  (cepagri$horario >= "2016-03-20 00:00:00" & cepagri$horario < "2016-06-20 00:00:00") |
  (cepagri$horario >= "2017-03-20 00:00:00" & cepagri$horario < "2017-06-20 00:00:00") |
  (cepagri$horario >= "2018-03-20 00:00:00" & cepagri$horario < "2018-06-20 00:00:00") |
  (cepagri$horario >= "2019-03-20 00:00:00" & cepagri$horario < "2019-06-20 00:00:00") |
  (cepagri$horario >= "2020-03-20 00:00:00" & cepagri$horario < "2020-06-20 00:00:00") |
  (cepagri$horario >= "2021-03-20 00:00:00" & cepagri$horario < "2021-06-20 00:00:00") |
  (cepagri$horario >= "2022-03-20 00:00:00" & cepagri$horario < "2022-06-20 00:00:00") | 
  (cepagri$horario >= "2023-03-20 00:00:00" & cepagri$horario < "2023-06-20 00:00:00") | 
  (cepagri$horario >= "2024-03-20 00:00:00" & cepagri$horario < "2024-06-20 00:00:00")
  ,8] <- estacoes[1]
    
cepagri[
  (cepagri$horario >= "2015-06-20 00:00:00" & cepagri$horario < "2015-09-22 00:00:00") |
    (cepagri$horario >= "2016-06-20 00:00:00" & cepagri$horario < "2016-09-22 00:00:00") |
    (cepagri$horario >= "2017-06-20 00:00:00" & cepagri$horario < "2017-09-22 00:00:00") |
    (cepagri$horario >= "2018-06-20 00:00:00" & cepagri$horario < "2018-09-22 00:00:00") |
    (cepagri$horario >= "2019-06-20 00:00:00" & cepagri$horario < "2019-09-22 00:00:00") |
    (cepagri$horario >= "2020-06-20 00:00:00" & cepagri$horario < "2020-09-22 00:00:00") |
    (cepagri$horario >= "2021-06-20 00:00:00" & cepagri$horario < "2021-09-22 00:00:00") |
    (cepagri$horario >= "2022-06-20 00:00:00" & cepagri$horario < "2022-09-22 00:00:00") | 
    (cepagri$horario >= "2023-06-20 00:00:00" & cepagri$horario < "2023-09-22 00:00:00") | 
    (cepagri$horario >= "2024-06-20 00:00:00" & cepagri$horario < "2024-09-22 00:00:00")
  ,8] <- estacoes[2]

cepagri[
  (cepagri$horario >= "2015-09-22 00:00:00" & cepagri$horario < "2015-12-21 00:00:00") |
    (cepagri$horario >= "2016-09-22 00:00:00" & cepagri$horario < "2016-12-21 00:00:00") |
    (cepagri$horario >= "2017-09-22 00:00:00" & cepagri$horario < "2017-12-21 00:00:00") |
    (cepagri$horario >= "2018-09-22 00:00:00" & cepagri$horario < "2018-12-21 00:00:00") |
    (cepagri$horario >= "2019-09-22 00:00:00" & cepagri$horario < "2019-12-21 00:00:00") |
    (cepagri$horario >= "2020-09-22 00:00:00" & cepagri$horario < "2020-12-21 00:00:00") |
    (cepagri$horario >= "2021-09-22 00:00:00" & cepagri$horario < "2021-12-21 00:00:00") |
    (cepagri$horario >= "2022-09-22 00:00:00" & cepagri$horario < "2022-12-21 00:00:00") | 
    (cepagri$horario >= "2023-09-22 00:00:00" & cepagri$horario < "2023-12-21 00:00:00") | 
    (cepagri$horario >= "2024-09-22 00:00:00" & cepagri$horario < "2024-12-21 00:00:00")
  ,8] <- estacoes[3]



# Gráfico com pontos médios de temperatura e linha conectando os pontos
# Criar os fatores a serem usados nos gráficos
cepagri$estacao <- as.factor(cepagri$estacao)
# Agrupar temperaturas medias por estaçao e ano
media_temp <- cepagri %>%
  group_by(ano, estacao) %>%
  summarise(temp_media = mean(temp))

ggplot(media_temp, aes(x=factor(ano), y=temp_media, group=estacao)) +
  geom_point(aes(color=estacao)) +
  geom_line(aes(color=estacao)) +
  facet_wrap(~estacao, scales="free_x") +
  labs(x="Ano", y="Temperatura Média", title="Temperatura Média Anual por Estação (2015-2024)",
       color="Estação")

# Boxplots de distribuicao de temperatura por estaçao
ggplot(cepagri, aes(x=factor(ano), y=temp, fill=factor(ano))) +
  geom_boxplot() +
  facet_wrap(~estacao) +
  labs(x="Ano", y="Temperatura", title="Distribuição de Temperatura por Estação (2021-2024)",
       fill="Ano")


# Heatmap de Temperatura Média Mensal por Estação (2021-2024)
ggplot(cepagri, aes(x=factor(mes), y=factor(ano), fill=temp)) +
  geom_tile() +
  facet_wrap(~estacao) +
  scale_fill_gradient(low="blue", high="red") +
  labs(x="Mês", y="Ano", title="Heatmap de Temperatura Média Mensal por Estação (2021-2024)",
       fill="Temperatura")

