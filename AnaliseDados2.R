library(ggplot2)


####################################################################################
#######################    CARREGANDO OS DADOS    ##################################
####################################################################################

# Ler dados da URL e atribuir nomes às colunas
# Preencher dados faltantes com NA
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names, fill = TRUE)
head(cepagri)

# SETAR O WORKING DIRECTORY
setwd("C:/Users/czset/OneDrive/Documentos/Mineração de Dados Complexos-CarlosSette/INF-612 Análise de dados/trabalhos/trabalho 2")


####################################################################################
#######################    TRATAMENTO DE DADOS    ##################################
####################################################################################

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

# Verificar o summary de todas as colunas
for (col in names(cepagri)){
  cat("coluna: ", col, "\n")
  print(summary(cepagri[col]))
  cat("\n")
}

# Aqui, vemos que há valores extremos para sensaçao térmica (99.9 e -8.20)

cepagri[!is.na(cepagri$sensa) & (cepagri$sensa < 0), ]
cepagri[!is.na(cepagri$sensa) & (cepagri$sensa == 99.9), ]

# Vamos remover o 99.9 e colocar NA no lugar.
cepagri[!is.na(cepagri$sensa) & (cepagri$sensa == 99.9), 5] <- NA
summary(cepagri$sensa)

# Criar função para remover dados duplicados
consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  return(result)
}

# checando se os dados de tmeperatura foram duplicados no intervalo de 1 dia
any(consecutive(cepagri$temp, 144))

# Criar um filtro. Será usado posteriormente para remover os dados duplicados.
filtro <- consecutive(cepagri$temp,144)
sum(filtro)
# Remover linhas com valores repetidos
cepagri <- cepagri[!filtro, ]

# Verificar o summary de todas as colunas
for (col in names(cepagri)){
  cat("coluna: ", col, "\n")
  print(summary(cepagri[col]))
  cat("\n")
}

# Filtrar datas 
cepagri <- cepagri[(cepagri$ano >= 2015 & cepagri$ano<2025), ]



####################################################################################
#######################    ANÁLISE DE DADOS    #####################################
####################################################################################

# 1. Analise de temperaturas ano a ano

cat("Temperatura média ano a ano:", "\n")
tapply(cepagri$temp, cepagri$ano, mean)
cat("Temp min e max ano a ano:", "\n")
tapply(cepagri$temp, cepagri$ano, range)
cat("Desvio padrao ano a ano:", "\n")
tapply(cepagri$temp, cepagri$ano, sd)
cat("Analisando a temperatura média, agrupando os anos e meses :", "\n")
tapply(cepagri$temp, list(cepagri$ano, cepagri$mes), mean)

cat("Analisando a temperatura média, mes a mes :", "\n")
tapply(cepagri$temp, cepagri$mes, mean)
cat("Analisando o desvio padrao da temperatura, mes a mes :", "\n")
tapply(cepagri$temp, cepagri$mes, sd)


# Analise geral
tapply(cepagri$temp, cepagri$ano, summary)
tapply(cepagri$vento, cepagri$ano, summary)
tapply(cepagri$umid, cepagri$ano, summary)
tapply(cepagri$sensa, cepagri$ano, summary)



####################################################################################
#######################    ANÁLISE DE GRAFICOS #####################################
####################################################################################

# Criar dataframes filtrados dos últimos 4 anos
cepagri2021 <- cepagri[cepagri$ano == 2021, ]
cepagri2022 <- cepagri[cepagri$ano == 2022, ]
cepagri2023 <- cepagri[cepagri$ano == 2023, ]
cepagri2024 <- cepagri[cepagri$ano == 2024, ]

# Criar df para ultimos 4 anos (periodo de interesse)
cepagri_last_years <- rbind(cepagri2021,cepagri2022, cepagri2023, cepagri2024)

# Criar os fatores a serem usados nos gráficos
cepagri$ano <- as.factor(cepagri$ano)
cepagri2021$mes <- as.factor(cepagri2021$mes)
cepagri2022$mes <- as.factor(cepagri2022$mes)
cepagri2023$mes <- as.factor(cepagri2023$mes)
cepagri2024$mes <- as.factor(cepagri2024$mes)

# Boxplots de temperatura ano a ano
ggplot(cepagri_last_years, aes(x = ano, y = temp, group = ano)) + geom_boxplot() + ggtitle("Boxplot de temperatura ano a ano")
ggplot(cepagri_last_years, aes(x = ano, y = temp, 
                       group = ano, fill = sensa))  +  geom_violin()

# Boxplots para anos 2021 a 2024
ggplot(cepagri2021, aes(x = mes, y = temp, group = mes)) + geom_boxplot() + ggtitle("2021 - Boxplot de temperatura mes a mes")
ggplot(cepagri2022, aes(x = mes, y = temp, group = mes)) + geom_boxplot() + ggtitle("2022 - Boxplot de temperatura mes a mes")
ggplot(cepagri2023, aes(x = mes, y = temp, group = mes)) + geom_boxplot() + ggtitle("2023 - Boxplot de temperatura mes a mes")
ggplot(cepagri2024, aes(x = mes, y = temp, group = mes)) + geom_boxplot() + ggtitle("2024 - Boxplot de temperatura mes a mes")

# Verificar qual foi a temperatura maxima de 2021
max(cepagri2021$temp)

# Criar plots com fatores
ggplot(cepagri_last_years, aes(x=umid, y = temp))+geom_point(alpha = 0.5)
ggplot(cepagri_last_years, aes(x=mes, y = temp, colour = ano))+geom_point(alpha = 0.5)
ggplot(cepagri_last_years, aes(x=mes,y = temp))+geom_boxplot()

# Criando um histograma sobreposto 2021 - 2024
# histograma 2021
hist(cepagri2021$temp, breaks=30, col=rgb(1,0,0,0.5), xlab="temperatura", 
     ylab="contagem", main="distribuicao de temperatura 2021 - 2024" )

# Adicionar histograma 2024
hist(cepagri2024$temp, breaks=30, col=rgb(0,0,1,0.5), add=T)

# Adicionar legenda
legend("topright", legend=c("2021","2024"), col=c(rgb(1,0,0,0.5), 
                                                  rgb(0,0,1,0.5)), pt.cex=2, pch=15 )


# Criando um histograma sobreposto 2021 e 2024
# histograma 2023
hist(cepagri2023$temp, breaks=30, col=rgb(1,0,0,0.5), xlab="temperatura", 
     ylab="contagem", main="distribuicao de temperatura 2023 - 2024" )

# Adicionar histograma 2024
hist(cepagri2024$temp, breaks=30, col=rgb(0,0,1,0.5), add=T)

# Adicionar legenda
legend("topright", legend=c("2023","2024"), col=c(rgb(1,0,0,0.5), 
                                                  rgb(0,0,1,0.5)), pt.cex=2, pch=15 )


library(hrbrthemes)
library(dplyr)
# Adicionar uma coluna de índice ao dataframe
cepagri_last_years <- cepagri_last_years %>%
  mutate(indice = row_number())
# Criar o gráfico com pontos, linha de tendência e polígonos
ggplot(cepagri_last_years, aes(x=indice, y=temp)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE) +
  geom_polygon(data=cepagri_last_years %>% 
                 group_by(ano) %>% 
                 summarise(indice=c(min(indice), min(indice), max(indice), max(indice)), 
                           temp=c(min(temp), max(temp), max(temp), min(temp))),
               aes(x=indice, y=temp, fill=factor(ano)), alpha=0.2) +
  labs(x="Índice", y="Temperatura", title="Distribuição de temperatura com tendência linear e anos",
       fill="Ano")
