######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Carlos Zerwes Amado Sette                                                               
#   - Igor Alejandro Sousa Santos   
#   - João Pedro Brandimarte Viccari
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(tokenizers)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
setwd("C:/Users/czset/OneDrive/Documentos/Mineração de Dados Complexos-CarlosSette/INF-611 Recuperação de Informação/trabalho1")


######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
# head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
# head(queries)
# Exemplo de acesso aos tokens de uma consulta
# q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
# head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
# ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
# names(ground_truths)[ground_truths[1,]==1]


# Computando a matriz de termo-documento

term_freq <- document_term_frequencies(docs, term = "word")
    

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, k=1.2, b=0.75))

# Visualizando as estatísticas da coleção (apenas para debuging)
# head(docs_stats)



######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  # Dica: você pode acessar a segunda coluna da query a partir de $word ou [["word"]]
  ranking <- get_ranking_by_stats(stat_name, stats, query$word)
  # Visualizando o ranking (apenas para debuging)
  head(ranking, n = 5)
  
  # Calculando a precisão
  # Dica: para calcular a precisão, revocação e utilizar a função plot_prec_e_rev,
  # utilize a coluna doc_id do ranking gerado (você pode acessar com $doc_id)
  p <- precision(ground_truth, ranking$doc_id, top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, k = top, text = text) 
}

# Definindo a consulta 1 
# Dicas para as variáveis consulta1 e n_consulta1:
# Para a variável consulta1, você deve acessar os tokens de uma consulta, conforme
# o exemplo da linha 52 e 53.
# Para a variável n_consulta1, você deve informar o número da consulta. Por exemplo,
# se usar a Query_01 como consulta, n_consulta1 deve receber o valor 1.

consulta1 <- queries[queries$doc_id=="Query_01",]
n_consulta1 <- 1

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ],docs_stats,"tf_idf",top = 20,"Consulta 1 - tf_idf")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ],docs_stats,"bm25",top = 20,"Consulta 1 - bm25")


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id=="Query_015",]
n_consulta2 <- 15

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2, ],docs_stats,"tf_idf",top = 20,"Consulta 2 - tf_idf")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2, ],docs_stats,"bm25",top = 20,"Consulta 2 - bm25")


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
#
# 
# 
# 
# Para k = 20, obtivemos os seguintes resultados:
# 
# Consulta 1
#   tf_idf: Precisão:  0.35 	Revocação:  1
#   bm25: 0.35 	Revocação:  1 
# Consulta 2
#   tf_idf:0.15 	Revocação:  1
#   bm25: 0.15 	Revocação:  1 
# 
# Olhando os gráficos plotados, vemos que o método bm25 performou melhor
# do que o tf_idf. 
# No gráfico de bm25 para a Consulta 1, conseguimos obter um valor de p = 0.85 e r = 0.85 
# para k = 7, enquanto que no gráfico tf_idf para Query01, esse patamar de p = 0.85 não é atingido.
# Realizando a mesma análise para Consulta 2, tiramos a mesma conclusão de que o bm25 performou melhor.
# Por exemplo, para k = 5, temos p(tf_idf) = 0.4 e r(tf_idf) = 0.65, ambas menores 
# que p(bm25) = 0.6 e r(bm25) = 1.0 para o mesmo valor de k.
# 
# Para efeito de comparação, vamos calcular a taxa F1 para k = 7 e para k = 5.
# 
# 
ranking_tfidf_q01 <- get_ranking_by_stats("tf_idf", docs_stats, consulta1$word)
f1_score_k5_tfidf_consulta01 <- f1_score(ground_truths[n_consulta1, ], ranking_tfidf_q01$doc_id,5); f1_score_k5_tfidf_consulta01

ranking_bm25_q01 <- get_ranking_by_stats("bm25", docs_stats, consulta1$word)
f1_score_k5_bm25_consulta01 <- f1_score(ground_truths[n_consulta1, ], ranking_bm25_q01$doc_id,5); f1_score_k5_bm25_consulta01

ranking_tfidf_q02 <- get_ranking_by_stats("tf_idf", docs_stats, consulta2$word)
f1_score_k7_tfidf_consulta02 <- f1_score(ground_truths[n_consulta2, ], ranking_tfidf_q02$doc_id,7); f1_score_k7_tfidf_consulta02

ranking_bm25_q02 <- get_ranking_by_stats("bm25", docs_stats, consulta2$word)
f1_score_k7_bm25_consulta02 <- f1_score(ground_truths[n_consulta2, ], ranking_bm25_q02$doc_id,7); f1_score_k7_bm25_consulta02

# Executando os comandos acima, vemos que a taxa F1 foi maior para o método bm25 para ambas
# consultas (Query01 e Query015.)
# Conclusão: para as queries 01 e 015, o modelo bm25 performou melhor que o tf_idf.

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
# head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
# head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc, term = "word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, k=1.2, b=0.75))


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id=="Query_01",]
n_consulta1_proc <- 1
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], docs_stats_proc,"tf_idf", top = 20, "[Texto Processado] Consulta 1 - tf_idf")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], docs_stats_proc,"bm25", top = 20, "[Texto Processado] Consulta 1 - bm25")


# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id=="Query_015",]
n_consulta2_proc <- 15

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], docs_stats_proc,"tf_idf", top = 20, "[Texto Processado] Consulta 2 - tf_idf")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], docs_stats_proc,"bm25", top = 20, "[Texto Processado] Consulta 2 - bm25")

# Criando as variáveis dos rankings com texto processado
ranking_tfidf_q01_proc <- get_ranking_by_stats("tf_idf", docs_stats_proc, consulta1_proc$word)
ranking_bm25_q01_proc <- get_ranking_by_stats("bm25", docs_stats_proc, consulta1_proc$word)
ranking_tfidf_q02_proc <- get_ranking_by_stats("tf_idf", docs_stats_proc, consulta2_proc$word)
ranking_bm25_q02_proc <- get_ranking_by_stats("bm25", docs_stats_proc, consulta2_proc$word)



# Calculando a média das precisões médias

mean_1 <- mean_average_precision(list(
                            list(ground_truths[n_consulta1, ], ranking_tfidf_q01$doc_id),
                            list(ground_truths[n_consulta2, ], ranking_tfidf_q02$doc_id)
                            ), 20)
print("Ranking médio para consultas 1 e 2 do modelo tf_idf, sem processamento de texto:")
mean_1

mean_2 <- mean_average_precision(list(
  list(ground_truths[n_consulta1, ], ranking_bm25_q01$doc_id),
  list(ground_truths[n_consulta2, ], ranking_bm25_q02$doc_id)
), 20)
print("Ranking médio para consultas 1 e 2 do modelo bm25, sem processamento de texto:")
mean_2

mean_3 <- mean_average_precision(list(
  list(ground_truths[n_consulta1_proc, ], ranking_tfidf_q01_proc$doc_id),
  list(ground_truths[n_consulta2_proc, ], ranking_tfidf_q02_proc$doc_id)
), 20)
print("Ranking médio para consultas 1 e 2 do modelo tf_idf, COM processamento de texto:")
mean_3

mean_4 <- mean_average_precision(list(
  list(ground_truths[n_consulta1_proc, ], ranking_bm25_q01_proc$doc_id),
  list(ground_truths[n_consulta2_proc, ], ranking_bm25_q02_proc$doc_id)
), 20)
print("Ranking médio para consultas 1 e 2 do modelo tf_idf, COM processamento de texto:")
mean_4

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# 
# Analisando os gráficos gerados pelos comandos acima, nota-se que a remoção de stopwords não impactou significativamente as
# curvas de precisão e revocação, exceto por uma pequena melhoria na curva de precisão para o modelo tf_idf, observado pelo
# aumento do MAP (mean average precision) após o processamento de texto.
#
# É interessante notar que o processamento de texto, embora não tenha afetado a curva significativamente do modelo bm25,
# reduziu o MAP em 2% após o processamento de texto.
# 
# Em conclusão, o melhor modelo foi o bm25 sem remoção de stopwords, com um MAP de 80.8%.


######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
#                              full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
#                               full.names = TRUE)
# file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################
































