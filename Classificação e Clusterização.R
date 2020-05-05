
install.packages("caTools")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071")

##lendo dataset e fazendo cópia
dataset <- read.csv('dataset.csv', stringsAsFactors = F)
data <- dataset

##tirando a coluna x pois é inútil
data$X <- NULL

## tratando as vírgulas, substituindo por ponto
data$Sepal.Length <- gsub(',','\\.',data$Sepal.Length)
data$Petal.Length <- gsub(',','\\.',data$Petal.Length)

##tratando strings com caracteres de expressão regular
data$Sepal.Width <- gsub('\\$','',data$Sepal.Width)
data$Petal.Length <- gsub('\\*','',data$Petal.Length)
data$Petal.Width <- gsub('\\*','',data$Petal.Width)
data$Species <- gsub('_','',data$Species)

##deixando em lower case
data$Species <- tolower(data$Species)

## tratando dados faltantes
data_t <- data
data_t <- data_t[data$Species !="n/d",]

##convertendo preditoras
data_t$Sepal.Length <- as.numeric(data_t$Sepal.Length)
data_t$Sepal.Width <- as.numeric(data_t$Sepal.Width)
data_t$Petal.Length <- as.numeric(data_t$Petal.Length)
data_t$Petal.Width <- as.numeric(data_t$Petal.Width)
data_t$Species <- as.factor(data_t$Species)

######################### CLASSIFICAÇÃO #################
dados_modelo_class <- data_t
library(caTools)

##separando em 80% treino e 20% teste
valores<-sample.split(dados_modelo_class$Species, SplitRatio = 0.8)
table(valores)

##criando nova coluna para separar esses 80% e 20% em true e false
dados_modelo_class$flag <- valores

##criando dados de treino e dados para testar o treino, e apagando a última coluna
dado_treino <- dados_modelo_class[dados_modelo_class$flag == T,]
dado_treino$flag <- NULL
dado_teste <- dados_modelo_class[dados_modelo_class$flag == F,]
dado_teste$flag <- NULL

##criando modelo
library(randomForest)
library(caret)
modelo_rf <-randomForest(Species ~ ., data = dado_treino)
modelo_rf

##fazendo a predição dos dados de testes
predicao <-predict(modelo_rf,newdata = dado_teste[,-5])

## olhar para comparar os acertos
dado_teste$predicao <- predicao
atual     <- dado_teste$Species
predicted <- as.factor(predicao)

## salvando a matrix de confusão (confusion matrix) em um objeto e printando
install.packages("e1071")
library(e1071)
cof_matrix <- caret::confusionMatrix(predicted, atual)
cof_matrix

## convertendo as métricas de avaliação em um data frame
metricas <- as.data.frame(cof_matrix$byClass)
View(metricas)


################################ CLUSTER ####################

##pegando apenas as características
dados_modelo_clus <- data_t
caracteristicas <- dados_modelo_clus[, 1:4]

##aplicando kmeans com 3 clusters
cluster <- kmeans(caracteristicas, 3)
#aplicando cluster
dados_modelo_clus$cluster <- cluster$cluster
cluster$size