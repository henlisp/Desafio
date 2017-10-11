library(readxl)
TodosOsProdutos <- read_excel("~/Desktop/PrevisaoTodosProdutos/TodosOsProdutos.xlsx")
TodosOsProdutos[1:12,1:2]
#View(TodosOsProdutos)

library(forecast)

TodosOsProdutos1 <- ts(TodosOsProdutos[,-1],f=12,s=2016+6/12) 
TodosOsProdutos1[1:12,1:2]
#View(TodosOsProdutos1)

ncols <- ncol(TodosOsProdutos1)
ncols
h <- 3 #períodos futuros para o forecast

fcastMean <- matrix(NA,nrow=h,ncol=ncols)
  for (i in 1:ncols) {
    fcastMean[,i] <- forecast(TodosOsProdutos1[,i],h=h)$mean
  }  
fcastMean[1:3,1:3] #Vendo as primeiras previsões

#Adicionando a coluna data
fcastMean1 <- cbind(c('06/2017', '07/2017', '08/2017'), fcastMean)
dim(fcastMean1)
fcastMean1[,1:4]

produtos <- colnames(TodosOsProdutos[2:ncol(TodosOsProdutos)])
head(produtos)

#Adicionamos uma linha em cima colocando o nome da cada produto na coluna das suas previsões.
fcastMean2 <- rbind(union("PredictionDates", produtos), fcastMean1)
fcastMean2[,1:4]

#Exportando o arquivo, disponível em https://github.com/hlispector/Desafio/blob/master/Forecast/PrevisaoJunhoJulhoAgosto2017.xlsx
write(t(fcastMean2),file="Desktop/PrevisaoTodosProdutos/PrevisaoJunhoJulhoAgosto2017.csv",sep=",",ncol=ncol(fcastMean2))

fcastMethod <- matrix(NA,nrow=h,ncol=ncols)
  for (i in 1:ncols) {
   fcastMethod[,i] <- forecast(TodosOsProdutos1[,i],h=h)$method
  }  
fcastMethod [1:3,1:3]

#Adicionando a coluna data
fcastMethod1 <- cbind(c('06/2017', '07/2017', '08/2017'), fcastMethod)
fcastMethod1[,1:4]

#Adicionamos uma linha em cima colocando o nome da cada produto na coluna das suas previsões.
fcastMethod2 <- rbind(union("PredictionDates", produtos), fcastMethod1)
fcastMethod2

# ERRORS WILL OCCUR IN fcastModel, level: confidence value associated with prediction interval
#, lower limits for prediction, upper, residuals, fitted due to missing values


