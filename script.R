rm(list=ls())
library(RSNNS)

plotImage <- function(row){
  img <- matrix(row, nrow=28, ncol=28)
  par(mar=c(0, 0, 0, 0))
  image(img, useRaster=TRUE, axes=FALSE)
}

dbTrain <- as.matrix(read.csv(file='trainReduzido.csv'))
#dbTest <- as.matrix(read.csv(file='validacao.csv'))

x <- dbTrain[,3:786]
x <- ifelse(x==0,0,255)

y <- as.matrix(dbTrain[,2])

model <- mlp(x, y)

confusionMatrix(y,fitted.values(model))

error <- model[["IterativeFitError"]]
plot(1:length(error),error, type="l")
