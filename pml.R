library(ggplot2)
library(caret)
library(randomForest)
library(gbm)

setwd('C:/Users/orugyag/Contacts/Desktop/WORK/Training/JHU_Data_Science_Specialization/Practical Machine Learning')

tr<-read.csv("pml-training.csv")
tr<-data.frame(tr)
cn<-colnames(tr)
statColInd<-c(grep("avg",cn),grep("kurtosis",cn),grep("skewness",cn),grep("max",cn),grep("min",cn),grep("amplitude",cn),grep("var",cn),grep("stddev",cn))
tr_sub<-tr[,-statColInd]
tr_sub2<-tr_sub[,-c(1,3,4,5,6,7)]


te<-read.csv("pml-testing.csv")
te<-data.frame(te)
cn_te<-colnames(te)
statColInd_te<-c(grep("avg",cn_te),grep("kurtosis",cn_te),grep("skewness",cn_te),grep("max",cn_te),grep("min",cn_te),grep("amplitude",cn_te),grep("var",cn_te),grep("stddev",cn_te))
te_sub<-te[,-statColInd_te]
te_sub2<-te_sub[,-c(1,3,4,5,6,7,60)]

set.seed(123)
boostFit <- train(classe ~ ., method = "gbm", data = tr_sub2, verbose = F, trControl = trainControl(method = "cv", number = 10))

# set.seed(123)
# rfFit <- train(classe ~ ., method = "rf", data = tr_sub2, importance = T, trControl = trainControl(method = "cv", number = 10))

tr_predict<-predict(boostFit,newdata = tr_sub2)
confusionMatrix(tr_predict,tr_sub2$classe)

te_predict<-as.character(predict(boostFit,newdata = te_sub2))

# write prediction files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./prediction/problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(te_predict)