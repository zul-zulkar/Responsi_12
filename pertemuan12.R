# read Data
data <- read.csv("seeds_dataset.txt", sep="\t", header=FALSE)

#Preprocessing
anyNA(data)

library(dplyr)
data$V8 <- recode(data$V8, '1'="Kama", '2'="Rosa", '3'="Canadian")
levels(data$V8)
data$V8 <- as.factor(data$V8)
str(data)

## Split Validation
set.seed(123) # angka random
sampling <- sample(1:nrow(data), 0.8*nrow(data))
sampling
training_set <- data[sampling,]
test_set <- data[-sampling,]


#model KNN dengan splitvalidation
library(class)
split_knn <- knn(training_set[,-8], test_set[,-8], training_set$V8, k=5)
split_knn

akurasi <- Accuracy(split_knn, test_set$V8)
akurasi
akurasi <- Specificity(y_pred=split_knn, y_true=test_set$V8)
akurasi

library(MLmetrics)
uji <- function(en){
  cat("k\tAkurasi\t\tSensitivity\tprecision\t recall \t F-1 score\n")
  for (i in 2:en) {
    pred <- knn(training_set[,-8], test_set[,-8], training_set$V8, k=i)
    akurasi <- Accuracy(pred, test_set$V8)
    sensitif <- Sensitivity(pred, test_set$V8)
    #spesifi <- Specificity(test_set$V8,pred)
    presisi <- Precision(pred, test_set$V8)
    rec <- Recall(pred, test_set$V8)
    f <- F1_Score(pred, test_set$V8)
    cat(i,"\t",akurasi,"\t",sensitif,"\t",presisi,"\t",rec,"\t",f,"\n")
  }
}

uji(20)


Accuracy(split_knn, test_set$V8)
Sensitivity(split_knn, test_set$V8, positive="Canadian")
Specificity(split_knn, test_set$V8, positive="Canadian")
Precision(split_knn, test_set$V8, positive="Canadian")
Recall(split_knn, test_set$V8, positive="Canadian")
F1_Score(split_knn, test_set$V8, positive="Canadian")


library(caret)

split_knn <- knn(training_set[,-8], test_set[,-8], training_set$V8, k=2)
confusionMatrix(split_knn, test_set$V8)

## Cross Validation
myControl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

crosval_knn <- train(V8~., data = training_set, method = "knn",
                     tuneGrid = expand.grid(k = 2:5), trControl = myControl,
                     preProc = c('center', 'scale'))
pred <- predict(crosval_knn, test_set)
confusionMatrix(pred, test_set$V8)

Precision(pred, test_set$V8, positive="Canadian")
Recall(pred, test_set$V8, positive="Canadian")
F1_Score(pred, test_set$V8, positive="Canadian")