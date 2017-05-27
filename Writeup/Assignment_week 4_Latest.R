setwd("D:/R working file/Course 8 Practical Machine Learning/Week 4")

library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

trainingset <- read.csv("D:/R working file/Course 8 Practical Machine Learning/Week 4/pml-training.csv",na.strings = c("NA","#DIV/0!","")) 
testingset <- read.csv("D:/R working file/Course 8 Practical Machine Learning/Week 4/pml-testing.csv",na.strings = c("NA","#DIV/0!",""))
dim(trainingset)
dim(testingset)

trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]

trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]


dim(trainingset)
dim(testingset)
head(trainingset)
head(testingset)

subsamples <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
subTraining <- trainingset[subsamples, ] 
subTesting <- trainingset[-subsamples, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)

plot(subTraining$classe, col="blue", main="Bar Plot of levels of the variable classe within the subTraining data set", xlab="classe levels", ylab="Frequency")

model1 <- rpart(classe ~ ., data=subTraining, method="class")

prediction1 <- predict(model1, subTesting, type = "class")

rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

confusionMatrix(prediction1, subTesting$classe)

model2 <- randomForest(classe ~. , data=subTraining, method="class")

prediction2 <- predict(model2, subTesting, type = "class")

confusionMatrix(prediction2, subTesting$classe)