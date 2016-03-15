
#remove all ls before starting
rm(list=ls())


#load caret library
library(caret)
library(data.table)

#read in source train and test
train.source <- read.csv(file = "c:/kaggle/data/train.csv")
test.source <- read.csv(file = "c:/kaggle/data/test.csv")

#trainingset <- subset(trainingset, Marriage!="NaN")
#divide training dataset into partitions. one for training and another for testing
set.seed(123)
train_ind <- createDataPartition(train.source$DEATHS, p=0.75)[[1]]
traindata <- as.data.table(train.source[train_ind, ])
validdata <- as.data.table(train.source[-train_ind, ])
testdata <- test.source


# fit model on training data
fit.all <-  train(DEATHS ~ .-id, method = 'glm', data = traindata)

# run model on training data
pred.train.glm <- predict(fit.all, newdata = traindata)

# run model on test data

pred.valid.glm <- predict(fit.all, newdata = validdata)
pred.test.glm <- predict(fit.all, newdata = testdata)

sqrt(mean((pred.train.glm-traindata$DEATHS)^2))
sqrt(mean((pred.valid.glm-validdata$DEATHS)^2))

submit1 <- as.data.frame(pred.test.glm)
names(submit1) <- "DEATHS"
write.table (submit1, sep=",", file = "c:/kaggle/data/submit1.csv", col.names=TRUE)
