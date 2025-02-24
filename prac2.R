install.packages(c("randomForest","caret"))#installing packages
library(randomForest)
library(caret)
getwd()#checking the working directory
data <-read.csv("C:/Users/201807729/Documents/Practical 2/Data/Task 1/CTG.csv",header = TRUE)
str(data)#checking the structure of data
data$NSP <- as.factor(data$NSP)
table(data$NSP)#CHECKING THE DISTRIBUTION OF THE CLASSES
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))

is()
train <- read.csv("C:/Users/201807729/Documents/Practical 2/Data/Task 1/CTG.csv")
test <- read.csv("c:/Users/201807729/Documents/practical 2/Data/Task 1/CTG.CSV")
install.packages("randomForest")
library(randomForest)
train <- data[ind==1,]
test <- data[ind==2,]
set.seed(222)
rf <- randomForest(NSP~., data=train,
                   ntree = 300,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)
attributes(rf)
p1<-predict(rf,train)#predicting the values
install.packages("caret")
library(caret)
confusionMatrix(p1,train$NSP)#confusion matrix
p2<-predict(rf,test)#predicting the values
confusionMatrix(p2,test$NSP)
plot(rf)
t <- tuneRF(train[,-22], train[,22],#tuning the random forest
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")#plotting the histogram
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)#importance of the variables
varUsed(rf)
partialPlot(rf, train, MSTV, "2")#partial plot
getTree(rf, 1, labelVar = TRUE)
MDSplot(rf, train$NSP)
