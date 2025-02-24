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
#task2
install.packages(c("randomForest","caret"))#installing packages
library(randomForest)
library(caret)
Data <- read.csv("C:/Users/201807729/Documents/Practical 2/Data/Task 2/sample_data.csv")
View(Data)
str(Data)
ind <- sample (2, nrow (Data), replace = TRUE , prob = c (0.7, 0.3))
training_data = Data[ind==1,]
testing_data = Data[ind==2,]
rf = randomForest (Salinity~., data=training_data)
plot(rf)
Salinity_Predict = predict(rf, testing_data)#predicting the values
testing_data$Salinity_Predict = Salinity_Predict
View(testing_data)
cfm = table(testing_data$Salinity, testing_data$Salinity_Predict)
cfm
classification_Accuracy = sum(diag(cfm)/sum(cfm))
classification_Accuracy
#part2
install.packages(c("stars","raster","ggplot2","sf","ExtractTrainData"))#instaliing packages
library(stars)
library(raster)
library(ggplot2)
library(sf)
library(ExtractTrainData)
image_BBS<-stack ("C:/Users/201807729/Documents/Practical 2/Data/Task 2/RGB.tif")#reading the image
image_BBS
plotRGB (image_BBS,3,2,1, scale =255, stretch='lin')
i<-0
for (i in seq (1,3,1)){
  fname = paste ("layer_",i,".tif",sep="")
  layers <- image_BBS [[i]]
  writeRaster( layers, fname,
               format = "GTiff", datatype='FLT4', overwrite =TRUE)
}
Layer_1<-raster("layer_1.tif")
Layer_2<-raster("layer_2.tif")
Layer_3<-raster("layer_3.tif")
data1<-stack(Layer_1,Layer_2,Layer_3)
data1
plotRGB (image_BBS,3,2,1, scale =255, stretch='lin')
features<-st_read("C:/Users/201807729/Documents/Practical 2/Data/Task 2/Shapefile/Super_training data.shp" )
features
plot(features)
str(features)
