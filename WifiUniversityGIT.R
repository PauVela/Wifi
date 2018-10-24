####install packages####
install.packages("caret")
library(caret)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("anytime")
library(anytime)
install.packages("matlab")
library(matlab)
install.packages("ggmap")
library("ggmap")
install.packages("caTools")
library("caTools")
install.packages("parallel")
library("parallel")
install.packages("foreach")
library("foreach")
####import database####
setwd("C:/Users/pauve/Documents/UBIQUM/SCANS/PRACTICA10")
library(readr)
trainingDATA<-read.csv("C:/Users/pauve/Documents/UBIQUM/SCANS/PRACTICA10/trainingData.csv")
validationDATA<-read.csv("C:/Users/pauve/Documents/UBIQUM/SCANS/PRACTICA10/validationData.csv")
validationnoWPA<-validationDATA[,521:529]
validationWPA<-validationDATA[,1:520]
trainingnoWPA<-trainingDATA[,521:529]
trainingWPA<-trainingDATA[,1:520]
str(validationnoWPA)
####pre-process####
validationDATA$PHONEID<-as.factor(validationDATA$PHONEID)
validationDATA$FLOOR<-as.factor(validationDATA$FLOOR)
validationDATA$BUILDINGID<-as.factor(validationDATA$BUILDINGID)
validationDATA$SPACEID<-as.factor(validationDATA$SPACEID)
validationDATA$RELATIVEPOSITION<-as.factor(validationDATA$RELATIVEPOSITION)
validationDATA$USERID<-as.factor(validationDATA$USERID)

trainingDATA$PHONEID<-as.factor(trainingDATA$PHONEID)
trainingDATA$FLOOR<-as.factor(trainingDATA$FLOOR)
trainingDATA$BUILDINGID<-as.factor(trainingDATA$BUILDINGID)
trainingDATA$SPACEID<-as.factor(trainingDATA$SPACEID)
trainingDATA$RELATIVEPOSITION<-as.factor(trainingDATA$RELATIVEPOSITION)
trainingDATA$USERID<-as.factor(trainingDATA$USERID)


####University area####
plot(trainingDATA$LONGITUDE,trainingDATA$LATITUDE)
##separated by building##
infmat <- subset(trainingDATA, BUILDINGID==0)
escsuperior <- subset(trainingDATA, BUILDINGID==1)
cienciaytec <- subset(trainingDATA, BUILDINGID==2)
Infomaths0 <- subset(trainingnoWPA, BUILDINGID==0)
superiorsc1<- subset(trainingnoWPA, BUILDINGID==1)
scienceandtec2 <- subset(trainingnoWPA, BUILDINGID==2)
##informatics and maths floors##
floor0_infomaths <- subset(Infomaths0, FLOOR==0)
floor1_infomaths <- subset(Infomaths0, FLOOR==1)
floor2_infomaths <- subset(Infomaths0, FLOOR==2)
floor3_infomaths <- subset(Infomaths0, FLOOR==3)
##superior school##
floor0_superiorsc <- subset(superiorsc1, FLOOR==0)
floor1_superiorsc <- subset(superiorsc1, FLOOR==1)
floor2_superiorsc <- subset(superiorsc1, FLOOR==2)
floor3_superiorsc <- subset(superiorsc1, FLOOR==3)
##sciensce and tecnology##
floor0_scienceandtec <- subset(scienceandtec2, FLOOR==0)
floor1_scienceandtec <- subset(scienceandtec2, FLOOR==1)
floor2_scienceandtec <- subset(scienceandtec2, FLOOR==2)
floor3_scienceandtec <- subset(scienceandtec2, FLOOR==3)
floor4_scienceandtec <- subset(scienceandtec2, FLOOR==4)

floor0_infomaths_reduced <- distinct(floor0_infomaths, SPACEID, LONGITUDE, LATITUDE)
build_draw <- ggplot(trainingnoWPA, aes(x=LONGITUDE)) + 
  geom_point(aes(y=LATITUDE))

build_draw
##distribution informatics and maths##
f0_infomathsMAP <- ggplot(floor0_infomaths, aes(x=LONGITUDE)) + geom_point(aes(y=LATITUDE, color=factor(SPACEID))) + ggtitle("Informatics and Maths, Floor 0") + xlab("Longitude") + ylab("Latitude")
f0_infomathsMAP
f1_infomathsMAP <- ggplot(floor1_infomaths, aes(x=LONGITUDE)) + geom_point(aes(y=LATITUDE, color=factor(SPACEID))) + ggtitle("Informatics and Maths, Floor 1") + xlab("Longitude") + ylab("Latitude")
f1_infomathsMAP
##dimension##
dim(trainingDATA)
##variance##
nearZeroVar<-nearZeroVar(trainingDATA,saveMetrics = TRUE)
head(nearZeroVar,5)
##map##
university<-c(-0.0673032,39.9926854)
map1 <- get_map(university, zoom = 17, scale = 1)
ggmap(map1)
##apply##
apply(trainingDATA[,1:520],1,which.max)
apply(trainingDATA[,1:520],1,function(x)names(which.max(x)))
any0<-apply(trainingDATA[,1:520],1,function(x)any(x==0))

zerofilter<-trainingDATA %>%
  filter(apply(trainingDATA[1:520],1,function(x)any(x==0)))

plot(zerofilter$PHONEID)##Problems with 19th PHONE
plot(zerofilter$USERID)##Problems with 6th USER

##remove useless information##

remove100T<- apply(trainingWPA, 2, function(x) length(unique(x))==1)
trainingD<-trainingDATA[,-c(which(remove100T==TRUE))]
remove100V<- apply(validationWPA, 2, function(x) length(unique(x))==1)
validationD<-validationDATA[,-c(which(remove100V==TRUE))]

in.training <- (colnames(trainingD)%in%colnames(validationD))
training.DATA<-trainingD[,-c(which(in.training==FALSE))]

in.validation<-(colnames(validationD)%in%colnames(trainingD))
validation.DATA<-validationD[,-c(which(in.validation==FALSE))]

DATATOG<-rbind(training.DATA,validation.DATA)

####remove values that are 100####
DATATOG[DATATOG==100]<- -105
DATATOG[,c(1:312)]<-DATATOG[,c(1:312)] +105
WapsOnlyDATATOG<-DATATOG[,c(1:312)]
Delzerovarrow<-apply(WapsOnlyDATATOG,1,function(x) length(unique(x))==1)
DATATOG<-DATATOG[-c(which(Delzerovarrow==TRUE)),]

####modeling####
Building1<- DATATOG%>%filter(BUILDINGID=="0")
Building1L<- DATATOG%>%filter(BUILDINGID=="0")
Building2<- DATATOG%>%filter(BUILDINGID=="1")
Building2L<- DATATOG%>%filter(BUILDINGID=="1")
Building3<- DATATOG%>%filter(BUILDINGID=="2")
Building3L<- DATATOG%>%filter(BUILDINGID=="2")
##building1##
onlyWapsB1<-Building1[,c(1:312)]
deletecolB1<-apply(onlyWapsB1,2, function(x) length(unique(x))==1)
onlyWapsB1<-onlyWapsB1[,-c(which(deletecolB1==TRUE))]
##building2##
onlyWapsB2<-Building2[,c(1:312)]
deletecolB2<-apply(onlyWapsB2,2, function(x)length(unique(x))==1)
onlyWapsB2<-onlyWapsB2[,-c(which(deletecolB2==TRUE))]
##building3##
onlyWapsB3<-Building3[,c(1:312)]
deletecolB3<-apply(onlyWapsB3,2,function(x) length(unique(x))==1)
onlyWapsB3<-onlyWapsB3[,-c(which(deletecolB3==TRUE))]
##matchs##
match1.2<-(colnames(onlyWapsB1)%in%colnames(onlyWapsB2))
onlyWapsB1<-onlyWapsB1[,-c(which(match1.2==TRUE))]

match1.3<-(colnames(onlyWapsB1)%in%colnames(onlyWapsB3))
onlyWapsB1<-onlyWapsB1[,-c(which(match1.3==TRUE))]

match2.3<-(colnames(onlyWapsB2)%in%colnames(onlyWapsB3))
onlyWapsB2<-onlyWapsB2[,-c(which(match2.3==TRUE))]
####BUILDING PREDICTION####
##KNN##
wapsused<-colnames(onlyWapsB1)
wapsused<-c(wapsused,colnames(onlyWapsB2))
wapsused<-c(wapsused,colnames(onlyWapsB3))
buildf<-paste("BUILDINGID","~", paste(wapsused, collapse = " +"))
buildf<- as.formula(buildf)

set.seed(123)
inTraining<-createDataPartition(DATATOG$BUILDINGID,p=.70,list = FALSE)
training<-DATATOG[inTraining,]
testing<-DATATOG[-inTraining,]

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE,
                           verboseIter = TRUE)
knnmodel<-train(buildf,
                          data=training,
                          method='knn',
                          trControl = fitControl,
                          preProcess = c("zv", "center", "scale"))
print(knnmodel)
buildingprediction<-predict(knnmodel,testing)
confusionMatrix(testing$BUILDINGID,buildingprediction)
####FLOOR PREDICTION####
##BUILDING1FLOORS##
Building1$TIMESTAMP<-NULL
Building1$PHONEID<-NULL
Building1$LONGITUDE<-NULL
Building1$LATITUDE<-NULL
Building1$BUILDINGID<-NULL

Building1$FLOOR<-as.numeric(Building1$FLOOR)
Building1$FLOOR<-as.factor(Building1$FLOOR)

set.seed(123)
TrainingFloor<-createDataPartition(Building1$FLOOR,p=.70, list = FALSE)
trainingF1<-Building1[TrainingFloor,]
testF1<-Building1[-TrainingFloor,]

clusterF1 <- makeCluster(detectCores()-1)
registerDoSNOW(clusterF1)

fitControlF1 <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)
##svm##
svm.floor1<-train(FLOOR ~.,
                  data=trainingF1,
                  method='svmRadial',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))
print(svm.floor1)

prediction.f1.svm<-predict(svm.floor1, testF1)
confusion_f1<-confusionMatrix(testF1$FLOOR,prediction.f1.svm)
confusion_f1
##knn##
knn.floor1<-train(FLOOR ~.,
                  data=trainingF1,
                  method='knn',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))
                  
                  

prediction.f1.knn<-predict(knn.floor1, testF1)
confusion_f1knn<-confusionMatrix(testF1$FLOOR,prediction.f1.knn)
confusion_f1knn

##BUILDING2FLOORS##
Building2$TIMESTAMP<-NULL
Building2$PHONEID<-NULL
Building2$LONGITUDE<-NULL
Building2$LATITUDE<-NULL
Building2$BUILDINGID<-NULL

Building2$FLOOR<-as.numeric(Building2$FLOOR)
Building2$FLOOR<-as.factor(Building2$FLOOR)

set.seed(123)
TrainingFloor2<-createDataPartition(Building2$FLOOR,p=.70, list = FALSE)
trainingF2<-Building2[TrainingFloor2,]
testF2<-Building2[-TrainingFloor2,]

clusterF2 <- makeCluster(detectCores()-1)
registerDoSNOW(clusterF2)

fitControlF2 <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)
##svm##
svm.floor2<-train(FLOOR ~.,
                  data=trainingF2,
                  method='svmRadial',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))
print(svm.floor2)

prediction.f2.svm<-predict(svm.floor2, testF2)
confusion_f2<-confusionMatrix(testF2$FLOOR,prediction.f2.svm)
confusion_f2
##BUILDING3FLOORS##
Building3$TIMESTAMP<-NULL
Building3$PHONEID<-NULL
Building3$LONGITUDE<-NULL
Building3$LATITUDE<-NULL
Building3$BUILDINGID<-NULL

Building3$FLOOR<-as.numeric(Building3$FLOOR)
Building3$FLOOR<-as.factor(Building3$FLOOR)

set.seed(123)
TrainingFloor3<-createDataPartition(Building3$FLOOR,p=.70, list = FALSE)
trainingF3<-Building3[TrainingFloor,]
testF3<-Building3[-TrainingFloor,]

#clusterF1 <- makeCluster(detectCores()-1)
#registerDoSNOW(clusterF1)

fitControlF3 <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)
##svm##
svm.floor3<-train(FLOOR ~.,
                  data=trainingF3,
                  method='svmRadial',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))
print(svm.floor3)

prediction.f3.svm<-predict(svm.floor3, testF3)
confusion_f3<-confusionMatrix(testF3$FLOOR,prediction.f3.svm)
confusion_f3
##knn##
knn.floor3<-train(FLOOR ~.,
                  data=trainingF3,
                  method='knn',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))
print(knn.floor3)

prediction.f3.knn<-predict(knn.floor3, testF3)
confusion_f3knn<-confusionMatrix(testF3$FLOOR,prediction.f3.knn)
confusion_f3knn

####Latitude###
##building1##
set.seed(123)
Building1L
inTrainingLat1<-createDataPartition(Building1L$LATITUDE,
                                    p=.70,
                                    list = FALSE)
trainingRFB1Lat<-Building1L[inTrainingLat1,]
testingRFB1Lat<-Building1L[-inTrainingLat1,]

fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

RFB1Lat<- train(LATITUDE ~ ., 
                data = trainingRFB1Lat,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))

predictionRFB1lat<-predict(RFB1Lat, testingRFB1Lat)
errorRFB1lat<-postResample(predictionRFB1lat, testingRFB1Lat$LATITUDE)
errorRFB1lat
predictionRFB1lat
##Building2##
set.seed(123)
Building2L
inTrainingLat2<-createDataPartition(Building2L$LATITUDE,
                                    p=.70,
                                    list = FALSE)
trainingRFB2Lat<-Building2L[inTrainingLat2,]
testingRFB2Lat<-Building2L[-inTrainingLat2,]

fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

RFB2Lat<- train(LATITUDE ~ ., 
                data = trainingRFB2Lat,
                method = "ranger",
                trControl = fitControl,
                preProcess = c("zv", "medianImpute"))

predictionRFB2lat<-predict(RFB2Lat, testingRFB2Lat)
errorRFB2lat<-postResample(predictionRFB2lat, testingRFB2Lat$LATITUDE)
errorRFB2lat
predictionRFB2lat
##Building3##
set.seed(123)
Building3L
inTrainingLat3<-createDataPartition(Building3L$LATITUDE,
                                    p=.70,
                                    list = FALSE)
trainingRFB3Lat<-Building3L[inTrainingLat3,]
testingRFB3Lat<-Building3L[-inTrainingLat3,]

fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

RFB3Lat<- train(LATITUDE ~ ., 
                data = trainingRFB3Lat,
                method = "ranger",
                trControl = fitControl,
                preProcess = c("zv", "medianImpute"))

predictionRFB3lat<-predict(RFB3Lat, testingRFB3Lat)
errorRFB3lat<-postResample(predictionRFB3lat, testingRFB3Lat$LATITUDE)
errorRFB3lat
predictionRFB3lat

####Longitude####
##Building1##
set.seed(123)
inTrainingLon1<-createDataPartition(Building1L$LONGITUDE,
                                p=.70,
                                list = FALSE)

trainingRFLon1<-Building1L[inTrainingLon1,]
testingRFLon1<-Building1L[-inTrainingLon1,]


fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rfB1Lon<- train(LONGITUDE ~ ., 
                 data = trainingRFLon1,
                 method = "ranger",
                 trControl = fitControl, 
                 preProcess = c("zv", "medianImpute"))


predictionRFB1lon<-predict(rfB1Lon, testingRFLon1)
errorRFB1lon<-postResample(predictionRFB1lon, testingRFLon1$LONGITUDE) 
errorRFB1lon
##building2##
set.seed(123)
inTrainingLon2<-createDataPartition(Building2L$LONGITUDE,
                                    p=.70,
                                    list = FALSE)

trainingRFLon2<-Building2L[inTrainingLon2,]
testingRFLon2<-Building2L[-inTrainingLon2,]


fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rfB2Lon<- train(LONGITUDE ~ ., 
                data = trainingRFLon2,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))


predictionRFB2lon<-predict(rfB2Lon, testingRFLon2)
errorRFB2lon<-postResample(predictionRFB2lon, testingRFLon2$LONGITUDE) 
errorRFB2lon

##building3##
set.seed(123)
inTrainingLon3<-createDataPartition(Building3L$LONGITUDE,
                                    p=.70,
                                    list = FALSE)

trainingRFLon3<-Building3L[inTrainingLon3,]
testingRFLon3<-Building3L[-inTrainingLon3,]


fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rfB3Lon<- train(LONGITUDE ~ ., 
                data = trainingRFLon1,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))


predictionRFB3lon<-predict(rfB3Lon, testingRFLon3)
errorRFB3lon<-postResample(predictionRFB3lon, testingRFLon3$LONGITUDE) 
errorRFB3lon