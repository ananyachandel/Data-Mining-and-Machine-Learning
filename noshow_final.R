#step1 collecting the data[DATA SELECTION]

noshow_data<- read.csv('noshow.csv',header = TRUE,stringsAsFactors = FALSE)

#Step2 preparing data and exploring data[DATA PREPROCESSING]

#checking summary and structure
summary(noshow_data)
str(noshow_data)

#removing the negative age 
noshow_data <-noshow_data[!(noshow_data$Age<0),]

#checking for null values as it can be seen there are no null values
#library(naniar)
#vis_miss(noshow_data,warn_large_data=FALSE)
library(Amelia)
missmap(noshow_data)
print(any(is.na(noshow_data)))
str(noshow_data)


#Step3 transforming Data[DATA TRANSFORMATION]

noshow_data$Gender <- factor(noshow_data$Gender)
noshow_data$ScheduledDay <- as.Date(noshow_data$ScheduledDay)
noshow_data$AppointmentDay <- as.Date(noshow_data$AppointmentDay)
noshow_data$Neighbourhood <- factor(noshow_data$Neighbourhood)
noshow_data$Scholarship <- factor(noshow_data$Scholarship)
noshow_data$Hipertension <- factor(noshow_data$Hipertension)
noshow_data$Diabetes <- factor(noshow_data$Diabetes)
noshow_data$Alcoholism <- factor(noshow_data$Alcoholism)
noshow_data$Handcap  <- factor(noshow_data$Handcap )
noshow_data$SMS_received <- factor(noshow_data$SMS_received)
noshow_data$No.show <- factor(noshow_data$No.show)

#calculating few new columns that may be useful 
noshow_data$dayofweek <- weekdays(noshow_data$AppointmentDay)
table(noshow_data$dayofweek)
noshow_data$dayofweek <-factor(noshow_data$dayofweek)
noshow_data$waitingtime <- noshow_data$AppointmentDay - noshow_data$ScheduledDay
noshow_data$waitingtime <- as.numeric(noshow_data$waitingtime)


str(noshow_data)

#waiting time less than 0 is of no significance so filtering data
library(dplyr)
noshow_data <- noshow_data %>% filter(noshow_data$waitingtime > 0)


#removing patient ID Appointment ID, ScheduledDay and AppointmentDay
noshow_data <- noshow_data[,-c(1,2,4,5)]
str(noshow_data)

#changing the names of the columns
names(noshow_data)<-  c('Gender',
               'Age',
               'Neighbourhood',
               'Scholarship',
               'Hypertension',
               'Diabetes',
               'Alcoholism',
               'Handicap',
               'Sms_received',
               'No_show','Dayofweek','Waitingtime')
head(noshow_data)


#visualising data on keeping noshow as target predicting variable[Data Visualisation]
library(ggplot2)
library(ggthemes)
#visual 1
ggplot(noshow_data, aes(x=Age, fill=No_show, color=No_show)) + geom_histogram(binwidth = 1) + labs(title="No shows")+ theme_bw()

ggplot(noshow_data, aes(x=Waitingtime, fill=No_show, color=No_show)) +geom_histogram(binwidth = 1) + labs(title="No shows")+theme_bw()
  

ggplot(noshow_data, aes(x = Gender, fill = factor(No_show))) + geom_bar(stat='count',position='dodge') +labs(x = 'Sex') + theme_few()


#split the data in train test
#library(caTools)
#set.seed(123)
#sample<-sample.split(noshow_data$No_show, SplitRatio = 0.75)
#noshow_train<-subset(noshow_data, split == TRUE)
#noshow_test <-subset(noshow_data, split == FALSE)



#Splitting the data into train and test using stratified random sampling 
indx <-createDataPartition(y=noshow_data$No_show,p=0.75,list = FALSE)
noshow_data_train <- noshow_data[indx, ]
noshow_data_test <- noshow_data[-indx, ]
table(noshow_data_train$No_show)
prop.table(table(noshow_data$No_show))



#for comparing output of train and test we create separate variables

x=noshow_data_train[,-10]
y=noshow_data_train$No_show

#STEP 4 ###########################MODEL Building##############################################[DATA MINING]

#Creating logistic model
log_model1 <- glm(No_show ~ . ,family=binomial(link='logit'),data=noshow_data_train)
log_model2 <- glm(No_show ~ Age+Scholarship+Hypertension+Alcoholism+Diabetes+Sms_received+Waitingtime,family=binomial(link='logit'),data=noshow_data_train)

#K-Fold Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

log_model3 <- train(No_show ~ Age+Scholarship+Hypertension+Alcoholism+Diabetes+Sms_received+Waitingtime,  data=noshow_data_train, method="glm", family="binomial",
                trControl = ctrl, tuneLength = 5)

#prediction and evaluation  for logistic model
pred1 = predict(log_model3, newdata=noshow_data_test)
#confusion matrix
confusionMatrix(data=pred1, noshow_data_test$No_show)
library(gmodels)
#crosstable
CrossTable(pred1,noshow_data_test$No_show)
#F score
result1 <- confusionMatrix(pred1,noshow_data_test$No_show,mode="prec_recall")
result1$byClass["Precision"]
result1$byClass["Recall"]
result1$byClass["F1"]


##########################################

#Applying Naive Bias model
library(e1071)

naive_model1 <- naiveBayes(x,y)
summary(naive_model1)
pred2 <- predict(naive_model1,noshow_data_test)
confusionMatrix(pred2,noshow_data_test$No_show)

#Optimising
naive_model2=naiveBayes(x,y,laplace = 1)

#Model Evaluation
#Predict testing set
pred3 <- predict(naive_model2,newdata = noshow_data_test ) 

#Get the confusion matrix to see accuracy value and other parameter 
confusionMatrix(pred3, noshow_data_test$No_show )
CrossTable(pred3, noshow_data_test$No_show )

#F score
result2 <- confusionMatrix(pred3,noshow_data_test$No_show,mode="prec_recall")
result2$byClass["Precision"]
result2$byClass["Recall"]
result2$byClass["F1"]
##################################################################################################
#building Random forest model

library(randomForest)
#removing Neighbourhood too many variables
noshow_data_train_rf <- x[,-3]
rf_model1 <- randomForest(noshow_data_train_rf,noshow_data_train$No_show,ntree=500,mtry=sqrt(10))
pred4 <- predict(rf_model1,newdata=noshow_data_test)
confusionMatrix(pred4,noshow_data_test$No_show)

CrossTable(pred4,noshow_data_test$No_show)

#F score
result3 <- confusionMatrix(pred4,noshow_data_test$No_show,mode="prec_recall")
result3$byClass["Precision"]
result3$byClass["Recall"]
result3$byClass["F1"]


#################################################################################################

