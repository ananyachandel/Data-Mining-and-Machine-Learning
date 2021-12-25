#step1 collecting the data[DATA SELECTION]

cardf<- read.csv('C:\\Users\\anany\\Downloads\\audi.csv',header = TRUE)


#Step2 preparing data and exploring data[DATA PREPROCESSING]
#checking structure of df
str(cardf)
#checking summary
summary(cardf)

#STEP2 Exploring and Data preprocessing 
#checking for NA values
any(is.na(cardf))
str(cardf)


#Step3 transforming Data[DATA TRANSFORMATION]

#changing the names of the columns
names(cardf)<-  c('model',
               'year',
               'price','transmission',
               'mileage',
               'fueltype',
               'tax',
               'milespergallon',
               'enginesize')


#calculating Age of the vehicle
cardf$age<- 2020-cardf$year

#dropping model and year
cardf<- cardf[,-c(1,2)]

#changing categorical variables as factors

cardf$transmission <- as.factor(cardf$transmission)
cardf$fueltype <- as.factor(cardf$fueltype)


#visualising price wrt predictors
library(ggplot2)
library(ggthemes)
pairs(~price + mileage + milespergallon + tax+age+enginesize, data = cardf)


#checking distribution of price
ggplot(cardf, aes(x=price)) + geom_histogram(bins = 10, color = 'red', aes(fill=..count..), alpha = 0.4) + xlab('Car Price') + ylab('Count') + ggtitle('Car Price distribution Plot')


cardf.boxplot <- boxplot(cardf$price, main = "Box plot", ylab = "car prices")

#checking outliers
#cardf.boxplot$out

#removing outliers
cardf <- subset(cardf, ! cardf$price %in% cardf.boxplot$out)
cardf.boxplot <- boxplot(cardf$price, main = "Box plot", ylab = "car prices")

#visualising price
hist(cardf$price,
     col = "yellow",
     border = "black",
     prob = TRUE,
     xlab = "Car prices",
     main = "Histogram")
lines(density(cardf$price),
      lwd = 2,
      col = "chocolate3")

cardf$price <- as.numeric(cardf$price)
cardf$mileage <- as.numeric(cardf$mileage)
cardf$tax <- as.numeric(cardf$tax)

#checking for correlation
# Grab only numeric columns
numeric_col <- sapply(cardf, is.numeric)

# Filter to numeric columns for correlation
correlation.data <- cor(cardf[,numeric_col])

correlation.data

#visualising coorelation
library(corrplot)
library(corrgram)
#corrplot(correlation.data,method='color')

corrgram(cardf,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

# Import Library
library(caTools)
# Set a random to see random results
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(cardf$price, SplitRatio = 0.70)

# Training Data
car_train = subset(cardf, sample == TRUE)

# Testing Data
car_test = subset(cardf, sample == FALSE)


#STEP 4 ###########################MODEL Building##############################################[DATA MINING]

#checking the best fit 
library(leaps)
model <- regsubsets(price ~ transmission + mileage + fueltype + tax + milespergallon + enginesize + age, data = car_train, nbest = 10)
model
plot(model, scale = "adjr2")

############MODEL1########################
car.fit1.lm <- lm(price ~ mileage+age+milespergallon+enginesize,data=car_train)
summary(car.fit1.lm)

#evaluating MSE and RMSE
#------ model 1  --------
pred1=predict(car.fit1.lm,car_test)
MSE1 = sum((pred1 - car_test$price)^2)/nrow(car_test)
print(MSE1)
var1 = sum((car_test$price - mean(car_test$price))^2)/(nrow(car_test)-1)
print(var1)
Rsqr1 = 1 - (MSE1/var1)
print(Rsqr1)
rmse <- (MSE1^0.5)
rmse
str(cardf)


#Trying to Improve the model performance by trying different model or backward selection
#training our model on training data

car.fit2.lm <- lm(price ~ .,data=car_train)


car.fit3.lm <- lm(price ~ age+milespergallon+enginesize,data=car_train)


car.fit4.lm <- lm(price ~ age+milespergallon+enginesize,data=cardf)


car.fit5.lm <- lm(log(price) ~ age+milespergallon+enginesize,data=cardf)
summary(car.fit5.lm)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(car.fit5.lm)
#evaluating MSE and RMSE
#------ model 2  --------
pred2=predict(car.fit5.lm,car_test)
MSE2 = sum((pred2 - car_test$price)^2)/nrow(car_test)
print(MSE2)
var2 = sum((car_test$price - mean(car_test$price))^2)/(nrow(car_test)-1)
print(var2)
rmse2 <- (MSE2^0.5)
rmse2
str(cardf)

############  DECISION TREE REGRESSION ################################################################################

library(rpart)
library(rpart.plot)
library(caret)

set.seed(1)


# creating another partition for Decision tree model
indx <- createDataPartition(cardf$price, p = 0.8, list = FALSE)

car_train_t <- cardf[indx,]
car_test_t <- cardf[-indx,]

# Step 3 : Training a model
# Decision tree regresser 

car.rpart <- rpart(price ~ ., data = car_train_t)
car.rpart
summary(car.rpart)

# visulazing decision tree
rpart.plot(car.rpart, digits = 3)


# Step 4 : Evaluating model performace
car.predict.rpart <- predict(car.rpart, car_test_t)
print(summary(car.predict.rpart))
print(summary(car_test_t$price))

# checking correlation 
cor(car.predict.rpart, car_test_t$price)

# cheking performace with MAE
mae <- function(actual, pred) {
  mean(abs(actual-pred))
}

mae(car_test_t$price, car.predict.rpart) # 3036.246
mae(mean(car_train_t$price), car.predict.rpart) # 6280.63


#RMSE
RMSE(car_test_t$price, car.predict.rpart)#0.2334286
RMSE(log(car_test_t$price), log(car.predict.rpart))#0.2334286



###########################




