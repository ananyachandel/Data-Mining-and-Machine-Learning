
#step 1 reading data
library(readr)
library(ggplot2)
bike_data <- read_csv("london_merged (1).csv")


#step2 preparing and exploring data
str(bike_data)
summary(bike_data)


#checking null values
print(any(is.na(bike_data)))
library(Amelia)
missmap(bike_data)
print(any(is.na(bike_data)))
str(bike_data)


#STEP3 Data transformation
#changing order of the columns to get Count first

bike_data<- bike_data[,c(2,3,4,5,1,6,10,7,8,9)]
str(bike_data)


#changing names as per convenience 
names(bike_data) <- c("Count",
                  "Temperature",
                  "FeelTemp",
                  "Humidity",
                  "timestamp",
                  "Windspeed",
                  'Season',
                  'Weather',
                  'Holiday',
                  'Workday')
head(bike_data)


#visualisations
library(psych)
#pairs.panels(bike_data)


ggplot(bike_data, aes(x=Count, fill=Season, color=Count)) +
  geom_histogram(binwidth = 1) + labs(title="Count")+ theme_bw()

ggplot(bike_data, aes(x=Count, fill=Weather, color=Count)) +
  geom_histogram(binwidth = 1) + labs(title="Count")+ theme_bw()

ggplot(bike_data, aes(x=Count, fill=Holiday)) +
  geom_histogram(binwidth = 1) + labs(title="Count")+ theme_bw()



library(lubridate)
#Breaking timestamp to days month and year
bike_data$date <- as.character(bike_data$timestamp, format = "%e/%d/%y")
bike_data$Year <- as.factor(as.character(bike_data$timestamp, format = "%Y"))
bike_data$Month <- as.character(bike_data$timestamp, format = "%m")
bike_data$Day <- as.character(bike_data$timestamp, format = "%u")
bike_data$Hour <- as.factor(hour(bike_data$timestamp))

#removing processed columns timestamp and date
bike_data<-bike_data[,-c(5,11)]
str(bike_data)

#defining factors for month, day, season,workday,holiday
bike_data$Month <- factor(bike_data$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"),
  labels = c('January','February', 'March', 'April','May', 'June', 'July', 'August','September','October', 'November', 'December'),
  ordered = TRUE)

bike_data$Day <- factor(bike_data$Day,levels = c(1,2,3,4,5,6,7),labels = c('Monday','Tuesday','Wednesday','Thursday','Friday', 'Saturday','Sunday'),ordered = TRUE)

bike_data$Season <- factor(bike_data$Season, levels = c(0,1,2,3),labels = c('Spring', 'Summer', 'Fall','Winter'),ordered = TRUE)

bike_data$Workday <- factor(bike_data$Workday,levels = c(0,1),labels = c('Workday', 'Weekend'))

bike_data$Holiday <- factor(bike_data$Holiday,levels = c(0,1),labels = c('Not a Holiday', 'Holiday'))

table(bike_data$Weather)
library(dplyr)
bike_data <- bike_data %>% filter(!(Weather %in% c(10,26)))
bike_data$Weather <- factor(bike_data$Weather,levels = c(1,2,3,4,7),labels = c('Clear', 'Scattered Clouds', 'Broken Clouds', 'Cloudy','Light Rain'))

# --- Rentals & Temperature by Season
options(repr.plot.width=12, repr.plot.height=8)
ggplot(bike_data, aes(Temperature, Count)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "red") +
  labs(y="Count of Rentals", title = "Rentals & Temperature by Season") +
  facet_grid(Season~.) + theme_linedraw(base_size = 16)

ggplot(bike_data, aes(Temperature, Count)) + 
  geom_jitter(width = 0.25, alpha = 0.9, color = "green") +
  labs(y="Count of Rentals", title = "Rentals & Temperature by Weather") + 
  facet_grid(~Weather) + theme_linedraw(base_size = 16)

###   -----  Rentals by Month & workday ---- #####
wd = c("cadetblue", "cadetblue1")
bike_data %>% group_by(Month, Workday) %>%
  summarise(n = n(), rent = sum(Count)) %>%
  ggplot(aes(Month, rent, fill = Workday)) + 
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_manual(values = wd) +
  facet_grid(~Workday) + theme_bw(base_size = 16) + 
  labs(title = "Rentals by Month & Workday", x = "", y = "Total Rentals") +
  scale_y_continuous(labels = scales::label_comma())

#checking for correlation
# Grab only numeric columns
numeric_col <- sapply(bike_data, is.numeric)
# Filter to numeric columns for correlation
correlation.data <- cor(bike_data[,numeric_col])
correlation.data

#visualising coorelation
library(corrplot)
library(corrgram)
corrplot(correlation.data,method='color')
#corrgram(bike_data,order=TRUE, lower.panel=panel.shade,
#         upper.panel=panel.pie, text.panel=panel.txt)
head(bike_data)

p4<- ggplot(bike_data, aes(x=Count, fill=Year, color=Count)) +
  geom_histogram(binwidth = 1) + labs(title="Count")+ theme_bw()
ggplot(bike_data, aes(x=Year, y=Count, fill=Year)) + geom_boxplot() +
  ggtitle("Average Bike Rentals Over Years") + ylab("Number of bikes rented") + xlab("Year")

#removing correlated data and data fro 2017 year as its limited
bike_data<-bike_data[,-c(3,12)]
bike_data <- bike_data %>% filter(Year != "2017")



names(bike_data)

#splitting the data
library(caTools)
set.seed(123)
split = sample.split(bike_data$Count, SplitRatio = 0.75)
bike_train = subset(bike_data, split == TRUE)
bike_test = subset(bike_data, split == FALSE)


str(bike_train)

library(rpart.plot)
library(e1071)
library(leaps)
library(rpart)
library(randomForest)
library(Metrics)
library(MLmetrics)

#Random Forest Regression 

#### MODEL0 Decision tree regressor ##################
set.seed(1234)
bike_m0 = rpart(formula = Count ~ .,
           data = bike_train,
           method = "anova")

rpart.plot(bike_m0, main = "Model 0");
summary(bike_m0)

###### MODEL0 Evaluation ###########################

pred0 = predict(bike_m0,bike_test)

MSE0 = MSE(bike_test$Count, pred0)
MAE0 = MAE(bike_test$Count, pred0)
RMSE0 = RMSE(bike_test$Count, pred0)
r20 = R2(bike_test$Count, pred0, form = "traditional")

cat(" MAE:", MAE0, "\n", "MSE:", MSE0, "\n", 
    "RMSE:", RMSE0, "\n", "R-squared:", r20)


##################### MODEL 1 Random forest regressor##################


bike_m1 <- randomForest(Count ~ ., data = bike_train, mtry = 3, 
                         importance = TRUE, na.action = na.omit)
print(bike_m1)

#################### MODEL 1 EVALUATION #############################
pred1 = predict(bike_m1,bike_test)

MSE1 = MSE(bike_test$Count, pred1)
MAE1 = MAE(bike_test$Count, pred1)
RMSE1 = RMSE(bike_test$Count, pred1)
r21 = R2(bike_test$Count, pred1, form = "traditional")

cat(" MAE:", MAE1, "\n", "MSE:", MSE1, "\n", 
    "RMSE:", RMSE1, "\n", "R-squared:", r21)

options(repr.plot.width=12, repr.plot.height=6)
vip::vip(bike_m1) + theme_minimal(base_size = 18) +
  labs(title = "Variable importance - Rentals")

################## MODEL 2 SVR #######################################

#SVR support vector regression

bike_m2 = svm(formula = Count ~ .,
         data = bike_train,
         type = 'eps-regression',
         kernel = 'linear')
summary(bike_m2)

#################### MODEL 2 EVALUATION #############################
pred2 = predict(bike_m2,bike_test)

MSE2 = MSE(bike_test$Count, pred2)
MAE2 = MAE(bike_test$Count, pred2)
RMSE2 = RMSE(bike_test$Count, pred2)
r22 = R2(bike_test$Count, pred2, form = "traditional")

cat(" MAE:", MAE2, "\n", "MSE:", MSE2, "\n", 
    "RMSE:", RMSE2, "\n", "R-squared:", r22)

################# MODEL 3 SVR TUNING ##################################
#making sample for tuning
SMPL <- dplyr::sample_frac(bike_data, size = 0.3,replace = F)

#performing tuning to improve model
tune_bike <- tune(svm, Count ~ ., data = SMPL, 
           ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
           tunecontrol = tune.control(sampling = "fix"))
print(tune_bike)

#applying tuned parameters
bike_m3 <- svm(formula = Count ~ .,
          data = bike_train,
          type = 'eps-regression',
          kernel = 'radial',
          gamma = 0.5,
          cost = 4)

#################### MODEL 3 EVALUATION #############################
pred3 = predict(bike_m3,bike_test)

MSE3 = MSE(bike_test$Count, pred3)
MAE3 = MAE(bike_test$Count, pred3)
RMSE3 = RMSE(bike_test$Count, pred3)
r23 = R2(bike_test$Count, pred3, form = "traditional")

cat(" MAE:", MAE3, "\n", "MSE:", MSE3, "\n", 
    "RMSE:", RMSE3, "\n", "R-squared:", r23)


