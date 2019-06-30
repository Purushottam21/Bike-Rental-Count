# Let's clean the R environment first

rm(list = ls())

# Next, set the working directory

setwd("C:/Users/Purushottam/Desktop/Data Science")

# Time to check the directory

getwd()

# Importing csv file into R environment

day = read.csv("day.csv")

### Our objective of this project is - Predication of bike rental count on daily based on the environmental and seasonal settings

### The flow we are gong to work is - 1) Exploratory Data Analysis, 2) Data pre-Processing, 3) Model Development
### 4) Model Evaluation, 5) Result - choosing the best model with lowest MAPE and R-square > 80%

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Let's perform EXPLORATORY DATA ANALYSIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

class(day) # Its DataFrame
head(day) # Let's have a look on first 6 observations
dim(day) # 731 observations & 16 variables
str(day) # Have a look on the structure
summary(day) # Data seems to be normalized 
names(day) # In names, we can see shortcuts like, hum for humidity

# It's time for us to remove variables (if any), which don't add any meaning 
# In our data, instant can be dropped as it simply represents the index
# Next, dteday can also be neglected as we are not dealing with time series analysis
# Finally, let's delete casual and registered variables, as our dependent variable is the total of the former variables

day = subset(day, select = -c(instant, dteday, casual, registered))

# Let's check our data after the dimension reduction

dim(day) # We are left with 731 observations and 12 variables  

names(day) # The above mentioned four variables are removed

# If we observe the names, they are given with shortcut words. Let's change it for our convenience

names(day)[2] = 'year'
names(day)[3] = 'month'
names(day)[7] = 'weather'
names(day)[8] = 'temperature'
names(day)[10] = 'humidity'
names(day)[12] = 'count'

# We have changed the names and let's have a look on this

names(day)

# It's time to divide our variables into numeric(num_var) and categorical(cat_var)

num_var = c('temperature', 'atemp', 'humidity', 'windspeed', 'count')

cat_var = c('season', 'year', 'month', 'holiday', 'weekday', 'workingday', 'weather')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~It's time for DATA PRE PROCESSING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~We will start with Missing Value Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sum(is.na(day)) # Line of code to know the sum of missing values in dataset

#-->> We got the Missing Values in the whole dataset as 0. No need to perform any missing values operations

# Next, we are going to check for OUTLIERS If outlierts are present we are going to impute them with KNN / Mean / Medain, 
# the one which gives the least value

day2=day
day=day2

# Boxplot Technique is used to check for the Outliers

for(i in 1:length(num_var)){
  assign(paste0("AB",i),ggplot(aes_string(x="count",y=(num_var[i])),d=subset(day))+
           geom_boxplot(outlier.color = "Red",outlier.shape = 18,outlier.size = 2,
                        fill="Purple")+theme_get()+
           stat_boxplot(geom = "errorbar",width=0.5)+
           labs(x="Bike Count", y=num_var[i])+
           ggtitle("Boxplot of bike count", num_var[i]))
}

gridExtra::grid.arrange(AB1,AB2,AB3,ncol=3)
gridExtra::grid.arrange(AB4,AB5,ncol=2)

#-->> We can observe outliers in windspeed and humidity variables.


# Let's go ahead by replacing outliers with NA

for(i in num_var){
  print(i)
  outlier = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
  print(length(outlier))
  day[,i][day[,i] %in% outlier] = NA
}

sum(is.na(day))

# We have total 15 outliers. Now, we are going to use Mean / Median / KNN to impute the values. We are using location day[1,11]

# Actual value is 0.160
# Mean = 0.186, after imputing
# Median = 0.179, after imputing
# KNN = 0.160, after imputing

library(DMwR)
library(rpart)

day = knnImputation(day, k = 5)
sum(is.na(day))

#-->> We found out KNN is giving the nearest value as 0.16 when compared with the location day[1,11], which is 0.16, so, KNN is freezed
#-->> All the outliers are imputed with KNN values.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Let's go for DATA UNDERSTANDING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time to plot some graphs, so let's install few libraries

library(ggplot2)
library(scales)
library(psych)
library(gplots)


# Barplot with x axis as season and y axis as count

ggplot(day, aes(x = day$season, y = day$count))+
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "Number of bikes rented with respect to season", x = "Seasons", y = "Count")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))

#-->> We can observe, season 3, has the highest count and on season 1, we got low count.


# Barplot with x axis as year and y axis as count

ggplot(day, aes(x = day$year, y = day$count))+
  geom_bar(stat = "identity", fill = "red")+
  labs(title = "Number of bikes rented with respect to year", x = "Years", y = "Count")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))

#-->> We can observe, Year 1, has the highest count and on year 0, we got low count.


# Barplot with x axis as weekday and y axis as count

ggplot(day, aes(x = day$weekday, y = day$count))+
  geom_bar(stat = "identity", fill = "navyblue")+
  labs(title = "Number of bikes rented with respect to days", x = "Days of the week", y = "count")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))

#-->> We can observe, on day 5, we have the highest count and on day 0, we got low count.


#Count with respect to temperature and humidity together

ggplot(day,aes(temperature,count)) + 
  geom_point(aes(color=humidity),alpha=0.5) +
  labs(title = "Bikes rented with respect to temperature and humidity", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

#-->> We can observe, when normalized temperature is between 0.5 to 0.75 and humidity is between 0.50 to 0.75, count is high.


# Count with respect to windspeed and weather together

ggplot(day, aes(x = windspeed, y = count))+
  geom_point(aes(color=weather), alpha=0.5) +
  labs(title = "Bikes rented with respect to windspeed and weather", x = "Windspeed", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

#-->> From the plot, we can say,  count is at peak, when windspeed is from 0.1 to 0.3 and weather is from 1.0 to 1.5.


# Count with respect to temperature and season together

ggplot(day, aes(x = temperature, y = count))+
  geom_point(aes(color=season),alpha=0.5) +
  labs(title = "Bikes rented with respect to temperature and season", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

#-->> From the plot, count is maximum when temperature is 0.50 to 0.75 & season 3 to season 4.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Now, let's move to FEATURE SELECTION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

day2=day
day=day2

# In Feature Selection, we perform Correlation Analysis and Anova test to find out the varaibles which are to be excluded before feeding to the model

# Correlation Analysis is performed between num_var (numeirc independent variables) & count(continuous target variable)

library(corrgram)

corrgram(day[,num_var],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis between numeric variables")

#-->> From the analysis diagram, we can understand that, temperature and atemp are highly correlated with each other.


# Anova Test is performed between cat_var (categorical independent variables) & count(continuous target variable) 

for(i in cat_var){
  print(i)
  Anova_test_result = summary(aov(formula = count~day[,i],day))
  print(Anova_test_result)
}

#-->> From the result, we can observe, holiday, weekday and workingday has p value > 0.05, by which, we accept null hypothesis.


# Deleting the below given continuous and categorical variables from day, as we found out they won't add any value to the model.

day = subset(day, select=-c(atemp,holiday,weekday,workingday))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Let's jump to FEATURE SCALING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

day2=day
day=day2

# Now, what are the numeric and categorical variables left with us

num_var = c("temperature","humidity","windspeed","count")

cat_var = c("season", "year", "month", "weather")


# Skewness test for continuous variables to know the symmetry

library(propagate)

for(i in num_var){
  print(i)
  skew = skewness(day[,i])
  print(skew)
}

#-->> After observing from console, we can conclude, the dataset is approximately symmetric as values are between -0.5 to +0.5.


# We shall go for the summary of dataset where we can see the min and max of continous variables  

for(i in num_var){
  print(summary(day[,i]))
}

#-->> From summary, the data seems to be normalized, so there's no need for scaling.


# We shall check for normality as Feature Scaling is all about normality

hist(day$temperature, col="Navyblue", xlab="Temperature", ylab="Frequency",
     main="Temperature Distribution")

hist(day$humidity, col="Blue", xlab="Humidity", ylab="Frequency",
     main="Humidity Distribution")

hist(day$windspeed,col="Dark green",xlab="Windspeed",ylab="Frequency",
     main="Windspeed Distribution")

#-->> As said above, the numeirc variables distribution is approximately symmetric


# Let's save the Pre-processed data for our further reference

write.csv(data,"Bike Rental Data (Pre Processed).csv",row.names=FALSE)


# Till now, we completed Exploratory Data Analysis, plotted some visualizations, worked on Data Pre Processing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Now, We can go for MODEL DEVELOPMENT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Before going there, let's define two functions which are called as Error Metrics.

# As we have regression problem, let's use MAPE and R square as our metrics to evaluate the model

# Let's clean R Environment, as it uses RAM which is limited

library(DataCombine)
rmExcept("day")

# Let's create copy of data for further reference

day2 = day
day = day2

# Defining R Square function - Correlation of original and predicted values

Rsquare = function(y,y1){
  cor(y,y1)^2
}

# Defining MAPE function - Mean Absolute Percentage Error (Calculated Errors)

MAPE = function(y,y1){
  mean(abs((y-y1)/y))*100
}

#-->> Error metrics to evaluate the model are defined


# Time to create dummies

cat_var = c("season","year","month","weather")

library(dummies)

day = dummy.data.frame(day, cat_var) # Why? because in regression analysis, it treats all independent variables as numerical


# Now, as we know, we have to divide the data into train and test. So, let's go for that.

set.seed(123)
train_index = sample(1:nrow(day),0.8*nrow(day)) # Using Simple Random Sampling Technique
train= day[train_index,]
test= day[-train_index,]

#-->> We can see our train and test data are available in the environment


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~We are going for LINEAR REGRESSION MODEL~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# We have to check for multicollinearity

num_var = c("temperature","humidity","windspeed")

num_var2 = day[,num_var]

library(usdm)

vifcor(num_var2, th = 0.7)

#-->> No variable from the 4 input variables has collinearity problem.

# Code for development of model

LRModel = lm(count~., train)

summary(LRModel)

# Predicting model on train data

LRTrain = predict(LRModel, train[-25])

# Predicting model on test data

LRTest = predict(LRModel, test[-25])

# Calculating MAPE for Train Data

LRMape_Train = MAPE(train[,25], LRTrain) 

# Calculating MAPE for Test Data

LRMape_Test = MAPE(test[,25], LRTest)

# Calculating Rsquare for Train Data

LRR2_Train = Rsquare(train[,25], LRTrain)

# Calculating Rsquare for Test Data

LRR2_Test = Rsquare(test[,25], LRTest)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~It's time for DECISION TREE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Code for development of model

library(rpart)

DTModel = rpart(count~., train, method = "anova")

# Predicting model on train data

DTTrain = predict(DTModel, train[-25])

# Predicting model on test data

DTTest = predict(DTModel, test[-25])

# Calculating MAPE for Train Data

DTMape_Train = MAPE(train[,25], DTTrain)

# Calculating MAPE for Test Data

DTMape_Test = MAPE(test[,25], DTTest)

# Calculating Rsquare for Train Data

DTR2_Train = Rsquare(train[,25], DTTrain)

# Calculating Rsquare for Test Data

DTR2_Test = Rsquare(test[,25], DTTest)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Here we go for RANDOM FOREST~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Code for development of model

library(randomForest)

RFModel = randomForest(count~., train, ntree = 500, method = "anova")

# Predicting model on train data

RFTrain = predict(RFModel, train[-25])

# Predicting model on test data

RFTest = predict(RFModel, test[-25])

# Calculating MAPE for Train Data

RFMape_Train = MAPE(train[,25], RFTrain)

# Calculating MAPE for Test Data

RFMape_Test = MAPE(test[,25], RFTest)

# Calculating Rsquare for Train Data

RFR2_Train = Rsquare(train[,25], RFTrain)

# Calculating Rsquare for Test Data

RFR2_Test = Rsquare(test[,25], RFTest)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Finally, it's Result Time~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Result = data.frame("Model" = c("Linear Regression", "Decision Tree", "Random Forest"),
                    "MAPE_Values_Train" = c(LRMape_Train, DTMape_Train, RFMape_Train),
                    "MAPE_Values_Test" = c(LRMape_Test, DTMape_Test, RFMape_Test),
                    "Rsquare_Values_Train" = c(LRR2_Train, DTR2_Train, RFR2_Train),
                    "Rsquare_Values_Test" = c(LRR2_Test, DTR2_Test, RFR2_Test))

# Lets's save the result
write.csv(Result,"Bike Rental Data (Result6 - Imputed KNN Values).csv",row.names=TRUE)

# Finally, the last question - Which model we are going to freeze?

# The answer is - Random Forest as it has the lowest MAPE value = 17.37557 and high Rsqaure value = 0.8679717

# Although, Linear Regression has lowest MAPE value than Random Forest, still we are going with Random Forest, 
# because Rsqaure is our priority.
