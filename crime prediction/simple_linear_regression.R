# Simple Linear Regression

# Importing the dataset
setwd('C:/Users/DUCS/Desktop/fff/dm project/dm project')
dataset = read.csv('test.csv')
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
col<-dataset[,2]
colo.fac<-as.factor(col)
colo.fac<-as.numeric(colo.fac)
colo.fac
colo.fac<-as.factor(colo.fac)
dataset<-dataset[,4:17]
dataset<-cbind(dataset,colo.fac)
dataset<-dataset[-501,]
  split = sample.split( dataset , SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = X2014 ~ . ,
               data = training_set)
#str(test_set$DISTRICT)
#str(training_set$DISTRICT)
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred


calculte_mod<-function(y_pred,test_set)
{ error<-0 
for(i in 1:length(test_set[,1]))
{ temp<-y_pred[i]-test_set[i,14]
if(temp<0)
{temp<-0-temp}
error<-error+temp

}
return((error/length(test_set[,1])))
}

error<-calculte_mod(y_pred,test_set)
error

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')