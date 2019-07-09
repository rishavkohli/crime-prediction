# Polynomial Regression

# Importing the dataset
#setwd('C:/Users/DUCS/Desktop/fff/dm project/dm project')
dataset = read.csv('crime.csv')
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
col<-dataset[,2]
colo.fac<-factor(col)
colo.fac
colo.fac<-as.numeric(colo.fac)
colo.fac
colo.fac<-as.factor(colo.fac)

dataset[,2]<-colo.fac

col<-dataset[,3]
colo.fac<-factor(col)
colo.fac
colo.fac<-as.numeric(colo.fac)
colo.fac
colo.fac<-as.factor(colo.fac)

dataset[,3]<-colo.fac


for(i in 1:length(dataset[,5]))
{ for(j in 5:11)
  if(dataset[i,j]=="NaN")
  {
    dataset[i,j]<-round(mean(dataset[,j],na.rm = TRUE),2)
  }
  
  
}

model_set=data.frame()
model_set = dataset[,2:4]
model_set<-cbind(model_set,dataset[,9])
model_set[,5]<-model_set[,3]*model_set[,3]
model_set[,6]<-model_set[,3]^3
model_set[,7]<-model_set[,3]^4

#model_set<-cbind(training_set,dataset[,2])



train_set<-data.frame()
for(i in 1:length(model_set[,1]))
{   
  if(model_set[i,1]==5)
  {train_set<- rbind(train_set,model_set[i,])
  }
  
}

train_set=train_set[,-1]

names(train_set)[3]<-"insult_to_modesty"









poly_reg = lm(formula = insult_to_modesty  ~ .,
              data = train_set)
summary(poly_reg)




training1_set<-data.frame()

for(i in 1:length(train_set[,1]))
{   
  if(train_set[i,1]==852)
  {training1_set<- rbind(training1_set,train_set[i,])
  }
  
}
training1_set<-training1_set[,-1]
poly_reg = lm(formula = insult_to_modesty ~ .,
              data = training1_set)

# Visualising the Linear Regression results
# install.packages('ggplot2')


# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = training1_set$Year, y = training1_set$insult_to_modesty),
             colour = 'red') +
  geom_line(aes(x = training1_set$Year, y = predict(poly_reg, newdata = training1_set)),
            colour = 'blue') +
  ggtitle('insult_to_modesty vs Year (bihar)(saharsa)') +
  xlab('year') +
  ylab('insult_to_modesty')

#test_set

test_set<-data.frame("Year"<-c(2016,2017,2018,2019))
names(test_set)[1]<-"Year"
test_set$V5<-test_set$Year^2
test_set$V6<-test_set$Year^3
test_set$V7<-test_set$Year^4

pred<-predict(poly_reg,test_set)
test_set$crulity_by_husband<-pred
write.csv(test_set,"insult_to_modesty_bihar(sahara).csv")
