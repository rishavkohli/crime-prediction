dataset = read.csv('crime.csv')
j=1
corr5<-c()
for(i in 6:10)
{corr5[j]<-cor(dataset[,5],dataset[,i], method = "pearson")
  j<-j+1
}
corr5


j=1
corr6<-c()
for(i in 7:10)
{corr6[j]<-cor(dataset[,6],dataset[,i], method = "pearson")
j<-j+1
}
corr6


j=1
corr7<-c()
for(i in 8:10)
{corr7[j]<-cor(dataset[,7],dataset[,i], method = "pearson")
j<-j+1
}
corr7

j=1
corr8<-c()
for(i in 9:10)
{corr8[j]<-cor(dataset[,8],dataset[,i], method = "pearson")
j<-j+1
}
corr8

j=1
corr9<-c()
corr9[1]<-cor(dataset[,9],dataset[,10], method = "pearson")

corr9

coorelation<-data.frame()
coorelation<-rbind(coorelation,corr5)
coorelation<-rbind(coorelation,corr6)
coorelation<-rbind(coorelation,corr7)
coorelation<-rbind(coorelation,corr8)
coorelation<-rbind(coorelation,corr9)
names(coorelation)[1]<-"kidnapping"
names(coorelation)[2]<-"dowry"
names(coorelation)[3]<-"assult"
names(coorelation)[4]<-"insult to modesty"
names(coorelation)[5]<-"cruelity by husband"

write.csv(coorelation,"coorelation.csv")
