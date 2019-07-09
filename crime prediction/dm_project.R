library(plyr)
library(caTools)
library(class)
library(ggplot2)

#install.packages("reshape")
library(reshape)
#impoting dataset
Dataset2012<-read.csv("dstrCAW_1.csv")
Dataset2013<-read.csv("dstrCAW_2013.csv")




Dataset2014<- read.csv("District-wise_Crimes_committed_against_Women_2014.csv", sep="\t")


Dataset2014[,4]<-Dataset2014[,5]+Dataset2014[,4]
Dataset2014<-Dataset2014[,-5]
dataset<-data.frame()
dataset<-rbind(Dataset2012, Dataset2013)
dataset<-rbind(dataset,Dataset2014)

dataset[,1]<-tolower(dataset[,1])
dataset[,2]<-tolower(dataset[,2])
temp=unique(dataset[,2])
temp




for(i in 1:length(dataset[,2]))
 {if(grepl("city", dataset[i,2]))
 {temp<-strsplit(dataset[i,2],split = ' ')
     
     dataset[i,2]<-paste(temp[[1]][1:(length(temp[[1]])-1)],sep=" ",collapse = ' ')
 }
  
  if(grepl("railway", dataset[i,2]))
  {temp<-strsplit(dataset[i,2],split = ' ')
  temp[[1]][length(temp[[1]])]<-"rly."
  dataset[i,2]<-paste(temp[[1]][1:length(temp[[1]])],sep=" ",collapse = ' ')
  }
  
  if(grepl("g.r.p.", dataset[i,2]))
  {temp<-strsplit(dataset[i,2],split = ' ')
  temp[[1]][length(temp[[1]])]<-"g. r. p."
  dataset[i,2]<-paste(temp[[1]][1:length(temp[[1]])],sep=" ",collapse = ' ')
  }
  

}

crime<-data.frame()
crime<-dataset
#for(i in c(5:10))
#{
 # crime[,4]<-crime[,4]+dataset[,i]
  
#}

test<-data.frame()
test<-crime
test[,2]<-tolower(test[,2])
test<-cast(test,STATE.UT+DISTRICT~Year,mean,value = 'Rape')



i=1
while(i<=length(test[,2]))
{
 temp<-test[i,] 
  count<-0
  print(i)
  for(j in 3:length(temp))
  { 
    if(temp[j]=="NaN")
    { 
      count<-count+1}
  }
  if(count>=5)
  { test<- test[-i,]
  i<-i-1}
  i<-i+1
}



for(i in 1:length(test[,1]))
{ temp<-c()
  temp<-test[i,3:16]
  print(i)
  for(k in 1:14)
  { 
    if(temp[[k]]=="NaN")
    { print(k)
      temp1<-c()
      for(j in 1:14)
      { 
        temp1[j]<-as.numeric(temp[[j]])
      }
         print(mean(temp1,na.rm = TRUE))
       test[i,k+2]<-round(mean(temp1,na.rm = TRUE),2)
       print(test[i,k+2])
      
      
    }
  }
  
  
}
  

  
  
  
 
   

write.csv(test,'test.csv')

write.csv(crime,'crime.csv')


