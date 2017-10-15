train<-read.csv("got_character_test.csv",sep=",")
train$age[is.na(train$age)]<-median(train$age,na.rm=T)
train$isAliveFather[is.na(train$isAliveFather)]<- -1
train$isAliveMother[is.na(train$isAliveMother)]<- -1
train$isAliveHeir[is.na(train$isAliveHeir)]<- -1
train$isAliveSpouse[is.na(train$isAliveSpouse)]<- -1
library(dplyr)
is_alive<-subset(train,isAlive==1)
c1<-count(is_alive)
is_notalive<-subset(train,isAlive==0)
c2<-count(is_notalive)
pie(c1)
