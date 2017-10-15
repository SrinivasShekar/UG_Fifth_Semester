liver_data<-read.csv("Indian Liver Patient Dataset (ILPD).csv",sep=",")
#liver_data<-subset(liver_data,is.na(liver_data$alkphos)==median(liver_data$alkphos))
liver_data$alkphos[is.na(liver_data$alkphos)] <- median(liver_data$alkphos, na.rm = TRUE)

#Females Male ratio
liver_f<-subset(liver_data,(gender=="Female" & is_patient==2))
liver_m<-subset(liver_data,(gender=="Male" & is_patient==2))
totliver_f<-subset(liver_data,gender=="Female")
totliver_m<-subset(liver_data,gender=="Male")

library(dplyr)
fem_percent<-(count(liver_f)/count(totliver_f))*100
#cat('Percentage of female who are suffering from liver disease')
fem_percent
male_percent<-(count(liver_m)/count(totliver_m))*100
#cat('Percentage of female who are suffering from liver disease')
male_percent

#vulnerable age group

sample<-subset(liver_data,is_patient==2)
library(ggplot2)

ggplot(data=liver_data, aes(age)) + 
  geom_histogram(breaks=seq(5, 95, by =2), 
                 col="red", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red")
library(dplyr)
liver<-liver_data %>% select(age,gender,tot_bilirubin,direct_bilirubin,tot_proteins,albumin,ag_ratio,sgpt,sgot,alkphos,is_patient)
#l<-cor(liver)
library(corrplot)
pairs(liver,main="Scatterplot matrix for the remaining attributes")
#corrplot(l, method="circle")


library(dummies)

train<-liver_data[1:409,]
test<-liver_data[410:nrow(liver_data),]
      
new_my_data<-liver_data 
new_my_data$gender<-ifelse(new_my_data$gender=="Male",1,0)
new_my_data <- subset(new_my_data, select = -c(is_patient))
str(new_my_data)
pca.train<-new_my_data[1:409,]
pca.test<-new_my_data[410:nrow(new_my_data),]
prin_comp <- prcomp(pca.train, scale. = T)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
train.data <- data.frame(is_patient= train$is_patient, prin_comp$x)
train.data <- train.data[,1:8]
library("e1071")
model<-svm(is_patient ~ ., data=train.data,type='C-classification',kernel='polynomial')
summary(model)

test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:7]
pred <- predict(model,data=test.data)
pred
a<-as.data.frame(pred)
library(caret)

confusionMatrix(pred,train$is_patient)


'''
library(randomForest)
model <- randomForest(is_patient ~., data = train.data)
model
pred <- predict(model, newdata = test.data)
length(pred)
confusionMatrix(pred, test$is_patient)
'''

rpart.model <- rpart(is_patient ~ .,data = train.data, method = "anova")
rpart.model

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:7]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
rpart.prediction<-as.data.frame(rpart.prediction)
rpart.prediction$outcome<-ifelse(rpart.prediction > 1.45,2,1)
confusionMatrix(rpart.prediction$outcome,test$is_patient)


set.seed(3033)
new_data<-liver_data
new_data$class<-ifelse(new_data$is_patient==1,"class1","class2")
#new_data$class<-ifelse(new_data$is_patient==1,"class1","class2")
new_data<-subset(new_data,select=-c(is_patient))
intrain <- createDataPartition(y = liver_data$is_patient, p= 0.7, list = FALSE)
training <- new_data[intrain,]
testing <- new_data[-intrain,]



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(class ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit
#install.packages("rpart.plot")
library(rpart.plot)
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


test_pred <- predict(dtree_fit, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$class )  #check accuracy

train<-liver_data[1:409,]
test<-liver_data[410:nrow(liver_data),]



fit <- rpart(is_patient ~.,data=train, method='class',control = rpart.control(minsplit = 40))
rpart.plot(fit)
plotcp(fit)
printcp(fit)
fit
preds <- predict(fit, test, type = 'class')
preds
confMat <- table(test$is_patient,preds)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy
