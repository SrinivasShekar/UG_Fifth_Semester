library(dplyr)
cancer<-read.csv('cancer_tailored.csv',sep=",")

cancer<-na.omit(cancer)

cancer_vis<-cancer %>% select(texture_mean,radius_mean,perimeter_mean,area_mean,area_se,area_worst,radius_worst,texture_worst,perimeter_worst)
#since variances are too small in other attributes we eliminate them

'''> var(cancer$id)
[1] 1.578478e+16
> var(cancer$radius_mean)
[1] 12.38727
> var(cancer$texture_mean)
[1] 18.41927
> var(cancer$perimeter_mean)
[1] 589.1235
> var(cancer$area_mean)
[1] 123487.2
> var(cancer$smoothness_mean)
[1] 0.0001992446
> var(cancer$compactness_mean)
[1] 0.002811854
> var(cancer$concavity_mean)
[1] 0.006383414
> var(cancer$concave.points_mean)
[1] 0.001506857
> var(cancer$symmetry_mean)
[1] 0.0007579178
> var(cancer$fractal_dimension_mean)
[1] 5.020212e-05
> var(cancer$radius_se)
[1] 0.07712471
> var(cancer$texture_se)
[1] 0.302477
> var(cancer$perimeter_se)
[1] 4.104117
> var(cancer$area_se)
[1] 2078.02
> var(cancer$smoothness_se)
[1] 9.061865e-06
> var(cancer$compactness_se)
[1] 0.0003237663
> var(cancer$concavity_se)
[1] 0.0009186614
> var(cancer$symmetry_se)
[1] 6.892303e-05
> var(cancer$fractal_dimension_se)
[1] 7.064119e-06
> var(cancer$radius_worst)
[1] 23.40133
> var(cancer$texture_worst)
[1] 37.61829
> var(cancer$perimeter_worst)
[1] 1131.687
> var(cancer$area_worst)
[1] 324915.6
> var(cancer$smoothness_worst)
[1] 0.0005243442
> var(cancer$compactness_worst)
[1] 0.02492564
> var(cancer$concavity_worst)
[1] 0.04377942
> var(cancer$concave.points_worst)
[1] 0.004320222
> var(cancer$symmetry_worst)
[1] 0.003860361
> var(cancer$fractal_dimension_worst)
[1] 0.0003277588
'''



library(corrplot)
M <- cor(cancer_vis)
corrplot(M,method="circle",title='Cor-relation plot of attributes',order='AOE')
#or
#pairs(cancer_vis,main="Scatterplot matrix for the remaining attributes")
library(caret)
#install.packages('e1071', dependencies=TRUE)
a<-confusionMatrix(cancer$predicted, cancer$diagnosis, positive = NULL, dnn = c("Prediction","Reference"))

cancer["newclass"] <- ifelse(cancer["diagnosis"]=='B' & cancer["predicted"]=='B', "TN", 
                             ifelse(cancer["diagnosis"]=='B' & cancer["predicted"]=='M', "FP",
                                    ifelse(cancer["diagnosis"]=='M' & cancer["predicted"]=='B', "FN", "TP")))

a_new <- table(cancer["newclass"])
#Confusion Matrix is
a_new
#Accuracy:(a_new['TP']+a_new['TN'])/(a_new['TP']+a_new['FN']+a_new['TN']+a_new['FP'])
cat("Overall accuracy:",a$overall['Accuracy'])
cat("Misclassification rate:",(a_new['FP']+a_new['FN'])/(a_new['TP']+a_new['FN']+a_new['TN']+a_new['FP']))
cat("Recall or Sensitivity:",
recall<-a_new['TP']/(a_new['TP']+a_new['FN']))
cat("Precision :",
precision<-a_new['TP']/(a_new['TP']+a_new['FP']))
cat("Specificity:",a_new['TN']/ (a_new['TN']+a_new['FP']) )
cat('F score with beta=0.5:',(0.5*0.5 + 1)*precision*recall / (0.5*0.5*precision + recall))
cat('F score with beta=2:',(2*2 + 1)*precision*recall / (2*2*precision + recall))



