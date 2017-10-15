got<-read.csv("got_viewership.csv",sep=',')
#STAGE 1:PRE_PROCESSING
#(a)
got$episode_num <- ifelse( got$Season<10,got$Season+(got$Epsisode/100), 0 )

#STAGE 2:BUILDING MODEL
#(b)Multiple regression since we have one dependent variable and more than one independent variable.
multi_wcrictics<-lm(Viewership..in.million ~ (episode_num+Year+Number.of.Major.Deaths+Critic.Ratings),got)
got$predicted_viewership_wc<-fitted(multi_wcrictics)

#(c)
multi_wocrictics<-lm(Viewership..in.million ~ (episode_num+Year+Number.of.Major.Deaths),got)
got$predicted_viewership_woc<-fitted(multi_wocrictics)


#STAGE 3:PREDICTION
#(d)
mean1<-mean(got$Number.of.Major.Deaths)
min1<-min(got$Critic.Ratings)
Year<-c(2017,2017,2017,2017,2017,2017,2017)
Season<-c(7,7,7,7,7,7,7)
Episode<-c(1,2,3,4,5,6,7)
Episode.Name<-c("NA","NA","NA","NA","NA","NA","NA")
Number.of.Major.Deaths<-c(mean1,mean1,mean1,mean1,mean1,mean1,mean1)
Critic.Ratings<- c(min1,min1,min1,min1,min1,min1,min1)
Viewership..in.million<-c(0,0,0,0,0,0,0)
episode_num<-c(7.01,7.02,7.03,7.04,7.05,7.06,7.07)
predicted_viewership_wc<-c(0,0,0,0,0,0,0)
predicted_viewership_woc<-c(0,0,0,0,0,0,0)

got_res<-data.frame(Year,Season,Episode,Episode.Name,Number.of.Major.Deaths,Critic.Ratings,Viewership..in.million,
                    episode_num,predicted_viewership_wc,predicted_viewership_woc)


multi_wcrictics_after<-predict(multi_wcrictics,got_res)
got_res$predicted_viewership_wc<-multi_wcrictics_after

got_res$predicted_viewership_woc<-predict(multi_wocrictics,got_res)
got_final<-read.csv("got_season7_ratings.csv",sep=",")
got_res$rms_wc<-(got_res$predicted_viewership_wc-got_final$ratings)^2
got_res$rms_woc<-(got_res$predicted_viewership_woc-got_final$ratings)^2
#rms of predicted values including critics ratings
rms_model1<-sqrt(mean(got_res$rms_wc))
#rms of predicted values excluding critics ratings
rms_model2<-sqrt(mean(got_res$rms_woc))
#since rms of the difference between actual and predicted values of model1(with ratings)
#is more than rms of the difference between actual and predicted values of model2(w/o ratings),
#model2 is better in predicting the viewership

#STAGE 4:VISUALISATION
#(e)

count<-data.frame(got_final$episode,got_final$ratings,got_res$predicted_viewership_wc,got_res$predicted_viewership_woc)
#install.packages('plotly')
library(plotly)

plot_ly(count,x=~got_final.episode,y=~got_final.ratings,type='bar',name="Actual") %>%
  add_trace(y=~got_res.predicted_viewership_wc,name="Model 1:With ratings") %>%
  add_trace(y=~got_res.predicted_viewership_woc,name="Model 2:Without ratings") %>%
  layout(yaxis=list(title='Viewership in millions'),
         xaxis=list(title='Episode'),
         title='Comparison of the predicted models versus actual data',
         barmode='group')



