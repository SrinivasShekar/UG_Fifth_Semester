library(ggplot2)
library(RColorBrewer)
#Creates nice looking color palettes especially for thematic maps
library(reshape2)
business <- read.csv("business_rankings.csv")
#converting country names into strings to display
business$Economy <- as.character(business$Economy)

business[,c(2:5)] <- 1/(business[,c(2:5)])*1000
business$overall <- business[,2]+business[,3]+business[,4]+business[,5]

#Ease of Doing Business Ratings(no sorting required ,top 20)
business_1 <- business[c(1:20),]
#converting rank into relative comaprable numbers
ggplot(data=business_1, aes(x=business_1$Economy, y=business_1$Ease.of.Doing.Business.Rank, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Country", y="Performance Points(Ease of Doing Business)") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Ease of Doing Business Ratings (TOP 20 COUNTRIES)")

#Starting a Business
business_2 <- data.frame()
start <- sort(business$Starting.a.Business,decreasing = TRUE)
start <- unique(start)
for (i in c(1:length(start))){
  business_2 <- rbind(business_2,subset(business,business$Starting.a.Business==start[i]))
}
business_2 <- business_2[c(1:20),]
ggplot(data=business_2, aes(x=business_2$Economy, y=business_2$Starting.a.Business, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Country", y="Performance Points(Starting a Business)") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Starting a Business Rating (TOP 20 COUNTRIES)")

#Dealing with Construction
business_3 <- data.frame()
start <- sort(business$Dealing.with.Construction.Permits,decreasing = TRUE)
start <- unique(start)
for (i in c(1:length(start))){
  business_3 <- rbind(business_3,subset(business,business$Dealing.with.Construction.Permits==start[i]))
}
business_3 <- business_3[c(1:20),]
ggplot(data=business_3, aes(x=business_3$Economy, y=business_3$Starting.a.Business, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Country", y="Performance Points(Dealing with Construction Permits)") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Dealing with Construction Permits (TOP 20 COUNTRIES)")

#Protecting Minority Investors
business_4 <- data.frame()
start <- sort(business$Protecting.Minority.Investors,decreasing = TRUE)
start <- unique(start)
for (i in c(1:length(start))){
  business_4 <- rbind(business_4,subset(business,business$Protecting.Minority.Investors==start[i]))
}
business_4 <- business_4[c(1:20),]
ggplot(data=business_4, aes(x=business_4$Economy, y=business_4$Starting.a.Business, width=.8)) +
  geom_bar(stat="identity", position="identity",fill="steelblue") + labs(x="Country", y="Performance Points(Protecting.Minority.Investors)") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Protecting Minority Investors (TOP 20 COUNTRIES)")

#overall(stacked_bar_plot)
#overall calculated giving equal weightage to every feature
business_5 <- data.frame()
start <- sort(business$overall,decreasing = TRUE)
start <- unique(start)
for (i in c(1:length(start))){
  business_5 <- rbind(business_5,subset(business,business$overall==start[i]))
}
business_5 <- business_5[c(1:20),]
business_5$overall=NULL
business_overall <- melt(business_5, id.var="Economy")
ggplot(business_overall, aes(x = Economy, y = value, fill = variable)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Stacked_bar_plot_analysis (TOP 20 COUNTRIES-overall)")+ coord_flip()

#which feature is most dominant amoung the 4
#overall vs each feature
par(mfrow=c(2,2))
#1
par(mar = rep(2,4))
plot(business$Ease.of.Doing.Business.Rank,business$overall,xlab="Overall Performance",
     ylab="Ease of doing Business Performance",xlim=c(1,1000),ylim=c(1,4000),
     main="Ease of Doing Business vs Overall",pch=3,
     frame.plot=FALSE,col="red")
#2
plot(business$Starting.a.Business,business$overall,xlab="Overall Performance",
     ylab="Starting a Business Performance",xlim=c(1,1000),ylim=c(1,2000),
     main="Starting a Business vs Overall",pch=4,cex.main=1.5,
     frame.plot=FALSE,col="purple")
#3
plot(business$Dealing.with.Construction.Permits,business$overall,xlab="Overall Performance",
     ylab="Dealing with Construction Permits",xlim=c(1,1000),ylim=c(1,2000),
     main="Dealing with Construction Permits vs Overall",pch=1,cex.main=1.5,
     frame.plot=FALSE,col="blue")
#4
plot(business$Protecting.Minority.Investors,business$overall,xlab="Overall Performance",
     ylab="Protecting Minority Investors",xlim=c(1,1000),ylim=c(1,2000),
     main="Protecting Minority Investors vs Overall",pch=2,cex.main=1.5,
     frame.plot=FALSE,col="green")
#-- The overall ranks have been calculated considering equal weightage for each feature.
overall <- data.frame()
start <- sort(business$overall,decreasing = TRUE)
for (i in c(1:length(start))){
  xyz <- subset(business,business$overall==start[i])
  xyz <- cbind(xyz,i)
  overall <- rbind(overall,xyz)
}
overall$Ease.of.Doing.Business.Rank=NULL
overall$Protecting.Minority.Investors=NULL
overall$Starting.a.Business=NULL
overall$Dealing.with.Construction.Permits=NULL
View(overall)
#-- Ease of doing business factor seemed to effect the overall more adversely comapared to other features.