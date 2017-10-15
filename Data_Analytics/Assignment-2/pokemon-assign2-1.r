
#Question 1:

poke<-read.csv("pokemon.csv",sep=",")
library(dplyr)
#SRS
poke_simple<-sample_frac(poke,0.7)
#Systematic 
sys.sample <-function(N,n){
  k = ceiling(N/n)
  r = sample(1, 1)
  sys.samp = seq(r, r + k*(n-1), k)
  
}

poke_sys<-sys.sample(721, 241)
poke_sys<-subset(poke,Number %in% poke_sys)
#Stratified
poke_male<-subset(poke,Pr_Male>=0.5 & hasGender==TRUE)
poke_female<-subset(poke,Pr_Male<0.5 & hasGender==TRUE)
library(survey)

#Clustered sampling
poke_type1<-poke %>% select(Type_1)
poke_type1<-unique(poke_type1)
poke_cluster<-(sample_frac(poke_type1,0.7))
frame1<-subset(poke,Type_1 %in% poke_cluster$Type_1)

library(ggplot2)
library(gridExtra)
library(grid)
p1<-ggplot(aes(Total),data=poke_simple)+geom_histogram(col='green')+
  scale_x_continuous(limits = c(160, 750),breaks = seq(160, 750, 20))+
  ggtitle("Simple Random Sampling")

p2<-ggplot(aes(Total),data=poke_male)+geom_histogram(col='red')+
  scale_x_continuous(limits = c(160, 700),breaks = seq(160, 700, 20))+
  ggtitle("Stratified Sampling")

p3<-ggplot(aes(Total),data=poke_female)+geom_histogram(col='yellow')+
  scale_x_continuous(limits = c(160, 650),breaks = seq(160, 650, 20))+
  ggtitle("Stratified Sampling")

p4<-ggplot(aes(Total),data=poke_sys)+geom_histogram(col='blue')+
  scale_x_continuous(limits = c(160, 700),breaks = seq(160, 700, 20))+
  ggtitle("Systematic Sampling")
p5<-ggplot(aes(Total),data=poke)+geom_histogram(col='green')+
  scale_x_continuous(limits = c(160, 740),breaks = seq(160, 760, 30))+
  ggtitle("Population")

p6<-ggplot(aes(Total),data=frame1)+geom_histogram(col='orange')+
  scale_x_continuous(limits = c(160, 740),breaks = seq(160, 760, 30))+
  ggtitle("Clustered Sampling")
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)




#Question 2:

#Computing distance between means
new<-poke[,c(3,6:11)]
grass<-subset(new,Type_1=='Grass')
fire<-subset(new,Type_1=='Fire')
dist_HP<-(mean(fire$HP)-mean(grass$HP))
dist_Attack<-(mean(fire$Attack)-mean(grass$Attack))
dist_Defense<-(mean(fire$Defense)-mean(grass$Defense))
dist_SpAtk<-(mean(fire$Sp_Atk)-mean(grass$Sp_Atk))
dist_SpDef<-(mean(fire$Sp_Def)-mean(grass$Sp_Def))
dist_Speed<-(mean(fire$Speed)-mean(grass$Speed))

#Normalisation and Standardisation Functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
standard <-function(x){
  return ((x-mean(x))/sd(x))
}

grassNorm <- as.data.frame(lapply(grass[2:7], normalize))
fireNorm<- as.data.frame(lapply(fire[2:7], normalize))

Hpfire<-mean(fireNorm$HP)
Attackfire<-mean(fireNorm$Attack)
Defensefire<-mean(fireNorm$Defense)
SpAtkfire<-mean(fireNorm$Sp_Atk)
SpDeffire<-mean(fireNorm$Sp_Def)
Speedfire<-mean(fireNorm$Speed)                            

Hpgrass<-mean(grassNorm$HP)
Attackgrass<-mean(grassNorm$Attack)
Defensegrass<-mean(grassNorm$Defense)
SpAtkgrass<-mean(grassNorm$Sp_Atk)
SpDefgrass<-mean(grassNorm$Sp_Def)
Speedgrass<-mean(grassNorm$Speed)                            

v<-c(Hpfire,Attackfire,Defensefire,SpAtkfire,SpDeffire,Speedfire)
w<-c(Hpgrass,Attackgrass,Defensegrass,SpAtkgrass,SpDefgrass,Speedgrass)

library(grDevices)
plot(v,xaxt="n",type="o",col="red",xlab="Parameters",ylab="Normalised values of mean")
lines(w,type="o",col="blue")
axis(1, at=1:6, labels=c("HP","Attack","Defense","Sp_Atk","Sp_Def","Speed"))
legend("topright",c("Fire Normalisation", "Grass Normalisation"),
       fill=c("red","blue"), horiz=TRUE)

#Computing kurtosis and skewness 
library(moments)
skewness(poke$Height_m)
skewness(poke$Weight_kg)
kurtosis(poke$Height_m)
kurtosis(poke$Weight_kg)

grassstd<-as.data.frame(lapply(poke[,c(5:11,20,21)], standard))
p1<-qplot(data=grassstd,HP)
p2<-qplot(data=grassstd,Attack)
p3<-qplot(data=grassstd,Defense)
p4<-qplot(data=grassstd,Sp_Atk)
p5<-qplot(data=grassstd,Sp_Def)
p6<-qplot(data=grassstd,Speed)
p7<-qplot(data=grassstd,Height_m,xlim=c(0,6))
p8<-qplot(data=grassstd,Weight_kg,xlim=c(0,1))
p9<-qplot(data=grassstd,Total)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=4)





#Question 3:
#Preliminary Analysis : To get rid off 2 variables.
q3<-poke %>% select(Total,HP,  Attack,  Defense,  Sp_Atk,  Sp_Def, Speed,Generation, Pr_Male,Height_m, Weight_kg,Catch_Rate)
Total_Var<-var(q3$Total)
HP_Var<-var(q3$HP)
Attack_var<-var(q3$Attack)
Defense_var<-var(q3$Defense)
SpAtk_var<-var(q3$Sp_Atk)
SpDef_var<-var(q3$Sp_Def)
Speed_var<-var(q3$Speed)
Gen_var<-var(q3$Generation)
Male_var<-var(q3$Pr_Male)
Weight_var<-var(q3$Weight_kg)
Height_var<-var(q3$Height_m)
Catch_var<-var(q3$Catch_Rate)

#Selecting the remaining 10 variables
q3<-q3 %>% select(Total,HP,  Attack,  Defense,  Sp_Atk,  Sp_Def, Speed,Generation, Weight_kg,Catch_Rate)
ir.pca <- prcomp(q3,
                 center = TRUE,
                 scale. = TRUE) 

#Plot the variance associated with each variables
plot((ir.pca$sdev)^2,col="blue",type="l",xlab="Variables",ylab="Variance")
summary(ir.pca)

#Data to be stored in Ash's Device
pca<-subset(ir.pca$x,select=c("PC1","PC2"))
pca

#Plot PC1 against PC2
plot(subset(pca,select=c("PC1")),subset(pca,select=c("PC2")),xlab="PC1",ylab="PC2")

