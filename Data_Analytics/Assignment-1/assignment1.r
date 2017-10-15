#Question 1
#Loading Data
data<-read.csv("deliveries.csv",sep=",")
d<-subset(data,match_id==577)
data_srh<-subset(d,batting_team=='Sunrisers Hyderabad')
data_rcb<-subset(d,batting_team=='Royal Challengers Bangalore')

s<-sum(data_srh$total_runs)
run_rate<-s/max(data_srh$over)





library(magrittr)
library(dplyr)
library



df_plot <-data_rcb %>% group_by(over)%>% summarise(runs=sum(total_runs)) %>% arrange(over)
df_plot %>% ggplot(aes(over,runs))+geom_histogram(stat="identity",color='blue',fill='cyan')+xlab('Overs')+
  ylab('Total runs scored')+ggtitle('Total runs scored by RCB in each over')

d_7<-subset(data,(match_id %in% c(7,27,67,171,414)) & is_super_over==0)
d_7 %>%  group_by(inning,match_id,batting_team) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)


#Question 2
result1<-read.csv('matches.csv',sep=',')
result2<-subset(result1,(result %in% c('normal')) &(dl_applied==0))


library(gdata)
result2 <- rename.vars(result2, from = "id", to = "match_id")
s1<-subset(data,match_id %in% result2$match_id)

q<-s1 %>%  group_by(match_id,inning,batting_team) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)

max_team<-subset(q,max(q$total_runs)==total_runs)
min_team<-subset(q,min(q$total_runs)==total_runs)

#Question 3

getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


msd<-subset(data,batsman=='MS Dhoni')
msd<-msd %>% group_by(match_id,batsman) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)
with(msd,sum(total_runs))
with(msd,summary(total_runs))
with(msd,getmode(total_runs))
with(msd,IQR(total_runs))
with(msd,sd(total_runs))

kohli<-subset(data,batsman=='V Kohli')
kohli<-kohli %>% group_by(match_id,batsman) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)
with(kohli,summary(total_runs))
with(kohli,sum(total_runs))
with(kohli,getmode(total_runs))
with(kohli,IQR(total_runs))
with(kohli,sd(total_runs))

abd<-subset(data,batsman=='AB de Villiers')
abd<-abd %>% group_by(match_id,batsman) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)
with(abd,summary(total_runs))
with(abd,sum(total_runs))
with(abd,getmode(total_runs))
with(abd,IQR(total_runs))
with(abd,sd(total_runs))


jaddu<-subset(data,batsman=='RA Jadeja')
jaddu<-jaddu %>% group_by(match_id,batsman) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)
with(jaddu,summary(total_runs))
with(jaddu,sum(total_runs))
with(jaddu,getmode(total_runs))
with(jaddu,IQR(total_runs))
with(jaddu,sd(total_runs))


raina<-subset(data,batsman=='SK Raina')
raina<-raina %>% group_by(match_id,batsman) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)
with(raina,summary(total_runs))
with(raina,sum(total_runs))
with(raina,getmode(total_runs))
with(raina,IQR(total_runs))
with(raina,sd(total_runs))




#Question 4

boxplot(msd$total_runs , kohli$total_runs,col = terrain.colors(4))
legend("topleft",title="Batsman",c("MS Dhoni","V Kohli"),fill=terrain.colors(4),horiz=TRUE)
summary(msd$total_runs)
summary(kohli$total_runs)
quantile(msd$total_runs,prob=0.9)
quantile(kohli$total_runs,prob=0.9)


#Question 5

gl<-subset(data,batting_team=='Gujarat Lions')
gl<-gl %>% group_by(match_id) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)

raina_gl<-subset(data,batting_team=='Gujarat Lions' & batsman=='SK Raina')
raina_gl<-raina_gl %>% group_by(match_id) %>% summarise(total_runs=sum(total_runs)) %>% arrange(match_id)




raina_vs_gl<-merge(raina_gl,gl,by='match_id')
raina_vs_gl$impact<-round((raina_vs_gl[2]/raina_vs_gl[3])*100,2)
raina_vs_gl


library(gdata)
win<-subset(result1,winner=="Gujarat Lions")
win1<-subset(raina_vs_gl,match_id %in% win$id)

colnames(win1)<-c("Match id","Raina's score","Team Score","Impact of Raina on the team")
win1 

