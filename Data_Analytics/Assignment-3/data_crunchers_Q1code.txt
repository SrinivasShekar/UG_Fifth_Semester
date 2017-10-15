library(plotly)
library(dplyr)
library(zoo)
time<-read.csv("MER_T12_06.csv",sep=",")
time1 <- time %>% mutate(
  dummy = as.character(YYYYMM),
  year = substr(dummy, 0, 4),
  year = as.factor(year),
  month = substr(dummy, 5, 6),
  month = as.factor(month),
  value =as.numeric(Value) 
) %>% dplyr::select(year, month,value , Description, -dummy, -YYYYMM)
#Now we remove the 13th month
myData <- time1 %>% filter( month != 13)

library(plotly)

q<-unique(myData$Description)


natural_gas<-list(
  x=myData$year, 
  y=subset(myData,Description==q[2]) %>% dplyr::select(value),
  xref='x', yref='y')


c1<-subset(myData,Description==q[1])
c2<-subset(myData,Description==q[2])
c3<-subset(myData,Description==q[3])
c4<-subset(myData,Description==q[4])
c5<-subset(myData,Description==q[5])
c6<-subset(myData,Description==q[6])
c7<-subset(myData,Description==q[7])
c8<-subset(myData,Description==q[8])
c9<-subset(myData,Description==q[9])


plot_ly() %>% add_data(c1)%>%
  add_trace(x=~year,y = ~value, name = 'coal',type='scatter', mode='lines') %>%
  add_data(c2) %>% add_trace(x=~year,y = ~value, name = 'natural gas', type='scatter', mode='lines') %>%
  add_data(c3) %>% add_trace(x=~year,y = ~value, name = 'Distillate fuel', type='scatter', mode='lines') %>%
  add_data(c4) %>% add_trace(x=~year,y = ~value, name = 'Petroleum Coke', type='scatter', mode='lines') %>%
  add_data(c5) %>% add_trace(x=~year,y = ~value, name = 'Residual Fuel', type='scatter', mode='lines') %>%
  add_data(c6) %>% add_trace(x=~year,y = ~value, name = 'Petroleum', type='scatter', mode='lines') %>%
  add_data(c7) %>% add_trace(x=~year,y = ~value, name = 'Geothermal Energy', type='scatter', mode='lines') %>%
  add_data(c8) %>% add_trace(x=~year,y = ~value, name = 'Non-Biomass', type='scatter', mode='lines') %>%
  add_data(c9) %>% add_trace(x=~year,y = ~value, name = 'Total Energy', type='scatter', mode='lines') %>%
  layout(
    title='Amount of CO2 emitted by various sectors',
    xaxis=list( 
      
      rangeslider = list(
        list(x=1,y=1,type = "months")
      #rangeslider =list(type="months")
      #rangeslider(myData$year[1], myData$year[4707])
      )),
  
    
    
    updatemenus = list(
      list(
        x=-0.3,
        y=0.7,
        
        label = 'Category',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, FALSE, FALSE,FALSE,FALSE,FALSE, FALSE,FALSE,FALSE)),
               label = "Coal"),
          list(method = "restyle",
               args = list('visible', c(FALSE, TRUE, FALSE,FALSE,FALSE,FALSE, FALSE,FALSE,FALSE)),
               label = "Natural gas"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, TRUE,FALSE,FALSE,FALSE, FALSE,FALSE,FALSE)),
               label = "Distillate fuel"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE,TRUE,FALSE,FALSE, FALSE,FALSE,FALSE)),
               label = "Petroleum Coke"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE,FALSE,TRUE,FALSE, FALSE,FALSE,FALSE)),
               label = "Residual Fuel"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE,FALSE,FALSE,TRUE, FALSE,FALSE,FALSE)),
               label = "Petroleum"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE,FALSE,FALSE,FALSE, TRUE,FALSE,FALSE)),
               label = "Geothermal"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE,FALSE,FALSE,FALSE, FALSE,TRUE,FALSE)),
               label = "Non-Biomass"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE,FALSE,FALSE,FALSE, FALSE,FALSE,TRUE)),
               label = "Total Energy")
          
        )
      )
    )
  )
