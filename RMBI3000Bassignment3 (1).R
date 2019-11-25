#2.0
stocks <- read.csv("lec03_Stocks.csv")
stocks$date<- as.Date(stocks$Date,format="%m/%d/%Y") 
#2.1
boxplot(cbind(stocks[format(stocks$date, "%Y")==2012, 2],stocks[format(stocks$date, "%Y")==2013, 2],stocks[format(stocks$date, "%Y")==2014, 2],stocks[format(stocks$date, "%Y")==2015, 2],stocks[format(stocks$date, "%Y")==2016, 2]),names = c("2012","2013","2014","2015","2016"), col=heat.colors(5) )
#2.2
months<-c(month.abb, month.abb, month.abb[1:4])
for (i in 1:7 ){
  stocks[,i]<-rev(stocks[,i])
}
price <- stocks[stocks$date>'2015-12-31',3:7]
shares<- c(1000,500,4000,200,10000)
for (i in 1:5){
price[,i]<-price[,i]*shares[i]
}
portfolio<- price$HSBC+price$HangSeng+price$TownGas+price$Tencent+price$Geely
plot(portfolio, type='l',ylim=c(0,320000))
#2.3
lineToBeAdded<-replicate(52,0) 
fc <- colorRampPalette(c("green", "brown"))
colorsgtb<-fc(5)
for (i in 1:5) {
  lineToBeAdded<-lineToBeAdded+price[,i]
  lines(lineToBeAdded,  type='l', lwd=1,
        lty=1, col=colorsgtb[i],fill=colorsgtb[i])
}
#2.4
for (i in 5:1){
  
  polygon(x=c(0,52,0,52),y=c(0,0,lineToBeAdded[1],lineToBeAdded[52]),col=colorsgtb[i])
  lineToBeAdded<-lineToBeAdded-price[,i]
  }


2.5
hangseng2016 <- c(stocks$HS.Index[210:261])

percentChangeHangseng<-(210:261)
for (i in 1:length(percentChangeHangseng))
  percentChangeHangseng[i]<- (((hangseng2016[i+1]-hangseng2016[i])/hangseng2016[i])*100)
plot (percentChangeHangseng,type="p", col= ifelse(percentChangeHangseng>=0,"green","red"), pch= 3, ylim = c(-10,10),main="Hang Seng Index %change in 2016",xlab="2016" )

