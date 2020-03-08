#timeseries----

pacman::p_load(gsheet,lubridate,zoo,xts,quantmod,TTR,forecast)


start<-as.Date('2020-01-01')
end<-as.Date('2020-02-29')
end-start
bday='27-Apr-1989'
str(bday)
bday1<-as.Date(bday,format='%d-%b-%Y')
str(bday1)
weekdays(bday1)

Sys.Date()-bday1
 
bday2='29-Nov-1991'
bday3<-as.Date(bday2,format='%d-%b-%Y')
Sys.Date()-bday3




internship=seq.Date(from=Sys.Date(),to=as.Date('2020-04-01'),by=1)
weekdays(internship)
range(internship)

start<-as.Date('2020-01-01')
end<-as.Date('2020-02-29')

getSymbols("SBIN.NS",src="yahoo",from=start,to=end)

df=SBIN.NS
head(df)
gsub(".",names(df))
colnames(df)


quantmod::getSymbols("ABB.NS","TATAMOTORS.NS",src="yahoo",from=start,to=end)

df1=ABB.NS
head(df1)

start<-as.Date('2020-01-01')
end<-as.Date('2020-02-29')

getSymbols("SBIN.NS",src="yahoo",from=start,to=end)

df=SBIN.NS
head(df)
names(df)
colnames(df)
unlist(strsplit("a.b.c","\\."))
unlist(strsplit(names(df),"\\."))
(newColNames<-unlist(strsplit(names(df),"\\."))[seq(3,18,3)])
names(df)=newColNames
head(df)
str(df)
index(df)  #rownames
coredata(df)  #column values

plot(df$Open)
plot(df,legend.loc = 'left')
plot(df,legend.loc = 'left',multi.panel = T)
plot(df[,c('Open','Close')],legend.loc='right',subset="2020-01-01/2020-02-20")
plot(df[,1:4],multi.panel = T)
plot(df[,1:4],multi.panel = T,type='h')
plot(df[,1:4],multi.panel = F,legend.loc = 'top')
candleChart(df,up.col="green",dn.col="red",theme = "white")
lineChart(df)




#properties----

periodicity(df)
to.weekly(df)
to.monthly(df)
to.quarterly(df)
to.yearly(df)
to.period(df,period="weeks")
nyears(df)
nweeks(df)
.indexwday(df)
weekdays(index(df))
start(df)
end(df)
time(df)
head(df)
tail(df)


apply.weekly(df,FUN=mean)
apply.monthly(df,FUN=mean)
apply.monthly(df$Open,FUN=sd)
apply.quarterly(df,FUN=mean)


?FUN


weekdays(index(df,5))
df[.indexwday(df)==5] #gives data of stock on friday


#endpoint

endpoints(df,on='weeks')
df[endpoints(df,on='weeks')]

endpoints(df,on='months')
df[endpoints(df,on='months')]

(wep<-endpoints(df,on='weeks'))
period.apply(df,INDEX = wep,FUN=mean)


#split data
(sdata<-split(df,f='months'))
sdata[[1]]
sdata[[2]]

(sdata<-split(df,f='weeks'))
sdata[[3]]



#subset
df['2020']
df['2020-01']
df['2020-01-17']


#first and last

first(df,'2 week')
first(last(df,'1 week'),'3 days')  #data of 1st 3 days of last week

df[c('2020-1-16','2020-2-17')]

df[seq(as.Date('2020-01-01'),as.Date('2020-01-31'),2),]








#airpassengers----

AirPassengers
monthplot(AirPassengers)
class(AirPassengers)
(inputData=as.vector(AirPassengers))
length(inputData)
ts(inputData,frequency=4,start=c(1959,2))

#frequency 4=> quarterly data
ts(inputData,frequency = 12,start=1990)

#frequency 12=> monthly data

(inputData=as.vector(AirPassengers))
(monTS<-ts(inputData,frequency = 12,start=c(2010,3)))

(monTSlagged<-stats::lag(monTS,k=-1))
monTS
monTSlagged
(monTS-monTSlagged)

diff(monTS,lag=1)
cbind(monTS,monTSlagged,difference=(monTS-monTSlagged),diff(monTS))
head(monTS)
c((132-112),(129-118),(121-132))




#rolling mean----
monTS
zoo::rollmean(monTS,k=1)
zoo::rollmean(monTS,k=2)
head(monTS)
zoo::rollapply(monTS,width=12,FUN=mean,align='right')


#moving average----
library(TTR)
head(monTS)
SMA(monTS,n=3)
EMA(monTS,n=3)
plot(monTS)
lines(SMA(monTS,n=12),col='red')

#exponential----
forecast::ets


#find tend, seasonal, irregular componets

AirPassengers
plot(AirPassengers)
plot(decompose(AirPassengers))


#forecast and arima----
library(forecast)
auto.arima(monTS)
#best order -d=2,p=1,q=1
#seasonality= P=0,D=0,Q=0
#mov-avg m=4
autoplot(monTS)

#without seasonal
auto.arima(monTS,seasonal = T)

#model
arimaModel=auto.arima(monTS)
checkresiduals(arimaModel)


#forecast

fcModel<-forecast(arimaModel)
fcModel$mean #10 elements ahead

autoplot(fcModel)


fcModela<-forecast(arimaModel,20)
fcModela$mean #20 elements ahead

autoplot(fcModela)



#lag
df.open=df[,'Open']
df.open
str(df.open)
