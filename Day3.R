#missing values----

(x=c(1,2,4,5))
#(x1=c(1,2,,4,,5))  #error
(x2=c(1,2,NA,4,NA,5))
sum(x)
sum(x2)
sum(x2,na.rm=T)  #na.rm excludes missing data to add the rest
length(x2)
is.na(x2)
sum(is.na(x2))
sum(is.na(x2)/length(x2))
mean(x2,na.rm=T)
x2[is.na(x2)]
x2[is.na(x2)]=mean(x2,na.rm=T)
x2


library(VIM)
library(dplyr)
data(sleep)
sleep;?sleep
head(sleep)
tail(sleep)
str(sleep)
dim(sleep)
str(sleep)
summary(sleep)
?summary
(x=200:300)
quantile(x)
quantile(x,seq(0,1,.1))
quantile(x,seq(0,1,.25))



is.na(sleep)
sum(is.na(sleep))
colSums(is.na(sleep))
rowSums(is.na(sleep))
complete.cases(sleep)
sum(complete.cases(sleep))
sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]
xy=colSums(is.na(sleep))
xy
xy[xy>0]
c1<-names(xy[xy>0])
sleep[,c1]
sleep%>%select(c1)  #executed using dplyr
sleep%>%select(-c1)%>%length()

`%notin%`<-Negate(`%in%`)










#data partitioning----

(x=1:100)
s1<-sample(x,size=70)
length(s1)
set.seed(134)
?set.seed

s2<-sample(x,size=.7*length(x))
length(s2)
x




mtcars
mtcars%>%sample_n(24)
mtcars%>%sample_frac(.7)

(index=sample(1:nrow(mtcars),size=.7*nrow(mtcars)))
mtcars[index,]
mtcars[-index,]



pinstall<-c('rpart','rpart.plot','catools','caret','arules','arulesviz','factorextra','denextend')
install.packages(pinstall)



pinstall<-c('readx1','modeest','fdth','rJava','xlsx','wordcloud','wordcloud2','e1071')
install.packages(pinstall)

tmpackages<-c('rtweet','curl','twitterR')
lppackages<-c('lpsolve','linprog','lpSolveAPI')



library(caTools)
sample=sample.split(Y=mtcars$am,SplitRatio=.70)
sample
prop.table(table(sample))
(y1=mtcars[sample==T,])
y2=mtcars[sample==F,]
prop.table(table(y1$am))
prop.table(table(y2$am))




library(caret)
(intrain<-createDataPartition(y=mtcars$am,p=.7,list=F))
train<-mtcars[intrain,]
test<-mtcars[-intrain,]
prop.table(table(train$am));prop.table(table(test$am))
end

#pinstall<-

#linear regression--------


women
head(women)
model=lm(weight~height,data=women)
summary(model)
plot(x=women$height,y=women$weight)
abline(model) #line of best fit
residuals(model)
women$weight

predict(model,newdata=women,type='response')
head(women)
predwt<-predict(model,newdata=women,type='response')
cbind(women,predwt)
head(women)
cbind(women,predwt,res=women$weight-predwt,res2=residuals(model))
summary(model)
sqrt(sum(residuals(model)^2))   #SSE
range(women$height)



ndata=data.frame(height=66.5)
predict(model,newdata=ndata,type='response')

(ndata=data.frame(height=c(66.5,75.8)))
predict(model,newdata=ndata,type='response')
confint(model)

#case 2 linear regression----
library(gsheet)
link="https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=2023826519"
df=as.data.frame(gsheet2tbl(link))
head(df)
model1=lm(Y~X,data=df)
summary(model1)
abline(model1)
plot(df$X,df$Y)
resid(df$X)
(ndata1=data.frame(X=c(3,4)))
predict(model1,newdata=ndata1,type='response')


#linear regression  2----

link1="https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=1595306231"
df3=as.data.frame(gsheet2tbl(link1))
head(df3)
plot(df3$price,df$sqty)
plot(df3$promotion,df3$sqty)
model3<-lm(sqty~promotion,data=dfr)
plot(model3)

model3<-lm(sqty~promotion+price,data=df3)
plot(model3)
summary(model3)
plot(dfr$promotion,dfr$sqty)
abline(model3)
(ndata3=data.frame(price=c(60,75),promotion=c(300,500)))
predict(model3,newdata=ndata3,type='response')








#decision tree----
# Decision Tree - Classification
#we want predict for combination of input variables, is a person likely to survive or not

#import data from online site
path = 'https://raw.githubusercontent.com/DUanalytics/datasets/master/csv/titanic_train.csv'
titanic <- read.csv(path)
head(titanic)
names(titanic)
data = titanic[,c(2,3,5,6,7)]  #select few columns only
head(data)
dim(data)
#load libraries
library(rpart)
library(rpart.plot)
str(data)
#Decision Tree
names(data)
table(data$Survived)
str(data)
data$Pclass = factor(data$Pclass)
fit <- rpart(Survived ~ ., data = data, method = 'class')
fit
rpart.plot(fit, extra = 104, cex=.8,nn=T)  #plot
head(data)
printcp(fit) #select complexity parameter
prunetree2 = prune(fit, cp=.018)  #select cp based n the least xerror(cross validation error)
rpart.plot(prunetree2, cex=.8,nn=T, extra=104)
prunetree2
nrow(data)
table(data$Survived)
prop.table(table(data$Survived))

# predict for Female, pclass=3, siblings=2, what is the chance of survival
library(dplyr)
#Predict class category or probabilities
(testdata = sample_n(data,2))
predict(prunetree2, newdata=testdata, type='class')
predict(prunetree2, newdata=testdata, type='prob')
str(data)
testdata2 = data.frame(Pclass=factor(2), Sex=factor('male'), Age=15, SibSp=2)
testdata2
predict(prunetree2, newdata = testdata2, type='class')
predict(prunetree2, newdata = testdata2, type='prob')

#Use decision trees for predicting
#customer is likely to buy a product or not with probabilities
#customer is likely to default on payment or not with probabilities
#Student is likely to get selected, cricket team likely to win etc

#Imp steps
#select columns for prediction
#load libraries, create model y ~ x1 + x2 
#prune the tree with cp value
#plot the graph
#predict for new cases

#rpart, CART, classification model
#regression decision = predict numerical value eg sales

