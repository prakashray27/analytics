table(mtcars$cyl)
summary(mtcars$cyl)
library(dplyr)
mtcars%>% group_by(cyl)%>%tally()
xatbs(~cyl, data=mtcars)
ftable(mtcars$cyl)

?xtabs

#cyl and mpg
xtabs(~cyl,~mpg,data=mtcars)
table(mtcars$cyl, mtcars$mpg)
table(mtcars$cyl,mtcars$mpg, dnn=c('cylinder','mpg'))
mtcars%>%group_by(cyl,mpg)%>%tally()
mtcars%>%group_by(cyl,mpg)%>%summarise(COUNT=n())
mtcars%>%group_by(cyl,mpg)%>%summarise(COUNT=n())%>% as.data.frame()

df=mtcars
head(df)
tail(df)
df
(df$am=ifelse(df$am==0,'Auto','Manual'))
mtcars%>%mutate(TxType=ifelse(am==0,'Auto','Manual'))
mtcars%>%mutate(TxType=ifelse(am==0,'Auto','Manual'))%>%group_by(TxType)%>%summarise(COUNT=n())
mtcars
df=mtcars
df$mpg
df
df=df%>%mutate(TxType=ifelse(am==0,'Auto','Manual'))
head(df)

#increase mileage by 10%

df$mpg*1.1



#add mpg+wt in new col mpgwt

df$mpg+df$wt
df$mpg*1.1+df$wt
head(df)
df$MPGWT=df$mpg+df$wt
head(df)

df%>%group_by(gear)%>%top_n(n=2,wt=mpg)%>%arrange(gear)


#list out details of any 2 cars picked randomly: then do 25% of the cars

df%>%sample_frac(.25)

df%>%sample_frac(.25)%>%arrange(mpg)

df%>%sample_frac(.25)%>%arrange(gear,desc(-mpg))


#min max of weight milaeage and hp for each gear type
df %>%select(mpg,wt,gear)%>%group_by(gear)%>%summarise_each(c(min,max))




#graphs----

hist(df$mpg)
barplot(table(df$gear),col=1:3)
pie(table(df$gear))
plot(df$wt,df$mpg)

L1<-paste(round(table(df$gear)/nrow(df)*100,'%'))
library(ggplot2)
library(reshape2)

(rollno=paste('IIM',1:10,sep='_'))
(name=paste('SName',1:10,sep=' '))
(marketing=trunc(rnorm(10,mean=60,sd=10)))
(operations=trunc(rnorm(10,mean=62,sd=7)))
(finance=trunc(rnorm(10,mean=55,sd=12)))
(gender=sample(c('M','F'),size=10,replace=T))
(program=sample(c('BBA','MBA'),size=10,replace=T))
students<-data.frame(rollno,name,gender,program,marketing,operations,finance, stringsAsFactors =F)
students
head(students)








(meltSum1<-melt(students,id.vars=c('rollno','name')))
(dcastSum1<-dcast(meltSum1,rollno+name~variable,value.var='value'))
?recast
recast(data=students,rollno+name+gender~variable)
recast(data=students,gender~variable, fun.aggregate=length)
recast(data=students,gender~variable, fun.aggregate=mean)

students<-data.frame(rollno,name,gender,program,marketing,operations,finance, stringsAsFactors =F)
students
(meltsum1<-melt(students,id.vars=c('rollno','name','gender','program'),variable.name='subject',value.name = 'marks'))
(dcastSum1<-dcast(meltSum1, rollno+name~variable,value.var='value'))





sum2<-meltSum1%>%group_by(program,gender,subject)%>%summarise(Meanmarks=mean(marks))
head(students,n=2)
students%>%group_by(gender)%>%summarise(COUNT=n())
ggplot(students%>%group_by(gender)%>%summarise(COUNT=n()),aes(x=gender,y=COUNT))+geom_bar(stat='identity')
ggplot(students,aes(x=gender,y=..count..))+geom_bar(stat='count')
ggplot(students%>%group_by(gender)%>%summarise(COUNT=n()),aes(x=gender,y=COUNT,fill=gender))+geom_bar(stat='identity')
ggplot(students%>%group_by(gender)%>%summarise(COUNT=n()),aes(x=gender,y=COUNT,fill=gender))+geom_bar(stat='identity')+geom_text(aes(label=COUNT))
ggplot(students%>%group_by(gender)%>%summarise(COUNT=n()),aes(x=gender,y=COUNT,fill=gender))+geom_bar(stat='identity')+geom_text(aes(label=COUNT))+labs(title='Gender wise count')
ggplot(students%>%group_by(gender,program)%>%summarise(COUNT=n()),aes(x=gender,y=COUNT,fill=program))+geom_bar(stat='identity')+geom_text(aes(label=COUNT))+labs(title='Gender wise coun-Program Count')

ggplot(students%>%group_by(gender,program)%>%summarise(COUNT=n()),aes(x=gender,y=COUNT,fill=program))+geom_bar(stat='identity',position_dodge2((.7)))+geom_text(aes(label=COUNT),position = position_dodge2(.7))+labs(title='Gender wise coun-Program Count')


#subject-program-gender-mean marks
names(students)
names(meltsum1)
head(meltsum1)
#ggplot(meltsum1%>%group_by(program,gender,subject)%>%summarise(meanMarks=round(mean(marks))),aes(x=gender,y=meanMarks,fill=program))+geom_bar(stat='identity',position=position_dodge2(.7))+geom_text(aes(label=meanMarks),position=position_dodge2(.7))+labs(title='subject-program-gender-mean marks')+facet_grid(~subject)


ggplot(mtcars,aes(x=wt,y=mpg,size=hp,color=factor(gear),shape=factor(am)))+geom_point()
                                                                          
