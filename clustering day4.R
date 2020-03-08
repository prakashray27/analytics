#clustering----
set.seed(1234)
subject1=trunc(rnorm(30,mean=60,sd=15))
range(subject1)
subject1
marks=data.frame(subject1)
head(marks)
marks
sort(marks$subject1)
k2=kmeans(marks,centers=2)
k2
k2$size
k2$iter
cbind(marks,k2cluster) #which data row into which cluster
length(marks[k2$cluster==1,]) #number of data points in cluster 1
marks[k2$cluster==2,] #actual data in cluster 2
marks[k2$cluster==1,] #actual data in cluster 1
mean(marks[k2$cluster==1,])
mean(marks[k2$cluster==2,])
k2$centers



k2a=kmeans(marks,centers=c(50,70))
k2a
k2a$centers



#clustering mtcars----
mtcars
head(mtcars)
table(mtcars$cyl)
mtscaled<-as.matrix(scale(mtcars))
heatmap(mtscaled,Colv = F,scale = 'none')



clustMTC= kmeans(mtcars[,c('wt','mpg')],center=3)
plot(x=mtcars$wt,y=mtcars$mpg,col=clustMTC$custer)





