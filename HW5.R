#############################
###### Excersise 21.4 #######
#############################
y<-rnorm(10000,mean=0,sd=1)
x=exp(y)
hist(x[x<40],breaks=200,freq=FALSE)
x1<-seq(0,30,by=0.01)
fx1<-(1/(x1*sqrt(2*pi)))*exp(-(log(x1))^2/2)
points(x1,fx1,type="l",col=2)


############################
##### Excersise 21.9 #######
############################

u<-runif(10000,0,1)
y=-2*(log(u))
hist(y,type="l",breaks=100,freq=FALSE)
y<-seq(0,20,by=0.01)
fy<-(-1/2)*exp(-y/2)
points(-1*fy~y,type="l")


x<-runif(1000,-1,1)
y<-ifelse(x>0,x,x*-1)








