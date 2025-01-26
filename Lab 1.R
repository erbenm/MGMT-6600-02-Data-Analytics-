#Import data
View(epi2024results06022024)
EPI_Data <- epi2024results06022024

#New Variable EPI.new
EPI.new # prints out values EPI_Data$EPI.new
NAs <- is.na(EPI.new) # records True values if the value is NA 
EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array 

#Exercise 1: exploring the distribution

summary(EPI.new) # stats 
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new)
boxplot(EPI.new, APO.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw=1.)) 
rug(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ")) 
rug(EPI.new) 
x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

# Exercise 2: fitting a distribution beyond histograms 

plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#New Variable APO.new
APO.new
NAs <- is.na(APO.new) # records True values if the value is NA 
APO.new.noNAs <- APO.new[!NAs] # filters out NA values, new array 

#Exercise 1a: exploring the distribution

summary(APO.new) # stats 
fivenum(APO.new,na.rm=TRUE) 
stem(APO.new) # stem and leaf plot 
hist(APO.new)
hist(EPI.new, seq(10., 90., 1.0), prob=TRUE) 
lines(density(APO.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(APO.new)
boxplot(EPI.new, APO.new) 
hist(APO.new, seq(5., 100., 1.0), prob=TRUE)
lines (density(APO.new,na.rm=TRUE,bw=1.)) 
rug(APO.new)
hist(APO.new, seq(5., 100., 1.0), prob=TRUE)
lines (density(APO.new,na.rm=TRUE,bw="SJ")) 
rug(APO.new) 
x<-seq(10,90,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

# Exercise 2a: fitting a distribution beyond histograms 

plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(APO.new); qqline(APO.new) 
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn") 
qqline(APO.new)
qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn") 
qqline(APO.new)

# New Variable AIR.new
AIR.new
NAs <- is.na(AIR.new)
AIR.new.noNAs <- AIR.new[!NAs]

#Exercise 1b: exploring the distribution

summary(AIR.new)
fivenum(AIR.new,na.rm=TRUE) 
stem(AIR.new)
hist(AIR.new)
hist(AIR.new, seq(5., 100., 1.0), prob=TRUE) 
lines(density(AIR.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(AIR.new)

boxplot(AIR.new, EPI.new) 
hist(AIR.new, seq(5., 100., 1.0), prob=TRUE)
lines (density(AIR.new,na.rm=TRUE,bw=1.)) 
rug(AIR.new)
hist(AIR.new, seq(5., 100., 1.0), prob=TRUE)
lines (density(AIR.new,na.rm=TRUE,bw="SJ")) 
rug(AIR.new) 
x<-seq(10,90,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

# Exercise 2b: fitting a distribution beyond histograms 

plot(ecdf(AIR.new), do.points=FALSE, verticals=TRUE) 
qqnorm(AIR.new); qqline(AIR.new) 
qqplot(rnorm(250), AIR.new, xlab = "Q-Q plot for norm dsn") 
qqline(AIR.new)
qqplot(rt(250, df = 5), AIR.new, xlab = "Q-Q plot for t dsn") 
qqline(AIR.new)



