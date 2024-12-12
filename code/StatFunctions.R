### R code from vignette source 'StatFunctions.Rnw'
rm(list=ls())
setwd("C:/Users/Slava/Dropbox/Documents/Teaching/MEES708N/6 Stat functions")
library(Jmisc)

booklengths <- scan("./dataRaw/Picbooks.dat")

length(booklengths)
hist(booklengths)
mean(booklengths)
median(booklengths)
summary(booklengths)
quantile(booklengths, probs = 0.5) == median(booklengths)
quantile(booklengths, probs = 0.75)

mu_hat <- mean(booklengths)
n <- length(booklengths)
sd_mu_hat <- sd(booklengths)/sqrt(n)

q <- qnorm(p = c(0.05, 0.95))
mu_hat+q*sd_mu_hat

QT <- qt(p = c(0.05, 0.95), df = n-1)
mu_hat+QT*sd_mu_hat
#


#Functions
FuncMean <- function(x) #arguments in parentheses
{
  #Some calculations
  M <- mean(x)
  print(paste("Mean is equal to ", M, sep=""))
}
FuncMean(booklengths)


#Control structures
for(i in 1:10){
  print(i)
}

for(i in 1:100){
  i
}

repeat{ #creates an inf. loop
  print(c(i+1))
}

i <- 1
repeat{ #creates an inf. loop
  if(i > 1000){
    break #stops the repeat() loop
  } else {
    i <- i+1
    if(i%%10==0) print(i)
  }
}

i <- 1
while(i<100){
  print(paste("i is exactly", i))
  i <- i + 1
}

M <- 0
i=1
while(M<=0){
  X <- rnorm(100)
  M <- mean(X)
  print(i)
  i=i+1
}


#
x <- booklengths
confLimit <- function(x,Alpha=0.05)
{
  mu <- mean(x,na.rm=T)
  sigma <- sd(x,na.rm=T)
  n <- sum(!is.na(x))
  se <- sigma/sqrt(n)
  tval <- qt(1-Alpha/2,df=n-1)
  r <- c(n,mu,median(x),
	  mu-tval*se,mu+tval*se)
  names(r)<-c("n","Mean",
	  "Median","lwr","upr")
#   cat("Summary of Picture 
# 	  Book Lengths\n")
  #r
  return(list(r=r, Mean=mu, StandDeviation=sigma))
}

confLimit(booklengths, 0.1)

tmp <- confLimit(booklengths, 0.1)
tmp$StandDeviation

#Works because R uses default Alpha=0.05
confLimit(booklengths)

#Names and order of arguments match exactly:
confLimit(booklengths, Alpha = 0.1)

#Alpha named, matched, next element is assigned to the fist argument in function specification, which is okay in this case:
confLimit(Alpha = 0.1, booklengths)

#Doesn't work, because R thinks, x=0.1, Alpha=booklength:
confLimit(0.1, booklengths) 


###################################################
### code chunk number 3: StatFunctions.Rnw:73-74
###################################################
confLimit(booklengths,alpha=0.10)


###################################################
### code chunk number 4: StatFunctions.Rnw:80-92
###################################################
skew <- function(x){
  n <- sum(!is.na(x))
  z <- (x-mean(x,na.rm=T))/sd(x,na.rm=T)
  n*sum(z^3)/(n-1)/(n-2)
}

kurt <- function(x){
  n <- sum(!is.na(x))
  c4n <- n*(n+1)/(n-1)/(n-2)/(n-3)
  z <- (x-mean(x,na.rm=T))/sd(x,na.rm=T)
  c4n*sum(z^4)-3*(n-1)^2/(n-2)/(n-3)
}


###################################################
### code chunk number 5: StatFunctions.Rnw:96-98
###################################################
skew(booklengths)
kurt(booklengths)


###################################################
### code chunk number 6: StatFunctions.Rnw:101-102
###################################################
quantile(booklengths,probs=c(0.05,0.95))


###################################################
### code chunk number 7: StatFunctions.Rnw:132-136
###################################################
bus <- as.data.frame(scan("Bus.dat",
  what=list(Bustype="",OnTimeOrLate="")))
count <- with(bus,table(Bustype,OnTimeOrLate))
chisq.test(count)


###################################################
### code chunk number 8: StatFunctions.Rnw:139-141
###################################################
chisq.test(count)$expected
count


###################################################
### code chunk number 9: StatFunctions.Rnw:147-151
###################################################
count <- matrix(c(6,4,2,8),ncol=2)
row.names(count) <- c("with","without")
colnames(count) <- c("TreeA","TreeB")
count


###################################################
### code chunk number 10: StatFunctions.Rnw:154-155
###################################################
chisq.test(count)$expected


###################################################
### code chunk number 11: StatFunctions.Rnw:163-164
###################################################
fisher.test(count)



###################################################
### code chunk number 12: StatFunctions.Rnw:192-197
###################################################
class <- as.data.frame(scan("Exercise.dat",
  what=list(Score=0,Television=0,Exercise=0)))
attach(class)
cor.test(Score,Television)
cor.test(Score,Exercise)


###################################################
### code chunk number 13: StatFunctions.Rnw:201-206
###################################################
cor.test(Score,Television,
 method="spearman")
cor.test(Score,Exercise,
 method="spearman")
detach(class)



###################################################
### code chunk number 15: StatFunctions.Rnw:259-274
###################################################
hits <- as.data.frame(scan("Baseball.dat", what=list(Height=0,Distance=0)))
hits.lm <- lm(Distance ~ Height, data=hits)
hits.lm
summary(hits.lm)

attach(hits)
with(hits,plot(Distance~Height,
  pch=21,col="blue",bg="red"))
abline(hits.lm,col="red")
fit <- hits.lm$fitted
joinFunc <- function(i)
  lines(c(Height[i],Height[i]),
  c(Distance[i],fit[i]),
  col="green")
tmp<-sapply(1:length(Distance),
  joinFunc)
detach(hits)


###################################################
### code chunk number 16: StatFunctions.Rnw:278-280
###################################################
anova(hits.lm)
coef(hits.lm)
coef(summary(hits.lm))
hist(residuals(hits.lm), 20)
hits.lm$residuals

#What if we need adjusted R^2
tmp <- summary(hits.lm)
ls(tmp)
tmp$adj.r.squared




###################################################
### code chunk number 17: StatFunctions.Rnw:323-326
###################################################
basket <- as.data.frame(scan("Basketball.dat",
  what=list(Team="",Height=0)))
basket.aov <- aov(Height~Team,data=basket)
basket.aov

###################################################
### code chunk number 18: StatFunctions.Rnw:329-330
###################################################
summary(basket.aov)
boxplot(Height~Team, data=basket)


###################################################
### code chunk number 19: StatFunctions.Rnw:334-336
###################################################
(out<-TukeyHSD(basket.aov,order=T))
out$Team[out$Team[,4]<0.05,]


out.lm <- lm(Height~Team, data=basket)
anova(out.lm)
summary(out.lm)

fcrit <- qf(p = 0.95, df1 =4 , df2=55 )

