# Homework 2
# Question 1
install.packages("alr4")
library("alr4")
stopmod<-lm(Distance~Speed,data=stopping)
plot(stopmod, add.smooth=FALSE)
max(residuals(stopmod))
min(residuals(stopmod))
which.max(hatvalues(stopmod))
hatvalues(stopmod)[61]
cooks.distance(stopmod)
which(cooks.distance(stopmod) >= 1)
plot(cooks.distance(stopmod), type="h")

# Question 2
drugmod<-lm(COST~RXPM+GS+RI+COPAY+AGE+F+MM,data=drugcost)
plot(drugmod, add.smooth=FALSE)
max(residuals(drugmod))
min(residuals(drugmod))
which.max(hatvalues(drugmod))
hatvalues(drugmod)[19]
cooks.distance(drugmod)
which(cooks.distance(drugmod) >= 1)
plot(cooks.distance(drugmod), type="h")

# Question 3
fuelmod<-lm(FuelC~Tax+Drivers+Income,data=fuel2001)
plot(residuals(fuelmod)~rstandard(fuelmod))
rstudent(fuelmod)
critval1<-qt(0.05/2,df=df.residual(fuelmod)-1, lower=FALSE)
which(abs(rstudent(fuelmod)) > critval1)
critval2<-qt(0.05/(2*nobs(fuelmod)),df=df.residual(fuelmod)-1, lower=FALSE)
which(abs(rstudent(fuelmod)) > critval2)

# Question4
par(pty="s")
par(mfrow=c(3,3))
for(i in 1:9){
  qqnorm(rgeom(50,0.4))
}
