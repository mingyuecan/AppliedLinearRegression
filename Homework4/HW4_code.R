## Homework 4
## 1
install.packages('alr4')
library(alr4)
lathe1mod<-lm(Life~Feed+Speed+I(Feed^2)+I(Speed^2)+Feed*Speed, data=lathe1)
summary(lathe1mod)
par(mfrow=c(2,2))
plot(lathe1mod)

library(MASS)
boxcox(lathe1mod)
lathe1mod2<-boxcox(lathe1mod, plotit=FALSE)
lathe1mod2$x[which.max(lathe1mod2$y)]
lathe1mod3<-lm(log(Life)~Feed+Speed+I(Feed^2)+I(Speed^2)+Feed*Speed, data=lathe1)
summary(lathe1mod3)
par(mfrow=c(2,2))
plot(lathe1mod3)

##2
treesmod<-lm(log(Volume)~log(Girth)+log(Height), data=trees)
summary(treesmod)
plot(treesmod)
confint(treesmod)
newtree<-data.frame(Girth=15.5, Height=83)
loginterval<-predict(treesmod, newdata=newtree, interval="prediction")
loginterval
exp(loginterval)

##3
aismod1<-lm(Bfat~1, data=ais)
indep.vars<-~Sex+Ht+Wt+LBM+BMI+SSF
add1(aismod1,indep.vars,test='F' )
aismod1<-update(aismod1, .~.+SSF)
add1(aismod1,indep.vars,test='F' )
aismod1<-update(aismod1, .~.+Sex)
add1(aismod1,indep.vars,test='F' )

aismod2<-lm(Bfat~Sex+Ht+Wt+LBM+BMI+SSF, data=ais)
drop1(aismod2, test='F')
aismod2<-update(aismod2, .~.-BMI)
drop1(aismod2, test='F')

install.packages('leaps')
library(leaps)
bestmods<-regsubsets(Bfat~Sex+Ht+Wt+LBM+BMI+SSF,data=ais,nbest=1, nvmax=6)
summary(bestmods)

step(aismod1,indep.vars)
