## Homework5
library(alr4)
library(MASS)
ais_mod<-lm(Wt~Ht,data=ais)
boxcox(ais_mod)
ais_mod1<-lm(log(Wt)~Ht, data=ais)
sum(ais_mod1$residuals^2)
ais_mod2<-lm(log(Wt)~log(Ht), data=ais)
sum(ais_mod2$residuals^2)
ais_mod3<-lm(log(Wt)~log(Ht)+Sex+log(Ht)*Sex,data=ais)
summary(ais_mod3)
plot(log(Wt)~log(Ht), type="n", data=ais)
with(ais, points(log(Ht[Sex==0]), log(Wt[Sex==0]), pch=1, col="red", cex=0.8))
with(ais, points(log(Ht[Sex==1]), log(Wt[Sex==1]), pch=2, col="blue", cex=0.8))
beta00<--8.4766
beta01<--8.4766-1.0036
beta10<-2.4661
beta11<-2.44661+0.1836
abline(beta00, beta10, col="red",lwd=2, lty=1)
abline(beta01, beta11, col="blue",lwd=2, lty=2)
text(5.3, 4.7, "Sex=0", col="red", cex=1.0)
text(5.1, 3.8, "Sex=1", col="blue", cex=1.0)
ais_mod4<-lm(log(Wt)~log(Ht)+Sex,data=ais)
summary(ais_mod4)
plot(log(Wt)~log(Ht), type="n", data=ais)
with(ais, points(log(Ht[Sex==0]), log(Wt[Sex==0]), pch=1, col="red", cex=0.8))
with(ais, points(log(Ht[Sex==1]), log(Wt[Sex==1]), pch=2, col="blue", cex=0.8))
beta00<--9.00810
beta01<--9.00810-0.04972
beta1<-2.56790
abline(beta00, beta1, col="red",lwd=2, lty=1)
abline(beta01, beta1, col="blue",lwd=2, lty=2)
text(5.3, 4.7, "Sex=0", col="red", cex=1.0)
text(5.1, 3.8, "Sex=1", col="blue", cex=1.0)

## 2
table(turk0$A)
turk0_mod=lm(Gain~factor(A), data = turk0)
summary(turk0_mod)
par(mfrow=c(2,2))
plot(turk0_mod, add.smooth=FALSE)
anova(turk0_mod)
TukeyHSD(aov(Gain~factor(A),data=turk0))

## 3
pine=read.table("~/Downloads/pine.dat", header = TRUE)
table(pine[,c(1,2)])
pine_mod<-lm(y~factor(shape)+factor(trt)+factor(shape)*factor(trt), data=pine)
summary(pine_mod)
par(mfrow=c(2,2))
plot(pine_mod,add.smooth=FALSE)
boxcox(pine_mod)
pine_mod1<-lm(sqrt(y)~factor(shape)+factor(trt)+factor(shape)*factor(trt), data=pine)
summary(pine_mod1)
interaction.plot(pine$shape,pine$trt,sqrt(pine$y),trace.label='acid treatment', 
                 xlab='shape',ylab='sqrt(y)')
anova(pine_mod1)
