## Homework 7
##1
Views<-read.table("/Users/Constance/Downloads/viewcounts.dat",header=TRUE)
viewmod<-lm(log(log(Views))~Channel, data=Views)
anova(viewmod)
library(lme4)
views.reml<-lmer(log(log(Views))~(1|Channel),data=Views)
summary(views.reml)
0.03475/(0.03475+0.0717)
views.ml<-lmer(log(log(Views))~(1|Channel),data=Views,REML=FALSE)
summary(views.ml)
0.02723
nullmod<-lm(log(log(Views))~1,data=Views)
llrts<-as.numeric(2*(logLik(views.ml)-logLik(nullmod)))
llrts
pchisq(llrts,1,lower=FALSE)
set.seed(1)
lrstats <- numeric(10000)
for(i in 1:10000){
  y <- unlist(simulate(nullmod))
  nullsim <- lm(y ~ 1)
  altsim <- lmer(y ~ (1|Channel), data=Views, REML=FALSE)
  lrstats[i] <- as.numeric(2 * (logLik(altsim) - logLik(nullsim)))
}
pval <- mean(lrstats >= llrts)
pval
se.pval <- sqrt(pval*(1-pval)/10000)
se.pval
ranef(views.reml)

##2
library(faraway)
choccake
choccake$batches<-choccake$recipe:choccake$batch
choccake$batches
choccake.reml<-lmer(breakang~recipe*factor(temp)+(1|batches),data=choccake)
anova(choccake.reml)
drop1(choccake.reml,test="Chisq")
drop1(update(choccake.reml,.~.-recipe:factor(temp)),test="Chisq")
choccake.aov<-aov(breakang~recipe*factor(temp)+Error(batches),data=choccake)
summary(choccake.aov)

##3
library(alr4)
shlogit<-glm(cbind(Y,m-Y)~Intensity, family=binomial,data=shocks)
summary(shlogit)
with(shocks, plot(Intensity, Y/m,xlab="Intensity",ylab="Prob.positive"))
curve(predict(shlogit, data.frame(Intensity=x), type="response"), add=TRUE,lty=1, col="blue")
fitted(shlogit)
residuals(shlogit)
prob<-predict(shlogit, newdata=data.frame(Intensity=1.5), type="response")
odds<-prob/(1-prob)
odds
pchisq(deviance(shlogit), df.residual(shlogit), lower=FALSE)
X.2 <- sum(residuals(shlogit,type="pearson")^2)
X.2
pchisq(X.2, df.residual(shlogit), lower=FALSE)
shprobit <- glm(cbind(Y,m-Y)~Intensity, family=binomial(link=probit),data=shocks)
summary(shprobit)
pchisq(deviance(shprobit), df.residual(shprobit), lower=FALSE)
X.2 <- sum(residuals(shprobit,type="pearson")^2)
X.2
pchisq(X.2, df.residual(shprobit), lower=FALSE)


