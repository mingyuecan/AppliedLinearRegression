## Homework 6
## Exercise 1
BFHS<-read.table("/Users/Constance/Desktop/BFHS.dat",header=TRUE,skip=6)
BFHS
summary(BFHS)
t.test(BFHS$Intervention-BFHS$ExternalComparison,var.equal=TRUE)
tstats<-replicate(100000,t.test((BFHS$Intervention-BFHS$ExternalComparison)*sample(c(-1,1),13,replace=TRUE))$statistic)
t.observed<-t.test(BFHS$Intervention-BFHS$ExternalComparison)$statistic
approx.pval<-mean(abs(tstats)>=abs(t.observed))
approx.pval
hist(tstats, breaks=50, freq=FALSE, col="grey90", ylim=c(0,0.4),main="Randomization Distrib. (Sim.), with t.observed and t(12) density")
abline(v=c(t.observed,-t.observed), lty=2, col="red")
curve(dt(x, df=12), add=TRUE, col="blue")

## Exercise 2
Barley<-read.csv("/Users/Constance/Desktop/Barley1928.csv",header=TRUE)
table(Barley$Block)
class(Barley$Block)
class(Barley$Treatment)
Barleymod<-lm(Yield~Block+factor(Treatment), data=Barley)
summary(Barleymod)
anova(Barleymod)
TukeyHSD(aov(Yield~Block+factor(Treatment), data=Barley))$"factor(Treatment)"

## Exercise 3
Spelling<-read.csv("/Users/Constance/Desktop/Spelling1941.csv",header=TRUE)
Spelling
class(Spelling$List)
class(Spelling$Group)
class(Spelling$Testing)
matrix(Spelling$Testing,4,4)
Spellingmod<-lm(Number~List+factor(Group)+Testing, data=Spelling)
summary(Spellingmod)
anova(Spellingmod)
TukeyHSD(aov(Number~List+factor(Group)+Testing, data=Spelling))$Testing
