## Question 1
library(alr4)
pairs(MinnWater)
minmod1<-lm(muniUse~., data=MinnWater)
summary(minmod1)
vif(minmod1)
minmod2<-lm(muniUse~allUse+irrUse+muniPrecip+statePop, data=MinnWater)
summary(minmod2)
vif(minmod2)

##Question 2
install.packages("faraway")
library(faraway)
fit1<-lm(crawling~temperature, data=crawl)
summary(fit1)
confint(fit1,"temperature")
fit2<-lm(crawling~temperature, weights=n,data=crawl)
summary(fit2)
confint(fit2, "temperature")
fit3<-lm(crawling~temperature, weights=n/(SD^2),data=crawl)
summary(fit3)
32/(7.08^2)
1/(32/(7.08^2))
confint(fit3, "temperature")

##Question 3
lakemod1<-lm(Length~Age, data=lakemary)
summary(lakemod1)
plot(lakemary$Age, lakemary$Length)
abline(lakemod1)
lakemod2<-lm(Length~factor(Age), data=lakemary)
anova(lakemod1,lakemod2)
lakemod3<-lm(Length ~ Age + I(Age^2), data=lakemary)
summary(lakemod3)
anova(lakemod3,lakemod2)

unweighted_res<-crawl$crawling-fitted(fit2)
residuals(fit2)
diag(crawl$n)^(1/2)%*%unweighted_res
