## Homework1
## Question 3
install.packages("alr4")
library(alr4)
aismod<-lm(Wt~Sex+Ht+SSF+Bfat, data=ais)
summary(aismod)
which.max((residuals(aismod)))
fitted(aismod)[163]
confint(aismod,level=0.95)
newdata<-data.frame(Sex=1, Ht=170, SSF=60, Bfat=0.12)
predict(aismod, newdata,interval = "prediction")
aismod2<-lm(Wt~Sex+Ht, data=ais)
anova(aismod2, aismod)

## Question 4
fuelmod<-lm(FuelC~Income+Pop+Tax,data=fuel2001)
summary(fuelmod)
fuelmod2<-lm(FuelC~Income+Pop+Tax+Drivers,data=fuel2001)
summary(fuelmod2)
anova(fuelmod2, fuelmod)

