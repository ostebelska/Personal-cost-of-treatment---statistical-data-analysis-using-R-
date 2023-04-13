dane <- read.csv("insurance.csv",stringsAsFactors = TRUE)

summary(dane)

plot(dane$charges,dane$bmi)
cor(dane$charges,dane$bmi)

cor.test(dane$charges,dane$bmi)

plot(dane$charges,dane$age)
cor(dane$charges,dane$age)

cor.test(dane$charges,dane$age)

srednia <- mean(dane$charges)
wariancja <- var(dane$charges)
odch.std <- sd(dane$charges) 
Me <- median(dane$charges) 
kwantyle <- quantile(dane$charges) 
zakres <- range(dane$charges) 

install.packages("moments")
install.packages("modeest")

library(moments)
As <- skewness(dane$charges)
K <- kurtosis(dane$charges)

library("modeest")
Mo <- mfv(dane$charges)

statystyki.opisowe <- cbind(srednia,odch.std,wariancja,Me,Mo,As,K)
kwantyle <- cbind(kwantyle)
zakres <- cbind(zakres)

install.packages("ggplot2")
library(ggplot2)

ggplot(dane,aes(x=charges, fill=sex))+
  geom_boxplot(outlier.colour="darkred", outlier.shape=4, outlier.size=4, coef=1.5, alpha=0.5)+
  facet_wrap(smoker~sex)

ggplot(dane,aes(x=charges))+
  geom_boxplot(outlier.colour="darkred", outlier.shape=4, outlier.size=4, coef=1.5, fill="cornflowerblue")


ggplot(dane,aes(x=charges, fill=sex))+
  geom_histogram(breaks=seq(1121.874,63770.428,1690), color="black", closed="right")+
  scale_x_continuous(breaks=seq(1122,63771,3380))

ggplot(dane,aes(x=charges, fill=smoker))+
  geom_histogram(breaks=seq(1121.874,63770.428,1690), color="black", closed="right")+
  scale_x_continuous(breaks=seq(1122,63771,6760))+
  facet_wrap(~smoker)+
  scale_fill_brewer(palette = "Dark2") #+

ggplot(dane,aes(x=charges, fill=sex))+
  geom_histogram(breaks=seq(1121.874,63770.428,1690), color="black", closed="right")+
  scale_x_continuous(breaks=seq(1122,63771,6760))+
  facet_grid(smoker~sex)

qqnorm(dane$charges)
qqline(dane$charges,col=2,lwd=2)
shapiro.test(dane$charges)

library(dplyr)
library(GGally)
dane2 <- select(dane,-sex,-smoker, -region)
ggcorr(dane2, label=T)

model <- lm(charges~. ,dane2)
summary(model)

cbind(cor.test(dane2$charges,dane2$age)$p.value,
      cor.test(dane2$charges,dane2$bmi)$p.value,
      cor.test(dane2$charges,dane2$children)$p.value)

plot(dane2$charges, model$residuals, ylab='Rezydua')
abline(h=0, lty=2, col="green")

qqnorm(model$residuals)
qqline(model$residuals,col='red')
shapiro.test(model$residuals)

mean(model$residuals)

install.packages("lmtest")
library(lmtest)
bptest(model)

library(car)
vif(model)

nowe <- data.frame(age=35, bmi=22, children =4)
predict(model,nowe)

install.packages('olsrr')
library(Metrics)
library(olsrr)

summary(model)
model$coefficients

model_min <- lm(dane$charges~1,dane2)
model_forward <- step(model_min,scope = list(lower = model_min,upper = model), direction="forward")


proba <- c(10355.641,24227.337,17352.680,41097.162,17179.522,12928.791,21978.677,5253.524,20177.671)
t.test(proba, mu=13270.42)
odch.std1 <- sd(proba)
srednia1 <- mean(proba)

18950.11-10200.64/sqrt(9)*qnorm(.975)
18950.11+10200.64/sqrt(9)*qnorm(.975)
