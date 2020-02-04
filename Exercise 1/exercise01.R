library(tidyverse)

#Data Visualization: flights at ABIA

head(AVIA)




# Regression Practice Number 1

summary(mod <- lm(creatclear ~ age, data = creatinine))
plot(creatclear ~ age, main = "creatclear x age", data = creatinine, col = "blue")

ggplot(data = creatinine) + 
  geom_point(mapping = aes(x = age, y = creatclear), color = "darkred", alpha = .6)

abline(mod,col = "red")

b <- coef(summary(mod))["(Intercept)", "Estimate"]
m <- coef(summary(mod))["age", "Estimate"]

#1
ffive_avg <- m * (55) + b

#2
print(m)
#On average, creatclearrate changes by a value of 
# -.6198159 ml/minute per year.

#3 
#40 year old w/ 135 | 60 year old w/ 112

fo_avg <- m * (40) + b
so_avg <- m * (60) + b
fo_pd <- (135 - fo_avg)/fo_avg
so_pd <- (112 - so_avg)/so_avg
print(fo_pd)
print(so_pd)
#Since the fourty year old is around 9.7 % higher than the average
#and the sixty year old is around 1.2% higher than the average at 
#their each relative age, the sixty year old is relatively healthier
#in their age group.

head(greenbuildings)
summary(greenbuildings)
xtabs(class_* ~ Rent, data = greenbuildings)
