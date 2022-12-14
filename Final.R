library("ggplot2")
library(tidyverse)
install.packages("ggpubr")
library("ggpubr")
install.packages("ISLR2")
range(Wage$age)
range(Wage$year)
range(Wage$wage)
#checking the maximum and minimum value for age, year and wage.
Wage
median(Wage$age)
median(Wage$year)
median(Wage$wage)
#checking the median for age, year and wage.

mean(Wage$age)
mean(Wage$year)
mean(Wage$wage)
#checking the mean for age, year and wage.

#change dummy variables to .factor for later process.
Wage$health_ins.f <- as.factor(Wage$health_ins)

print(class(Wage$health_ins.f))

Wage$jobclass.f <- as.factor(Wage$jobclass)

print(class(Wage$jobclass.f))

Wage$maritl.f <- as.factor(Wage$maritl)

print(class(Wage$maritl.f))


#boxplots for health_ins vs wage, maritl vs wage and jobclass vs wage
p1 <- ggplot(Wage, aes(x=health_ins.f, y=wage)) + 
  geom_boxplot()
p1

p2 <- ggplot(Wage, aes(x=maritl.f, y=wage)) + 
  geom_boxplot()
p2

p3 <- ggplot(Wage, aes(x=jobclass.f, y=wage)) + 
  geom_boxplot()
p3

p4 <- ggplot(Wage, aes(x=age, y=wage)) + 
  geom_boxplot()
p4

p5 <- ggplot(Wage, aes(x=wage, y=health_ins))+geom_point()
p5
#linear regression for wage vs age, control maritl, health_ins and jobclass

lmACS <- lm(wage ~ age + maritl.f + health_ins.f + jobclass.f, data = Wage)
print(summary(lmACS))

lmACS1 <- glm(health_ins ~ age + maritl.f + wage + jobclass.f, data = Wage)
print(summary(lmACS1))

ggplot(Wage, aes(x = age, y = wage))+ geom_point() + stat_smooth()

ggplot(Wage, aes(age, wage))+ geom_point() + stat_smooth(method = lm)

# set facter for race dummy variable
Wage$race.f <- as.factor(Wage$race)

print(class(Wage$race.f))



newdata = as.numeric(as.factor(Wage$health_ins))
newdata
for(i in 1:length(newdata)){
  if (newdata[i] == 2){
    newdata[i] = 0
  }
}
newdata
newdata.f = as.factor(newdata)
#Logistic regression for health_ins vs jobclass, income, and race.
fvLogit <- glm(newdata.f ~ jobclass.f + wage + race.f + age, data = Wage, family = binomial)
summary(fvLogit)

fvLogit1 <- glm(newdata.f ~ wage, data = Wage, family = binomial)
summary(fvLogit1)

ggplot(Wage, aes(x=wage,y=newdata))+geom_point()+ stat_smooth(method="glm", color = "green", se = FALSE, method.args = list(family=binomial))

library(Hmisc)
cutWage = cut2(Wage$wage, g=3)
table(cutWage)

white_yes = 1740 / (1740+740)
white_no = 740/(1740+740)
black_yes = 197/(197+96)
black_no = 96/(197+96)
asian_yes = 126/(126+64)
asian_no = 64/(126+64)
other_yes = 20 / (20+17)

other_no = 17 / (20+17)

t1 = table(Wage$health_ins, Wage$race)
t2 = table(Wage$health_ins, Wage$jobclass)

cat_var <- c( white_yes, white_no,black_yes, black_no, asian_yes, asian_no, other_yes, other_no)
labels = c( "white_yes", "white_no","black_yes", "black_no", "asian_yes", "asian_no", "other_yes", "other_no")
pie(cat_var, labels, main = ("race and insurance pie chart"), coll = rainbow(length(cat_var)))


# Create a table from the data
cat <- table(cat_var)

# Pie
pie(cat,
    col = hcl.colors(length(cat), "BluYl"))


qplot(wage, color = health_ins, data = Wage, geom="density")
qplot(age, color = health_ins, data = Wage, geom="density")
qplot(race, color = health_ins, data = Wage, geom="density")
qplot(jobclass, color = health_ins, data = Wage, geom="density")

pl = qplot(cutWage, age, data = Wage, fill = cutWage, geom =c("boxplot"))
pl


#T-test for wage/insurance
#and T-test for age/insurance

af = Wage$wage[newdata == 0]
af1 = Wage$wage[newdata == 1]
t.test(af,af1)
af2 = Wage$age[newdata == 0]
af3 = Wage$age[newdata == 1]
t.test(af2,af3)

fvLogit2 <- glm(newdata.f ~ jobclass.f + wage + race.f + age + maritl.f + education, data = Wage, family = binomial)
summary(fvLogit2)

