
library("ggplot2")

range(Wage$age)
range(Wage$year)
range(Wage$wage)
#checking the maximum and minimum value for age, year and wage.

median(Wage$age)
median(Wage$year)
median(Wage$wage)
#checking the median for age, year and wage.

mean(Wage$age)
mean(Wage$year)
mean(Wage$wage)
#checking the mean for age, year and wage.

p1 <- ggplot(Wage, aes(x=health_ins.f, y=wage)) + 
  geom_boxplot()
p1

p2 <- ggplot(Wage, aes(x=maritl.f, y=wage)) + 
  geom_boxplot()
p2

p3 <- ggplot(Wage, aes(x=jobclass.f, y=wage)) + 
  geom_boxplot()
p3
#boxplot for health_ins vs wage, maritl va wage and jobclass vs wage

Wage$health_ins.f <- as.factor(Wage$health_ins)

Wage$health_ins = factor(Wage$health_ins, levels = c("No","Yes"))

print(class(Wage$health_ins.f))

Wage$jobclass.f <- as.factor(Wage$jobclass)

print(class(Wage$jobclass.f))

Wage$maritl.f <- as.factor(Wage$maritl)

print(class(Wage$maritl.f))

lmACS <- lm(wage ~ age + maritl.f, data = Wage)
print(summary(lmACS))
