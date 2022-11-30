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

ggplot(Wage, aes(x = age, y = wage))+ geom_point() + stat_smooth()

ggplot(Wage, aes(age, wage))+ geom_point() + stat_smooth(method = lm)
#Call:
#lm(formula = wage ~ age + maritl.f + health_ins.f + jobclass.f, 
#   data = Wage)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-98.858 -22.411  -5.498  14.786 203.918 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               87.44733    2.82834  30.918  < 2e-16 ***
#  age                        0.26432    0.06768   3.905 9.61e-05 ***
#  maritl.f2. Married        19.75629    1.89221  10.441  < 2e-16 ***
#  maritl.f3. Widowed         0.05845    8.88904   0.007    0.995    
#maritl.f4. Divorced        2.80503    3.20022   0.877    0.381    
#maritl.f5. Separated       3.87483    5.36463   0.722    0.470    
#health_ins.f2. No        -23.37665    1.53035 -15.275  < 2e-16 ***
#  jobclass.f2. Information  12.91943    1.40117   9.220  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 37.82 on 2992 degrees of freedom
#Multiple R-squared:  0.1807,	Adjusted R-squared:  0.1788 
#F-statistic: 94.27 on 7 and 2992 DF,  p-value: < 2.2e-16





# set facter for race dummy variable
Wage$race.f <- as.factor(Wage$race)

print(class(Wage$race.f))


#Logistic regression for health_ins vs jobclass, income, and race.
fvLogit <- glm(health_ins.f ~ jobclass.f + wage + race.f, data = Wage, family = binomial)
summary(fvLogit)


#Call:
#glm(formula = health_ins.f ~ jobclass.f + wage + race.f, family = binomial, 
#    data = Wage)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6389  -0.8587  -0.6021   1.0677   3.4985  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               1.890894   0.161964  11.675  < 2e-16 ***
#  jobclass.f2. Information -0.403310   0.087109  -4.630 3.66e-06 ***
#  wage                     -0.024572   0.001568 -15.675  < 2e-16 ***
#  race.f2. Black            0.001430   0.139626   0.010   0.9918    
#race.f3. Asian            0.380410   0.175864   2.163   0.0305 *  
#  race.f4. Other            0.276649   0.353979   0.782   0.4345    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 3693.5  on 2999  degrees of freedom
#Residual deviance: 3288.9  on 2994  degrees of freedom
#AIC: 3300.9

#Number of Fisher Scoring iterations: 5


