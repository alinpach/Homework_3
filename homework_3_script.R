# Applied Economics (040178)
# Script Homework 3
# Iby 11701349
# Pacher 12026049


###################################################
# Exercise 1

library(tidyverse)
library(readxl)
pwt100 <- read_excel("pwt100.xlsx", sheet = "Data")
View(pwt100)

# creating a new data frame only with the relevant variables

data_reg <- data.frame(pwt100$country, pwt100$year, pwt100$rgdpna, pwt100$rnna, pwt100$rtfpna, pwt100$pop, pwt100$emp, pwt100$avh, pwt100$hc)
colnames(data_reg)[1] = "country"
colnames(data_reg)[2] = "year"
colnames(data_reg)[3] = "rgdpna"
colnames(data_reg)[4] = "rnna"
colnames(data_reg)[5] = "rtfpna"
colnames(data_reg)[6] = "pop"
colnames(data_reg)[7] = "emp"
colnames(data_reg)[8] = "avh"
colnames(data_reg)[9] = "hc"
data_reg

# filter year
data_2019 <- data_reg[data_reg$year == "2019",]
data_2019

## 1a #############################################

# linear regression model y=real GDP, x1=capital stock
rgdpna <- data_2019$rgdpna
rnna <- data_2019$rnna

reg_gdp_capital <- lm(rgdpna ~ rnna)
summary(reg_gdp_capital)

# The gross domestic product of a country is not only determined by the country's accessible capital.
# Other factors like population, employment, technology, human capital factors and productivity effect GDP as well.
# Therefore, the regression reg_gdp_capital has an omitted variable bias.


## 1b #############################################

# linear regression model y=real GDP, x1=capital stock, x2=population, x3=productivity
pop <- data_2019$pop
rtfpna <- data_2019$rtfpna

reg_gdp_capital_popul_product <- lm(rgdpna ~ rnna + pop + rtfpna)
summary(reg_gdp_capital_popul_product)


## 1c #############################################

plot(rgdpna, rnna, main = "GDP and capital stock",
     xlab = "real GDP at constant prices of 2017 (in million USD", ylab = "Capital stock at constant prices of 2017",
     pch = 1, frame = FALSE)
# relationship looks linear

plot(rgdpna, pop, main ="GDP and population",
     xlab="real GDP at constant prices of 2017 (in million USD", ylab="Population in Millions",
     pch = 1, frame = FALSE)
# relationship looks also linear bit with bigger variance


## 1d #############################################

# regression model y=log(real GDP), x1=log(capital stock), x2=log(population), x3=log(productivity)
log_rgdpna <- log(rgdpna)
log_rnna <- log(rnna)
log_pop <- log(pop)
log_rtfpna <- log(rtfpna)

reg_log <- lm(log_rgdpna ~ log_rnna + log_pop + log_rtfpna)
summary(reg_log)


## 1e #############################################

# H_0 = beta_1 + beta_2 = 0
# oder:
# H_1 = beta_1 + beta_2 = 1
# H_0 = beta_1 + beta_2 != 1
reg_log_coefficiants <- as.matrix(reg_log$coefficients)
reg_log_coefficiants

beta_1 <- reg_log_coefficiants[2]
beta_2 <- reg_log_coefficiants[3]

var_beta_1 <- vcov(reg_log)[2,2]
var_beta_2 <- vcov(reg_log)[3,3]
cov_beta_1_2 <- vcov(reg_log)[2,3]

se_beta_1_2 <- sqrt(var_beta_1 + var_beta_2 + 2*cov_beta_1_2)

t <- (beta_1 + beta_2 - 1)/se_beta_1_2
t

# für 5%iges Signifikanzniveau
t_statistic <- abs(t)
c.0025 <- 1.96
if((t_statistic > c.0025)){
  print("reject H_0")
  } else
    print("do not reject H_0")


beta_sum <- log_rnna + log_pop
beta_sum

reg_beta_sum <- lm(log_rgdpna ~ beta_sum)
summary(reg_beta_sum)
se_beta_sum <- 0.5641

reg_beta_sum$coefficients
beta_sum <- reg_beta_sum$coefficients[2]
beta_sum
t_2 <- beta_sum/se_beta_sum
t_2

# für 5%iges Signifikanzniveau
t_statistic_2 <- abs(t_2)
c.0025 <- 1.96
if((t_statistic_2 > c.0025)){
  print("reject H_0")
} else
  print("do not reject H_0")


## 1f #############################################

reg_log_rnna_pop <- lm(log_rgdpna ~ log_rnna + log_pop)
summary(reg_log_rnna_pop)

log_emp <- log(data_2019$emp)
reg_log_emp_pop <- lm(log_rgdpna ~ log_emp + log_pop)
summary(reg_log_emp_pop)

###################################################
# Exercise 2

## 2a #############################################

## specification (2)
## p-value for a two-sided hypothesis test 
2*(1-pt(2.24, df=173))

## specification (3)
## p-value for a two-sided hypothesis test 
2*(1-pt((.100/.049), df=171))

## 2b #############################################

## ceoten
## p-value for a two-sided hypothesis test 
2*(1-pt((.017/.006), df=171))

## comten
## p-value for a two-sided hypothesis test 
2*(1-pt(3, df=171))

## 2c #############################################

## 10% significance niveau 

cv10 <- qf(0.995, 2, 171)
cv10

## 5% significance niveau

cv5 <- qf(0.995, 2, 171)
cv5

## 1% significance niveau

cv1 <- qf(0.995, 2, 171)
cv1

## 2d #############################################