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

# creating a new dataframe only with the relevant variables

data_reg <- data.frame(pwt100$rgdpna, pwt100$rnna, pwt100$rtfpna, pwt100$pop, pwt100$emp, pwt100$avh, pwt100$hc)
colnames(data_reg)[1] = "rgdpna"
colnames(data_reg)[2] = "rnna"
colnames(data_reg)[3] = "rtfpna"
colnames(data_reg)[4] = "pop"
colnames(data_reg)[5] = "emp"
colnames(data_reg)[6] = "avh"
colnames(data_reg)[7] = "hc"
data_reg

## 1a #############################################

# linear regression model y=real GDP, x1=capital stock
rgdpna <- data_reg$rgdpna
rnna <- data_reg$rnna

reg_gdp_capital <- lm(rgdpna ~ rnna)
summary(reg_gdp_capital)

# The gross domestic product of a country is not only detrement by the country's accessable capital.
# Other factors like population, empolyment, technology, human capital factors and prodictivity effect GDP as well.
# Therefore the regression reg_gdp_capital has high omitted variable bios.


## 1b #############################################

# linear regression model y=real GDP, x1=capital stock, x2=population, x3=productivity
pop <- data_reg$pop
rtfpna <- data_reg$rtfpna

reg_gdp_capital_popul_product <- lm(rgdpna ~ rnna + pop + rtfpna)
summary(reg_gdp_capital_popul_product)


## 1c #############################################

plot(rgdpna, rnna, main = "GDP and capital stock",
     xlab = "real GDP at constant prices of 2017 (in million USD", ylab = "Capital stock at constant prices of 2017",
     pch = 19, frame = FALSE)
abline(lm(rgdpna ~ rnna), col = "blue")
# regression line or data points not correct!!! -> needs to be changed

plot(rgdpna, pop, main="GDP and population",
     xlab="real GDP at constant prices of 2017 (in million USD", ylab="Population in Millions",
     pch = 19, frame = FALSE)


## 1d #############################################

# regression model y=log(real GDP), x1=log(capital stock), x2=log(population), x3=log(productivity)
reg_log_gdp_capital_popul_product <- lm(log(rgdpna) ~ log(rnna) + log(pop) + log(rtfpna))
summary(reg_log_gdp_capital_popul_product)


## 1e #############################################


###################################################