library(broom)
library(knitr)
library(readr)
library(ggplot2)

#Replace path depending on user
data <- read_csv("/Users/renasong/Documents/github/f20econ140proj/R/data.csv")

#Draw scatterplots for EDA

'Median House Price v. Mean Test Score'
ggplot(data=data,aes(x=testscore,y=houseprice))+
  geom_point()+
  xlab('Mean Test Score')+
  ylab('Median House Price')+
  ggtitle('California Test Scores (CAASPP) v. Median House Price')


#Regression 1: houseprice on testscore
reg1 = lm(data$houseprice ~ data$testscore, data=data)
summary(reg1)

#Regression 2: houseprice on testscore and ces
reg2 = lm(data$houseprice ~ data$testscore + data$inspectionscore, data=data)
summary(reg2)

#Regression 2: log(houseprice) on testscore and ces
reg2 = lm(log(data$houseprice) ~ data$testscore + data$inspectionscore, data=data)
summary(reg2)
