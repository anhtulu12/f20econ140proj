library(broom)
library(knitr)
library(readr)
library(ggplot2)
library(stargazer)

#Replace path depending on user
data <- read_csv("/Users/renasong/Documents/github/f20econ140proj/data_cleaning/data_clean.csv")

#Draw scatterplots for EDA

ggplot(data=data,aes(x=testscore,y=houseprice))+
  geom_point()+
  xlab('Mean Test Score')+
  ylab('Median House Price')+
  ggtitle('California Test Scores (CAASPP) v. Median House Price')


data$recip.ces <- 1/data$ces
ggplot(data=data,aes(x=recip.ces,y=houseprice))+
  geom_point()+
  xlab('Reciprocal of CES Score')+
  ylab('Median House Price')+
  ggtitle('Pollution (CES) v. Median House Price')

ggplot(data=data,aes(x=ces,y=houseprice))+
  geom_point()+
  xlab('CES Score')+
  ylab('Median House Price')+
  ggtitle('Pollution (CES) v. Median House Price')

ggplot(data=data,aes(x=transitscore,y=houseprice))+
  geom_point()+
  xlab('Transit Performance Score')+
  ylab('Median House Price')+
  ggtitle('Transit Quality v. Median House Price')


#Regression 1: log(houseprice) on testscore
reg1 <- lm(log(data$houseprice) ~ data$testscore, data=data)
summary(reg1)

#Regression 2: log(houseprice) on transit score
reg2 <-  lm(log(data$houseprice) ~ data$transitscore, data=data)
summary(reg2)

#Regression 3: log(houseprice) on ces
reg3 <-  lm(log(data$houseprice) ~ data$ces, data=data)
summary(reg3)

#Regression 4: log(houseprice) on 1/ces
reg4 <-  lm(log(data$houseprice) ~ data$recip.ces, data=data)
summary(reg4)

#Regression 5: log(houseprice) on test score, transit score, ces, 1/ces
reg5 <-  lm(log(data$houseprice) ~ data$testscore + data$transitscore + 
              data$ces + data$recip.ces, data=data)
summary(reg5)





stargazer(reg1, reg2, reg3, reg4, reg5,
          covariate.labels = c("Test Score", "Transit Score", "CES", "1/CES"),
          out='results.doc',
          type='html',
          title='Results',
          align=TRUE)
