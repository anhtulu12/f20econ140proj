library(broom)
library(knitr)
library(readr)
library(ggplot2)
library(stargazer)

#Replace path depending on user
data <- read_csv("/Users/renasong/Documents/github/f20econ140proj/data_cleaning/data_clean.csv")

#Draw scatterplots for EDA: no linear relationships

ggplot(data=data,aes(x=testscore,y=houseprice))+
  geom_point()+
  xlab('Mean Test Score')+
  ylab('Median House Price')+
  ggtitle('California Test Scores (CAASPP) v. Median House Price')

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

#plots of nonlinear relationships

ggplot(data=data,aes(x=testscore,y=log(houseprice)))+
  geom_point()+
  xlab('Mean Test Score')+
  ylab('log of Median House Price')+
  ggtitle('California Test Scores (CAASPP) v. Median House Price')

ggplot(data=data,aes(x=ces,y=log(houseprice)))+
  geom_point()+
  xlab('CES Score')+
  ylab('log of Median House Price')+
  ggtitle('Pollution (CES) v. Median House Price')

ggplot(data=data,aes(x=transitscore,y=log(houseprice)))+
  geom_point()+
  xlab('Transit Performance Score')+
  ylab('log of Median House Price')+
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


#Regression 4: log(houseprice) on test score, transit score, ces
reg4 <-  lm(log(data$houseprice) ~ data$testscore + data$transitscore + 
              data$ces, data=data)
summary(reg4)





stargazer(reg1, reg2, reg3, reg4,
          covariate.labels = c("Test Score", "Transit Score", "CES"),
          out='results.doc',
          type='html',
          title='Results',
          align=TRUE)
