#Q1
pricefunction <- function(CV,FV,n,y){
  pn=-y[n]*n
  p2=FV*exp(pn)
  p1=0
  b=2*n
for (j in 1:b) {pj=-y[j]*(j)
  P1 <- CV*exp(pj)+p1
  }
  P=P1+P2
  return(p)
}


#Q3
library(readxl)
singapore_economy <- read_excel("C:/Users/X/Desktop/acct890/take home exam1/singapore.economy.xlsx")
View(singapore_economy)
dataset<-singapore_economy
dataset<-na.omit(dataset)
plot(dataset$gdp~dataset$time,xlab="Time",ylab = "GDP(%)",main="Singapore GDP growth")
mean<-tapply(dataset$gdp,dataset$period,mean)
standard_deviation<-tapply(dataset$gdp,dataset$period,sd)
period<-c(1,2,3)
stat.table<-data.frame(period,mean,standard_deviation)
pairs(dataset$gdp~dataset$exp+dataset$epg+dataset$hpr+dataset$gdpus+dataset$oil)
model1<-lm(gdp~exp,data=dataset)
summary(model1)
model2<-lm(gdp~exp+epg+hpr+oil+gdpus+crd,data=dataset)
summary(model2)
quantile(dataset$gdp,0.05)
dataset$state<-ifelse(dataset$gdp<=-2.518,0,1)
train<-(dataset$time<2008)
dataset.2007=dataset[train,]
model3<-glm(state~bci,data=dataset ,family =binomial ,subset =train)
glm.probs =predict (model3,dataset.2007 )
glm.pred=rep ("0 " ,72)
glm.pred[glm.probs >0.5]=" 1"
table(glm.pred,dataset.2007$state)
