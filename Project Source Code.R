setwd("C:/Users/Emal Ismail/Desktop/AI/University/2019 - Fall/ITC - 255 - Statistical Data Analysis/Project - Emal Ismail/Project 1 - univariable data analysis")
library(frequency)
library(plyr)
library(Hmisc)
library(xtable)
library(DescTools)
library(e1071)

data=read.csv("Emal Ismail.csv")
data=as.data.frame(data)
View(data)

##-----------------------------------------
##------- Analysis of Categorical Varibales
##-----------------------------------------


access_net=data$Access.to.internet.0.no.1.yes.
Generator=data$Generator.0.no.1.yes
solar_panel=data$Solar.panel.0.no.1.yes
main_oc=data$Main.OCC.Farming.0.No.1.Yes
literacy=data$Literate.0.no.1.yes
provience=data$Province

freq_fun = function (x){
  

Table=count(x)
Table$freq
percent=(Table$freq/sum(Table$freq))*100
percent

Table=cbind(Table,percent)
Table
Table=rename(Table, c("x"="Catagory", "freq"="Counts", 
                      "percent"="Percent"))
Table

}

freq_fun(data$Province)
freq_fun(access_net)
freq_fun(Generator)
freq_fun(solar_panel)
freq_fun(literacy)
freq_fun(main_oc)


# Pie chart for solar panel


catagory=c("Yes", "No"); 
Percent=c(6.73,93.27); 
#par(mfrow=c(1,1))
pie(x=Percent, labels = c("[1401] 93.27%", "[101] 6.73%") ,
    col = c('white','grey'), 
    main = "Solar Panel", 
    radius = 0.9) 
#rainbow(length(catagory)
legend(-1.7,1.2,"left",legend=catagory, fill=c('white','grey'))

# pie chart for Generator

catagory=c("Yes", "No"); 
Percent=c(9.86,90.14); 
#par(mfrow=c(1,1))
pie(x=Percent, labels = c("[1354] 90.14%", "[148] 9.86%") ,
    col = c('white','grey'), 
    main = "Geneator", 
    radius = 0.9) 
#rainbow(length(catagory)
legend(-1.7,1.2,"left",legend=catagory, fill=c('white','grey'))



##---------------------------------------------
##------- End of categorical Variables Analysis
##---------------------------------------------

##----------------------------------------------------
##------- Analysis of Numerical and Discrete varibales
##----------------------------------------------------

### Net Income data analysis for outliers and visualizaiton

live_stock=data$Livestock.Net.Income

live_stock_min_max=c(min(live_stock), max(live_stock))
live_stock_min_max
boxplot(live_stock, horizontal = T)
live_stock_outfree=live_stock[live_stock<110000 & live_stock > -25000]
live_stock_outfree

boxplot(live_stock_outfree, horizontal = T)
hist(live_stock_outfree)


live_stock_outfree
## hist for livestock income

hist(live_stock_outfree,
     main="Farmers Livestock Net Income",
     xlab="Livestock Net Income in AFs",
     col="chocolate",
     border="brown"
)

# skewness and Kurtosis of live stock for caption of Histogram and explaination of outliers:
testfun = function(x,y){
  
  re = skewness(x)
  ku = kurtosis(x)
  
  re1 = skewness(y)
  ku1 = kurtosis(y)
  
  if (re < -1 || re > 1){
    cat(" Based on the skewness of first dataset which is equal to: ", re , "\
and also shown in the first graph we can conclude that the distribution seems to be highly skewed.")
  } else if(re > -1 && re < -0.5){
    cat(" Based on the skewness of first dataset which is equal to: ", re , "\
and also shown in the first graph we can conclude that the distribution is moderatley Skewed. \
Where it has long left tail therefore, it is Left-skewed distributions also called negatively-skewed distribution.")
  } else if (re >0.5 && re <1){
    cat(" Based on the skewness of first dataset which is equal to: ", re ,"\
and also shown in the first graph we can conclude that the distribution is moderatley Skewed. \
Where it has long right tail therefore it is Right-skewed distributions a also called positive-skew distribution.")
  }else if(re > -0.5 && re < 0.5){
    cat(" Based on the skewness of first dataset which is equal to: ", re ,"\
and also shown in the first graph we can conclude that the distribution is approximately Symmetric.")
  }
  
  if (ku > -2 && ku < 2){
    cat("\n")
    cat("Subsequently, kurtosis of first dataset which is equal to: ", ku ,"is between -2 and +2 and it is considered acceptable in\
order to prove normal univariate distribution")
  } else {
    cat("\n")
    cat("Subsequently, kurtosis of first dataset which is equal to: ", ku ,"is greater or less than -2 and +2. \
Therefore, it's not considered to be acceptable in order to prove normal univariate distribution.")
  }
  
  
  ##------------------------- Second Dataset analysis
  cat("\n")
  cat("\n")
  cat("-------------------------------------------------------------------")
  cat("\n")
  cat("Furthermore the second datasets Skewness and Kurtosis is as follow:  ")
  cat("\n")
  
  if (re1 < -1 || re1 > 1){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 , "\
and also shown in the second graph we can conclude that the distribution seems to be highly skewed.")
  } else if(re1 > -1 && re1 < -0.5){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 , "\
and also shown in the second graph we can conclude that the distribution is moderatley Skewed. \
Where it has long left tail therefore, it is Left-skewed distributions also called negatively-skewed distribution.")
  } else if (re1 >0.5 && re1 <1){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 ,"\
and also shown in the second graph we can conclude that the distribution is moderatley Skewed. \
Where it has long right tail therefore it is Right-skewed distributions a also called positive-skew distribution.")
  }else if(re1 > -0.5 && re1 < 0.5){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 ,"\
and also shown in the second graph we can conclude that the distribution is approximately Symmetric.")
  }
  
  if (ku1 > -2 && ku1 < 2){
    cat("\n")
    cat("Subsequently, kurtosis of second dataset which is equal to: ", ku1 ,"is between -2 and +2 and \
it is considered acceptable in order to prove normal univariate distribution.")
  } else {
    cat("\n")
    cat("Subsequently, kurtosis of second dataset which is equal to: ", ku1 ,"is greater or less than -2 and +2. \
Therefore, it's not considered to be acceptable in order to prove normal univariate distribution.")
  }
  
  par(mfrow=c(2,1))
  plot(density(x), col= "red")
  plot(density(y), col= "darkgreen")
}

testfun(motorBike,TV)


# add a normal distribution line in histogram
figure
hist(live_stock_outfree, freq=FALSE, 
     col="gray", 
     xlab="Livestock Net Income in AFGs",
     ylab = "Density",
     main="Livestock Net Income without outliers")

curve(dnorm(x, mean=mean(live_stock_outfree), 
            sd=sd(live_stock_outfree)), 
      add=TRUE, col="red") 

#png(filename = "Hist_LiveStock.png")
##----------- with outliers
dev.off()

hist(live_stock, freq=FALSE, 
     col="gray", 
     xlab="Livestock Net Income in AFGs",
     ylab = "Density",
     main="Farmers Livestock Net Income")

curve(dnorm(x, mean=mean(live_stock), 
            sd=sd(live_stock)), 
      add=TRUE, col="red") 

#png(filename = "Hist_LiveStock.png")

dev.off()
#line


##----------------------------------------------------------------------
## Numerical variables:


##--- Continues
live_stock_outfree
rainfall=data$r5
temperature=data$t5

#--- Discrete

Mobile=data$Mobile
TV=data$TV
computer=data$Computer
motorBike=data$MotorBike
car=data$Car
No_Elec=data$No.Hrs.with.electricity

### Functions for finding mean, mode median, skewness, Kurtosis

cont_fun = function(result){
  
  maxx=max(result)
  minn= min(result)
  meanX=mean(result)
  skew=skewness(result)
  kur=kurtosis(result)
  stan.Dev=sd(result)
  
  
  C=c("Min",minn,"max: ",maxx,"mean: ", meanX ,"Std. Dev",stan.Dev ,
      "skewness: ", skew, "kurtosis: ", kur)
  final = as.data.frame(C)
  return(final)
  
}

cont_fun(live_stock_outfree)
cont_fun(rainfall)
cont_fun(temperature)
cont_fun(Mobile)
cont_fun(TV)
cont_fun(motorBike)
cont_fun(car)
cont_fun(No_Elec_outfree)


boxplot(No_Elec, horizontal = T)
No_Elec_outfree=No_Elec[No_Elec<24 & No_Elec > -1]
boxplot(No_Elec_outfree, horizontal = T)

###------------ Sub-Sample Analysis

cross_fun = function(x){
  stan_d  =sd(x)
  mean_c= mean(x)
  C = c("standard Dev: ", stan_d, "Mean: ",mean_c)
  final = as.data.frame(C)
  return(final)
}

cross_fun(354)
sd(literacy)
data$Literate.0.no.1.yes

provience1_livestock=live_stock_outfree[data$Province==1]
provience2_livestock=live_stock_outfree[data$Province==2]
provience3_livestock=live_stock_outfree[data$Province==3]

provience1_livestock[is.na(provience1_livestock)] = 0
provience2_livestock[is.na(provience2_livestock)] = 0
provience3_livestock[is.na(provience3_livestock)] = 0

provience1_livestock
provience2_livestock
provience3_livestock

cross_fun(provience3_livestock)


provience1_rainfall=rainfall[data$Province==1]
provience2_rainfall=rainfall[data$Province==2]
provience3_rainfall=rainfall[data$Province==3]

cross_fun(provience1_rainfall)
cross_fun(provience2_rainfall)
cross_fun(provience3_rainfall)

provience1_temperature=temperature[data$Province==1]
provience2_temperature=temperature[data$Province==2]
provience3_temperature=temperature[data$Province==3]

View(provience1_temperature)

cross_fun(provience1_temperature)
cross_fun(provience2_temperature)
cross_fun(provience3_temperature)

### live stock sub  figure
live_sub_vis = data.frame(color=c("First Province","Second Province","Third Province"),
                  mean.temp=c(24990,28654,30517),
                  sd=c(26138,28201,27651))

plot(1:3,live_sub_vis$mean.temp,pch=19,ylab="Livestock Net Income",xlab="Provinces",xaxt="n",xlim=c(0.5,3.5),
     ylim=c(min(live_sub_vis$mean.temp-live_sub_vis$sd),max((live_sub_vis$mean.temp+live_sub_vis$sd))),
     main = "Sub-sample Analysis of Livestock \
     Net Income Across Three provinces")

lines(rbind(1:3,1:3,NA),rbind(live_sub_vis$mean.temp-live_sub_vis$sd,live_sub_vis$mean.temp+live_sub_vis$sd,NA))

axis(side=1,at=1:3,labels=live_sub_vis$color)

### Rainfall sbu  figure

live_sub_vis = data.frame(color=c("First Province","Second Province","Third Province"),
                          mean.temp=c(8.29,11.6,13.4),
                          sd=c(1.18,2.3,1.4))

plot(1:3,live_sub_vis$mean.temp,pch=19,ylab="Temperature",xlab="Provinces",xaxt="n",xlim=c(0.5,3.5),
     ylim=c(min(live_sub_vis$mean.temp-live_sub_vis$sd),max((live_sub_vis$mean.temp+live_sub_vis$sd))),
     main = "Sub-sample Analysis of Temperature \
     Across Three provinces")

lines(rbind(1:3,1:3,NA),rbind(live_sub_vis$mean.temp-live_sub_vis$sd,live_sub_vis$mean.temp+live_sub_vis$sd,NA))

axis(side=1,at=1:3,labels=live_sub_vis$color)

### Rainfall sub  figure



