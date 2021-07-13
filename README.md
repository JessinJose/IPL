# IPL
https://github.com/JessinJose/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv
IPL Ball-by-Ball 2008-2020.csv

## Assignment A1b

setwd('https://github.com/JessinJose/IPL/blob/main/IPL%20Ball-by-Ball%202008-2020.csv')
dir()

# To read csv file
df  = read.csv('IPL Ball-by-Ball 2008-2020.csv', header=TRUE)
names(df)
head(df)
tail(df)
class(df)
summary(df)
View(df)
dim(df)

# We want to know which distribution fits the runs and wickets data
# Subset batsman runs and bowler wickets

## Subset runs
library(dplyr)
runs  = df %>%
  group_by(batsman, id)%>%
  summarize(score = sum(batsman_runs))
View(runs)
unique(runs$batsman)
dim(runs)

## Subset wickets
wickets  = df %>%
  group_by(bowler, id)%>%
  summarize(wicket = sum(is_wicket))
View(wickets)
unique(wickets$bowler)
dim(wickets)

runs_batsman = aggregate(cbind(runs$score),by = list(runs$batsman),FUN=sum)
colnames(runs_batsman)=c("batsman","total_runs")
View(runs_batsman)

wickets_bowler = aggregate(cbind(wickets$wicket),by = list(wickets$bowler),FUN=sum)
colnames(wickets_bowler)=c("bowler","total_wickets")
View(wickets_bowler)


# step1 to check fitting is to plot data
hist(runs_batsman$total_runs,freq=F,main="Total Runs")

# We have to make the wickets into numeric data
wickets_num = as.numeric(wickets$wicket)
hist(wickets_num,freq=F,main= "Wickets of the Bowlers")

# Plot data
# Histogram
## Poisson Distribution: goodness of fit of bowler M Ntini
ntini = wickets[wickets$bowler == 'M Ntini',]
View(ntini)
hist(ntini$wicket,freq=F,breaks=c(0,1,2,3,4,5),main="Histogram of M Ntinis Wickets",col='lightgreen')
m=mean(ntini$wicket);std=sd(ntini$wicket);m;std
lines(density(ntini$wicket),col="red")

# FITTING A POISSON DISTRIBUTION TO DATA
# For discrete data
# H0: The distribution follows Poisson distribution
# H1: The distribution does not follows Poisson distribution
library(vcd) 
gf.ntini <- goodfit(ntini$wicket, type = "poisson", par = NULL)
summary(gf.ntini)
plot(gf.ntini, main="M Ntini wicket distribution")
# p value is more than 0.05. Therefore we accept H0 null hypothesis.
# It follows poisson Distribution


# CONTINUOUS DISTRIBUTION
# to test the testing the goodness fit of batsman MS Dhoni
# Method descdist() in fitdistplus package
library(MASS)
library(fitdistrplus)
a =  runs[runs$batsman == 'MS Dhoni',]
View(a)
hist(a$score,5,col='lightgreen')
descdist(a$score)

# Fit distribution

# A) WEIBULL DISTRIBUTION
# H0: The distribution follows Weibull distribution
# H1: The distribution does not follows Weibull distribution
library(MASS)
b1 = fitdistr(a$score+0.00001,"weibull",lower=0.001)
b1$estimate
dhoni_wei = rweibull(1000, shape=1.051027, scale = 25.811602)
plot(density(dhoni_wei))
ks.test(a$score,"pweibull", scale=25.811602, shape=1.051027)  
# p value is less than 0.05. Therefore we reject H0 null hypothesis.
# It does not follows Weibull distribution 

# B) GAMMA DISTRIBUTION
# H0: The distribution follows Gamma distribution 
# H1: The distribution does not follows Gamma distribution 
b2 = fitdistr(a$score+.001,"gamma",lower=0.001)
b2$estimate
dhoni_gam = rgamma(1000, shape= 1.04313615, rate = 0.04098525)
plot(density(dhoni_gam))
lines(density(dhoni_wei),col= 'red')
ks.test(a$score+0.001,"gamma") 
# p value is less than 0.05. Therefore we reject H0 null hypothesis.
# It does not follows gamma distribution

# C) LOGNORMAL DISTRIBUTION
# H0: The distribution follows Lognormal distribution 
# H1: The distribution does not follows Lognormal distribution 
b3 = fitdistr(a$score+.001,"lognormal",lower=0.001)
b3$estimate
dhoni_lognorm = plnorm(1000, meanlog=2.686130, sdlog=1.739866)
plot(density(dhoni_lognorm))
ks.test(a$score,"plnorm",meanlog=2.686130, sdlog=1.739866) 
# p value is less than 0.05. Therefore we reject H0 null hypothesis.
# It does not follows lognormal distribution

# D) NORMAL DISTRIBUTION
# H0: The distribution follows Normal distribution 
# H1: The distribution does not follows Normal distribution 
b4 = fitdistr(a$score+.001,"normal",lower=0.001)
b4$estimate
dhoni_norm = pnorm(1000, mean=25.45155, sd=18.75156)
plot(density(dhoni_norm))
ks.test(a$score,"pnorm", mean=25.45155, sd=18.75156)
# p value is more than 0.05. Therefore we accept H0 null hypothesis.
# It follows Normal Distribution

b3 = fitdistr(a$score+.001,"Exponential",lower=0.001)
b3$estimate


hist(a$score, prob=TRUE,col='lightgreen')
curve(dweibull(x,0.7209374,20.1115283),col="red",lwd=2,add=T)
curve(dexp(x,rate= 0.04347637),col="purple",lwd=2,add=T)
curve(dgamma(x,0.66893219, 0.02908096),col="blue",lwd=2,add=T)
lines(density(a$score),col="darkgreen")


