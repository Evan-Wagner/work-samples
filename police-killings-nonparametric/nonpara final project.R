
library(mosaic)
require(BDSA)
require(NSM3)
require(agricolae)
require(pgirmess)
require(binom)
require(pspearman)
require(mpoly)
require(Rfit)

setwd("/Users/evan/Documents/Schoolwork/F21/Nonparametrics")

rm(list=ls())

jackKnife <- function(X,Y,alpha = 0.05) {
  m=length(X); n=length(Y); S0=log(var(X)); T0=log(var(Y))
  Xbar=D=S=A=Ybar=E=T=B=c()
  
  for( i in 1:m) {
    Xbar[i]=mean(X[-i])
    D[i]=var(X[-i])
    S[i]=log(D[i])
    A[i]=m*S0-(m-1)*S[i]
  }
  
  for(j in 1:n) {
    Ybar[j]=mean(Y[-j])
    E[j]=var(Y[-j])
    T[j]=log(E[j])
    B[j]=n*T0-(n-1)*T[j]
  }
  
  Abar=mean(A); Bbar=mean(B)
  V1=sum((A-Abar)^2)/(m*(m-1)); V2=sum((B-Bbar)^2)/(n*(n-1))
  
  gamma2bar=exp(Abar-Bbar)
  #  cat("The Jackknife estimator is: ",gamma2bar,"\n")
  
  Q=(Abar-Bbar)/sqrt(V1+V2)
  z=-qnorm(alpha/2)
  l_int=exp(Abar-Bbar-z*(V1+V2)^{1/2})
  u_int=exp(Abar-Bbar+z*(V1+V2)^{1/2})
  #  cat("The ", (1-alpha)*100, "% confidence interval is: (",l_int,",",u_int,")\n")
  
  z=-qnorm(alpha)
  l_bound=exp(Abar-Bbar-z*(V1+V2)^{1/2})
  u_bound=exp(Abar-Bbar+z*(V1+V2)^{1/2})
  #  cat("The ", (1-alpha)*100, "% lower confidence bound is: (",l_bound,", inf)\n")
  #  cat("The ", (1-alpha)*100, "% upper confidence bound is: (0,",u_bound,")\n")
  
  result=list(gamma2bar=gamma2bar,Q=Q,Abar=Abar,Bbar=Bbar,V1=V1,V2=V2,B=B,T=T,E=E,Ybar=Ybar,Xbar=Xbar,D=D,S=S,A=A,S0=S0,T0=T0,l_int=l_int,u_int=u_int,l_bound=l_bound,u_bound=u_bound)
  return(result)
}

### data prep ----

PoliceKillings2015 <- read.csv("PoliceKillings2015.csv")

## quants

favstats(age~raceethnicity,data=PoliceKillings2015)

PoliceKillings2015$age <- as.numeric(PoliceKillings2015$age)
PoliceKillings2015$pop <- as.numeric(PoliceKillings2015$pop)
PoliceKillings2015$share_white <- as.numeric(PoliceKillings2015$share_white)
PoliceKillings2015$share_black <- as.numeric(PoliceKillings2015$share_black)
PoliceKillings2015$share_hispanic <- as.numeric(PoliceKillings2015$share_hispanic)
PoliceKillings2015$pov <- as.numeric(PoliceKillings2015$pov)
PoliceKillings2015$urate <- as.numeric(PoliceKillings2015$urate)
PoliceKillings2015$college <- as.numeric(PoliceKillings2015$college)
PoliceKillings2015$p_income <- as.numeric(PoliceKillings2015$p_income)
PoliceKillings2015$h_income <- as.numeric(PoliceKillings2015$h_income)
PoliceKillings2015$county_income <- as.numeric(PoliceKillings2015$county_income)

PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$age),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$share_white),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$share_black),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$pov),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$urate),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$college),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$p_income),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$h_income),]
PoliceKillings2015 <- PoliceKillings2015[!is.na(PoliceKillings2015$county_income),]

## factors

PoliceKillings2015 <- PoliceKillings2015[PoliceKillings2015$raceethnicity!="Unknown",]
PoliceKillings2015 <- PoliceKillings2015[PoliceKillings2015$cause!="Unknown",]
PoliceKillings2015 <- PoliceKillings2015[PoliceKillings2015$armed!="Unknown",]

PoliceKillings2015$raceethnicity <- as.factor(PoliceKillings2015$raceethnicity)
PoliceKillings2015$gender <- as.factor(PoliceKillings2015$gender)
PoliceKillings2015$cause <- as.factor(PoliceKillings2015$cause)
PoliceKillings2015$armed <- as.factor(PoliceKillings2015$armed)

levels(PoliceKillings2015$raceethnicity)
levels(PoliceKillings2015$gender)
levels(PoliceKillings2015$cause)
levels(PoliceKillings2015$armed)

## binary

PoliceKillings2015$white <- ifelse(PoliceKillings2015$raceethnicity=="White",1,0)
PoliceKillings2015$male <- ifelse(PoliceKillings2015$gender=="Male",1,0)
PoliceKillings2015$armedyes <- ifelse(PoliceKillings2015$armed=="No",0,1)

head(PoliceKillings2015)

### EDA ----

boxplot(age~raceethnicity,data=PoliceKillings2015) #
boxplot(pop~raceethnicity,data=PoliceKillings2015)
boxplot(share_white~raceethnicity,data=PoliceKillings2015) #
boxplot(share_black~raceethnicity,data=PoliceKillings2015) #
boxplot(share_hispanic~raceethnicity,data=PoliceKillings2015) 
boxplot(pov~raceethnicity,data=PoliceKillings2015) #
boxplot(urate~raceethnicity,data=PoliceKillings2015) #
boxplot(college~raceethnicity,data=PoliceKillings2015)
boxplot(p_income~raceethnicity,data=PoliceKillings2015)
boxplot(h_income~raceethnicity,data=PoliceKillings2015)
boxplot(county_income~raceethnicity,data=PoliceKillings2015) #

boxplot(age~cause,data=PoliceKillings2015) #
boxplot(pop~cause,data=PoliceKillings2015)
boxplot(share_white~cause,data=PoliceKillings2015)
boxplot(share_black~cause,data=PoliceKillings2015)
boxplot(share_hispanic~cause,data=PoliceKillings2015) 
boxplot(pov~cause,data=PoliceKillings2015)
boxplot(urate~cause,data=PoliceKillings2015)
boxplot(college~cause,data=PoliceKillings2015)
boxplot(p_income~cause,data=PoliceKillings2015)
boxplot(h_income~cause,data=PoliceKillings2015)
boxplot(county_income~cause,data=PoliceKillings2015)

boxplot(age~armed,data=PoliceKillings2015)
boxplot(pop~armed,data=PoliceKillings2015)
boxplot(share_white~armed,data=PoliceKillings2015) #
boxplot(share_black~armed,data=PoliceKillings2015)
boxplot(share_hispanic~armed,data=PoliceKillings2015) 
boxplot(pov~armed,data=PoliceKillings2015)
boxplot(urate~armed,data=PoliceKillings2015)
boxplot(college~armed,data=PoliceKillings2015)
boxplot(p_income~armed,data=PoliceKillings2015)
boxplot(h_income~armed,data=PoliceKillings2015)
boxplot(county_income~armed,data=PoliceKillings2015)

boxplot(age~white,data=PoliceKillings2015) #
boxplot(pop~white,data=PoliceKillings2015)
boxplot(share_white~white,data=PoliceKillings2015) #
boxplot(share_black~white,data=PoliceKillings2015)
boxplot(share_hispanic~white,data=PoliceKillings2015) 
boxplot(pov~white,data=PoliceKillings2015) #
boxplot(urate~white,data=PoliceKillings2015)
boxplot(college~white,data=PoliceKillings2015)
boxplot(p_income~white,data=PoliceKillings2015)
boxplot(h_income~white,data=PoliceKillings2015) #
boxplot(county_income~white,data=PoliceKillings2015)

boxplot(age~male,data=PoliceKillings2015)
boxplot(pop~male,data=PoliceKillings2015)
boxplot(share_white~male,data=PoliceKillings2015)
boxplot(share_black~male,data=PoliceKillings2015)
boxplot(share_hispanic~male,data=PoliceKillings2015) 
boxplot(pov~male,data=PoliceKillings2015)
boxplot(urate~male,data=PoliceKillings2015)
boxplot(college~male,data=PoliceKillings2015) #
boxplot(p_income~male,data=PoliceKillings2015)
boxplot(h_income~male,data=PoliceKillings2015) #
boxplot(county_income~male,data=PoliceKillings2015)

boxplot(age~armedyes,data=PoliceKillings2015) #
boxplot(pop~armedyes,data=PoliceKillings2015)
boxplot(share_white~armedyes,data=PoliceKillings2015) #
boxplot(share_black~armedyes,data=PoliceKillings2015) 
boxplot(share_hispanic~armedyes,data=PoliceKillings2015) 
boxplot(pov~armedyes,data=PoliceKillings2015) #
boxplot(urate~armedyes,data=PoliceKillings2015)
boxplot(college~armedyes,data=PoliceKillings2015)
boxplot(p_income~armedyes,data=PoliceKillings2015)
boxplot(h_income~armedyes,data=PoliceKillings2015)
boxplot(county_income~armedyes,data=PoliceKillings2015)


### two-sample location & dispersion ----

## age vs. white

boxplot(sqrt(age)~white,data=PoliceKillings2015)
favstats(sqrt(age)~white,data=PoliceKillings2015) # sds very similar
histogram(~sqrt(age)|white,data=PoliceKillings2015)

X1 = sqrt(PoliceKillings2015[PoliceKillings2015$white==1,"age"])
Y1 = sqrt(PoliceKillings2015[PoliceKillings2015$white==0,"age"])

mean(age~white,data=PoliceKillings2015)[2]-mean(age~white,data=PoliceKillings2015)[1]

(t1 = t.test(X1,Y1)) # pval = 1.381e-09, conf int: (.3947, .7617)
(est = t1$estimate[[1]]^2 - t1$estimate[[2]]^2) # est = 6.941
(l_int = t1$estimate[[1]]^2 - (t1$estimate[[1]] - t1$conf.int[1])^2)
(u_int = t1$estimate[[1]]^2 - (t1$estimate[[1]] - t1$conf.int[2])^2) # (4.810, 9.004)

ttest1 = aov(sqrt(age)~white,data=PoliceKillings2015)

plot(ttest1,1)
levene.test(y=sqrt(PoliceKillings2015[,"age"]),group=PoliceKillings2015[,"white"])
# pval = .008438, variances unequal

plot(ttest1,2)
ad.test(ttest1$residuals) # pval = .0396, residuals not normal

jk1 = jackKnife(X1,Y1)
jk1$gamma2bar # 1.396
jk1$l_int; jk1$u_int; jk1$l_bound; jk1$u_bound # (1.065, 1.829)

jk1$Q
pnorm(abs(jk1$Q),lower.tail=FALSE) # pval = .00777, diff dispersions. Fligner Policello is appropriate

pFligPoli(Y1,X1,method="Asymptotic") # pval = 2.111e-10, U = 6.353
wilcox.test(X1^2,Y1^2,alternative="two.sided",conf.int=TRUE) # W = 31777, pval = 2.998e-09, deltahat = 7.000, (5.000, 9.000)

## age vs. armedyes

boxplot(sqrt(age)~armedyes,data=PoliceKillings2015)
favstats(sqrt(age)~armedyes,data=PoliceKillings2015) # sds very similar
histogram(~sqrt(age)|armedyes,data=PoliceKillings2015)

X2 = sqrt(PoliceKillings2015[PoliceKillings2015$armedyes==1,"age"])
Y2 = sqrt(PoliceKillings2015[PoliceKillings2015$armedyes==0,"age"])

(t2 = t.test(X2,Y2)) # pval = .0328, conf int: (-.515, -.0223)
(est = t2$estimate[[1]]^2 - t2$estimate[[2]]^2) # est = -3.274
(l_int = t2$estimate[[1]]^2 - (t2$estimate[[1]] - t2$conf.int[1])^2)
(u_int = t2$estimate[[1]]^2 - (t2$estimate[[1]] - t2$conf.int[2])^2) # (-6.403, -0.266)

ttest2 = aov(sqrt(age)~armedyes,data=PoliceKillings2015)

plot(ttest2,1)
levene.test(y=sqrt(PoliceKillings2015[,"age"]),group=PoliceKillings2015[,"armedyes"])
# pval = .08504, variances unequal

plot(ttest2,2)
ad.test(ttest2$residuals) # pval = 5.75e-10, residuals not normal

jk2 = jackKnife(X2,Y2)
jk2$gamma2bar
jk2$l_int; jk2$u_int; jk2$l_bound; jk2$u_bound

jk2$Q
pnorm(abs(jk2$Q),lower.tail=FALSE) # pval = .0584, diff dispersions. Fligner Policello is appropriate

pFligPoli(X2^2,Y2^2,method="Asymptotic") # pval = .0454, U = 2.0011

wilcox.test(X2^2,Y2^2,alternative="two.sided",conf.int=TRUE)
# pval = .04003, Deltahat = 3.000, conf int: (0.0000587, 6.000)


## pov vs. armedyes

boxplot(sqrt(pov)~armedyes,data=PoliceKillings2015)
favstats(sqrt(pov)~armedyes,data=PoliceKillings2015) # sds very similar
histogram(~sqrt(pov)|armedyes,data=PoliceKillings2015)

X3 = sqrt(PoliceKillings2015[PoliceKillings2015$armedyes==1,"pov"])
Y3 = sqrt(PoliceKillings2015[PoliceKillings2015$armedyes==0,"pov"])

(t3 = t.test(X3,Y3)) # pval = .1483, conf int: (-.0834, .5478)
(est = t3$estimate[[1]]^2 - t3$estimate[[2]]^2) # est = 2.009
(l_int = t3$estimate[[1]]^2 - (t3$estimate[[1]] - t3$conf.int[1])^2)
(u_int = t3$estimate[[1]]^2 - (t3$estimate[[1]] - t3$conf.int[2])^2) # (-.749, 4.568)

ttest3 = aov(sqrt(pov)~armedyes,data=PoliceKillings2015)

plot(ttest3,1)
levene.test(y=PoliceKillings2015[,"pov"],group=PoliceKillings2015[,"armedyes"])
levene.test(y=sqrt(PoliceKillings2015[,"pov"]),group=PoliceKillings2015[,"armedyes"])
# pval = .6036, cant conclude unequal variance. Wilcoxon rank sum is appropriate

plot(ttest3,2)
ad.test(ttest3$residuals) # pval = .002039, residuals not normal

jk3 = jackKnife(X3,Y3)
jk3$gamma2bar
jk3$l_int; jk3$u_int; jk3$l_bound; jk3$u_bound

jk3$Q
pnorm(abs(jk3$Q),lower.tail=FALSE) # pval = .1733, cant conclude diff dispersions. Wilcoxon is appropriate

wilcox.test(X3^2,Y3^2,alternative="two.sided",conf.int=TRUE)
# pval = .1657, Deltahat = -1.800, conf int: (-4.400, .800)



### one-way anova ----

## age vs. race

ThreeRaces <- PoliceKillings2015[
  PoliceKillings2015$raceethnicity=="White"
  | PoliceKillings2015$raceethnicity=="Black"
  | PoliceKillings2015$raceethnicity=="Hispanic/Latino",]

ThreeRaces$raceethnicity <- as.factor(as.character(ThreeRaces$raceethnicity))

boxplot(sqrt(age)~raceethnicity,data=ThreeRaces)
favstats(sqrt(age)~raceethnicity,data=ThreeRaces)
hist(sqrt(ThreeRaces[ThreeRaces$raceethnicity=="White","age"]))
hist(sqrt(ThreeRaces[ThreeRaces$raceethnicity=="Black","age"]))
hist(sqrt(ThreeRaces[ThreeRaces$raceethnicity=="Hispanic/Latino","age"]))

# mean & median of white seems much higher than black & latino
# sds pretty similar for WBL

aov1 = aov(sqrt(age)~raceethnicity,data=ThreeRaces)
anova(aov1) # pval = 4.273e-09

plot(aov1,1)
levene.test(y=ThreeRaces$age,group=ThreeRaces$raceethnicity)       # pval = .001319, variances unequal
levene.test(y=sqrt(ThreeRaces$age),group=ThreeRaces$raceethnicity) # pval = .02425, sqrt improves variance

plot(aov1,2)
ad.test(aov(age~raceethnicity,data=ThreeRaces)$residuals)       # pval = 5.498e-07, residuals not normal
ad.test(aov(sqrt(age)~raceethnicity,data=ThreeRaces)$residuals) # pval = .0564, sqrt improves normality greatly

(HSD1 = HSD.test(aov1,"raceethnicity",group=TRUE))$groups
plot(HSD1) # white victims are older than black and hispanic

(Tk1 = TukeyHSD(aov1,"raceethnicity"))

(ests = mean(sqrt(age)~raceethnicity,data=ThreeRaces))

(diff13 = ests[[3]]^2 - ests[[1]]^2) # 6.577
(l_int13 = ests[[3]]^2 - (ests[[3]] - Tk1$raceethnicity[2,2])^2)
(u_int13 = ests[[3]]^2 - (ests[[3]] - Tk1$raceethnicity[2,3])^2) # (3.595, 9.430)

(diff23 = ests[[3]]^2 - ests[[2]]^2) # 8.452
(l_int13 = ests[[3]]^2 - (ests[[3]] - Tk1$raceethnicity[3,2])^2)
(u_int13 = ests[[3]]^2 - (ests[[3]] - Tk1$raceethnicity[3,3])^2) # (4.687, 12.001)

(diff12 = ests[[2]]^2 - ests[[1]]^2) # -1.874
(l_int12 = ests[[2]]^2 - (ests[[2]] + Tk1$raceethnicity[1,3])^2)
(u_int12 = ests[[2]]^2 - (ests[[2]] + Tk1$raceethnicity[1,2])^2) # (-2.132, 5.521)

pKW(x=ThreeRaces$age,g=ThreeRaces$raceethnicity,method="Asymptotic") # H' = 37.515, pval = 7.140e-09

pSDCFlig(x=ThreeRaces$age,g=ThreeRaces$raceethnicity,method="Asymptotic")
# white victims are older than black and hispanic

White = ThreeRaces[ThreeRaces$raceethnicity=="White","age"]
Black = ThreeRaces[ThreeRaces$raceethnicity=="Black","age"]
Latino = ThreeRaces[ThreeRaces$raceethnicity=="Hispanic/Latino","age"]

alpha = .05; n = c(length(White),length(Black),length(Latino))
N = sum(n)

(wstar_alpha = cSDCFlig(alpha,n,method="Asymptotic")$cutoff.U)

a12 = n[1]*n[2]/2 - wstar_alpha*sqrt(n[1]*n[2]*(n[1]+n[2]+1)/24) + 1
a13 = n[1]*n[3]/2 - wstar_alpha*sqrt(n[1]*n[3]*(n[1]+n[3]+1)/24) + 1
a23 = n[2]*n[3]/2 - wstar_alpha*sqrt(n[2]*n[3]*(n[2]+n[3]+1)/24) + 1

D12 = c(); for (i in 1:n[1]) {for (j in 1:n[2]) {D12 = c(D12,White[i]-Black[j])}}; D12 = sort(D12)
D13 = c(); for (i in 1:n[1]) {for (j in 1:n[3]) {D13 = c(D13,White[i]-Latino[j])}}; D13 = sort(D13)
D23 = c(); for (i in 1:n[2]) {for (j in 1:n[3]) {D23 = c(D23,Black[i]-Latino[j])}}; D23 = sort(D23)

Z12 = median(D12)
Z13 = median(D13)
Z23 = median(D23)

deltabar1 = ( Z12*n[2] + Z13*n[3])/N
deltabar2 = (-Z12*n[1] + Z23*n[3])/N
deltabar3 = (-Z13*n[1] - Z23*n[2])/N

(thetahat12 = deltabar1 - deltabar2)
cat("conf int for tau1 - tau2: (",D12[a12],", ",D12[n[1]*n[2]-a12+1],")\n")
(thetahat13 = deltabar1 - deltabar3)
cat("conf int for tau1 - tau3: (",D13[a13],", ",D13[n[1]*n[3]-a13+1],")\n")
(thetahat23 = deltabar2 - deltabar3)
cat("conf int for tau2 - tau3: (",D23[a23],", ",D23[n[2]*n[3]-a23+1],")\n")



## income vs. race

boxplot(p_income~raceethnicity,data=ThreeRaces)
boxplot(sqrt(p_income)~raceethnicity,data=ThreeRaces)
favstats(sqrt(p_income)~raceethnicity,data=ThreeRaces)
# mean & median of white seems higher than black & latino
# sds pretty similar for WBL

aov2 = aov(p_income~raceethnicity,data=ThreeRaces)
anova(aov2) # pval = .007022

plot(aov2,1)
levene.test(y=ThreeRaces$p_income,group=ThreeRaces$raceethnicity)       # pval = .4169, cant conclude variances unequal
levene.test(y=sqrt(ThreeRaces$p_income),group=ThreeRaces$raceethnicity) # pval = .1909, sqrt worsens variance

plot(aov(p_income~raceethnicity,data=ThreeRaces),2)
plot(aov(sqrt(p_income)~raceethnicity,data=ThreeRaces),2)
ad.test(aov(p_income~raceethnicity,data=ThreeRaces)$residuals)       # pval = 2.2e-16, residuals not normal
ad.test(aov(sqrt(p_income)~raceethnicity,data=ThreeRaces)$residuals) # pval = 7.181e-08, sqrt improves normality but not nearly enough

(HSD2 = HSD.test(aov2,"raceethnicity",group=TRUE))$groups
plot(HSD2)

(Tk2 = TukeyHSD(aov2,"raceethnicity"))

pKW(x=sqrt(ThreeRaces$p_income),g=ThreeRaces$raceethnicity,method="Asymptotic") # pval = .0021

pSDCFlig(x=ThreeRaces$p_income,g=ThreeRaces$raceethnicity,method="Asymptotic")
# killings of whites occur in tracts with higher avg personal incomes than killings
# of blacks and latinos

White = ThreeRaces[ThreeRaces$raceethnicity=="White","p_income"]
Black = ThreeRaces[ThreeRaces$raceethnicity=="Black","p_income"]
Latino = ThreeRaces[ThreeRaces$raceethnicity=="Hispanic/Latino","p_income"]

alpha = .05; n = c(length(White),length(Black),length(Latino))
N = sum(n)

(wstar_alpha = cSDCFlig(alpha,n,method="Asymptotic")$cutoff.U)

a12 = n[1]*n[2]/2 - wstar_alpha*sqrt(n[1]*n[2]*(n[1]+n[2]+1)/24) + 1
a13 = n[1]*n[3]/2 - wstar_alpha*sqrt(n[1]*n[3]*(n[1]+n[3]+1)/24) + 1
a23 = n[2]*n[3]/2 - wstar_alpha*sqrt(n[2]*n[3]*(n[2]+n[3]+1)/24) + 1

D12 = c(); for (i in 1:n[1]) {for (j in 1:n[2]) {D12 = c(D12,White[i]-Black[j])}}; D12 = sort(D12)
D13 = c(); for (i in 1:n[1]) {for (j in 1:n[3]) {D13 = c(D13,White[i]-Latino[j])}}; D13 = sort(D13)
D23 = c(); for (i in 1:n[2]) {for (j in 1:n[3]) {D23 = c(D23,Black[i]-Latino[j])}}; D23 = sort(D23)

Z12 = median(D12)
Z13 = median(D13)
Z23 = median(D23)

deltabar1 = ( Z12*n[2] + Z13*n[3])/N
deltabar2 = (-Z12*n[1] + Z23*n[3])/N
deltabar3 = (-Z13*n[1] - Z23*n[2])/N

(thetahat12 = deltabar1 - deltabar2)
cat("conf int for tau1 - tau2: [",D12[a12],", ",D12[n[1]*n[2]-a12+1],")\n")
(thetahat13 = deltabar1 - deltabar3)
cat("conf int for tau1 - tau3: [",D13[a13],", ",D13[n[1]*n[3]-a13+1],")\n")
(thetahat23 = deltabar2 - deltabar3)
cat("conf int for tau2 - tau3: [",D23[a23],", ",D23[n[2]*n[3]-a23+1],")\n")


## age vs. armed

favstats(age~armed,data=PoliceKillings2015)

ThreeWeapons <- PoliceKillings2015[
  PoliceKillings2015$armed=="No"
  | PoliceKillings2015$armed=="Firearm"
  | PoliceKillings2015$armed=="Knife",]

ThreeWeapons$armed <- as.factor(as.character(ThreeWeapons$armed))

boxplot(sqrt(age)~armed,data=ThreeWeapons)
boxplot(sqrt(age)~armed,data=ThreeWeapons)
favstats(sqrt(age)~armed,data=ThreeWeapons)
hist(sqrt(ThreeWeapons[ThreeWeapons$armed=="No","age"]))
hist(sqrt(ThreeWeapons[ThreeWeapons$armed=="Firearm","age"]))
hist(sqrt(ThreeWeapons[ThreeWeapons$armed=="Knife","age"]))

aov3 = aov(sqrt(age)~armed,data=ThreeWeapons)
anova(aov3) # pval = .09144

plot(aov3,1)
levene.test(y=ThreeWeapons$age,group=ThreeWeapons$armed)       # pval = .1432, cant conclude equal variance
levene.test(y=sqrt(ThreeWeapons$age),group=ThreeWeapons$armed) # pval = .2497, sqrt improves variance

plot(aov3,2)
ad.test(aov(age~armed,data=ThreeWeapons)$residuals)       # pval = 8.331e-09, residuals not normal
ad.test(aov(sqrt(age)~armed,data=ThreeWeapons)$residuals) # pval = .006729, sqrt improves normality

(HSD3 = HSD.test(aov3,"armed",group=TRUE))$groups
plot(HSD3)

TukeyHSD(aov3,"armed") # knife wielders younger than unarmed at alpha .1 but not .05

pKW(x=ThreeWeapons$age,g=ThreeWeapons$armed,method="Asymptotic") # H' = 5.061, pval = .0796

pSDCFlig(x=ThreeWeapons$age,g=ThreeWeapons$armed,method="Asymptotic")
# knife wielders younger than unarmed at alpha .1 but not .5

Unarmed = ThreeWeapons[ThreeWeapons$armed=="No","age"]
Firearm = ThreeWeapons[ThreeWeapons$armed=="Firearm","age"]
Knife = ThreeWeapons[ThreeWeapons$armed=="Knife","age"]

alpha = .05; n = c(length(Unarmed),length(Firearm),length(Knife))
N = sum(n)

(wstar_alpha = cSDCFlig(alpha,n,method="Asymptotic")$cutoff.U)

a12 = n[1]*n[2]/2 - wstar_alpha*sqrt(n[1]*n[2]*(n[1]+n[2]+1)/24) + 1
a13 = n[1]*n[3]/2 - wstar_alpha*sqrt(n[1]*n[3]*(n[1]+n[3]+1)/24) + 1
a23 = n[2]*n[3]/2 - wstar_alpha*sqrt(n[2]*n[3]*(n[2]+n[3]+1)/24) + 1

D12 = c(); for (i in 1:n[1]) {for (j in 1:n[2]) {D12 = c(D12,Unarmed[i]-Firearm[j])}}; D12 = sort(D12)
D13 = c(); for (i in 1:n[1]) {for (j in 1:n[3]) {D13 = c(D13,Unarmed[i]-Knife[j])}}; D13 = sort(D13)
D23 = c(); for (i in 1:n[2]) {for (j in 1:n[3]) {D23 = c(D23,Firearm[i]-Knife[j])}}; D23 = sort(D23)

Z12 = median(D12)
Z13 = median(D13)
Z23 = median(D23)

deltabar1 = ( Z12*n[2] + Z13*n[3])/N
deltabar2 = (-Z12*n[1] + Z23*n[3])/N
deltabar3 = (-Z13*n[1] - Z23*n[2])/N

(thetahat12 = deltabar1 - deltabar2)
cat("conf int for tau1 - tau2: (",D12[a12],", ",D12[n[1]*n[2]-a12+1],")\n")
(thetahat13 = deltabar1 - deltabar3)
cat("conf int for tau1 - tau3: (",D13[a13],", ",D13[n[1]*n[3]-a13+1],")\n")
(thetahat23 = deltabar2 - deltabar3)
cat("conf int for tau2 - tau3: (",D23[a23],", ",D23[n[2]*n[3]-a23+1],")\n")


## pov vs. armed

boxplot(sqrt(pov)~armed,data=ThreeWeapons)
favstats(pov~armed,data=ThreeWeapons)
hist(sqrt(ThreeWeapons[ThreeWeapons$armed=="No","pov"]))
hist(sqrt(ThreeWeapons[ThreeWeapons$armed=="Firearm","pov"]))
hist(sqrt(ThreeWeapons[ThreeWeapons$armed=="Knife","pov"]))

aov4 = aov(sqrt(pov)~armed,data=ThreeWeapons)
anova(aov4) # pval = .06717

(HSD4 = HSD.test(aov4,"armed",group=TRUE))$groups
plot(HSD4)

(Tk4 = TukeyHSD(aov(pov~armed,data=ThreeWeapons),"armed"))

(ests = mean(sqrt(age)~armed,data=ThreeWeapons))

(diff13 = ests[[3]]^2 - ests[[1]]^2) # 2.558
(l_int13 = ests[[3]]^2 - (ests[[3]] + Tk4$armed[2,2])^2)
(u_int13 = ests[[3]]^2 - (ests[[3]] + Tk4$armed[2,3])^2) # (-9.038, 1.256)

(diff23 = ests[[3]]^2 - ests[[2]]^2) # 4.122
(l_int13 = ests[[3]]^2 - (ests[[3]] + Tk4$armed[3,2])^2)
(u_int13 = ests[[3]]^2 - (ests[[3]] + Tk4$armed[3,3])^2) # (-5.531, 7.200)

(diff12 = ests[[1]]^2 - ests[[2]]^2) # 1.567
(l_int12 = ests[[1]]^2 - (ests[[1]] + Tk4$armed[1,3])^2)
(u_int12 = ests[[1]]^2 - (ests[[1]] + Tk4$armed[1,2])^2) # (-0.911, 9.196)

plot(aov4,1)
levene.test(y=sqrt(ThreeWeapons$pov),group=ThreeWeapons$armed)
# pval = .05415, variances unequal at alpha .1 but not .05

plot(aov4,2)
ad.test(aov4$residuals) # pval = 2.198e-13, residuals not normal

pKW(x=ThreeWeapons$pov,g=ThreeWeapons$armed,method="Asymptotic") # H' = 5.0263, pval = .081

pSDCFlig(x=ThreeWeapons$pov,g=ThreeWeapons$armed,method="Asymptotic")
# no significant differences

Unarmed = ThreeWeapons[ThreeWeapons$armed=="No","pov"]
Firearm = ThreeWeapons[ThreeWeapons$armed=="Firearm","pov"]
Knife = ThreeWeapons[ThreeWeapons$armed=="Knife","pov"]

alpha = .05; n = c(length(Unarmed),length(Firearm),length(Knife))
N = sum(n)

(wstar_alpha = cSDCFlig(alpha,n,method="Asymptotic")$cutoff.U)

a12 = n[1]*n[2]/2 - wstar_alpha*sqrt(n[1]*n[2]*(n[1]+n[2]+1)/24) + 1
a13 = n[1]*n[3]/2 - wstar_alpha*sqrt(n[1]*n[3]*(n[1]+n[3]+1)/24) + 1
a23 = n[2]*n[3]/2 - wstar_alpha*sqrt(n[2]*n[3]*(n[2]+n[3]+1)/24) + 1

D12 = c(); for (i in 1:n[1]) {for (j in 1:n[2]) {D12 = c(D12,Unarmed[i]-Firearm[j])}}; D12 = sort(D12)
D13 = c(); for (i in 1:n[1]) {for (j in 1:n[3]) {D13 = c(D13,Unarmed[i]-Knife[j])}}; D13 = sort(D13)
D23 = c(); for (i in 1:n[2]) {for (j in 1:n[3]) {D23 = c(D23,Firearm[i]-Knife[j])}}; D23 = sort(D23)

Z12 = median(D12)
Z13 = median(D13)
Z23 = median(D23)

deltabar1 = ( Z12*n[2] + Z13*n[3])/N
deltabar2 = (-Z12*n[1] + Z23*n[3])/N
deltabar3 = (-Z13*n[1] - Z23*n[2])/N

(thetahat12 = deltabar1 - deltabar2)
cat("conf int for tau1 - tau2: (",D12[a12],", ",D12[n[1]*n[2]-a12+1],")\n")
(thetahat13 = deltabar1 - deltabar3)
cat("conf int for tau1 - tau3: (",D13[a13],", ",D13[n[1]*n[3]-a13+1],")\n")
(thetahat23 = deltabar2 - deltabar3)
cat("conf int for tau2 - tau3: (",D23[a23],", ",D23[n[2]*n[3]-a23+1],")\n")


### regression ----

## simple

(cors = abs(cor(PoliceKillings2015[,c("age","pop","share_white","share_black",
                                      "share_hispanic","pov","urate","college",
                                      "p_income","h_income","county_income",
                                      "white","male","armedyes")])))
cors > .5
# p_income and h_income highly correlated w/ pov, college, urate
# other high cors: share_white-pov, share_white-share_black, pov-urate

cor.test(PoliceKillings2015$pov,PoliceKillings2015$p_income,method="kendall") # pval approx 0
cor.test(PoliceKillings2015$college,PoliceKillings2015$p_income,method="kendall") # pval approx 0
cor.test(PoliceKillings2015$urate,PoliceKillings2015$p_income,method="kendall") # pval approx 0

plot(PoliceKillings2015$pov,PoliceKillings2015$p_income)
plot(PoliceKillings2015$college,PoliceKillings2015$p_income)
plot(PoliceKillings2015$urate,PoliceKillings2015$p_income)

plot(PoliceKillings2015$pov,log(PoliceKillings2015$p_income))
plot(PoliceKillings2015$college,log(PoliceKillings2015$p_income))
plot(PoliceKillings2015$urate,log(PoliceKillings2015$p_income))

rows = sample.int(nrow(PoliceKillings2015)-50,200)

regNP1 = theil(x=PoliceKillings2015$college[rows],y=log(PoliceKillings2015$p_income[rows]),
          alpha=.05,beta.0=0,type="t") # pval = .001
regNP2 = theil(x=PoliceKillings2015$pov[rows],y=log(PoliceKillings2015$p_income[rows]),
          alpha=.05,beta.0=0,type="t") # pval = .001
regNP3 = theil(x=PoliceKillings2015$urate[rows],y=log(PoliceKillings2015$p_income[rows]),
          alpha=.05,beta.0=0,type="t") # pval = .024

regP1 = lm(log(p_income)~college,data=PoliceKillings2015)
regP2 = lm(log(p_income)~pov,data=PoliceKillings2015)
regP3 = lm(log(p_income)~urate,data=PoliceKillings2015)

plot(regP1,1)
plot(regP2,1)
plot(regP3,1)

plot(regP1,2)
plot(regP2,2)
plot(regP3,2)

round(cbind(slopeNP = c(regNP1$beta.hat,regNP2$beta.hat,regNP3$beta.hat),
            interceptNP = c(regNP1$alpha.hat,regNP2$alpha.hat,regNP3$alpha.hat),
            slopeP = c(regP1$coefficients[2],regP2$coefficients[2],regP3$coefficients[2]),
            interceptP = c(regP1$coefficients[1],regP2$coefficients[1],regP3$coefficients[1]),
            levene.pval = c(levene.test(y=PoliceKillings2015$college,group=PoliceKillings2015$raceethnicity)$p.val,
                            levene.test(y=PoliceKillings2015$pov,group=PoliceKillings2015$raceethnicity)$p.val,
                            levene.test(y=PoliceKillings2015$urate,group=PoliceKillings2015$raceethnicity)$p.val
                            ),
            ad.pval = c(ad.test(regP1$residuals)$p.val,ad.test(regP2$residuals)$p.val,ad.test(regP3$residuals)$p.val)
            ),3)

## multiple

cor.test(PoliceKillings2015$pov,PoliceKillings2015$college,method="kendall") # pval approx 0
cor.test(PoliceKillings2015$college,PoliceKillings2015$urate,method="kendall") # pval approx 0
cor.test(PoliceKillings2015$urate,PoliceKillings2015$pov,method="kendall") # pval approx 0

multregNP = rfit(log(p_income)~pov+college+urate+pov:college,data=PoliceKillings2015)
summary(multregNP)

confint.rfit(multregNP)

multregNP

multregP = lm(log(p_income)~pov+college+urate+pov:college,data=PoliceKillings2015)
summary(multregP)

cbind(slopeNP = multregNP$coefficients,
      slopeNP_lb = multregNP$
      slopeP = multregP$coefficients
      )

plot(multregP,2)

ad.test(multregP$residuals)

minus343 <- PoliceKillings2015[-c(341),]

multregP = lm(log(p_income)~pov+college+urate+pov:college,data=minus343)
summary(multregP)

plot(multregP,1)
plot(multregP,2)

ad.test(multregP$residuals)


