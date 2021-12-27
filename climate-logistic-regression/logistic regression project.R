
library(Stat2Data)
library(mosaic)
library(nortest)
library(asbio)
library(agricolae)
library(lawstat)
library(performance)
library(forecast)
library(lmtest)
library(bestglm)

setwd("/Users/evan/Documents/Schoolwork/F21/Data Analysis")

## data prep

rm(list=ls())
dev.off()

names = c("Income","Gender","Age","AgeRange","PartyAffil","TrumpJobApprove","Educ",
          "EducOther","Race","RaceOther","Married","MarriedOther","q8x",
          "JobWillBeAutomated","ClimateChangeReal","TransformersMoviesSeen","q11x",
          "ScientistsGood","VaccinesGood","BooksInPastYear","q14x","BelievesInGhosts",
          "EstPctFedBudgetForScience","q16x","FedFundingScienceTooHighOrLow",
          "EarthFartherInWinter","SmartSadDumbHappy","ShowerPeeingOk")

POTN <- read.csv("PulseOfTheNation.csv",col.names=names)
head(POTN)

POTN <- POTN[POTN$Gender=="Male" | POTN$Gender=="Female",]
POTN <- POTN[POTN$ScientistsGood=="Somewhat Disagree" | POTN$ScientistsGood=="Strongly Disagree"
             | POTN$ScientistsGood=="Somewhat Agree" | POTN$ScientistsGood=="Strongly Agree",]
POTN <- POTN[POTN$VaccinesGood=="Somewhat Disagree" | POTN$VaccinesGood=="Strongly Disagree"
             | POTN$VaccinesGood=="Somewhat Agree" | POTN$VaccinesGood=="Strongly Agree",]

POTN <- POTN[POTN$SmartSadDumbHappy!="DK/REF",]
POTN <- POTN[POTN$BelievesInGhosts!="DK/REF",]
POTN <- POTN[POTN$ShowerPeeingOk!="DK/REF",]
POTN <- POTN[POTN$Educ!="DK/REF" & POTN$Educ!="Other",]
POTN <- POTN[POTN$Married!="DK/REF",]
POTN <- POTN[POTN$JobWillBeAutomated!="DK/REF",]

levels(as.factor(POTN$Race))

# These reduce the data greatly
#POTN <- POTN[POTN$EarthFartherInWinter=="TRUE" | POTN$BelievesInGhosts=="FALSE",]
#POTN <- POTN[!is.na(POTN$Income),]
#POTN <- POTN[!is.na(POTN$EstPctFedBudgetForScience),]
POTN <- POTN[!is.na(POTN$Age),]
POTN <- POTN[!is.na(POTN$BooksInPastYear),]
POTN <- POTN[!is.na(POTN$TransformersMoviesSeen),]


POTN$ShowerPeeingOk <- ifelse(POTN$ShowerPeeingOk=="Acceptable",1,0)
POTN$AnthClimateChangeReal <- ifelse(POTN$ClimateChangeReal=="Real and Caused by People",1,0)
POTN$HasSeenTransformers <- ifelse(POTN$TransformersMoviesSeen==0,0,1)
POTN$ScientistsGood <- ifelse(POTN$ScientistsGood=="Somewhat Agree"
                              | POTN$ScientistsGood=="Strongly Agree",1,0)
POTN$VaccinesGood <- ifelse(POTN$VaccinesGood=="Somewhat Agree"
                            | POTN$VaccinesGood=="Strongly Agree",1,0)
POTN$Married <- ifelse(POTN$Married=="Married",1,0)
POTN$College <- ifelse(POTN$Educ=="Some college"
                       | POTN$Educ=="College degree"
                       | POTN$Educ=="Graduate degree",1,0)
POTN$SmartSadDumbHappy <- ifelse(POTN$SmartSadDumbHappy=="Smart and Sad",1,0)
POTN$JobWillBeAutomated <- ifelse(POTN$JobWillBeAutomated=="Likely",1,0)
POTN$Male <- ifelse(POTN$Gender=="Male",1,0)
POTN$BelievesInGhosts <- ifelse(POTN$BelievesInGhosts=="Yes",1,0)

#POTN$logIncome <- ifelse(POTN$Income!=0,log(POTN$Income),0)
#POTN$logEstPctFedBudgetForScience <- ifelse(POTN$EstPctFedBudgetForScience!=0,log(POTN$EstPctFedBudgetForScience),0)
POTN$logBooksInPastYear <- ifelse(POTN$BooksInPastYear!=0,log(POTN$BooksInPastYear),0)

is.na(POTN$HasSeenTransformers)

sum(POTN$AnthClimateChangeReal)/nrow(POTN)


## EDA

boxplot(Age~AnthClimateChangeReal,data=POTN) # maybe
boxplot(BooksInPastYear~AnthClimateChangeReal,data=POTN) # maybe
boxplot(logBooksInPastYear~AnthClimateChangeReal,data=POTN) # maybe

histogram(~Age|AnthClimateChangeReal,data=POTN) # maybe
histogram(~BooksInPastYear|AnthClimateChangeReal,data=POTN) # maybe
histogram(~logBooksInPastYear|AnthClimateChangeReal,data=POTN) # maybe

(table = table(POTN$AnthClimateChangeReal,POTN$ScientistsGood))
table[2,]/(table[1,]+table[2,]) # yes

(table = table(POTN$AnthClimateChangeReal,POTN$SmartSadDumbHappy))
table[2,]/(table[1,]+table[2,]) # yes

(table = table(POTN$AnthClimateChangeReal,POTN$Male))
table[2,]/(table[1,]+table[2,]) # prob

(table = table(POTN$AnthClimateChangeReal,POTN$VaccinesGood))
table[2,]/(table[1,]+table[2,]) # prob
 
(table = table(POTN$AnthClimateChangeReal,POTN$College))
table[2,]/(table[1,]+table[2,]) # prob

(table = table(POTN$AnthClimateChangeReal,POTN$Married))
table[2,]/(table[1,]+table[2,]) # prob

(table = table(POTN$AnthClimateChangeReal,POTN$HasSeenTransformers))
table[2,]/(table[1,]+table[2,]) # maybe

(table = table(POTN$AnthClimateChangeReal,POTN$BelievesInGhosts))
table[2,]/(table[1,]+table[2,]) # maybe

(table = table(POTN$AnthClimateChangeReal,POTN$JobWillBeAutomated))
table[2,]/(table[1,]+table[2,]) # no

(table = table(POTN$AnthClimateChangeReal,POTN$ShowerPeeingOk))
table[2,]/(table[1,]+table[2,]) # no

# interaction

POTN$Male <- as.factor(POTN$Male)
POTN$HasSeenTransformers <- as.factor(POTN$HasSeenTransformers)
POTN$BelievesInGhosts <- as.factor(POTN$BelievesInGhosts)
POTN$Married <- as.factor(POTN$Married)

plot(POTN[,c("Age","logBooksInPastYear")])
emplogitplot2(AnthClimateChangeReal~Age*Male,ngroups=20,data=POTN
               ,putlegend="bottomleft") #
emplogitplot2(AnthClimateChangeReal~Age*as.factor(College),ngroups=20,data=POTN)
emplogitplot2(AnthClimateChangeReal~Age*as.factor(Married),ngroups=20,data=POTN)
emplogitplot2(AnthClimateChangeReal~Age*as.factor(JobWillBeAutomated),ngroups=20,data=POTN)
emplogitplot2(AnthClimateChangeReal~Age*HasSeenTransformers,ngroups=20,data=POTN
              ,putlegend="topright") #
emplogitplot2(AnthClimateChangeReal~Age*as.factor(ScientistsGood),ngroups=20,data=POTN)
emplogitplot2(AnthClimateChangeReal~Age*as.factor(VaccinesGood),ngroups=20,data=POTN)
emplogitplot2(AnthClimateChangeReal~Age*BelievesInGhosts,ngroups=20,data=POTN
              ,putlegend="bottomleft") #
emplogitplot2(AnthClimateChangeReal~Age*as.factor(SmartSadDumbHappy),ngroups=20,data=POTN)
emplogitplot2(AnthClimateChangeReal~Age*as.factor(ShowerPeeingOk),ngroups=20,data=POTN)

emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(Male),ngroups=8,data=POTN) 
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(College),ngroups=8,data=POTN) 
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*Married,ngroups=8,data=POTN
              ,putlegend="topleft") #
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(JobWillBeAutomated),ngroups=8,data=POTN)
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(HasSeenTransformers),ngroups=8,data=POTN)
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(ScientistsGood),ngroups=8,data=POTN)
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(VaccinesGood),ngroups=8,data=POTN)
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(BelievesInGhosts),ngroups=8,data=POTN)
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(SmartSadDumbHappy),ngroups=8,data=POTN)
emplogitplot2(AnthClimateChangeReal~logBooksInPastYear*as.factor(ShowerPeeingOk),ngroups=8,data=POTN)


## model selection

formula = c(AnthClimateChangeReal~Age,
            AnthClimateChangeReal~logBooksInPastYear,
            AnthClimateChangeReal~Male,
            AnthClimateChangeReal~College,
            AnthClimateChangeReal~Married,
            AnthClimateChangeReal~JobWillBeAutomated,
            AnthClimateChangeReal~HasSeenTransformers,
            AnthClimateChangeReal~ScientistsGood,
            AnthClimateChangeReal~VaccinesGood,
            AnthClimateChangeReal~BelievesInGhosts,
            AnthClimateChangeReal~SmartSadDumbHappy,
            AnthClimateChangeReal~ShowerPeeingOk,
            AnthClimateChangeReal~Age*Male,
            AnthClimateChangeReal~Age*HasSeenTransformers,
            AnthClimateChangeReal~Age*BelievesInGhosts,
            AnthClimateChangeReal~logBooksInPastYear*Married
            )


pval = c();
for (k in 1:length(formula)) {
  pval <- c(pval,summary(glm(formula[[k]],family="binomial",data=POTN))$coefficients[2,4])
}

cbind(as.character(formula),pval)

full = glm(AnthClimateChangeReal ~
             Age +
             logBooksInPastYear +
             Male +
             College +
             Married +
             JobWillBeAutomated +
             HasSeenTransformers +
             ScientistsGood +
             VaccinesGood +
             BelievesInGhosts +
             SmartSadDumbHappy +
             ShowerPeeingOk +
             Age:Male +
             Age:HasSeenTransformers +
             Age:BelievesInGhosts +
             logBooksInPastYear*Married
             ,family="binomial",data=POTN)

null = glm(AnthClimateChangeReal ~
             1,family="binomial",data=POTN)

step(full,scope=list(lower=formula(null)),direction="backward") # suggests model 1
step(null,scope=list(upper=formula(full)),direction="forward") # suggests model 2

POTN$intx_age_male <- POTN$Age*POTN$Male
POTN$intx_age_transformers <- POTN$Age*POTN$HasSeenTransformers
POTN$intx_age_ghosts <- POTN$Age*POTN$BelievesInGhosts
POTN$intx_logbooks_married <- POTN$logBooksInPastYear*POTN$Married

bestglm(cbind(POTN[,c("Age",
                      "logBooksInPastYear",
                      "Male",
                      "College",
                      "Married",
                      "JobWillBeAutomated",
                      "HasSeenTransformers",
                      "ScientistsGood",
                      "VaccinesGood",
                      "BelievesInGhosts",
                      "SmartSadDumbHappy",
                      "ShowerPeeingOk",
                  #    "intx_age_male",
                      "intx_age_transformers",
                      "intx_age_ghosts",
                      "intx_logbooks_married",
                      "AnthClimateChangeReal")]
)) #suggests Model 3

testmodel1 = glm(AnthClimateChangeReal ~
                  ScientistsGood
                + SmartSadDumbHappy
                + Age
                + logBooksInPastYear
                + Male
                + Married
                + HasSeenTransformers
                + Age:HasSeenTransformers
                + logBooksInPastYear:Married
                ,family="binomial",data=POTN)
summary(testmodel1)

testmodel2 = glm(AnthClimateChangeReal ~
                   ScientistsGood
                 + SmartSadDumbHappy
                 + Age
                # + logBooksInPastYear
                 + Male
                 + JobWillBeAutomated
                 ,family="binomial",data=POTN)
summary(testmodel2)


testmodel3 = glm(AnthClimateChangeReal ~
                   ScientistsGood
                 + SmartSadDumbHappy
                 + logBooksInPastYear*Married
                 ,family="binomial",data=POTN)
summary(testmodel3)


# subset test if I want
G = model1$deviance - model2$deviance
df = model1$df.residual - model2$df.residual

(pval = pchisq(G, df, lower.tail=FALSE))


# choosing between models

compare.models <- function() {
  rows = sample.int(nrow(POTN),500)
  trainset <- POTN[rows,]
  remainset <- POTN[-rows,]
  
  trainmodel1 = glm(AnthClimateChangeReal ~
                     ScientistsGood
                   + SmartSadDumbHappy
                   + Age
                   + logBooksInPastYear
                   + Male
                   + Married
                   + HasSeenTransformers
                   + Age:HasSeenTransformers
                   + logBooksInPastYear:Married
                   ,family="binomial",data=POTN)
  
  trainmodel2 = glm(AnthClimateChangeReal ~
                     ScientistsGood
                   + SmartSadDumbHappy
                   + Age
                   # + logBooksInPastYear
                   + Male
                   + JobWillBeAutomated
                   ,family="binomial",data=POTN)
  
  trainmodel3 = glm(AnthClimateChangeReal ~
                     ScientistsGood
                   + SmartSadDumbHappy
                   + logBooksInPastYear*Married
                   ,family="binomial",data=POTN)
  
  # choosing model 1 or 2 by prediction accuracy
  xh1 = remainset[,c("ScientistsGood","SmartSadDumbHappy","Age","logBooksInPastYear",
                     "Male","Married","HasSeenTransformers","intx_age_transformers",
                     "intx_logbooks_married")]
  odds1 = exp(predict.glm(trainmodel1,newdata=xh1))
  prob1 = odds1/(1+odds1)
  answer1 = remainset[,"AnthClimateChangeReal"]
  head(round(cbind(xh1,odds1,prob1,answer1),3))
  
  xh2 = remainset[,c("ScientistsGood","SmartSadDumbHappy","Age","Male","JobWillBeAutomated")]
  odds2 = exp(predict.glm(trainmodel2,newdata=xh2))
  prob2 = odds2/(1+odds2)
  answer2 = remainset[,"AnthClimateChangeReal"]
  head(round(cbind(xh2,odds2,prob2,answer2),3))
  
  xh3 = remainset[,c("ScientistsGood","SmartSadDumbHappy","logBooksInPastYear",
                     "Married","intx_logbooks_married")]
  odds3 = exp(predict.glm(trainmodel3,newdata=xh3))
  prob3 = odds3/(1+odds3)
  answer3 = remainset[,"AnthClimateChangeReal"]
  head(round(cbind(xh3,odds3,prob3,answer3),3))
  
  correct1 = ifelse((prob1>.5 & answer1==1) | (prob1<.5 & answer1==0),1,0)
  propcorrect1 = sum(correct1)/nrow(remainset)
  
  correct2 = ifelse((prob2>.5 & answer2==1) | (prob2<.5 & answer2==0),1,0)
  propcorrect2 = sum(correct2)/nrow(remainset)
  
  correct3 = ifelse((prob3>.5 & answer3==1) | (prob3<.5 & answer3==0),1,0)
  propcorrect3 = sum(correct3)/nrow(remainset)
  
  return(cbind(propcorrect1,propcorrect2,propcorrect3)) # the models are very similar
}

x = c(); y = c(); z = c()
for(i in 1:1000) {
  x = c(x,compare.models()[1,1])
  y = c(y,compare.models()[1,2])
  z = c(z,compare.models()[1,3])
}

histogram(x,xlab="prop. correct answers, Model 1")
histogram(y,xlab="prop. correct answers, Model 2")
histogram(z,xlab="prop. correct answers, Model 3")

cbind(mean(x),mean(y),mean(z))

# 1: .6966731  .6975513  .6965833  avg: .697
# 2: .6867372  .6855577  .6862244  avg: .686
# 3: .6987308  .6989936  .7000128  avg: .699


## full models

model1 = glm(AnthClimateChangeReal ~
                    ScientistsGood
                  + SmartSadDumbHappy
                  + Age
                  + logBooksInPastYear
                  + Male
                  + Married
                  + HasSeenTransformers
                  + Age:HasSeenTransformers
                  + logBooksInPastYear:Married
                  ,family="binomial",data=POTN)

model2 = glm(AnthClimateChangeReal ~
                    ScientistsGood
                  + SmartSadDumbHappy
                  + Age
                  # + logBooksInPastYear
                  + Male
                  + JobWillBeAutomated
                  ,family="binomial",data=POTN)

model3 = glm(AnthClimateChangeReal ~
                    ScientistsGood
                  + SmartSadDumbHappy
                  + logBooksInPastYear
                  + Married
                  + logBooksInPastYear:Married
                  ,family="binomial",data=POTN)

summary(model1)
summary(model2)
summary(model3)

## conditions

# linearity (binary are automatically linear)
emplogitplot1(AnthClimateChangeReal~Age,ngroups=20,data=POTN)
emplogitplot1(AnthClimateChangeReal~logBooksInPastYear,ngroups=8,data=POTN)

check_collinearity(glm(AnthClimateChangeReal ~
                         ScientistsGood
                       + SmartSadDumbHappy
                       + Age
                       + logBooksInPastYear
                       + Male
                       + Married
                       + HasSeenTransformers
                       #+ Age:HasSeenTransformers
                       #+ logBooksInPastYear:Married
                       ,family="binomial",data=POTN))
check_collinearity(model2)
check_collinearity(glm(AnthClimateChangeReal ~
                         ScientistsGood
                       + SmartSadDumbHappy
                       + logBooksInPastYear
                       + Married
                       #+ logBooksInPastYear:Married
                       ,family="binomial",data=POTN))

cor(POTN[,c("ScientistsGood","SmartSadDumbHappy","Age","logBooksInPastYear","Male",
            "Married","HasSeenTransformers","JobWillBeAutomated")])

cor.test(logBooksInPastYear~SmartSadDumbHappy,data=POTN)
cor.test(Age~HasSeenTransformers,data=POTN)
cor.test(Age~Married,data=POTN)
cor.test(logBooksInPastYear~Male,data=POTN)

emplogitplot1(AnthClimateChangeReal~Age,ngroups=20,data=POTN)
emplogitplot1(AnthClimateChangeReal~logBooksInPastYear,ngroups=8,data=POTN)

# randomness and independence (can't check w R, based on data collection methods)

## using the models

# coefficients and odds ratios (should I do family confidence?)
coeff1 = model1$coefficients
se1 = as.numeric(summary(model1)$coefficients[,2])
zstar1 = qnorm(.05/2,lower.tail=FALSE)

coeff_lb1 = coeff1 - zstar1*se1
coeff_ub1 = coeff1 + zstar1*se1

oddsratio1 = exp(coeff1)
oddsratio_lb1 = exp(coeff_lb1)
oddsratio_ub1 = exp(coeff_ub1)

round(cbind(coeff1,oddsratio1,oddsratio_lb1,oddsratio_ub1),3)

coeff2 = model2$coefficients
se2 = as.numeric(summary(model2)$coefficients[,2])
zstar2 = qnorm(.05/2,lower.tail=FALSE)

coeff_lb2 = coeff2 - zstar2*se2
coeff_ub2 = coeff2 + zstar2*se2

oddsratio2 = exp(coeff2)
oddsratio_lb2 = exp(coeff_lb2)
oddsratio_ub2 = exp(coeff_ub2)

round(cbind(coeff2,oddsratio2,oddsratio_lb2,oddsratio_ub2),3)

coeff3 = model3$coefficients
se3 = as.numeric(summary(model3)$coefficients[,2])
zstar3 = qnorm(.05/2,lower.tail=FALSE)

coeff_lb3 = coeff3 - zstar3*se3
coeff_ub3 = coeff3 + zstar3*se3

oddsratio3 = exp(coeff3)
oddsratio_lb3 = exp(coeff_lb3)
oddsratio_ub3 = exp(coeff_ub3)

round(cbind(coeff3,oddsratio3,oddsratio_lb3,oddsratio_ub3),3)

model1 = glm(AnthClimateChangeReal ~
               ScientistsGood
             + SmartSadDumbHappy
             + Age
             + logBooksInPastYear
             + Male
             + Married
             + HasSeenTransformers
             + Age:HasSeenTransformers
             + logBooksInPastYear:Married
             ,family="binomial",data=POTN)

model2 = glm(AnthClimateChangeReal ~
               ScientistsGood
             + SmartSadDumbHappy
             + Age
             # + logBooksInPastYear
             + Male
             + JobWillBeAutomated
             ,family="binomial",data=POTN)

model3 = glm(AnthClimateChangeReal ~
               ScientistsGood
             + SmartSadDumbHappy
             + logBooksInPastYear
             + Married
             + logBooksInPastYear:Married
             ,family="binomial",data=POTN)


# prediction
xh = data.frame(cbind(
  ScientistsGood = c(1,0,1),
  SmartSadDumbHappy = c(1,0,1),
  Age = c(18,75,20),
  logBooksInPastYear = c(log(50),log(1),log(2)),
  Male = c(0,1,1),
  Married = c(1,0,0),
  JobWillBeAutomated = c(1,0,0),
  HasSeenTransformers = c(0,1,1),
  intx_age_transformers = c(18*0,75*1,20*1),
  intx_logbooks_married = c(log(50)*1,log(1)*0,log(2)*0)
))

odds1 = exp(predict.glm(model1,newdata=xh))
prob1 = odds1/(1+odds1)

odds2 = exp(predict.glm(model2,newdata=xh))
prob2 = odds2/(1+odds2)

odds3 = exp(predict.glm(model3,newdata=xh))
prob3 = odds3/(1+odds3)

round(cbind(xh,prob1,prob2,prob3),3)
