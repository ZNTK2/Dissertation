
## Reproducable code for my undergraduate dissertation (UCL BSc Politics and International Relations)

## Loading dataset

setwd("C:/Users/ZNTK2/Desktop/Dissertation/Data Sources")
d <- read.csv("disso data.csv")
colnames(d)[1] <- gsub('^...','',colnames(d)[1])


## Loding packages

install.packages("stargazer")
library(stargazer)

install.packages('car')
library(car)

install.packages("lmtest")
library(lmtest)

install.packages("corrplot")
library(corrplot)


## Binary logistic regression models

## Simple model without trend controls 

lm1 <- glm(dv_identity ~ poltransform + econtransform + socialtransform + factor(region) + age + gender + educ + 
             colyears + factor(colpower) + colpopulation + loggdp1980 + loggdp2013 + democ2013, 
           data = d, family = binomial)

## Simple model with ethndom

lm2 <- glm(dv_identity ~ poltransform + econtransform + socialtransform + factor(region) + age + gender + educ + 
             colyears + factor(colpower) + colpopulation + loggdp1980 + loggdp2013 + democ2013 + ethndom, 
           data = d, family = binomial)

## Simple model with ethndom and conflictyears

lm3 <- glm(dv_identity ~ poltransform + econtransform + socialtransform + factor(region) + age + gender + educ + 
             colyears + factor(colpower) + colpopulation + loggdp1980 + loggdp2013 + democ2013 + ethndom + 
             conflictyears, data = d, family = binomial)

## Standard model

standardmodel <- glm(dv_identity ~ poltransform + econtransform + socialtransform + factor(region) + age + gender + 
                       educ + colyears + factor(colpower) + colpopulation + loggdp1980 + loggdp2013 + democ2013 + 
                       ethndom + conflictyears + coastal, data = d, family = binomial)

models <- list(lm1, lm2, lm3, standardmodel)


## Logistic regression model output

stargazer(models, title = "Table 2: Logit Model Results", align = TRUE, 
          column.labels = c(" Simplified model ", " With ethndom " , " With conflictyears ", " Standard model "), 
          covariate.labels = c("poltransform", "econtransform", "socialtransform", "region: MA", "region: SA", 
                               "region: WA", "age", "gender", "educ", "colyears", "colpower: France", 
                               "colpower: Portugal", "colpower: UK", "colpower: US"), 
          dep.var.labels = " ", 
          dep.var.caption = "Dependent variable is identity choice",
          out.header = TRUE, type = "html", out = "logitresults.html")


## Converting the coefficients 

exp(0.044) ## poltransform 1.045 = 4.5%
exp(0.005) ## econtransform 1.005 = 0.5%
exp(0.038) ## socialtransform 1.039 = 3.9%


## Likelihood ratio test for differences in model

lrtest(lm1, standardmodel)


## Tanzania identity data

tandata <- subset(d, d$country_alpha == 30) ## Subsetting for Tanzania

## Proportion for identity choice in Tanzania

prop.table(table(tandata$dv_identity)) ##68.8% of population in Tanzania choosing national identity 

## Proportion for identity choice in the region

prop.table(table(d$dv_identity)) ## 49.7% of the region choosing national identity



## Logistic regression model without Tanzania

notandata <- subset(d, !d$country_alpha == 30)

notanmodel <- glm(dv_identity ~ poltransform + econtransform + socialtransform + factor(region) + age + gender + 
                    educ + colyears + factor(colpower) + colpopulation + loggdp1980 + loggdp2013 + democ2013 + 
                    ethndom + conflictyears + coastal, data = notandata, family = binomial)

stargazer(notanmodel, title = "Table 4: Logit Model (w/o Tanzania) Results", align = TRUE, 
          dep.var.labels = " ", dep.var.caption = "Dependent variable is identity choice", 
          out.header = TRUE, type = "html", out = "notanmodel.html")


## Nigeria identity data

nigeriadata <- subset(d, d$country_alpha == 24)

## Proportion for identity choice in Nigeria

prop.table(table(nigeriadata$dv_identity)) ## 14.5% choosing national identity  


## Pairwise correlations for variables

cordata <- d[, c("poltransform", "econtransform","socialtransform", "age", "gender", "educ", 
                 "colyears", "loggdp1980", "loggdp2013", "democ2013", "ethndom", "conflictyears", "coastal")]
pwcor <- cor(cordata)


## Pairwise correlations output

corrplot(pwcor, method = 'square', type = 'lower', addCoef.col = 'black', tl.col = 'black', number.cex = 0.5)

prop.table(table(nigeriadata$dv_identity)) ## 14.5% choosing national identity 


## Proportion of survey answers

prop.table(table(d$rawidentity)) ## 39.8% chose option 3 


## Alternative model: Multiple linear regression model

linearmodel <- lm(dv_identity ~ poltransform + econtransform + socialtransform + factor(region) + age + gender + 
                    educ + colyears + factor(colpower) + colpopulation + loggdp1980 + loggdp2013 + democ2013 + 
                    ethndom + conflictyears + coastal, data = d)

stargazer(linearmodel, title = "Table 3: Multiple Linear Regression Results", 
          align = TRUE, type = "html", out = "results.html")



