Untitled
================

``` r
library(e1071)
library(dplyr)




titanic_train = read.csv("D://train.csv", header=T)

titanic_test = read.csv("D://test.csv", header = T)


titanic_test$Age[is.na(titanic_test$Age)] = mean(titanic_test$Age, na.rm=TRUE)
titanic_train$Age[is.na(titanic_train$Age)] = mean(titanic_train$Age, na.rm=TRUE)



titanic_train$Survived= as.factor(titanic_train$Survived)
titanic_train$Pclass= as.factor(titanic_train$Pclass)
titanic_train$Sex= as.factor(titanic_train$Sex)




titanic_test$Survived= as.factor(titanic_test$Survived)
 titanic_test$Pclass= as.factor(titanic_test$Pclass)
titanic_test$Sex= as.factor(titanic_test$Sex)


model = naiveBayes(Survived~Age+Pclass+Sex+Parch, data = titanic_train)
model
```

    ## 
    ## Naive Bayes Classifier for Discrete Predictors
    ## 
    ## Call:
    ## naiveBayes.default(x = X, y = Y, laplace = laplace)
    ## 
    ## A-priori probabilities:
    ## Y
    ##         0         1 
    ## 0.6161616 0.3838384 
    ## 
    ## Conditional probabilities:
    ##    Age
    ## Y       [,1]     [,2]
    ##   0 30.41510 12.45737
    ##   1 28.54978 13.77250
    ## 
    ##    Pclass
    ## Y           1         2         3
    ##   0 0.1457195 0.1766849 0.6775956
    ##   1 0.3976608 0.2543860 0.3479532
    ## 
    ##    Sex
    ## Y      female      male
    ##   0 0.1475410 0.8524590
    ##   1 0.6812865 0.3187135
    ## 
    ##    Parch
    ## Y        [,1]     [,2]
    ##   0 0.3296903 0.823166
    ##   1 0.4649123 0.771712

``` r
model$importance
```

    ## NULL

``` r
Survived=predict(model, titanic_test)
View(Survived)


PassengerId=titanic_test$PassengerId
View(PassengerId)

PassengerId= cbind(PassengerId, Survived)
View(PassengerId)

write.csv(PassengerId, file = "D://titanic.csv")







titanic_train = read.csv("D://train.csv", header=T)

titanic_test = read.csv("D://test.csv", header = T)

model = glm(Survived~Age+Sex+Pclass, titanic_train, family = "binomial")

model
```

    ## 
    ## Call:  glm(formula = Survived ~ Age + Sex + Pclass, family = "binomial", 
    ##     data = titanic_train)
    ## 
    ## Coefficients:
    ## (Intercept)          Age      Sexmale       Pclass  
    ##     5.05601     -0.03693     -2.52213     -1.28855  
    ## 
    ## Degrees of Freedom: 713 Total (i.e. Null);  710 Residual
    ##   (177 observations deleted due to missingness)
    ## Null Deviance:       964.5 
    ## Residual Deviance: 647.3     AIC: 655.3

``` r
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Age + Sex + Pclass, family = "binomial", 
    ##     data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7270  -0.6799  -0.3947   0.6483   2.4668  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  5.056006   0.502128  10.069  < 2e-16 ***
    ## Age         -0.036929   0.007628  -4.841 1.29e-06 ***
    ## Sexmale     -2.522131   0.207283 -12.168  < 2e-16 ***
    ## Pclass      -1.288545   0.139259  -9.253  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 964.52  on 713  degrees of freedom
    ## Residual deviance: 647.29  on 710  degrees of freedom
    ##   (177 observations deleted due to missingness)
    ## AIC: 655.29
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
res=predict(model, titanic_test, type = "response")
View(res)

sur= ifelse(res>0.5,1,0)
View(sur)


sur = cbind(sur, titanic_test$PassengerId)
```
