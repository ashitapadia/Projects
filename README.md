# Project
#Fall 2019 Projects 
#Statistics Project
#World Happiness Rank Prediction

---
title: "Project"
author: "Team : Movies 2"
date: "November 21, 2019"
output: html_document
---


```{r}
#uploading the data
data <- read.csv("data.csv")
```

```{r}
#Renaming the columns
colnames(data)[colnames(data)=="GINI.index..World.Bank.estimate...average.2000.16"] <- "Gini Index"
colnames(data)[colnames(data)=="gini.of.household.income.reported.in.Gallup..by.wp5.year"] <- "GI_household"
```

```{r}
#Data types of the variables
str(data)
#write description about it in Google docs
```

```{r}
#subset the data because we don't need a few column like year, country name as they don't have any signiicant use. 
#Use of Domain
data <- data[c(-1,-2,-3)]
```


```{r}
#Seeing distributions of the variables
#As we have just continuous distributions we will plot histograms and density plots 
#Target Variable
h <- hist(data$Happiness.score, main="Target Variable", col="pink", xlab="Happiness Score")
xfit <-seq(min(data$Happiness.score),max(data$Happiness.score),length=40)
yfit<-dnorm(xfit,mean=mean(data$Happiness.score),sd=sd(data$Happiness.score))
yfit <- yfit*diff(h$mids[1:2])*length(data$Happiness.score)
lines(xfit, yfit, col="blue", lwd=2)
#The Target Variable is normally distributed.
```


```{r}
#Predictor Variables
#GDP
h <- hist(data$Log.GDP.per.capita, main="Predictor Variable 1", col="maroon", xlab="GDP")
xfit <-seq(min(data$Log.GDP.per.capita),max(data$Log.GDP.per.capita),length=40)
yfit<-dnorm(xfit,mean=mean(data$Log.GDP.per.capita),sd=sd(data$Log.GDP.per.capita))
yfit <- yfit*diff(h$mids[1:2])*length(data$Log.GDP.per.capita)
lines(xfit, yfit, col="yellow", lwd=2)
#GDP is not normally distributed. So we can say that it is the real case, because the GDP of different countries differ.

#Social Support
h <- hist(data$Social.support, main="Predictor Variable 2", col="yellow", xlab="Social Support")
xfit <-seq(min(data$Social.support),max(data$Social.support),length=40)
yfit<-dnorm(xfit,mean=mean(data$Social.support),sd=sd(data$Social.support))
yfit <- yfit*diff(h$mids[1:2])*length(data$Social.support)
lines(xfit, yfit, col="maroon", lwd=2)
#Social Support is left skewed.

#Healthy life expectancy at birth
h <- hist(data$Healthy.life.expectancy.at.birth, main="Predictor Variable 3", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Healthy.life.expectancy.at.birth),max(data$Healthy.life.expectancy.at.birth),length=40)
yfit<-dnorm(xfit,mean=mean(data$Healthy.life.expectancy.at.birth),sd=sd(data$Healthy.life.expectancy.at.birth))
yfit <- yfit*diff(h$mids[1:2])*length(data$Healthy.life.expectancy.at.birth)
lines(xfit, yfit, col="maroon", lwd=2)

#Freedom to make life choices
h <- hist(data$Freedom.to.make.life.choices, main="Predictor Variable 4", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Freedom.to.make.life.choices),max(data$Freedom.to.make.life.choices),length=40)
yfit<-dnorm(xfit,mean=mean(data$Freedom.to.make.life.choices),sd=sd(data$Freedom.to.make.life.choices))
yfit <- yfit*diff(h$mids[1:2])*length(data$Freedom.to.make.life.choices)
lines(xfit, yfit, col="maroon", lwd=2)
#It is left skewed.

#Generosity
h <- hist(data$Generosity, main="Predictor Variable 5", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Generosity),max(data$Generosity),length=40)
yfit<-dnorm(xfit,mean=mean(data$Generosity),sd=sd(data$Generosity))
yfit <- yfit*diff(h$mids[1:2])*length(data$Generosity)
lines(xfit, yfit, col="maroon", lwd=2)

#Perceptions of corruption 
h <- hist(data$Perceptions.of.corruption, main="Predictor Variable 6", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Perceptions.of.corruption),max(data$Perceptions.of.corruption),length=40)
yfit<-dnorm(xfit,mean=mean(data$Perceptions.of.corruption),sd=sd(data$Perceptions.of.corruption))
yfit <- yfit*diff(h$mids[1:2])*length(data$Perceptions.of.corruption)
lines(xfit, yfit, col="maroon", lwd=2)

#Positive affect  
h <- hist(data$Positive.affect, main="Predictor Variable 7", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Positive.affect),max(data$Positive.affect),length=40)
yfit<-dnorm(xfit,mean=mean(data$Positive.affect),sd=sd(data$Positive.affect))
yfit <- yfit*diff(h$mids[1:2])*length(data$Positive.affect)
lines(xfit, yfit, col="maroon", lwd=2)

#Negative affect 
h <- hist(data$Negative.affect, main="Predictor Variable 8", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Negative.affect),max(data$Negative.affect),length=40)
yfit<-dnorm(xfit,mean=mean(data$Negative.affect),sd=sd(data$Negative.affect))
yfit <- yfit*diff(h$mids[1:2])*length(data$Negative.affect)
lines(xfit, yfit, col="maroon", lwd=2)

#Confidence in national government
h <- hist(data$Confidence.in.national.government, main="Predictor Variable 9", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Confidence.in.national.government),max(data$Confidence.in.national.government),length=40)
yfit<-dnorm(xfit,mean=mean(data$Confidence.in.national.government),sd=sd(data$Confidence.in.national.government))
yfit <- yfit*diff(h$mids[1:2])*length(data$Confidence.in.national.government)
lines(xfit, yfit, col="maroon", lwd=2)

#Democratic Quality 
h <- hist(data$Democratic.Quality, main="Predictor Variable 10", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Democratic.Quality),max(data$Democratic.Quality),length=40)
yfit<-dnorm(xfit,mean=mean(data$Democratic.Quality),sd=sd(data$Democratic.Quality))
yfit <- yfit*diff(h$mids[1:2])*length(data$Democratic.Quality)
lines(xfit, yfit, col="maroon", lwd=2)

#Delivery Quality
h <- hist(data$Delivery.Quality, main="Predictor Variable 11", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Delivery.Quality),max(data$Delivery.Quality),length=40)
yfit<-dnorm(xfit,mean=mean(data$Delivery.Quality),sd=sd(data$Delivery.Quality))
yfit <- yfit*diff(h$mids[1:2])*length(data$Delivery.Quality)
lines(xfit, yfit, col="maroon", lwd=2)

#Gini Index   
h <- hist(data$Gini Index, main="Predictor Variable 12", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$Gini Index),max(data$Gini Index),length=40)
yfit<-dnorm(xfit,mean=mean(data$Gini Index),sd=sd(data$Gini Index))
yfit <- yfit*diff(h$mids[1:2])*length(data$Gini Index)
lines(xfit, yfit, col="maroon", lwd=2)

#GI_household
h <- hist(data$GI_household, main="Predictor Variable 13", col="yellow", xlab="Healthy life expectancy at birth")
xfit <-seq(min(data$GI_household),max(data$GI_household),length=40)
yfit<-dnorm(xfit,mean=mean(data$GI_household),sd=sd(data$GI_household))
yfit <- yfit*diff(h$mids[1:2])*length(data$GI_household)
lines(xfit, yfit, col="maroon", lwd=2)
```

```{r}
#Correlations amongst Predictor Variables
#Pearson
cor_pearson <- cor(data, use="complete.obs", method="pearson")
cor_pearson
#Spearman
cor_spearman <- cor(data, use="complete.obs", method="spearman")
cor_spearman

#Plotting the correlations 
install.packages("corrplot")
library("corrplot")
corrplot1 <- corrplot(cor_pearson, method = "circle", type = "upper")
corrplot2 <- corrplot(cor_spearman, method = "ellipse", type = "upper")
```

```{r}
#Scatterplot
#Social Support vs GDP
plot1 <- plot(data$Social.support, data$Log.GDP.per.capita, main="Social Support vs GDP",xlab="Social Support ", ylab="GDP ", pch=19, col= "grey")

#Corruption vs Confidence in Government
plot2 <- plot(data$Perceptions.of.corruption , data$Confidence.in.national.government, main = "Corruption vs Confidence in Government", xlab = "Corruption", ylab = "Confidence in Government", pch = 19, col = "grey")

#Freedom to make choice vs Democratic Quality
plot3 <- plot(data$Freedom.to.make.life.choices , data$Democratic.Quality, main = "Freedom to make Choice vs Democratic Quality", xlab = "Freedom ", ylab = "Democratic Quality", pch = 19, col = "grey")

```
```{r}
#Probability Concepts
#If Happiness Score >= Mean of Happiness Score then it is High Score else Low Score
data$cat_Happiness.Score <- ifelse(
  data$Happiness.score >= mean(data$Happiness.score), "High Score", "Low Score"
)

#If GDP >= Mean of GDP then it is High GDP else Low GDP
data$cat_Log.GDP.per.capita <- ifelse(
  data$Log.GDP.per.capita >= mean(data$Log.GDP.per.capita), "High GDP", "Low GDP"
)

#Table
table(data$cat_Happiness.Score,
```


```{r}
data$cat_Log.GDP.per.capita)
```


```{r}
#Probabilties Q3. - ALEX
```

```{r}
#Chi-Square Test

# State the hypothesis :
# Null hypothesis: Happiness Score is unrelated to GDP per capita.
# Alternative hypothesis: Hapiness Score is related to GDP per capita.

# Run a chi-squared test
chisq.test(data$cat_Happiness.Score,
           data$cat_Log.GDP.per.capita,
           correct = FALSE)

# The p-value is less than 0.05, we can reject the null hypothesis,
# and say happiness score and GDP per capita are in fact dependent!

# After calculating by hand, the chi-squared statistic is 33.323
# The degree of freedom DF = (2-1)*（2-1） = 1

# run a chisq test
1 - pchisq(33.3349，1)

# The result shows p-value = 7.757784e-09
# With such a small p-value, we reject the null hypothesis (NULL = there is no difference between happiness score and GDP per capita) 
# and accept the alternative hypothesis (there is a difference between the two groups)!

```
```{r}
#Q4. to be typed here - Alex
```

```{r}
#As our target variable is continuous (numeric target) therefore we will use linear regression for building the model.
```

```{r}
#a. Fit a full model
fit.full <- lm(Happiness.score ~. ,  data=data)
summary(fit.full)
```

```{r}
#b. Fit a null model
fit.null = lm(Happiness.score ~1 , data=data)
summary(fit.null)
```

```{r}
#c. Using some form of stepwise regression (forward, backward, both), fit a reduced model.
#VIF - to find the correlated Predictor Variables
car::vif(fit.full)
```


```{r}
# Let us build a model using forward stepwise regression
stepforward_null <- step(fit.null,scope=list(lower=formula(fit.null),upper=formula(fit.full)),direction="forward")
summary(stepforward_null)
```

```{r}
# Let us build a model using backward stepwise regression
stepback_full <- step(fit.full)
summary(stepback_full)
```

```{r}
#Mixed Model
stepmixed_full <- step(fit.null,scope=list(lower=formula(fit.null),upper=formula(fit.full)),direction="both")
summary(stepmixed_full)
```

```{r}
#On comparing the models,we get the same variable selection using different stepwise regression - 
fit.reduced <- stepmixed_full 
summary(fit.full)
summary(fit.reduced)
```

```{r}
#In the fit full model, the p-value of Healthy life expectancy at birth, Generosity, Democratic Quality, Delivery Quality and GINI index on average is more than 0.05 so these variables are not significant. Therefore, we need to do the variable selection. In the reduced model, we keep 9 variables using stepwise regression and VIF, which include Log GDP per capita, Positive affect, Negative affect, GINI household income, Perceptions of corruption, Social support, Confidence in national government, Freedom to make life choices and GINI index on average. And p-value of these variables are less than 0.05 and significant. The coefficient of “Log GDP per capita” is estimated as 0.75708, This mean that, aLL else equal, for every one unit change in the Log GDP per capita , the happiness score will increase by 0.75708. The coefficient of “perception of corruption” is estimated as -1.40081, This mean that, aLL else equal, for every one unit change in the perception of corruption, the happiness score will decrease by 1.40081.
```

```{r}
#For Full Model
#Actual vs Predicted Target Variable : Happiness Score
plot(y=fit.full$fitted.values,x=data$Happiness.score,pch=19,col="red",
     main="Actual vs Predicted Target : Happiness Score",
     ylab="Predicted",
     xlab="Actual")
abline(0,1)
conf_interval1 <-predict(fit.full,data,interval="confidence",level=0.95)
conf_interval1
pred_interval1 <- predict(fit.full,data,interval="predict",
                         level = 0.95)
pred_interval1
lines(data$Happiness.score, conf_interval1[,2], col="yellow", lty=2)
lines(data$Happiness.score, conf_interval1[,3], col="yellow", lty=2)
lines(data$Happiness.score, pred_interval1[,2], col="orange", lty=2)
lines(data$Happiness.score, pred_interval1[,3], col="orange", lty=2)

data$preds1 <- fit.full$fitted.values
RMSE1<- sqrt(mean((data$Happiness.score-data$preds1)^2))
RMSE1

MAE1 <- mean(abs(data$Happiness.score-data$preds1))
MAE1
summary(fit.full)
```

```{r}
#For Reduced Model
#Actual vs Predicted Target Variable : Happiness Score
plot(y=fit.reduced$fitted.values,x=data$Happiness.score,pch=19,col="red",
     main="Actual vs Predicted Target : Happiness Score",
     ylab="Predicted",
     xlab="Actual")
abline(0,1)
conf_interval2 <- predict(fit.reduced,data,interval="confidence",level=0.95)
conf_interval2
pred_interval2 <- predict(fit.reduced,data,interval="predict",
                         level = 0.95)
pred_interval2
lines(data$Happiness.score, conf_interval2[,2], col="yellow", lty=2)
lines(data$Happiness.score, conf_interval2[,3], col="yellow", lty=2)
lines(data$Happiness.score, pred_interval2[,2], col="orange", lty=2)
lines(data$Happiness.score, pred_interval2[,3], col="orange", lty=2)

data$preds2 <- fit.reduced$fitted.values
RMSE2 <-sqrt(mean((data$Happiness.score-data$preds2)^2))
RMSE2

MAE2 <- mean(abs(data$Happiness.score-data$preds2))
MAE2
summary(fit.reduced)
```

```{r}
#We focused on the adjusted R square value because adjusted R square uses the penalty on the number of predictor variables. The R square of fit full model is 0.8169 and the R-square of fit reduced model is 0.818. The two values are roughly equal. And the RMSE of fit full model is 0.4521839 and the MAE of fit full model is 0.3619354,which is not much different from the RMSE(0.4574851) and MAE (0.3656677)of fit reduced model. 
#Therefore, for the accuracy of the prediction model, the two models (fit.full and fit.reduced) are similar. 
```


```{r}
#Model Comparision
#AIC and BIC
#AIC
AIC(fit.full)
AIC(fit.reduced)
#BIC
BIC(fit.full)
BIC(fit.reduced)

#The value of AIC and BIC for the reduced model is less than the value of AIC and BIC for the full model. 
#The lesser the value of AIC and BIC, the better the model fit is  Therefore, the reduced model fits better than the full model.
```





