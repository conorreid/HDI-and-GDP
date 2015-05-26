#now that our data is good, let's do some analysis
#first, let's import the data
matrix2 <- read.csv("~/R Stuff/HDI and GDP per capita/matrix2.csv")
#now, a simple linear model of the pooled data
model1 <- lm(HDI ~ GDP.per.capita, data=matrix2)
summary(model1)
#however, this data is panel data
#therefore, let's import the plm package, made for working with panel data
install.packages("plm")
library(plm)
#with plm installed, we can run a fixed effect model
model2 <- plm(HDI ~ GDP.per.capita, data=matrix2, index=c("Country", "Year"), model="within")
summary(model2)
#now let's do an F test for Fixed Effect
pFtest(model2, model1)
#with Fixed Effects definitely present, let's make a random effect model
model3 <- plm(HDI ~ GDP.per.capita, data=matrix2, index=c("Country", "Year"), model="random")
summary(model3)
#now let's see if FE or RE model is better
#to do so, run a Hausman test
phtest(model2, model3)
#therefore, fixed effect model is more appropriate
#now, due to this being time series data, let's check stationarity
#first, let's import a package to let us do that
install.packages("tseries")
library(tseries)
#now, let's do some Dickey-Fuller tests
adf.test(matrix2$HDI)
adf.test(matrix2$GDP.per.capita)
#looks like, surprisingly, both are stationary
#now, let's test heteroskedasticity
#first, import a package to let us do so
install.packages("lmtest")
library(lmtest)
#now, let's do a Breusch Pagan test
bptest(model2)
#looks like there's definitely some heteroskedasticity
#next, let's look at our data in a plot
plot(matrix2$GDP.per.capita, matrix2$HDI)
plot(matrix2$HDI, matrix2$GDP.per.capita)
#looks like a logrothimic relationship
#a small movement of GDP results in a big change in HDI
#that changes, however, the higher in HDI you go
#at the high end, more GDP makes little difference
#this looks like a sqrt(x) graph
plot(matrix2$GDP.per.capita.sqrt, matrix2$HDI)
#nope, never mind; that did not help
#instead, let's take the log of GDP
plot(matrix2$log.GDP, matrix2$HDI)
#that did the trick it seems
#now, a simple linear model of the pooled data with the new variable
modelln1 <- lm(HDI ~ log.GDP, data=matrix2)
summary(modelln1)
#a good model! Now time for panel data with fixed effect
modelln2 <- plm(HDI ~ log.GDP, data=matrix2, index=c("Country", "Year"), model="within")
summary(modelln2)
#now let's do an F test for Fixed Effect
pFtest(modelln2, modelln1)
#with Fixed Effects definitely present, let's make a random effect model
modelln3 <- plm(HDI ~ log.GDP, data=matrix2, index=c("Country", "Year"), model="random")
summary(modelln3)
#now let's see if FE or RE model is better
#to do so, run a Hausman test
phtest(modelln2, modelln3)
#again, fixed effect model is the better model
#it has a much stronger R-squared (.62 versus around .20) with the log variable
#let's test for stationarity of the log variable
adf.test(matrix2$log.GDP)
