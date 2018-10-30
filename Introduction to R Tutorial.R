####### Welcome to the Introduction to  R workshop
# is the method to comment out lines, everything you see in green will not be run in R
remove(list=ls()) #This removes your entire workspace.

####### Saving and Loading a File
##
a<-1
save(a,file="a.Rdata")
load("a.Rdata")

save.image() #This saves your entire workspace
#--------------------------------------------------------------------------------------------------------------------#

####### Reading Data into R
##
####### read.csv(), read.table(), read.delim(),read.dat() are all functions you can use to read stuff into R
##
help(read.csv)
args(read.csv) #This function tells you about what arguments you need to input to read.csv


####### But what about STATA datasets?
####### R is a collaborative open source software, so people write code and publish it online not only for credit 
####### but to help others
####### There's a software package we can use, and in order to do that, we install a package for reading in foreign datasets
##
install.packages("foreign")

####### Once you do that, you need to load the package.
library(foreign)

####### To access the help files for the foreign package, enter the following below:
##
help(package="foreign")
dataset <- read.dta("C:/Users/urnti/Downloads/hire771.dta") 

####### Let's look at the data
####### summary() and plot() can be useful here
##
summary(dataset)

####### What's the mean age?
##
mean(dataset$age)
plot(x=dataset$age,y=dataset$salary) #Let's look at age and salary
plot(x=dataset$age,y=dataset$salary,col=dataset$female+3) #We can even color the datapoints by sex!
legend("topleft",c("Females","Males"),lty=1,col=c("green","blue")) #And we can add a legend!

####### What's the mean age for females? tapply is a function which lets you apply a function to subgroup when 
####### you have a subgroup index (like dataset$female)
##
help(tapply)
tapply(dataset$salary,dataset$female,mean)
table(dataset$female,dataset$educ2) #Use table to get counts

#--------------------------------------------------------------------------------------------------------------------#
####### Let's run a linear regression now! Let's regress log salary on sex to see 
####### is there a difference in log salary between males and females
####### To create a new variable in our data, all we have to do is use the $ operator and call it a new thing
##
dataset$logsal <- log(dataset$salary)
Model <- lm(logsal~female,data=dataset)
summary(Model)

####### You'll notice the summary command did something different on the model 
####### than what happened when we ran summary command on the dataset
####### That's because R 
##
class(dataset)
class(Model)


####### Let's run diagnostics on our model to see how well we're doing
##
plot(Model)

####### Yikes it doesn't look like this fits well at all, let's add some more variables
##
Model2<-lm(logsal~female+educ2,data=dataset)

####### Let's run an ANOVA, the likelihood ratio test is a goodness of fit test
##
anova(Model,Model2)

####### Let's say half of the dataset is coded incorrectly and we want to remove it, how would we do so?
####### Subsetting! When you identify the rows or columns you want,
##
n<-dim(dataset)[2]
dataset2<-dataset[1:n,]

Model2<-lm(y~x,data=dataset2)
summary(Model2)
anova(Model2)

#--------------------------------------------------------------------------------------------------------------------#
####### How to write a function
####### A function requires a set of inputs that you perform set operations on to get an output
####### Note that you can only get one output for a function
##
Function1<-function(Francis){
  a<-3
  b<-Francis+3
  return(b) #return is what specifies the output of the function. If you try and return more than one output, R will get mad.
}

Function1(3)
Function1("pork")

####### Uh oh, an error has occurred. Set the option below when you want R to pause when it hits an error and let you explore
####### the various environments you're in. 
##
options(error=recover)

####### When you want it to go back to normal, set it back using this.
##
options(error=NULL)

#######  Other useful functions include the browser function, which let you stop in the middle and figure out what's going on by pretending you're running this on the command line
##
Function2<-function(Francis){
  a<-3
  browser()
  b<-Francis+3
  return(list(a,b))
}
pork<-Function2(4)
unlist(pork)

#######  debug() basically acts as if you put a browser on the first line of the function. Use undebug() to turn it off.
##
debug(Function1)
Function1(3)
undebug(Function1)

#--------------------------------------------------------------------------------------------------------------------#
####### Let's see it in action! Let's write a function for calculating robust standard errors
####### These functions taken from Dan Gillen's Stats 211 Class List of Helper Functions at UC Irvine.
##
robust.vcov.lm <- function( lm.obj ){
  X <- model.matrix( lm.obj )
  eps <- lm.obj$residuals
  robust.cov <- solve( t(X)%*%X ) %*%( t(X) %*% diag(eps^2) %*% X ) %*% solve( t(X)%*%X )
  dimnames( robust.cov ) <- dimnames( vcov(lm.obj) )
  return( robust.cov )
}

robust.se.lm <- function( model) { 
  s <- summary( model) 
  X <- model.matrix( model )
  sandwich.cov <- robust.vcov.lm( model )
  sand.se <- sqrt( diag( sandwich.cov )) 
  t <- model$coefficients/sand.se
  p <- 2*pt( -abs( t ), dim(X)[1]-dim(X)[2] ) 
  ci95.lo <- model$coefficients - qt( .975, dim(X)[1]-dim(X)[2] ) * sand.se
  ci95.hi <- model$coefficients + qt( .975, dim(X)[1]-dim(X)[2] ) * sand.se
  rslt <- cbind( model$coefficients, sand.se, ci95.lo, ci95.hi, t, p ) 
  dimnames(rslt)[[2]] <- c( dimnames( s$coefficients )[[2]][1], "Robust SE", "ci95.lo", "ci95.hi", dimnames( s$coefficients )[[2]][3:4] ) 
  rslt 
} 
robust.se.lm(Model)
#--------------------------------------------------------------------------------------------------------------------#
####### Generalized Linear Models!
####### How do you fit a logistic regression? Let's suppose we wanted to examine the probability of being married by sex
####### And let's also fit one that examines the probability of being married by sex and education level
##
LogisticModel<-glm(married~female,data=dataset,family="binomial")
LogisticModel2<-glm(married~female+educ2,data=dataset,family="binomial")

#What if I only wanted to look at certain things?
##
attributes(summary(LogisticModel))

####### When assessing model fit, how do you do a likelihood ratio test?
##
####### Under GLMs, you rely on asymptotic normality, and so you compare likelihoods via the Chi-Squared test.
##
####### How do you get the chi-squared CDF?
##
summary(LogisticModel)
summary(LogisticModel2) 

####### Look at the difference in the residual deviance in the model, and the difference in degrees of freedom
##
pchisq(4207.6-4190.7,3,lower.tail=FALSE)

####### You can use pchisq, dchisq, qchisq rchisq to either find the distribution, density, qchisq and even generate a random chi squared variable.
####### For well known distributions (usually exponential family), you can repeat this for *norm, *poisson, *binom, *exp, *gamma, etc.

####### Let's fit a poisson regression!
##
PoissonModel<-glm(salary~female,data=dataset,family="poisson")
summary(PoissonModel)

####### How to fit a QuasiPoisson?
##
####### Why are we fitting this?
##
QuasiPoissonModel<-glm(salary~female,data=dataset,family="quasipoisson")
summary(QuasiPoissonModel)

install.packages("MASS")
library(MASS)

####### Let's suppose the functional form of the variance is akin to a Gamma Poisson mixture (giving us a negative binomial distributed errors)
##
NegBinModel<-glm.nb(salary~female,data=dataset)
summary(NegBinModel)

#--------------------------------------------------------------------------------------------------------------------#
####### Longitudinal data!
##
install.packages("gee")
install.packages("nlme")
library(gee)
library(nlme)

####### gee() for generalized estimating equations (semi-parametric, treat correlation structure between observations as nuisance)
##
####### lme() for longitudinal mixed effects. Model fully parametric
## 
install.packages("readstata13")
library(readstata13)

#######  Data taken from Cross-National Time Series Data Archive https://www.cntsdata.com/
##
YBData<-read.dta13("D:/Francis/Downloads/YB and conflict data 5.dta") 

####### We need to change id to a numeric variable, otherwise it won't run
##
YBData$country1<-match(YBData$country,unique(YBData$country))
head(YBData) #Look we appended this onto the dataset

####### Let's fit a model with no correlation between observations looking at number of revolutions cross-nationally since the 1950s.
##
fit1 <- gee( revolutions~ I(year-1950), id=country1, family=poisson,data=YBData, corstr="independence") 
summary( fit1 )

#######	Let's fit a model where observations have an exchangeable correlation structure 
##
fit2 <- gee( revolutions ~ I(year-1950),id=country1,family=poisson,data=YBData, corstr="exchangeable")
summary( fit2 )

####### Linear Mixed Effects
##
####### Is a random intercepts AND slopes model justified?
YBData1<-YBData[-c(which(is.na(YBData$year))), ]
fit3<-lme( revolutions ~ I(year-1950),
               method = "REML",
               random = reStruct( ~ 1 | country, pdClass="pdSymm"),
               data = YBData1 )

fit4<-lme( revolutions ~ I(year-1950),
           method = "REML",
           random = reStruct( ~ 1+ I(year-1950)| country, pdClass="pdSymm"),
           data = YBData1 )
anova(fit3,fit4)

####### By now, we should have the hour mark. This is probably where I'm going to leave off. Thank you for listening.
