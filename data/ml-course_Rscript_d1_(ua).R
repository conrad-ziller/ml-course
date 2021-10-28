#Mulitlevel modelling, University of Zurich, Fall 2021, Instructor: Conrad Ziller


#Installing and loading of required packages
#install.packages("foreign")
#install.packages("lme4") # Allows you to fit linear mixed-effects models.
#install.packages("effects") # Helps you visualize interactions
#install.packages("corrplot") # Helps you to visualize correlations
#install.packages("sjPlot") # Makes tables
#install.packages("stargazer") # Makes tables
#install.packages("car") # Helps with Diagnostics
#install.packages("MASS") # For stepwise regression
#install.packages("ggplot2") # To create plots
#install.packages("texreg") # Allows you to save tables.
#install.packages("margins") # Allows you to create marginal effects of interactions.
#install.packages("predictmeans")

#Loading of required packages
library(foreign)
library(lme4)
library(effects)
library(corrplot)
library(car)
library(MASS)
library(ggplot2)
library(texreg)
library(sjPlot)
library(stargazer)
library(margins)
library(predictmeans)


#Loading of survey data


essdata <- read.dta("ESS9e03_1.dta", convert.factors=F)


#Creating numeric ID from string 

#essdata$id2 <- as.numeric(essdata$cntry)
#essdata$id <- factor(essdata$cntry)
#table(essdata$id)

macro <- read.dta("gdp_2018.dta", convert.factors=F)
essdata <- merge(essdata, macro, by="cntry")

table(macro$gdp_2018)
table(essdata$gdp_2018)


#Some recodings and index computations
essdata$trust <- (essdata$ppltrst +
                    essdata$pplfair +
                    essdata$pplhlp)/3 

essdata$income <- essdata$hinctnt
essdata$age <- essdata$agea
essdata$relig <- essdata$rlgdgr


##
##Random Intercept model
#Null model
M0 <- 
summary(M0)

#Calculating ICC from Null-Model
ICC.Model<-function(ri_null.mod) {
  tau.Null<-as.numeric(lapply(summary(M0)$varcor, diag))
  sigma.Null <- as.numeric(attr(summary(M0)$varcor, "sc")^2)
  ICC.Null <- tau.Null/(tau.Null+sigma.Null)
  return(ICC.Null)
}

ICC.Model(M0)
0.9535/(0.9535+3.2600) #test via variance components


#+Excluding observations with missing, reference: most complex model
Mfull <- 
summary(Mfull)
stargazer(Mfull,type="text") #shows No of obs
esample.n <- nobs(Mfull) #identifying valid cases and subsetting the data
esample <- rownames(as.matrix(resid(Mfull)))
essdata <- essdata[esample,]

#Reestimation of Model 0
M0 <- 
summary(M0)

#Model with individual-level predictors
M1 <- 
summary(M1)

#Model with individual-level  + country-level predictors
M2 <- 
summary(M2)

#Model comparisons



#Does the effect of income vary accross countries? --> plots
theme_set(theme_bw(base_size = 12, base_family = "")) 
Model.Plot.Friends <-ggplot(data = essdata, aes(x = income, y=trust,group=cntry))+   
  facet_grid( ~ cntry)+    
  geom_point(aes(colour = cntry))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = cntry))+  
  xlab("Education")+ylab("Trust")+    
  theme(legend.position = "none")   
Model.Plot.Friends  


#Model with random slope (no correlation btw RI and RS)
M3 <- 
summary(M3)



#Model with random slope (unstructured covariance)
M4 <- 
summary(M4)
coef(M4)


##Testing of Regression Assumptions
Plot.Model.F <- plot(M4)
Plot.Model.F
require("lattice")
qqmath(M4, id=0.05)
residplot(M4, level=1) 


#Model with logged gpd variable
essdata$ln_gdp <- log(essdata$gdp_2018)
M5 <- 
summary(M5)


##Interactions
M6 <- 
summary(M6)



#Create a table
tab_model(M0, M1, M5, M6)
