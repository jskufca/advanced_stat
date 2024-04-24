################################################
# Code file for
# Introduction to Survival Analysis in R workshop
#
# Solutions to exercises at bottom
################################################

## ----packages, message=F-------------------------------------------------------------
library(survival)
library(survminer) # for customizable graphs of survival function
library(broom) # for tidy output 
library(ggplot2) # for graphing (actually loaded by survminer)
head(jasa1)


## ----Kaplan-Meier estimation with survfit()------------------------------------------
# ~ 1 indicates KM survival estimtes for whole sample
KM <- survfit(Surv(time, status) ~ 1, data=aml)


## ----survfit print-------------------------------------------------------------------
print(KM) 
# same as print(KM)
KM


## ----Table of KM survival function---------------------------------------------------
# save KM survival function as tibble (modern data.frame)
KM.tab <- tidy(KM) 
KM.tab # same as print(KM.tab)



## ----Graphing the survival function--------------------------------------------------
plot(KM, ylab="survival probability", xlab="months")


## ----Stratified Kaplan-Meier estimates-----------------------------------------------
# stratify by x variable
KM.x <- survfit(Surv(time, status) ~ x, data=aml)

# median survival by strata
KM.x


## ----tidy KM strat-------------------------------------------------------------------
# KM estimated survival functions by strata
tidy(KM.x)


## ----Graphing stratified KM estimates of survival------------------------------------
# stratified KM curves with 95% CI, 2 colors
plot(KM.x, ylab="survival probability", xlab="months",
     conf.int=T, col=c("red", "blue")) 


## ----Customizable, informative survival plots with survminer-------------------------
ggsurvplot(KM.x, conf.int=T)


## ----adding risk table---------------------------------------------------------------
ggsurvplot(KM.x, conf.int=T,
           risk.table=T)


## ----passing ggplot arguments--------------------------------------------------------
ggsurvplot(KM.x, conf.int=T, 
           risk.table=T,
           palette="Accent", # argument to scale_color_brewer()
           size=2, # argument to geom_line()
           ggtheme = theme_minimal()) # changing ggtheme


## ----traditional ggplot syntax-------------------------------------------------------
g <- ggsurvplot(KM.x, conf.int=T,
           risk.table=T)$plot  # this is the ggplot object
g + scale_fill_grey() + scale_color_grey()


## ----Comparing survival functions with `survdiff()`----------------------------------
# log rank test, default is rho=0
survdiff(Surv(time, status) ~ x, data=aml)


## ----survdiff rho1-------------------------------------------------------------------
# rho=1 specifies Peto & Peto modification of Gehan-Wilcoxon,
#   more weight put on earlier time points
survdiff(Surv(time, status) ~ x, data=aml, rho=1)


#### EXERCISE 1 ####






## ----Fitting a Cox model-------------------------------------------------------------
# fit cox model and save results
lung.cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data=lung)
# summary of results
summary(lung.cox)


## ----Tidy coxph() results------------------------------------------------------------
# save summarized results as data.frame
#  exponentiate=T returns hazard ratios
lung.cox.tab <- tidy(lung.cox, exponentiate=T, conf.int=T) 

# display table   
lung.cox.tab


## ----plot of coefficients------------------------------------------------------------
# plot of hazard ratios and 95% CIs
ggplot(lung.cox.tab, 
       aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high)) + 
  geom_pointrange() +  # plots center point (x) and range (xmin, xmax)
  geom_vline(xintercept=1, color="red") + # vertical line at HR=1
  labs(x="hazard ratio", title="Hazard ratios and 95% CIs") +
  theme_classic()


## ----Predicting survival after coxph() with survfit()--------------------------------
# predict survival function for subject with means on all covariates
surv.at.means <- survfit(lung.cox) 

#table of survival function
tidy(surv.at.means)


## ----Plotting survival curves--------------------------------------------------------
# plot of predicted survival for subject at means of covariates
plot(surv.at.means, xlab="days", ylab="survival probability")


## ----Predicting survival at specific covariate values--------------------------------
# create new data for plotting: 1 row for each sex
#  and mean age and wt.loss for both rows
plotdata <- data.frame(age=mean(lung$age),
                       sex=1:2,
                       wt.loss=mean(lung$wt.loss, na.rm=T))

# look at new data
plotdata


## ----tidy predicted by sex-----------------------------------------------------------
# get survival function estimates for each sex
surv.by.sex <- survfit(lung.cox, newdata=plotdata) # one function for each sex

# tidy results
tidy(surv.by.sex)


## ----Plotting multiple predicted survival functions----------------------------------
# plot survival estimates
plot(surv.by.sex, xlab="days", ylab="survival probability",
     conf.int=T, col=c("blue", "red"))


## ----ggsurvplot multiple survival functions------------------------------------------
# data= is the same data used in survfit()
#  censor=F removes censoring symbols
ggsurvplot(surv.by.sex, data=plotdata, censor=F,
           legend.labs=c("male", "female")) 


## ----Assessing the proportional hazards assumption-----------------------------------
cox.zph(lung.cox)


## ----PH assessment plot--------------------------------------------------------------
plot(cox.zph(lung.cox))


## ----original model------------------------------------------------------------------
# reprinting original model results
summary(lung.cox)


## ----Stratified Cox model------------------------------------------------------------
lung.strat.sex <- coxph(Surv(time, status) ~ age + wt.loss + strata(sex), data=lung)
summary(lung.strat.sex)


## ----Modeling time-varying coefficients----------------------------------------------
# notice sex and tt(sex) in model formula
lung.sex.by.time <- coxph(Surv(time, status) ~ age + wt.loss + sex + tt(sex), # sex and tt(sex) in formula
                          data=lung,
                          tt=function(x,t,...) x*t) # linear change in effect of sex
summary(lung.sex.by.time)



## ----graph of smoothed residuals again-----------------------------------------------
plot(cox.zph(lung.cox), var="sex")


## ----Time-varying covariates---------------------------------------------------------
head(jasa1)


## ----coxph with time-varying covariates----------------------------------------------
jasa1.cox <- coxph(Surv(start, stop, event) ~ transplant + age + surgery, data=jasa1)
summary(jasa1.cox)


## ----after coxph with time-varying covariates----------------------------------------
# check PH assumptions
cox.zph(jasa1.cox)

# plot predicted survival by transplant group at mean age and surgery=0
plotdata <- data.frame(transplant=0:1, age=-2.48, surgery=0)
surv.by.transplant <- survfit(jasa1.cox, newdata=plotdata)
ggsurvplot(surv.by.transplant, data=plotdata) # remember to supply data to ggsurvplot() for predicted survival after coxph()


#### EXERCISE 2 ####





#### Solutions to Exercises ###

## ----Exercise 1, echo=F, eval=F----------------------------------------------------------
## # estimate survival for whole data set
KM.all <- survfit(Surv(time,status)~1, data=veteran)
## # median survival time
KM.all
## # plot survival function
plot(KM.all)
## # table of survival function
tidy(KM.all)
## 
## # survival stratified by treatment
KM.treat <- survfit(Surv(time,status)~trt, data=veteran)
## # stratified plot
ggsurvplot(KM.treat, conf.int=T)
## # log-rank test
survdiff(Surv(time,status)~trt, data=veteran)


## ----Exercise 2, echo=F, eval=F------------------------------------------------------
# fit model
veteran.cox <- coxph(Surv(time, status) ~ trt + age, data=veteran)
summary(veteran.cox)
# asses PH
cox.zph(veteran.cox)
plot(cox.zph(veteran.cox))
# fit model allowing effect of trt to vary with time
veteran.cox.nonph <- coxph(Surv(time, status) ~ trt + tt(trt) + age, data=veteran, tt=function(x,t,...) x*t)
summary(veteran.cox.nonph)

