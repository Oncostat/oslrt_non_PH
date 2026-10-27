###To install the package nphsim###
# install.packages("remotes")
# remotes::install_github("keaven/nphsim")

##Load the libraries##
library(nph)
library(survival)
library(nphsim)
library(flexsurv)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(lubridate)
library(IPDfromKM)
library(parfm)
library(ggsurvfit)


rm(list = ls())

################################################################################
########################Modified One-Sample Log-Rank Test#######################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logisitic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by 
#    default pi = 0 (sampling variability not taken into account)
#return a list with the statistic Z, the p-value and the expected number of events E
mOSLRT <- function(data, rate_exp, shape_weib, scale_weib, shape_llogis,
                   scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                   sigma_gengamma, Q_gengamma, distr, pi = 0){
  X <- data$t
  delta <- data$e
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(u, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    M <- H(X, rate_exp)
  }
  if(distr=='Weibull'){
    S <- function(u, shape, scale){1-pweibull(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    M <- H(X, shape_weib, scale_weib)
  }
  if(distr=='Log-logistic'){
    S <- function(u, shape, scale){1-pllogis(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    M <- H(X, shape_llogis, scale_llogis) 
  }
  if(distr=='Log-normal'){
    S <- function(u, mean, sd){1-plnorm(u, mean, sd)}
    H <- function(u, mean, sd){-log(S(u, mean, sd))}
    M <- H(X, mean_lnorm, sd_lnorm)
  }
  if(distr=='Gen Gamma'){
    S <- function(u, mu, sigma, Q){1-pgengamma(u, mu, sigma, Q)}
    H <- function(u, mu, sigma, Q){-log(S(u, mu, sigma, Q))}
    M <-H(X, mu_gengamma, sigma_gengamma, Q_gengamma)
  }
  O <- sum(delta) 
  E <- sum(M)
  V <- (O+E)/2
  Z <- (O-E)/sqrt(V)
  R <- sqrt(1/(1+pi))
  pvalue <- 1-pnorm(-Z*R)
  return(c(Z*R, pvalue, E))
}



################################################################################
############################One-Sample Log-Rank Test############################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logisitic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by
#    default pi = 0 (sampling variability not taken into account)
#return the p-value
OSLRT <- function(data, rate_exp, shape_weib, scale_weib, shape_llogis,
                  scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                  sigma_gengamma, Q_gengamma, distr, pi = 0){
  X <- data$t
  delta <- data$e
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(u, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    M <- H(X, rate_exp)
  }
  if(distr=='Weibull'){
    S <- function(u, shape, scale){1-pweibull(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    M <- H(X, shape_weib, scale_weib)
  }
  if(distr=='Log-logistic'){
    S <- function(u, shape, scale){1-pllogis(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    M <- H(X, shape_llogis, scale_llogis) 
  }
  if(distr=='Log-normal'){
    S <- function(u, mean, sd){1-plnorm(u, mean, sd)}
    H <- function(u, mean, sd){-log(S(u, mean, sd))}
    M <- H(X, mean_lnorm, sd_lnorm)
  }
  if(distr=='Gen Gamma'){
    S <- function(u, mu, sigma, Q){1-pgengamma(u, mu, sigma, Q)}
    H <- function(u, mu, sigma, Q){-log(S(u, mu, sigma, Q))}
    M <-H(X, mu_gengamma, sigma_gengamma, Q_gengamma)
  }
  O <- sum(delta)
  E <- sum(M)
  Z <- (O-E)/sqrt(E)
  R <- sqrt(1/(1+pi))
  pvalue <- 1-pnorm(-Z*R)
  return(pvalue)
}

################################################################################
#########################Score test for crossing hazards########################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logisitic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by
#    default pi = 0 (sampling variability not taken into account)
#return the p-value
Score_RC <- function(data, rate_exp, shape_weib, scale_weib, shape_llogis,
                     scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                     sigma_gengamma, Q_gengamma, distr, pi = 0){
  X <- data$t
  delta <- data$e
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(u, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    M <- H(X, rate_exp)
  }
  if(distr=='Weibull'){
    S <- function(u, shape, scale){1-pweibull(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    M <- H(X, shape_weib, scale_weib)
  }
  if(distr=='Log-logistic'){
    S <- function(u, shape, scale){1-pllogis(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    M <- H(X, shape_llogis, scale_llogis) 
  }
  if(distr=='Log-normal'){
    S <- function(u, mean, sd){1-plnorm(u, mean, sd)}
    H <- function(u, mean, sd){-log(S(u, mean, sd))}
    M <- H(X, mean_lnorm, sd_lnorm)
  }
  if(distr=='Gen Gamma'){
    S <- function(u, mu, sigma, Q){1-pgengamma(u, mu, sigma, Q)}
    H <- function(u, mu, sigma, Q){-log(S(u, mu, sigma, Q))}
    M <- H(X, mu_gengamma, sigma_gengamma, Q_gengamma)
  }
  logH0 <- log(M)
  O <- sum(delta)
  Exp <- (delta-M)*logH0
  Num <- O+sum(Exp)
  V <- delta*logH0-M*logH0*(1+logH0)
  Var <- sum(V)
  Z <- Num/sqrt(-Var)
  R <- sqrt(1/(1+pi))
  pval <- 1-pnorm(-Z*R)
  return(c(Z*R, pval, sum(M*logH0), sum(Exp)))
}

################################################################################
###########################Score test for early effect##########################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#CP: the expected change-point of the model (effect between 0 and CP) at which the test is calculated
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logistic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by
#    default pi = 0 (sampling variability not taken into account)
#return a list with the statistic Z, the p-value and the 'expected' number of events for early effect of treatment
Score_EE <- function(data, CP, rate_exp, shape_weib, scale_weib, shape_llogis,
                     scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                     sigma_gengamma, Q_gengamma, distr, pi = 0){
  data2 <- arrange(data, t)
  t_CP <- data2$t[which(data2$t<=CP)]
  O_CP <- data2$e[which(data2$t<=CP)]
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(u, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    H0 <- H(t_CP, rate_exp)
    H0_CP <- H(CP, rate_exp)
  }
  if(distr=='Weibull'){
    S <- function(u, shape, scale){1-pweibull(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    H0 <- H(t_CP, shape_weib, scale_weib)
    H0_CP <- H(CP, shape_weib, scale_weib)
  }
  if(distr=='Log-logistic'){
    S <- function(u, shape, scale){1-pllogis(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    H0 <- H(t_CP, shape_llogis, scale_llogis)
    H0_CP <- H(CP, shape_llogis, scale_llogis)
  }
  if(distr=='Log-normal'){
    S <- function(u, mean, sd){1-plnorm(u, mean, sd)}
    H <- function(u, mean, sd){-log(S(u, mean, sd))}
    H0 <- H(t_CP, mean_lnorm, sd_lnorm)
    H0_CP <- H(CP, mean_lnorm, sd_lnorm)
  }
  if(distr=='Gen Gamma'){
    S <- function(u, mu, sigma, Q){1-pgengamma(u, mu, sigma, Q)}
    H <- function(u, mu, sigma, Q){-log(S(u, mu, sigma, Q))}
    H0 <-H(t_CP, mu_gengamma, sigma_gengamma, Q_gengamma)
    H0_CP <- H(CP, mu_gengamma, sigma_gengamma, Q_gengamma)
  }
  H_tau2 <- rep(H0_CP, length(data2$t[which(data2$t>=CP)]))
  H_tau <- sum(H_tau2)
  Obs <- O_CP-H0
  Num <- sum(Obs)-H_tau
  Var <- sum(H0)+H_tau
  Z  <- Num/sqrt(Var)
  R <- sqrt(1/(1+pi))
  pval <- 1-pnorm(-Z*R)
  return(c(Z*R, pval, sum(H0)+H_tau))
}

################################################################################
###########################Score test for middle effect#########################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#CP1 and CP2: the expected change-points of the model (effect between CP1 and CP2) at which the test is calculated
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logisitic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by
#    default pi = 0 (sampling variability not taken into account)
#return the p-value
Score_ME <- function(data, CP1, CP2, rate_exp, shape_weib, scale_weib, 
                     shape_llogis,scale_llogis, mean_lnorm, sd_lnorm, 
                     mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  data2 <- arrange(data, t)
  t_CP <- data2$t[which(data2$t>CP1 & data2$t<=CP2)]
  O_CP <- data2$e[which(data2$t>CP1 & data2$t<=CP2)]
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(u, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    H0_CPs <- H(t_CP, rate_exp)
    H0_CP1 <- H(CP1, rate_exp)
    H0_CP2 <- H(CP2, rate_exp)
    H0_CPs2 <- H(data2$t[which(data2$t>=CP1 & data2$t<=CP2)], rate_exp)
  }
  if(distr=='Weibull'){
    S <- function(u, shape, scale){1-pweibull(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    H0_CPs <- H(t_CP, shape_weib, scale_weib)
    H0_CP1 <- H(CP1, shape_weib, scale_weib)
    H0_CP2 <- H(CP2, shape_weib, scale_weib)
    H0_CPs2 <- H(data2$t[which(data2$t>=CP1 & data2$t<=CP2)], shape_weib, scale_weib)
  }
  if(distr=='Log-logistic'){
    S <- function(u, shape, scale){1-pllogis(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    H0_CPs <- H(t_CP, shape_llogis, scale_llogis)
    H0_CP1 <- H(CP1, shape_llogis, scale_llogis)
    H0_CP2 <- H(CP2, shape_llogis, scale_llogis)
    H0_CPs2 <- H(data2$t[which(data2$t>=CP1 & data2$t<=CP2)], shape_llogis, scale_llogis)
  }
  if(distr=='Log-normal'){
    S <- function(u, mean, sd){1-plnorm(u, mean, sd)}
    H <- function(u, mean, sd){-log(S(u, mean, sd))}
    H0_CPs <- H(t_CP, mean_lnorm, sd_lnorm)
    H0_CP1 <- H(CP1, mean_lnorm, sd_lnorm)
    H0_CP2 <- H(CP2, mean_lnorm, sd_lnorm)
    H0_CPs2 <- H(data2$t[which(data2$t>=CP1 & data2$t<=CP2)], mean_lnorm, sd_lnorm)
  }
  if(distr=='Gen Gamma'){
    S <- function(u, mu, sigma, Q){1-pgengamma(u, mu, sigma, Q)}
    H <- function(u, mu, sigma, Q){-log(S(u, mu, sigma, Q))}
    H0_CPs <-H(t_CP, mu_gengamma, sigma_gengamma, Q_gengamma)
    H0_CP1 <- H(CP1, mu_gengamma, sigma_gengamma, Q_gengamma)
    H0_CP2 <- H(CP2, mu_gengamma, sigma_gengamma, Q_gengamma)
    H0_CPs2 <- H(data2$t[which(data2$t>=CP1 & data2$t<=CP2)], mu_gengamma, sigma_gengamma, Q_gengamma)
  }
  H_CP1 <- rep(H0_CP1, length(data2$t[which(data2$t>=CP1)]))
  H_CP1 <- sum(H_CP1)
  H_CP2 <- rep(H0_CP2, length(data2$t[which(data2$t>=CP2)]))
  H_CP2 <- sum(H_CP2)
  Obs <- O_CP-H0_CPs
  Num <- sum(Obs)+H_CP1-H_CP2
  Var <- sum(H0_CPs2)-H_CP1+H_CP2
  Z <- Num/sqrt(Var)
  R <- sqrt(1/(1+pi))
  pval <- 1-pnorm(-Z*R)
  return(c(Z*R, pval, sum(H0_CPs), Var))
}



################################################################################
##########################Score test for delayed effect#########################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#CP: the expected change-point of the model (effect between CP and infty) at which the test is calculated
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logistic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by
#    default pi = 0 (sampling variability not taken into account)
#return a list with the statistic Z, the p-value and the 'expected' number of events for delayed effect of treatment
Score_DE <- function(data, CP, rate_exp, shape_weib, scale_weib, shape_llogis,
                     scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                     sigma_gengamma, Q_gengamma, distr, pi = 0){
  data2 <- arrange(data, t)
  t_CP <- data2$t[which(data2$t>CP)]
  O_CP <- data2$e[which(data2$t>CP)]
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(u, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    H0 <- H(t_CP, rate_exp)
    H0_CP <- H(CP, rate_exp)
  }
  if(distr=='Weibull'){
    S <- function(u, shape, scale){1-pweibull(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    H0 <- H(t_CP, shape_weib, scale_weib)
    H0_CP <- H(CP, shape_weib, scale_weib)
  }
  if(distr=='Log-logistic'){
    S <- function(u, shape, scale){1-pllogis(u, shape, scale)}
    H <- function(u, shape, scale){-log(S(u, shape, scale))}
    H0 <- H(t_CP, shape_llogis, scale_llogis)
    H0_CP <- H(CP, shape_llogis, scale_llogis)
  }
  if(distr=='Log-normal'){
    S <- function(u, mean, sd){1-plnorm(u, mean, sd)}
    H <- function(u, mean, sd){-log(S(u, mean, sd))}
    H0 <- H(t_CP, mean_lnorm, sd_lnorm)
    H0_CP <- H(CP, mean_lnorm, sd_lnorm)
  }
  if(distr=='Gen Gamma'){
    S <- function(u, mu, sigma, Q){1-pgengamma(u, mu, sigma, Q)}
    H <- function(u, mu, sigma, Q){-log(S(u, mu, sigma, Q))}
    H0 <-H(t_CP, mu_gengamma, sigma_gengamma, Q_gengamma)
    H0_CP <- H(CP, mu_gengamma, sigma_gengamma, Q_gengamma)
  }
  H_tau <- rep(H0_CP, length(H0))
  Obs <- O_CP-H0+H_tau
  Num <- sum(Obs)
  V <- H0 - H_tau
  Var <- sum(V)
  Z <- Num/sqrt(Var)
  R <- sqrt(1/(1+pi))
  pval <- 1-pnorm(-Z*R)
  return(c(Z*R, pval, sum(H0-H_tau)))
}

################################################################################
############################Value of RMST in one arm############################
################################################################################
#time: time-to-event
#status: status associated with time, 1 if the event is realized, 0 if censored
#tau: time at which the RMST is calculated, defined as min(max(time_exp), max(time_control)) 
#return value of the RMST at tau and its standard error
rmst_single <- function(time, status, tau){
  ft <- survfit(Surv(time, status)~1)
  idx <- ft$time<=tau
  
  wk.time <- sort(c(ft$time[idx], tau))
  wk.surv <- ft$surv[idx]
  wk.n.risk <- ft$n.risk[idx]
  wk.n.event <- ft$n.event[idx]
  
  time.diff <- diff(c(0, wk.time))
  areas <- time.diff*c(1, wk.surv)
  rmst <- sum(areas)
  
  wk.var <- ifelse((wk.n.risk-wk.n.event)==0, 0, wk.n.event/(wk.n.risk*(wk.n.risk-wk.n.event)))
  wk.var <- c(wk.var, 0)
  rmst.var <- sum(cumsum(rev(areas[-1]))^2*rev(wk.var)[-1])
  rmst.se <- sqrt(rmst.var)
  
  R <- c(rmst, rmst.se)
  return(R)
}

################################################################################
###############################Test of RMST in SA###############################
################################################################################
#rmst_exp: value of the RMST for the experimental group
#se_rmst_exp: standard error of the RMST for the experimental group
#tau: time at which the RMST is calculated, defined as min(max(time_exp), max(time_control))
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logistic', 
#       'Log-normal', 'Gen Gamma'
#return a list with the statistic Z, the p-value and the 'expected' number of events for delayed effect of treatment
test_SA <- function(rmst_exp, se_rmst_exp, tau, rate_exp, shape_weib, 
                    scale_weib, shape_llogis, scale_llogis, mean_lnorm, 
                    sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr){
  if(distr=='Exponential'){
    rmst_control <- rmst_exp(t = tau, rate = rate_exp, start = 0)
  }
  if(distr=='Weibull'){
    rmst_control <- rmst_weibull(t = tau, shape = shape_weib, scale = scale_weib, start = 0)
  }
  if(distr=='Log-logistic'){
    rmst_control <- rmst_llogis(t = tau, shape = shape_llogis, scale = scale_llogis, start = 0)
  }
  if(distr=='Log-normal'){
    rmst_control <- rmst_lnorm(t = tau, meanlog = mean_lnorm, sdlog = sd_lnorm, start = 0)
  }
  if(distr=='Gen Gamma'){
    rmst_control <- rmst_gengamma(t = tau, mu = mu_gengamma, sigma = sigma_gengamma, Q = Q_gengamma, start = 0)
  }
  D <- rmst_exp - rmst_control
  Z <- D/se_rmst_exp
  pval <- 1-pnorm(D/se_rmst_exp)
  return(pval)
}


################################################################################
#######################Combination test with PH, EE and DE######################
################################################################################
#data: data for the experimental group, should be on the form t for the time-to-event and e for the indicator of the event 
#       (1 if the patient realised the event or 0 if he is censored)
#CP1 and CP2: change-points for the early effect tests with CP1!=CP2
#CP3 and CP4: change-points for the delayed effect tests with CP3!=CP4
#rate_exp: rate parameter for the exponential distribution of the external control group
#shape_weib: shape parameter of the Weibull distribution for the external control group (if exponential distribution : 1)
#scale_weib: scale parameter of the Weibull distribution for the external control group
#shape_llogis: shape parameter of the log-logistic distribution for the external control group
#scale_llogis: scale parameter of the log-logistic distribution for the external control group
#mean_lnorm: mean of the log-normal distribution for the external control group
#sd_lnorm: standard deviation of the log-normal distribution for the external control group
#mu_gengamma: mu parameter ('location' parameter) of the generalized gamma distribution for the external control group
#sigma_gengamma: sigma parameter ('scale' parameter) of the generalized gamma distribution for the external control group
#Q_gengamma: Q parameter ('shape' parameter) of the generalized gamma distribution for the external control group
#distr: distribution that fits the data of the external control group, should be 'Exponential', 'Weibull', 'Log-logisitic', 
#       'Log-normal', 'Gen Gamma'
#pi: ratio between the number of patients in the experimental group and in the external control group (nexp/ncontrol), by
#    default pi = 0 (sampling variability not taken into account)
#return the p-value
maxcombo1 <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                      scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                      sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[4, 2] <- V[2, 4] <- 0
  V[5, 2] <- V[2, 5] <- 0
  V[4, 3] <- V[3, 4] <- 0
  V[5, 3] <- V[3, 5] <- 0
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  for(k in 1:5){
    for(l in 1:5){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

##Reduce combination tests##
maxcombo_remDE1 <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                            scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                            sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE2[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE2[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[4, 2] <- V[2, 4] <- 0
  V[4, 3] <- V[3, 4] <- 0
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  for(k in 1:4){
    for(l in 1:4){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

maxcombo_remDE2 <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                            scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                            sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[4, 2] <- V[2, 4] <- 0
  V[4, 3] <- V[3, 4] <- 0
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  for(k in 1:4){
    for(l in 1:4){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

maxcombo_remDE <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                           scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                           sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  for(k in 1:3){
    for(l in 1:3){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

maxcombo_remEE1 <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                           scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                           sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE2[1], Z_DE1[1], Z_DE2[1])
  pval <- c(Z_moslrt[2], Z_EE2[2], Z_DE1[2], Z_DE2[2])
  n_indiv <- c(Z_moslrt[3], Z_EE2[3], Z_DE1[3], Z_DE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[4, 2] <- V[2, 4] <- 0
  V[4, 3] <- V[3, 4] <- sqrt(n_indiv[4]/n_indiv[3])
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- 0
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

maxcombo_remEE2 <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                            scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                            sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_DE1[1], Z_DE2[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_DE1[2], Z_DE2[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_DE1[3], Z_DE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[4, 2] <- V[2, 4] <- 0
  V[4, 3] <- V[3, 4] <- sqrt(n_indiv[4]/n_indiv[3])
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- 0
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

maxcombo_remEE <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                           scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                           sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_DE1[1], Z_DE2[1])
  pval <- c(Z_moslrt[2], Z_DE1[2], Z_DE2[2])
  n_indiv <- c(Z_moslrt[3], Z_DE1[3], Z_DE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[3]/n_indiv[2])
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

##Augmented combination tests##
maxcombo_addME <- function(data, CP1, CP2, CP3, CP4, CP_ME1, CP_ME2,
                           rate_exp, shape_weib, scale_weib, shape_llogis,  
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma, 
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_ME <- Score_ME(data = data_exp, CP1 = CP_ME1, CP2 = CP_ME2, 
                   rate_exp = rate_exp, 
                   shape_weib = shape_weib, scale_weib = scale_weib, 
                   shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                   mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                   mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                   Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1], Z_ME[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2], Z_ME[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3], Z_ME[4])
  n_indiv_bis <- Z_ME[3]
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[1, 6] <- V[6, 1] <- ifelse((n_indiv[1]>n_indiv[6]), sqrt(n_indiv[6]/n_indiv[1]), 0)
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[2, 4] <- V[4, 2] <- 0
  V[2, 5] <- V[5, 2] <- 0
  V[2, 6] <- V[6, 2] <- 0
  V[3, 4] <- V[4, 3] <- 0
  V[3, 5] <- V[5, 3] <- 0
  V[3, 6] <- V[6, 3] <- ifelse((n_indiv[3]>n_indiv_bis), sqrt(n_indiv_bis/n_indiv[3]), 0)
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  V[4, 6] <- V[6, 4] <- ifelse((n_indiv[6]>n_indiv[4]), (sqrt(n_indiv[4]/n_indiv[6])-0.28), 0)
  V[5, 6] <- V[6, 5] <- 0
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  V2 <- as.matrix(nearPD(V, corr = TRUE)$mat)
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V2)
  return(c(pval_max_hoch, pmult))
}

maxcombo_addCH <- function(data, CP1, CP2, CP3, CP4, CP_ME1, CP_ME2,
                           rate_exp, shape_weib, scale_weib, shape_llogis,  
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma, 
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_CH <- Score_RC(data = data_exp, rate_exp = rate_exp, 
                   shape_weib = shape_weib, scale_weib = scale_weib, 
                   shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                   mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                   mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                   Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1], Z_CH[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2], Z_CH[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3], abs(Z_CH[4]))
  n_indiv_bis <- Z_CH[3]
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[1, 6] <- V[6, 1] <- ifelse((n_indiv[1]>n_indiv_bis), sqrt(n_indiv_bis/n_indiv[1]), 0) ##pour ne pas avoir une variance superieure a 1
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[2, 4] <- V[4, 2] <- 0
  V[2, 5] <- V[5, 2] <- 0
  V[2, 6] <- V[6, 2] <- ifelse((n_indiv[6]>n_indiv[2]), -sqrt(n_indiv[2]/n_indiv[6]), 0)
  V[3, 4] <- V[4, 3] <- 0
  V[3, 5] <- V[5, 3] <- 0
  V[3, 6] <- V[6, 3] <- 0
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  V[4, 6] <- V[6, 4] <- ifelse((n_indiv[6]>n_indiv[4]), sqrt(n_indiv[4]/n_indiv[6]), 0)
  V[5, 6] <- V[6, 5] <- ifelse((n_indiv[6]>n_indiv[5]), sqrt(n_indiv[5]/n_indiv[6]), 0)
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  V2 <- as.matrix(nearPD(V, corr = TRUE)$mat)
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V2)
  return(c(pval_max_hoch, pmult))
}

maxcombo_addMECH <- function(data, CP1, CP2, CP3, CP4, CP_ME1, CP_ME2,
                             rate_exp, shape_weib, scale_weib, shape_llogis,  
                             scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma, 
                             sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_ME <- Score_ME(data = data_exp, CP1 = CP_ME1, CP2 = CP_ME2, 
                   rate_exp = rate_exp, 
                   shape_weib = shape_weib, scale_weib = scale_weib, 
                   shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                   mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                   mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                   Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  Z_CH <- Score_RC(data = data_exp, rate_exp = rate_exp, 
                   shape_weib = shape_weib, scale_weib = scale_weib, 
                   shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                   mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                   mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                   Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1], Z_ME[1], Z_CH[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2], Z_ME[2], Z_CH[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3], Z_ME[4], abs(Z_CH[4]))
  n_indiv_bis <- c(Z_ME[3], Z_CH[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[1, 6] <- V[6, 1] <- ifelse((n_indiv[1]>n_indiv[6]), sqrt(n_indiv[6]/n_indiv[1]), 0)
  V[1, 7] <- V[7, 1] <- ifelse((n_indiv[1]>n_indiv_bis[2]), sqrt(n_indiv_bis[2]/n_indiv[1]), 0) ##pour ne pas avoir une variance superieure a 1
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[2, 4] <- V[4, 2] <- 0
  V[2, 5] <- V[5, 2] <- 0
  V[2, 6] <- V[6, 2] <- 0
  V[2, 7] <- V[7, 2] <- ifelse((n_indiv[7]>n_indiv[2]), -sqrt(n_indiv[2]/n_indiv[7]), 0)
  V[3, 4] <- V[4, 3] <- 0
  V[3, 5] <- V[5, 3] <- 0
  V[3, 6] <- V[6, 3] <- ifelse((n_indiv[3]>n_indiv_bis[1]), sqrt(n_indiv_bis[1]/n_indiv[3]), 0)
  V[3, 7] <- V[7, 3] <- 0
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  V[4, 6] <- V[6, 4] <- ifelse((n_indiv[6]>n_indiv[4]), (sqrt(n_indiv[4]/n_indiv[6])-0.28), 0)
  V[4, 7] <- V[7, 4] <- ifelse((n_indiv[7]>n_indiv[4]), sqrt(n_indiv[4]/n_indiv[7]), 0)
  V[5, 6] <- V[6, 5] <- 0
  V[5, 7] <- V[7, 5] <- ifelse((n_indiv[7]>n_indiv[5]), sqrt(n_indiv[5]/n_indiv[7]), 0)
  V[6, 7] <- V[7, 6] <- ifelse((n_indiv[7]>n_indiv_bis[1]), sqrt(n_indiv_bis[1]/n_indiv[7]), 0)
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  V2 <- as.matrix(nearPD(V, corr = TRUE)$mat)
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V2)
  return(c(pval_max_hoch, pmult))
}

maxcombo_addEE <- function(data, CP1, CP2, CP3, CP4, CP5,
                           rate_exp, shape_weib, scale_weib, shape_llogis,  
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma, 
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE3 <- Score_EE(data = data, CP = CP5,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1], Z_EE3[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2], Z_EE3[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3], Z_EE3[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[1, 6] <- V[6, 1] <- sqrt(n_indiv[6]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[2, 4] <- V[4, 2] <- 0
  V[2, 5] <- V[5, 2] <- 0
  V[2, 6] <- V[6, 2] <- sqrt(n_indiv[2]/n_indiv[6])
  V[3, 4] <- V[4, 3] <- 0
  V[3, 5] <- V[5, 3] <- 0
  V[3, 6] <- V[6, 3] <- sqrt(n_indiv[6]/n_indiv[3])
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  V[4, 6] <- V[6, 4] <- 0
  V[5, 6] <- V[6, 5] <- 0
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V)
  return(c(pval_max_hoch, pmult))
}

maxcombo_addDE <- function(data, CP1, CP2, CP3, CP4, CP6,
                           rate_exp, shape_weib, scale_weib, shape_llogis,  
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma, 
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE3 <- Score_EE(data = data, CP = CP5,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE3 <- Score_DE(data = data, CP = CP6, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1], Z_DE3[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2], Z_DE3[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3], Z_DE3[3])
  n_indiv_bis <- c(Z_EE2[4], Z_DE3[4])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[1, 6] <- V[6, 1] <- sqrt(n_indiv[6]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[2, 4] <- V[4, 2] <- 0
  V[2, 5] <- V[5, 2] <- 0
  V[2, 6] <- V[6, 2] <- 0
  V[3, 4] <- V[4, 3] <- 0
  V[3, 5] <- V[5, 3] <- 0
  V[3, 6] <- V[6, 3] <- sqrt(n_indiv_bis[1]/n_indiv_bis[2])
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  V[4, 6] <- V[6, 4] <- sqrt(n_indiv[4]/n_indiv[6])
  V[5, 6] <- V[6, 5] <- sqrt(n_indiv[5]/n_indiv[6])
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  V2 <- as.matrix(nearPD(V, corr = TRUE)$mat)
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V2)
  return(c(pval_max_hoch, pmult))
}

maxcombo_addEEDE <- function(data, CP1, CP2, CP3, CP4, CP5, CP6,
                             rate_exp, shape_weib, scale_weib, shape_llogis,  
                             scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma, 
                             sigma_gengamma, Q_gengamma, distr, pi = 0){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr, pi = pi)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Z_DE3 <- Score_DE(data = data, CP = CP6, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr, pi = pi)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1], Z_EE3[1], Z_DE3[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2], Z_EE3[2], Z_DE3[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3], Z_EE3[3], Z_DE3[3])
  n_indiv_bis <- c(Z_EE2[4], Z_DE3[4], Z_EE3[4])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- min(na.omit(Score))
  ind <- which(Score==Zmax)
  pval_max_hoch <- pval_hoch[ind]
  z_up <- Score[ind]
  low <- rep(z_up, m)
  up <- rep(Inf, m)
  V <- diag(1, m)
  V[1, 2] <- V[2, 1] <- sqrt(n_indiv[2]/n_indiv[1])
  V[1, 3] <- V[3, 1] <- sqrt(n_indiv[3]/n_indiv[1])
  V[1, 4] <- V[4, 1] <- sqrt(n_indiv[4]/n_indiv[1])
  V[1, 5] <- V[5, 1] <- sqrt(n_indiv[5]/n_indiv[1])
  V[1, 6] <- V[6, 1] <- sqrt(n_indiv[6]/n_indiv[1])
  V[1, 7] <- V[7, 1] <- sqrt(n_indiv[7]/n_indiv[1])
  V[2, 3] <- V[3, 2] <- sqrt(n_indiv[2]/n_indiv[3])
  V[2, 4] <- V[4, 2] <- 0
  V[2, 5] <- V[5, 2] <- 0
  V[2, 6] <- V[6, 2] <- sqrt(n_indiv[2]/n_indiv[6])
  V[2, 7] <- V[7, 2] <- 0
  V[3, 4] <- V[4, 3] <- 0
  V[3, 5] <- V[5, 3] <- 0
  V[3, 6] <- V[6, 3] <- sqrt(n_indiv[6]/n_indiv[3])
  V[3, 7] <- V[7, 3] <- sqrt(n_indiv_bis[1]/n_indiv_bis[2])
  V[4, 5] <- V[5, 4] <- sqrt(n_indiv[5]/n_indiv[4])
  V[4, 6] <- V[6, 4] <- 0
  V[4, 7] <- V[7, 4] <- sqrt(n_indiv[4]/n_indiv[7])
  V[5, 6] <- V[6, 5] <- 0
  V[5, 7] <- V[7, 5] <- sqrt(n_indiv[5]/n_indiv[7])
  V[6, 7] <- V[7, 6] <- sqrt(n_indiv_bis[3]/n_indiv_bis[2])
  for(k in 1:m){
    for(l in 1:m){
      if(is.na(V[k, l])==TRUE){
        V[k, l] <- V[l, k] <- 0
      }
    }
  }
  V2 <- as.matrix(nearPD(V, corr = TRUE)$mat)
  pmult <- 1 - mvtnorm::pmvnorm(lower = low, upper = up, corr = V2)
  return(c(pval_max_hoch, pmult))
}

#####Functions to run the simulations#####

generate_initial <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                          lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                          HR1, shape1, scale1, shape0, scale0,
                          CP_EE, CP_ME, CP_DE, CP_CH){
  u <- runif(n = n_pat, 0, ta)
  if(scenario=='NE'){
    time <- rweibull(n = n_pat, shape = shape0, scale = scale0)
    if(lambda_NE==0){
      del <- rep(1, n_pat)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    if(lambda_NE!=0){
      cens <- rexp(n = n_pat, lambda_NE)
      time <- ifelse(time < cens, time, cens)
      del <- ifelse(time < cens, 1, 0)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    CP_tEE <- 4
    CP_tDE <- 2
    CP_tME1 <- 1
    CP_tME2 <- 6
  }
  if(scenario=='PH'){
    time <- rweibull(n = n_pat, shape = shape1, scale = scale1)
    if(lambda_PH==0){
      del <- rep(1, n_pat)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    if(lambda_PH!=0){
      cens <- rexp(n = n_pat, lambda_PH)
      time <- ifelse(time < cens, time, cens)
      del <- ifelse(time < cens, 1, 0)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    CP_tEE <- 4
    CP_tDE <- 2
    CP_tME1 <- 1
    CP_tME2 <- 6
  }
  if(scenario=='EE'){
    time <- rpwexp(n = n_pat, rate = c(1/scale0, 1/scale0*HR), intervals = CP_EE, cumulative = FALSE)
    if(lambda_EE==0){
      del <- rep(1, n_pat)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    if(lambda_EE!=0){
      cens <- rexp(n = n_pat, lambda_EE)
      time <- ifelse(time < cens, time, cens)
      del <- ifelse(time < cens, 1, 0)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    CP_tEE <- CP_EE
    CP_tDE <- CP_EE
    CP_tME1 <- CP_EE
    CP_tME2 <- 7
  }
  if(scenario=='ME'){
    time <- rpwexp(n = n_pat, rate = c(1/scale0, 1/scale0*HR, 1/scale0), intervals = CP_ME, cumulative = FALSE)
    if(lambda_ME==0){
      del <- rep(1, n_pat)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    if(lambda_ME!=0){
      cens <- rexp(n = n_pat, lambda_ME)
      time <- ifelse(time < cens, time, cens)
      del <- ifelse(time < cens, 1, 0)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    CP_tEE <- CP_ME[2]+1
    CP_tDE <- CP_ME[1]
    CP_tME1 <- CP_ME[1]
    CP_tME2 <- CP_ME[2]+1
  }
  if(scenario=='DE'){
    time <- rpwexp(n = n_pat, rate = c(1/scale0*HR, 1/scale0), intervals = CP_DE, cumulative = FALSE)
    if(lambda_DE==0){
      del <- rep(1, n_pat)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    if(lambda_DE!=0){
      cens <- rexp(n = n_pat, lambda_DE)
      time <- ifelse(time < cens, time, cens)
      del <- ifelse(time < cens, 1, 0)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    CP_tEE <- CP_DE
    CP_tDE <- CP_DE
    CP_tME1 <- 0
    CP_tME2 <- CP_DE
  }
  if(scenario=='CH'){
    time <- rpwexp(n = n_pat, rate = c(1/scale0*HR1, 1/scale0*HR), intervals = CP_CH, cumulative = FALSE)
    if(lambda_CH==0){
      del <- rep(1, n_pat)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    if(lambda_CH!=0){
      cens <- rexp(n = n_pat, lambda_CH)
      time <- ifelse(time < cens, time, cens)
      del <- ifelse(time < cens, 1, 0)
      t <- pmin(time, ta+tf-u)
      delta <- ifelse(time < ta+tf-u, del, 0)
    }
    CP_tEE <- CP_CH
    CP_tDE <- CP_CH
    CP_tME1 <- CP_CH
    CP_tME2 <- 4
  }
  expe <- data.frame(t = t, e = delta)
  return(list(data = expe, CP_EE = CP_tEE, CP_ME1 = CP_tME1,
              CP_ME2 = CP_tME2, CP_DE = CP_tDE))
}

evaluate_initial <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                           lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                           HR1, shape1, scale1, shape0, scale0, CP_EE_gen, 
                           CP_ME_gen, CP_DE_gen, CP_CH_gen, CP1, CP2, CP3, CP4,
                           rate_exp, shape_weib, scale_weib, shape_llogis, 
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  experimental <- generate_initial(scenario = scenario, n_pat = n_pat, ta = ta, tf = tf,
                        lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                        lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                        lambda_DE = lambda_DE, lambda_CH = lambda_CH, HR = HR, 
                        HR1 = HR1, shape1 = shape1, scale1 = scale1, 
                        shape0 = shape0, scale0 = scale0, 
                        CP_EE = CP_EE_gen, CP_ME = CP_ME_gen,
                        CP_DE = CP_DE_gen, CP_CH = CP_CH_gen)
  data_exp <- experimental$data
  
  obs <- sum(data_exp$e)
  
  tau <- max(data_exp$t)
  rmst_expe <- rmst_single(time = data_exp$t, status = data_exp$e, tau = tau)
  
  oslrt <- OSLRT(data = data_exp, rate_exp = rate_exp, 
                 shape_weib = shape_weib, scale_weib = scale_weib, 
                 shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                 mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                 mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                 Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  moslrt <- mOSLRT(data = data_exp, rate_exp = rate_exp, 
                   shape_weib = shape_weib, scale_weib = scale_weib, 
                   shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                   mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                   mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                   Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  ee <- Score_EE(data = data_exp, CP = experimental$CP_EE, rate_exp = rate_exp, 
                 shape_weib = shape_weib, scale_weib = scale_weib, 
                 shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                 mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                 mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                 Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  me <- Score_ME(data = data_exp, CP1 = experimental$CP_ME1, CP2 = experimental$CP_ME2, 
                 rate_exp = rate_exp, 
                 shape_weib = shape_weib, scale_weib = scale_weib, 
                 shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                 mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                 mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                 Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  de <- Score_DE(data = data_exp, CP = experimental$CP_DE, rate_exp = rate_exp, 
                 shape_weib = shape_weib, scale_weib = scale_weib, 
                 shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                 mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                 mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                 Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  ch <- Score_RC(data = data_exp, rate_exp = rate_exp, 
                 shape_weib = shape_weib, scale_weib = scale_weib, 
                 shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                 mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                 mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                 Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  rmst <- test_SA(rmst_exp = rmst_expe[1], se_rmst_exp = rmst_expe[2], tau = tau,
                  rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                  shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                  mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                  mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                  Q_gengamma = Q_gengamma, distr = distr)
  combo <- maxcombo1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                     rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                     shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                     mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                     mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                     Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  
  err_oslrt <- ifelse(oslrt<0.05, 1, 0)
  err_moslrt <- ifelse(moslrt < 0.05, 1, 0)
  err_ee <- ifelse(ee < 0.05, 1, 0)
  err_me <- ifelse(me < 0.05, 1, 0)
  err_de <- ifelse(de < 0.05, 1, 0)
  err_ch <- ifelse(ch < 0.05, 1, 0)
  err_rmst <- ifelse(rmst < 0.05, 1, 0)
  err_hoch <- ifelse(combo[1] < 0.05, 1, 0)
  err_exact <- ifelse(combo[2] < 0.05, 1, 0)
  
  return(list(obs = obs,
              err_oslrt = err_oslrt,
              err_moslrt = err_moslrt,
              err_ee = err_ee,
              err_me = err_me, 
              err_de = err_de,
              err_ch = err_ch,
              err_rmst = err_rmst,
              err_hoch = err_hoch,
              err_exact = err_exact))
}

evaluate_misspe_cp <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                               lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                               HR1, shape1, scale1, shape0, scale0, CP_EE_gen, 
                               CP_ME_gen, CP_DE_gen, CP_CH_gen, CP1, CP2, CP3, CP4,
                               rate_exp, shape_weib, scale_weib, shape_llogis, 
                               scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                               sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  experimental <- generate_initial(scenario = scenario, n_pat = n_pat, ta = ta, tf = tf,
                                   lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                   lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                   lambda_DE = lambda_DE, lambda_CH = lambda_CH, HR = HR, 
                                   HR1 = HR1, shape1 = shape1, scale1 = scale1, 
                                   shape0 = shape0, scale0 = scale0, 
                                   CP_EE = CP_EE_gen, CP_ME = CP_ME_gen,
                                   CP_DE = CP_DE_gen, CP_CH = CP_CH_gen)
  data_exp <- experimental$data
  
  oslrt <- OSLRT(data = data_exp, rate_exp = rate_exp, 
                 shape_weib = shape_weib, scale_weib = scale_weib, 
                 shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                 mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                 mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                 Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  moslrt <- mOSLRT(data = data_exp, rate_exp = rate_exp, 
                   shape_weib = shape_weib, scale_weib = scale_weib, 
                   shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                   mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                   mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                   Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  if(scenario == "EE"){
    score <- Score_EE(data = data_exp, CP = experimental$CP_EE, rate_exp = rate_exp, 
                      shape_weib = shape_weib, scale_weib = scale_weib, 
                      shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                      mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                      mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                      Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_rem_3 <- Score_EE(data = data_exp, CP = experimental$CP_EE-0.25, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_rem_6 <- Score_EE(data = data_exp, CP = experimental$CP_EE-0.5, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_add_3 <- Score_EE(data = data_exp, CP = experimental$CP_EE+0.25, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_add_6 <- Score_EE(data = data_exp, CP = experimental$CP_EE+0.5, rate_exp = rate_exp, 
                           shape_weib = shape_weib, scale_weib = scale_weib, 
                           shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                           mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                           mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                           Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  }
  else if(scenario == "DE"){
    score <- Score_DE(data = data_exp, CP = experimental$CP_DE, rate_exp = rate_exp, 
                      shape_weib = shape_weib, scale_weib = scale_weib, 
                      shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                      mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                      mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                      Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_rem_3 <- Score_DE(data = data_exp, CP = experimental$CP_DE-0.25, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_rem_6 <- Score_DE(data = data_exp, CP = experimental$CP_DE-0.5, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_add_3 <- Score_DE(data = data_exp, CP = experimental$CP_DE+0.25, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
    score_add_6 <- Score_DE(data = data_exp, CP = experimental$CP_DE+0.5, rate_exp = rate_exp, 
                            shape_weib = shape_weib, scale_weib = scale_weib, 
                            shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                            mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                            mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                            Q_gengamma = Q_gengamma, distr = distr, pi = pi)[2]
  }

  err_oslrt <- ifelse(oslrt<0.05, 1, 0)
  err_moslrt <- ifelse(moslrt < 0.05, 1, 0)
  err_score <- ifelse(score < 0.05, 1, 0)
  err_rem_3 <- ifelse(score_rem_3 < 0.05, 1, 0)
  err_rem_6 <- ifelse(score_rem_6 < 0.05, 1, 0)
  err_add_3 <- ifelse(score_add_3 < 0.05, 1, 0)
  err_add_6 <- ifelse(score_add_6 < 0.05, 1, 0)
  
  return(list(err_oslrt = err_oslrt,
              err_moslrt = err_moslrt,
              err_score = err_score,
              err_remove_3 = err_rem_3,
              err_remove_6 = err_rem_6,
              err_add_3 = err_add_3,
              err_add_6 = err_add_6))
}

evaluate_remDE <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                             lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                             HR1, shape1, scale1, shape0, scale0, CP_EE_gen, 
                             CP_ME_gen, CP_DE_gen, CP_CH_gen, CP1, CP2, CP3, CP4,
                             rate_exp, shape_weib, scale_weib, shape_llogis, 
                             scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                             sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  experimental <- generate_initial(scenario = scenario, n_pat = n_pat, ta = ta, tf = tf,
                                   lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                   lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                   lambda_DE = lambda_DE, lambda_CH = lambda_CH, HR = HR, 
                                   HR1 = HR1, shape1 = shape1, scale1 = scale1, 
                                   shape0 = shape0, scale0 = scale0, 
                                   CP_EE = CP_EE_gen, CP_ME = CP_ME_gen,
                                   CP_DE = CP_DE_gen, CP_CH = CP_CH_gen)
  data_exp <- experimental$data

  combo <- maxcombo1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                     rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                     shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                     mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                     mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                     Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_remDE1 <- maxcombo_remDE1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                  rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                  shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                  mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                  mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                  Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_remDE2 <- maxcombo_remDE2(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                  rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                  shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                  mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                  mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                  Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_remDE <- maxcombo_remDE(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  
  err_init_hoch <- ifelse(combo[1] < 0.05, 1, 0)
  err_init_exact <- ifelse(combo[2] < 0.05, 1, 0)
  err_rem_one_hoch <- ifelse(combo_remDE1[1] < 0.05, 1, 0)
  err_rem_one_exact <- ifelse(combo_remDE1[2] < 0.05, 1, 0)
  err_rem_two_hoch <- ifelse(combo_remDE2[1] < 0.05, 1, 0)
  err_rem_two_exact <- ifelse(combo_remDE2[2] < 0.05, 1, 0)
  err_rem_both_hoch <- ifelse(combo_remDE[1] < 0.05, 1, 0)
  err_rem_both_exact <- ifelse(combo_remDE[2] < 0.05, 1, 0)
  
  return(list(err_hoch = err_hoch,
              err_exact = err_exact,
              err_rem_one_hoch = err_rem_one_hoch,
              err_rem_one_exact = err_rem_one_exact,
              err_rem_two_hoch = err_rem_two_hoch,
              err_rem_two_exact = err_rem_two_exact,
              err_rem_both_hoch = err_rem_both_hoch,
              err_rem_both_exact = err_rem_both_exact))
}

evaluate_remEE <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                           lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                           HR1, shape1, scale1, shape0, scale0, CP_EE_gen, 
                           CP_ME_gen, CP_DE_gen, CP_CH_gen, CP1, CP2, CP3, CP4,
                           rate_exp, shape_weib, scale_weib, shape_llogis, 
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  experimental <- generate_initial(scenario = scenario, n_pat = n_pat, ta = ta, tf = tf,
                                   lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                   lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                   lambda_DE = lambda_DE, lambda_CH = lambda_CH, HR = HR, 
                                   HR1 = HR1, shape1 = shape1, scale1 = scale1, 
                                   shape0 = shape0, scale0 = scale0, 
                                   CP_EE = CP_EE_gen, CP_ME = CP_ME_gen,
                                   CP_DE = CP_DE_gen, CP_CH = CP_CH_gen)
  data_exp <- experimental$data
  
  combo <- maxcombo1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                     rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                     shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                     mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                     mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                     Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_remEE1 <- maxcombo_remEE1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                  rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                  shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                  mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                  mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                  Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_remEE2 <- maxcombo_remEE2(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                  rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                  shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                  mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                  mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                  Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_remEE <- maxcombo_remEE(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  
  err_init_hoch <- ifelse(combo[1] < 0.05, 1, 0)
  err_init_exact <- ifelse(combo[2] < 0.05, 1, 0)
  err_rem_one_hoch <- ifelse(combo_remEE1[1] < 0.05, 1, 0)
  err_rem_one_exact <- ifelse(combo_remEE1[2] < 0.05, 1, 0)
  err_rem_two_hoch <- ifelse(combo_remEE2[1] < 0.05, 1, 0)
  err_rem_two_exact <- ifelse(combo_remEE2[2] < 0.05, 1, 0)
  err_rem_both_hoch <- ifelse(combo_remEE[1] < 0.05, 1, 0)
  err_rem_both_exact <- ifelse(combo_remEE[2] < 0.05, 1, 0)
  
  return(list(err_hoch = err_hoch,
              err_exact = err_exact,
              err_rem_one_hoch = err_rem_one_hoch,
              err_rem_one_exact = err_rem_one_exact,
              err_rem_two_hoch = err_rem_two_hoch,
              err_rem_two_exact = err_rem_two_exact,
              err_rem_both_hoch = err_rem_both_hoch,
              err_rem_both_exact = err_rem_both_exact))
}

evaluate_addMECH <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                             lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                             HR1, shape1, scale1, shape0, scale0, CP_EE_gen, 
                             CP_ME_gen, CP_DE_gen, CP_CH_gen, CP1, CP2, CP3, CP4,
                             CP_ME1, CP_ME2,
                             rate_exp, shape_weib, scale_weib, shape_llogis, 
                             scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                             sigma_gengamma, Q_gengamma, distr, pi = 0){
  experimental <- generate_initial(scenario = scenario, n_pat = n_pat, ta = ta, tf = tf,
                                   lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                   lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                   lambda_DE = lambda_DE, lambda_CH = lambda_CH, HR = HR, 
                                   HR1 = HR1, shape1 = shape1, scale1 = scale1, 
                                   shape0 = shape0, scale0 = scale0, 
                                   CP_EE = CP_EE_gen, CP_ME = CP_ME_gen,
                                   CP_DE = CP_DE_gen, CP_CH = CP_CH_gen)
  data_exp <- experimental$data
  
  combo <- maxcombo1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                     rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                     shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                     mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                     mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                     Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_addME <- maxcombo_addME(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                CP_ME1 = CP_ME1, CP_ME2 = CP_ME2,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_addCH <- maxcombo_addCH(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                CP_ME1 = CP_ME1, CP_ME2 = CP_ME2,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_add <- maxcombo_addMECH(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                              CP_ME1 = CP_ME1, CP_ME2 = CP_ME2,
                              rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                              shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                              mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                              mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                              Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  
  err_init_hoch <- ifelse(combo[1] < 0.05, 1, 0)
  err_init_exact <- ifelse(combo[2] < 0.05, 1, 0)
  err_add_one_hoch <- ifelse(combo_addME[1] < 0.05, 1, 0)
  err_add_one_exact <- ifelse(combo_addME[2] < 0.05, 1, 0)
  err_add_two_hoch <- ifelse(combo_addCH[1] < 0.05, 1, 0)
  err_add_two_exact <- ifelse(combo_addCH[2] < 0.05, 1, 0)
  err_add_both_hoch <- ifelse(combo_add[1] < 0.05, 1, 0)
  err_add_both_exact <- ifelse(combo_add[2] < 0.05, 1, 0)
  
  return(list(err_hoch = err_hoch,
              err_exact = err_exact,
              err_add_one_hoch = err_add_one_hoch,
              err_add_one_exact = err_add_one_exact,
              err_add_two_hoch = err_add_two_hoch,
              err_add_two_exact = err_add_two_exact,
              err_add_both_hoch = err_add_both_hoch,
              err_add_both_exact = err_add_both_exact))
}

evaluate_addEEDE <- function(scenario, n_pat, ta, tf, lambda_NE, lambda_PH,
                             lambda_EE, lambda_ME, lambda_DE, lambda_CH, HR,
                             HR1, shape1, scale1, shape0, scale0, CP_EE_gen, 
                             CP_ME_gen, CP_DE_gen, CP_CH_gen, CP1, CP2, CP3, CP4,
                             CP5, CP6,
                             rate_exp, shape_weib, scale_weib, shape_llogis, 
                             scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                             sigma_gengamma, Q_gengamma, distr, pi = 0){
  experimental <- generate_initial(scenario = scenario, n_pat = n_pat, ta = ta, tf = tf,
                                   lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                   lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                   lambda_DE = lambda_DE, lambda_CH = lambda_CH, HR = HR, 
                                   HR1 = HR1, shape1 = shape1, scale1 = scale1, 
                                   shape0 = shape0, scale0 = scale0, 
                                   CP_EE = CP_EE_gen, CP_ME = CP_ME_gen,
                                   CP_DE = CP_DE_gen, CP_CH = CP_CH_gen)
  data_exp <- experimental$data
  
  combo <- maxcombo1(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                     rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                     shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                     mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                     mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                     Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_addEE <- maxcombo_addEE(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                CP5 = CP5,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_addDE <- maxcombo_addDE(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                CP6 = CP6,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  combo_add <- maxcombo_addEEDE(data = data_exp, CP1 = CP1, CP2 = CP2, CP3 = CP3, CP4 = CP4,
                                CP5 = CP5, CP6 = CP6,
                                rate_exp = rate_exp, shape_weib = shape_weib, scale_weib = scale_weib, 
                                shape_llogis = shape_llogis, scale_llogis = scale_llogis, 
                                mean_lnorm = mean_lnorm, sd_lnorm = sd_lnorm, 
                                mu_gengamma = mu_gengamma, sigma_gengamma = sigma_gengamma, 
                                Q_gengamma = Q_gengamma, distr = distr, pi = pi)
  
  err_init_hoch <- ifelse(combo[1] < 0.05, 1, 0)
  err_init_exact <- ifelse(combo[2] < 0.05, 1, 0)
  err_add_one_hoch <- ifelse(combo_addEE[1] < 0.05, 1, 0)
  err_add_one_exact <- ifelse(combo_addEE[2] < 0.05, 1, 0)
  err_add_two_hoch <- ifelse(combo_addDE[1] < 0.05, 1, 0)
  err_add_two_exact <- ifelse(combo_addDE[2] < 0.05, 1, 0)
  err_add_both_hoch <- ifelse(combo_add[1] < 0.05, 1, 0)
  err_add_both_exact <- ifelse(combo_add[2] < 0.05, 1, 0)
  
  return(list(err_hoch = err_hoch,
              err_exact = err_exact,
              err_add_one_hoch = err_add_one_hoch,
              err_add_one_exact = err_add_one_exact,
              err_add_two_hoch = err_add_two_hoch,
              err_add_two_exact = err_add_two_exact,
              err_add_both_hoch = err_add_both_hoch,
              err_add_both_exact = err_add_both_exact))
}


run_simulations <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                           lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                           lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                           CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                           CP1, CP2, CP3, CP4,
                           rate_exp, shape_weib, scale_weib, shape_llogis, 
                           scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                           sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
      
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
      
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_initial(scenario = sc, n_pat = n, ta = ta, tf = tf,
                              lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                              lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                              lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                              HR = HR, HR1 = HR1, shape1 = shape1, 
                              scale1 = scale1, shape0 = shape0, scale0 = scale0,
                              CP_EE_gen = CP_EE_gen, 
                              CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                              CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                              CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                              rate_exp = rate_exp, shape_weib = shape_weib, 
                              scale_weib = scale_weib, shape_llogis = shape_llogis, 
                              scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                              sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                              sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                              distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}

run_remDE <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                            lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                            lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                            CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                            CP1, CP2, CP3, CP4,
                            rate_exp, shape_weib, scale_weib, shape_llogis, 
                            scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                            sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
    
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_remDE(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                                HR = HR, HR1 = HR1, shape1 = shape1, 
                                scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                CP_EE_gen = CP_EE_gen, 
                                CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                rate_exp = rate_exp, shape_weib = shape_weib, 
                                scale_weib = scale_weib, shape_llogis = shape_llogis, 
                                scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                                sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                                sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                                distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}

run_remEE <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                      lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                      lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                      CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                      CP1, CP2, CP3, CP4,
                      rate_exp, shape_weib, scale_weib, shape_llogis, 
                      scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                      sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
    
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_remEE(scenario = sc, n_pat = n, ta = ta, tf = tf,
                              lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                              lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                              lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                              HR = HR, HR1 = HR1, shape1 = shape1, 
                              scale1 = scale1, shape0 = shape0, scale0 = scale0,
                              CP_EE_gen = CP_EE_gen, 
                              CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                              CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                              CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                              rate_exp = rate_exp, shape_weib = shape_weib, 
                              scale_weib = scale_weib, shape_llogis = shape_llogis, 
                              scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                              sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                              sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                              distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}

run_addMECH <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                      lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                      lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                      CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                      CP1, CP2, CP3, CP4, CP_ME1, CP_ME2,
                      rate_exp, shape_weib, scale_weib, shape_llogis, 
                      scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                      sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
    
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_addMECH(scenario = sc, n_pat = n, ta = ta, tf = tf,
                              lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                              lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                              lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                              HR = HR, HR1 = HR1, shape1 = shape1, 
                              scale1 = scale1, shape0 = shape0, scale0 = scale0,
                              CP_EE_gen = CP_EE_gen, 
                              CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                              CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                              CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                              CP_ME1 = CP_ME1, CP_ME2 = CP_ME2,
                              rate_exp = rate_exp, shape_weib = shape_weib, 
                              scale_weib = scale_weib, shape_llogis = shape_llogis, 
                              scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                              sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                              sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                              distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}

run_addEEDE <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                        lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                        lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                        CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                        CP1, CP2, CP3, CP4, CP5, CP6,
                        rate_exp, shape_weib, scale_weib, shape_llogis, 
                        scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                        sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
    
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_addEEDE(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                                HR = HR, HR1 = HR1, shape1 = shape1, 
                                scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                CP_EE_gen = CP_EE_gen, 
                                CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                CP5 = CP5, CP6 = CP6,
                                rate_exp = rate_exp, shape_weib = shape_weib, 
                                scale_weib = scale_weib, shape_llogis = shape_llogis, 
                                scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                                sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                                sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                                distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}

calculate_means <- function(results){
  
  mean_results <- list()
  
  for(sc in names(results)){
    mean_results[[sc]] <- list()
    
    for(n in names(results[[sc]])){
      
      res_n <- results[[sc]][[n]]
      
      mean_results[[sc]][[n]] <- sapply(names(res_n[[1]]), function(var){
        mean(sapply(res_n, function(x) x[[var]]))
      })
    }
  }
  return(mean_results)
}

calculate_means_simp <- function(results){
  
  mean_results <- list()
  
  for(n in names(results)){
      
    res_n <- results[[n]]
      
    mean_results[[n]] <- sapply(names(res_n[[1]]), function(var){
      mean(sapply(res_n, function(x) x[[var]]))
      })
    }
  return(mean_results)
}


run_misspe_CP <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                            lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                            lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                            CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                            CP1, CP2, CP3, CP4,
                            rate_exp, shape_weib, scale_weib, shape_llogis, 
                            scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                            sigma_gengamma, Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
    
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_misspe_cp(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                                HR = HR, HR1 = HR1, shape1 = shape1, 
                                scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                CP_EE_gen = CP_EE_gen, 
                                CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                rate_exp = rate_exp, shape_weib = shape_weib, 
                                scale_weib = scale_weib, shape_llogis = shape_llogis, 
                                scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                                sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                                sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                                distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}

run_misspe_param <- function(nsim, scenarios, patients, ta, tf, lambda_NE,
                            lambda_PH, lambda_EE, lambda_ME, lambda_DE,
                            lambda_CH, HR, HR1, shape1, scale1, shape0, scale0,
                            CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                            CP1, CP2, CP3, CP4,
                            shape_weib, scale_weib, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(patients))
    names(results[[sc]]) <- patients
    
    for(n in patients){
      results[[sc]][[as.character(n)]] <- vector("list", nsim)
      
      for(i in 1:nsim){
        res <- evaluate_initial(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                                HR = HR, HR1 = HR1, shape1 = shape1, 
                                scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                CP_EE_gen = CP_EE_gen, 
                                CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                shape_weib = shape_weib, 
                                scale_weib = scale_weib[i], distr = distr, pi = pi)
        results[[sc]][[as.character(n)]][[i]] <- res
      }
    }
  }
  return(results)
}


run_simulations_complete <- function(nsim, scenarios, patients, ta, tf, tx_cens,
                                     lambda_NE, lambda_PH, lambda_EE, lambda_ME, 
                                     lambda_DE, lambda_CH, HR, HR1, 
                                     shape1, scale1, shape0, scale0,
                                     CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                                     CP1, CP2, CP3, CP4,
                                     rate_exp, shape_weib, scale_weib, 
                                     shape_llogis, scale_llogis, mean_lnorm, 
                                     sd_lnorm, mu_gengamma, sigma_gengamma, 
                                     Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    results[[sc]] <- vector("list", length(tx_cens))
    names(results[[sc]]) <- tx_cens 
    
    for(tx in length(tx_cens)){
     
     results[[sc]][[tx]] <- vector("list", length(patients))
     names(results[[sc]][[tx]]) <- patients
     
     for(n in patients){
       results[[sc]][[tx]][[as.character(n)]] <- vector("list", nsim)
       
       for(i in 1:nsim){
         res <- evaluate_initial(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                 lambda_NE = lambda_NE[tx], lambda_PH = lambda_PH[tx],
                                 lambda_EE = lambda_EE[tx], lambda_ME = lambda_ME[tx], 
                                 lambda_DE = lambda_DE[tx], lambda_CH = lambda_CH[tx], 
                                 HR = HR, HR1 = HR1, shape1 = shape1, 
                                 scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                 CP_EE_gen = CP_EE_gen, 
                                 CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                 CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                 CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                 rate_exp = rate_exp, shape_weib = shape_weib, 
                                 scale_weib = scale_weib, shape_llogis = shape_llogis, 
                                 scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                                 sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                                 sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                                 distr = distr, pi = pi)
         results[[sc]][[tx]][[as.character(n)]][[i]] <- res
       }
     }
   }
  }
  return(results)
}

run_misspe_cps_complete <- function(nsim, scenarios, patients, ta, tf, tx_cens,
                                     lambda_NE, lambda_PH, lambda_EE, lambda_ME, 
                                     lambda_DE, lambda_CH, HR, HR1, 
                                     shape1, scale1, shape0, scale0,
                                     CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                                     CP1, CP2, CP3, CP4,
                                     rate_exp, shape_weib, scale_weib, 
                                     shape_llogis, scale_llogis, mean_lnorm, 
                                     sd_lnorm, mu_gengamma, sigma_gengamma, 
                                     Q_gengamma, distr, pi = 0){
  
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    results[[sc]] <- vector("list", length(tx_cens))
    names(results[[sc]]) <- tx_cens 
    
    for(tx in length(tx_cens)){
      
      results[[sc]][[tx]] <- vector("list", length(patients))
      names(results[[sc]][[tx]]) <- patients
      
      for(n in patients){
        results[[sc]][[tx]][[as.character(n)]] <- vector("list", nsim)
        
        for(i in 1:nsim){
          res <- evaluate_misspe_cp(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                    lambda_NE = lambda_NE[tx], lambda_PH = lambda_PH[tx],
                                    lambda_EE = lambda_EE[tx], lambda_ME = lambda_ME[tx], 
                                    lambda_DE = lambda_DE[tx], lambda_CH = lambda_CH[tx], 
                                    HR = HR, HR1 = HR1, shape1 = shape1, 
                                    scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                    CP_EE_gen = CP_EE_gen, 
                                    CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                    CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                    CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                    rate_exp = rate_exp, shape_weib = shape_weib, 
                                    scale_weib = scale_weib, shape_llogis = shape_llogis, 
                                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                                    distr = distr, pi = pi)
          results[[sc]][[tx]][[as.character(n)]][[i]] <- res
        }
      }
    }
  }
  return(results)
}


run_misspe_complete <- function(nsim, scenarios, patients, ta, tf, tx_cens,
                                lambda_NE, lambda_PH, lambda_EE, lambda_ME, 
                                lambda_DE, lambda_CH, HR, HR1, 
                                shape1, scale1, shape0, scale0,
                                CP_EE_gen, CP_ME_gen, CP_DE_gen, CP_CH_gen, 
                                CP1, CP2, CP3, CP4,
                                shape_weib, scale_weib, distr, pi = 0){
  results <- vector("list", length(scenarios))
  names(results) <- scenarios
  
  for(sc in scenarios){
    
    results[[sc]] <- vector("list", length(tx_cens))
    names(results[[sc]]) <- tx_cens 
    
    for(tx in length(tx_cens)){
      
      results[[sc]][[tx]] <- vector("list", length(patients))
      names(results[[sc]][[tx]]) <- patients
      
      for(n in patients){
        results[[sc]][[tx]][[as.character(n)]] <- vector("list", nsim)
      }
      
      for(i in 1:nsim){
        res <- evaluate_initial(scenario = sc, n_pat = n, ta = ta, tf = tf,
                                lambda_NE = lambda_NE, lambda_PH = lambda_PH,
                                lambda_EE = lambda_EE, lambda_ME = lambda_ME, 
                                lambda_DE = lambda_DE, lambda_CH = lambda_CH, 
                                HR = HR, HR1 = HR1, shape1 = shape1, 
                                scale1 = scale1, shape0 = shape0, scale0 = scale0,
                                CP_EE_gen = CP_EE_gen, 
                                CP_ME_gen = CP_ME_gen, CP_DE_gen = CP_DE_gen, 
                                CP_CH_gen = CP_CH_gen, CP1 = CP1, 
                                CP2 = CP2, CP3 = CP3, CP4 = CP4, 
                                shape_weib = shape_weib, 
                                scale_weib = scale_weib[i], distr = distr, pi = pi)
      }
    }
  }
  return(results)
}
