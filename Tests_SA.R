library(nph)
library(survival)
library(nphsim)
library(flexsurv)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(lubridate)

#rm(list = ls())

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
#return a list with the statistic Z, the p-value and the expected number of events E
mOSLRT <- function(data, rate_exp, shape_weib, scale_weib, shape_llogis,
                   scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                   sigma_gengamma, Q_gengamma, distr){
  X <- data$t   # observed failure time 
  delta <- data$e # censoring indicate 1-event 0-censoring 
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(t, rate)}
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
  O <- sum(delta)       # observed number of events 
  E <- sum(M)
  V <- (O+E)/2
  Z <- (O-E)/sqrt(V)
  pvalue <- 1-pnorm(-Z)
  return(c(Z, pvalue, E))
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
#return the p-value
OSLRT <- function(data, rate_exp, shape_weib, scale_weib, shape_llogis,
                  scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                  sigma_gengamma, Q_gengamma, distr){
  X <- data$t   # observed failure time 
  delta <- data$e # censoring indicate 1-event 0-censoring 
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(t, rate)}
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
  O <- sum(delta)       # observed number of events 
  E <- sum(M)
  Z <- (O-E)/sqrt(E)
  pvalue <- 1-pnorm(-Z)
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
#return the p-value
Score_RC <- function(data, rate_exp, shape_weib, scale_weib, shape_llogis,
                     scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                     sigma_gengamma, Q_gengamma, distr){
  X <- data$t   # observed failure time 
  delta <- data$e # censoring indicate 1-event 0-censoring 
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(t, rate)}
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
  pval <- 1-pnorm(-Z)
  return(pval)
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
#return a list with the statistic Z, the p-value and the 'expected' number of events for early effect of treatment
Score_EE <- function(data, CP, rate_exp, shape_weib, scale_weib, shape_llogis,
                     scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                     sigma_gengamma, Q_gengamma, distr){
  data2 <- arrange(data, t)
  t_CP <- data2$t[which(data2$t<=CP)]  #times less than tau
  O_CP <- data2$e[which(data2$t<=CP)]  #obs associated with times less than tau
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(t, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    H0 <- H(t_CP, rate_exp) #exp for times less than tau
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
  H_tau2 <- rep(H0_CP, length(data2$t[which(data2$t>=CP)]))  #H0(tau) for all times better than tau
  H_tau <- sum(H_tau2)
  Obs <- O_CP-H0
  Num <- sum(Obs)-H_tau
  Var <- sum(H0)+H_tau
  Z  <- Num/sqrt(Var)
  pval <- 1-pnorm(-Z)
  return(c(Z, pval, sum(H0)+H_tau))
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
#return the p-value
Score_ME <- function(data, CP1, CP2, rate_exp, shape_weib, scale_weib, 
                     shape_llogis,scale_llogis, mean_lnorm, sd_lnorm, 
                     mu_gengamma, sigma_gengamma, Q_gengamma, distr){
  data2 <- arrange(data, t)
  t_CP <- data2$t[which(data2$t>CP1 & data2$t<=CP2)]  #times between tau1 and tau2
  O_CP <- data2$e[which(data2$t>CP1 & data2$t<=CP2)]  #obs associated with times between tau1 and tau2
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(t, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    H0_CPs <- H(t_CP, rate_exp) #exp associated with times between tau1 and tau2
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
  H_CP1 <- sum(H_CP1)   #sum_ti>=tau1(H0(tau1))
  H_CP2 <- rep(H0_CP2, length(data2$t[which(data2$t>=CP2)]))
  H_CP2 <- sum(H_CP2)   #sum_ti>=tau2(H0(tau2))
  Obs <- O_CP-H0_CPs
  Num <- sum(Obs)+H_CP1-H_CP2
  Var <- sum(H0_CPs2)-H_CP1+H_CP2
  Z <- Num/sqrt(Var)
  pval <- 1-pnorm(-Z)
  return(pval)
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
#return a list with the statistic Z, the p-value and the 'expected' number of events for delayed effect of treatment
Score_DE <- function(data, CP, rate_exp, shape_weib, scale_weib, shape_llogis,
                     scale_llogis, mean_lnorm, sd_lnorm, mu_gengamma,
                     sigma_gengamma, Q_gengamma, distr){
  data2 <- arrange(data, t)
  t_CP <- data2$t[which(data2$t>CP)]  #times better than tau
  O_CP <- data2$e[which(data2$t>CP)]  #obs associated with times better than tau
  if(distr=='Exponential'){
    S <- function(u, rate){1-pexp(t, rate)}
    H <- function(u, rate){-log(S(u, rate))}
    H0 <- H(t_CP, rate_exp) #exp for times less than tau
    H0_CP <- H(CP, rate_exp) #H0(tau)
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
  pval <- 1-pnorm(-Z)
  return(c(Z, pval, sum(H0-H_tau)))
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
#return the p-value
maxcombo1 <- function(data, CP1, CP2, CP3, CP4, rate_exp, shape_weib, 
                      scale_weib, shape_llogis,  scale_llogis, mean_lnorm, 
                      sd_lnorm, mu_gengamma, sigma_gengamma, Q_gengamma, distr){
  Z_moslrt <- mOSLRT(data = data, rate_exp = rate_exp, shape_weib = shape_weib, 
                     scale_weib = scale_weib, shape_llogis = shape_llogis,  
                     scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                     sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                     sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                     distr = distr)
  Z_EE1 <- Score_EE(data = data, CP = CP1, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr)
  Z_EE2 <- Score_EE(data = data, CP = CP2,rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr)
  Z_DE1 <- Score_DE(data = data, CP = CP3, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr)
  Z_DE2 <- Score_DE(data = data, CP = CP4, rate_exp = rate_exp, shape_weib = shape_weib, 
                    scale_weib = scale_weib, shape_llogis = shape_llogis,  
                    scale_llogis = scale_llogis, mean_lnorm = mean_lnorm, 
                    sd_lnorm = sd_lnorm, mu_gengamma = mu_gengamma, 
                    sigma_gengamma = sigma_gengamma, Q_gengamma = Q_gengamma, 
                    distr = distr)
  Score <- c(Z_moslrt[1], Z_EE1[1], Z_EE2[1], Z_DE1[1], Z_DE2[1])
  pval <- c(Z_moslrt[2], Z_EE1[2], Z_EE2[2], Z_DE1[2], Z_DE2[2])
  n_indiv <- c(Z_moslrt[3], Z_EE1[3], Z_EE2[3], Z_DE1[3], Z_DE2[3])
  pval_hoch <- p.adjust(pval, method = 'hochberg')
  m <- length(Score)
  Zmax <- max(na.omit(abs(Score)))
  ind <- which(abs(Score)==Zmax)
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
