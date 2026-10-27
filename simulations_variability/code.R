#######Simulations of Section 6#######

##Load functions##

source("../../functions.R")

#nit can be change to reduce the compilation time
#nit <- 100
nit <- 10000 

scenarios <- c('NE', 'PH', 'EE', 'ME', 'DE', 'CH')
n_patients  <- c(20, 30, 50, 60, 80, 100, 150, 200)

ta <- 3
tf <- 7

lambda_NE <- 0.07
lambda_PH <- 0.03
lambda_EE <- 0.05
lambda_ME <- 0.05
lambda_DE <- 0.05
lambda_CH <- 0.06

HR <- 0.5
HR1 <- 2

shape_cont <- 1
scale_cont <- 2/(-log(0.5))^(1/shape_cont)
shape_exp <- 1
scale_exp <- 4/(-log(0.5))^(1/shape_exp)

CP_EE_gen <- 1
CP_ME_gen <- c(1, 3)
CP_DE_gen <- 3
CP_CH_gen <- 1

m_miss <- rgamma(n = nit, shape = 80, rate = 40)  #random generation with gamma distribution G(80, 40) => E = 2 and Var = 0.05
scale_miss <- m_miss/(-log(0.5))^(1/shape_cont)

##Section 6.1 - variability in the exponential parameter##

#Generation of data for Figure 4#

res_misspe_param <- run_misspe_param(nsim = nit, scenarios = scenarios, 
                                     patients = n_patients, ta = ta, tf = tf,
                                     lambda_NE = lambda_NE, lambda_PH = lambda_PH, 
                                     lambda_EE = lambda_EE, lambda_ME = lambda_ME,
                                     lambda_DE = lambda_DE, lambda_CH = lambda_CH,
                                     HR = HR, HR1 = HR1, 
                                     shape1 = shape_exp, scale1 = scale_exp,
                                     shape0 = shape_cont, scale0 = scale_cont,
                                     CP_EE_gen = CP_EE_gen, CP_ME_gen = CP_ME_gen,
                                     CP_DE_gen = CP_DE_gen, CP_CH_gen = CP_CH_gen,
                                     CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, 
                                     shape_weib = shape_cont, scale_weib = scale_miss,
                                     distr = 'Weibull')
mean_misspe_param <- calculate_means(results = res_misspe_param)
for(sc in scenarios){
  res_15 <- mean_misspe_param[[sc]]
  save(res_15, file = paste0('./param_misspe/intermediate_results/', sc, '.RData'))
}


##Section 6.2 - sampling variability of the external control group##

#Generation of data for Figure 5#

res_samp_var <- run_simulation(nsim = nit, scenarios = scenarios,
                               patients = n_patients, ta = ta, tf = tf,
                               lambda_NE = lambda_NE, lambda_PH = lambda_PH, 
                               lambda_EE = lambda_EE, lambda_ME = lambda_ME,
                               lambda_DE = lambda_DE, lambda_CH = lambda_CH,
                               HR = HR, HR1 = HR1,
                               shape1 = shape_exp, scale1 = scale_exp, 
                               shape0 = shape_cont, scale0 = scale_cont,
                               CP_EE_gen = CP_EE_gen, CP_ME_gen = CP_ME_gen,
                               CP_DE_gen = CP_DE_gen, CP_CH_gen = CP_CH_gen,
                               CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, 
                               shape_weib = shape_cont, scale_weib = scale_cont, 
                               distr = 'Weibull', pi = 0.6)
mean_samp_var <- calculate_means(results = res_samp_var)
for(sc in scenarios){
  res_15 <- mean_samp_var[[sc]]
  save(res_15, file = paste0('./sampling_variability/intermediate_results/', sc, '.Rdata'))
}


##Section 6.3 - model misspecification of the survival distribution of the external control curve##

#Generation of data for Figure 6#

shape_llogis <- 1.7
scale_llogis <- 2

res_misspe_distr <- run_simulation(nsim = nit, scenarios = scenarios,
                                   patients = n_patients, ta = ta, tf = tf,
                                   lambda_NE = lambda_NE, lambda_PH = lambda_PH, 
                                   lambda_EE = lambda_EE, lambda_ME = lambda_ME,
                                   lambda_DE = lambda_DE, lambda_CH = lambda_CH,
                                   HR = HR, HR1 = HR1,
                                   shape1 = shape_exp, scale1 = scale_exp, 
                                   shape0 = shape_cont, scale0 = scale_cont,
                                   CP_EE_gen = CP_EE_gen, CP_ME_gen = CP_ME_gen,
                                   CP_DE_gen = CP_DE_gen, CP_CH_gen = CP_CH_gen,
                                   CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, 
                                   shape_llogis = shape_llogis, scale_llogis = scale_llogis,
                                   distr = 'Log-logistic')
mean_misspe_distr <- calculate_means(results = res_misspe_distr)
for(sc in scenarios){
  res_15 <- mean_misspe_distr[[sc]]
  save(res_15, file = paste0('./distribution_misspe/intermediate_results/', sc, '.RData'))
}
