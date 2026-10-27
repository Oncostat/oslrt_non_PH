#######Appendix of Section 6#######

##Load functions

source("../../functions.R")

#nit can be change to reduce the compilation time
#nit <- 100
nit <- 10000

n_patients  <- c(20, 30, 50, 60, 80, 100, 150, 200)
tx_cens < - c('0%', '5%', '15%', '25%', '35%')

ta <- 3
tf <- 4

shape_cont <- 1
scale_cont <- 2/(-log(0.5))^(1/shape_cont)

CP_EE_gen <- 1
CP_ME_gen <- c(1, 3)
CP_DE_gen <- 3
CP_CH_gen <- 1

scenarios <- c('NE', 'PH', 'EE', 'ME', 'DE', 'CH')

lambda_NE <- c(0, 0.02, 0.07, 0.12, 0.2)
lambda_PH <- c(0, 0.01, 0.03, 0.06, 0.1)
lambda_EE <- c(0, 0.018, 0.05, 0.095, 0.15)
lambda_ME <- c(0, 0.015, 0.05, 0.09, 0.14)
lambda_DE <- c(0, 0.015, 0.05, 0.09, 0.17)
lambda_CH <- c(0, 0.018, 0.06, 0.11, 0.19)

shape_exp <- 1
scale_exp <- 4/(-log(0.5))^(1/shape_exp)

HR <- 0.5
HR1 <- 2


#Generation of data for Figures 23 and 24#

m_miss <- rgamma(n = nit, shape = 80, rate = 40)
scale_miss <- m_miss/(-log(0.5))^(1/shape_cont)

res_param <- run_misspe_complete(nsim = nit, scenarios = scenarios,
                                 patients = n_patients, ta = ta,
                                 tf = tf, tx_cens = tx_cens, 
                                 lambda_NE = lambda_NE, 
                                 lambda_PH = lambda_PH,
                                 lambda_EE = lambda_EE,
                                 lambda_ME = lambda_ME,
                                 lambda_DE = lambda_DE,
                                 lambda_CH = lambda_CH,
                                 HR = HR, HR1 = HR1,
                                 shape1 = shape_exp, scale1 = scale_exp,
                                 shape0 = shape_cont, scale0 = scale_cont,
                                 CP_EE_gen = CP_EE_gen, 
                                 CP_ME_gen = CP_ME_gen,
                                 CP_DE_gen = CP_DE_gen,
                                 CP_CH_gen = CP_CH_gen,
                                 CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5,
                                 shape_weib = shape_cont, scale_weib = scale_miss,
                                 distr = "Weibull")

for(sc in scenarios){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_param[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./misspe_param/', sc, '_complete.RData'))
}


#Generation of data for Figures 25 and 26#

res_complete_pi06 <- run_simulations_complete(nsim = nit, scenarios = scenarios,
                                              patients = n_patients, ta = ta,
                                              tf = tf, tx_cens = tx_cens, 
                                              lambda_NE = lambda_NE, 
                                              lambda_PH = lambda_PH,
                                              lambda_EE = lambda_EE,
                                              lambda_ME = lambda_ME,
                                              lambda_DE = lambda_DE,
                                              lambda_CH = lambda_CH,
                                              HR = HR, HR1 = HR1,
                                              shape1 = shape_exp, scale1 = scale_exp,
                                              shape0 = shape_cont, scale0 = scale_cont,
                                              CP_EE_gen = CP_EE_gen, 
                                              CP_ME_gen = CP_ME_gen,
                                              CP_DE_gen = CP_DE_gen,
                                              CP_CH_gen = CP_CH_gen,
                                              CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5,
                                              shape_weib = shape_cont,
                                              scale_weib = scale_cont,
                                              distr = 'Weibull', pi = 0.6)
for(sc in scenarios){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_complete_pi06[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./sampling_variability/', sc, '_complete_pi0.6.RData'))
}


#Generation of data for Figure 27 and 28#

res_complete_pi1 <- run_simulations_complete(nsim = nit, scenarios = scenarios,
                                              patients = n_patients, ta = ta,
                                              tf = tf, tx_cens = tx_cens, 
                                              lambda_NE = lambda_NE, 
                                              lambda_PH = lambda_PH,
                                              lambda_EE = lambda_EE,
                                              lambda_ME = lambda_ME,
                                              lambda_DE = lambda_DE,
                                              lambda_CH = lambda_CH,
                                              HR = HR, HR1 = HR1,
                                              shape1 = shape_exp, scale1 = scale_exp,
                                              shape0 = shape_cont, scale0 = scale_cont,
                                              CP_EE_gen = CP_EE_gen, 
                                              CP_ME_gen = CP_ME_gen,
                                              CP_DE_gen = CP_DE_gen,
                                              CP_CH_gen = CP_CH_gen,
                                              CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5,
                                              shape_weib = shape_cont,
                                              scale_weib = scale_cont,
                                              distr = 'Weibull', pi = 1)
for(sc in scenarios){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_complete_pi1[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./sampling_variability/', sc, '_complete_pi1.RData'))
}

#Generation of data for Figure 29 and 30#

shape_llogis <- 1.7
scale_llogis <- 2

res_distrib <- run_simulations_complete(nsim = nit, scenarios = scenarios,
                                        patients = n_patients, ta = ta, tf = tf,
                                        tx_cens = tx_cens, 
                                        lambda_NE = lambda_NE, 
                                        lambda_PH = lambda_PH, 
                                        lambda_EE = lambda_EE, 
                                        lambda_ME = lambda_ME,
                                        lambda_DE = lambda_DE, 
                                        lambda_CH = lambda_CH,
                                        HR = HR, HR1 = HR1,
                                        shape1 = shape_exp, scale1 = scale_exp, 
                                        shape0 = shape_cont, scale0 = scale_cont,
                                        CP_EE_gen = CP_EE_gen, CP_ME_gen = CP_ME_gen,
                                        CP_DE_gen = CP_DE_gen, CP_CH_gen = CP_CH_gen,
                                        CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, 
                                        shape_llogis = shape_llogis, 
                                        scale_llogis = scale_llogis,
                                        distr = 'Log-logistic')
for(sc in scenarios){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_distrib[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./misspe_distribution/', sc, '_complete.RData'))
}
