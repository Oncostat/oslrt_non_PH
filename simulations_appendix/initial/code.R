#######Appendix of Section 5#######

##Load functions##

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

#Generation of data for Figure 12#

scenarios_05 <- c('NE', 'PH', 'EE', 'ME', 'DE', 'CH')

lambda_NE_05 <- c(0, 0.02, 0.07, 0.12, 0.2)
lambda_PH_05 <- c(0, 0.01, 0.03, 0.06, 0.1)
lambda_EE_05 <- c(0, 0.018, 0.05, 0.095, 0.15)
lambda_ME_05 <- c(0, 0.015, 0.05, 0.09, 0.14)
lambda_DE_05 <- c(0, 0.015, 0.05, 0.09, 0.17)
lambda_CH_05 <- c(0, 0.018, 0.06, 0.11, 0.19)

shape_exp <- 1
scale_exp <- 4/(-log(0.5))^(1/shape_exp)

HR <- 0.5
HR1 <- 2

res_complete_05 <- run_simulations_complete(nsim = nit, scenarios = scenarios_05,
                                            patients = n_patients, ta = ta,
                                            tf = tf, tx_cens = tx_cens, 
                                            lambda_NE = lambda_NE_05, 
                                            lambda_PH = lambda_PH_05,
                                            lambda_EE = lambda_EE_05,
                                            lambda_ME = lambda_ME_05,
                                            lambda_DE = lambda_DE_05,
                                            lambda_CH = lambda_CH_05,
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
                                            distr = 'Weibull')

for(sc in scenarios_05){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_complete_05[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./intermediate_results/initial/', sc, '_0.5_complete.RData'))
}


#Generation of data for Figure 13#

scenarios_07 <- c('PH', 'EE', 'ME', 'DE')

lambda_PH_07 <- c(0, 0.015, 0.04, 0.08, 0.13)
lambda_EE_07 <- c(0, 0.018, 0.055, 0.105, 0.17)
lambda_ME_07 <- c(0, 0.015, 0.05, 0.1, 0.16)
lambda_DE_07 <- c(0, 0.015, 0.06, 0.1, 0.17)

shape_exp <- 1
scale_exp <- 2.84/(-log(0.5))^(1/shape_exp)

HR <- 0.7
HR1 <- 2

res_complete_07 <- run_simulations_complete(nsim = nit, scenarios = scenarios_07,
                                            patients = n_patients, ta = ta,
                                            tf = tf, tx_cens = tx_cens, 
                                            lambda_NE = lambda_NE_07, 
                                            lambda_PH = lambda_PH_07,
                                            lambda_EE = lambda_EE_07,
                                            lambda_ME = lambda_ME_07,
                                            lambda_DE = lambda_DE_07,
                                            lambda_CH = lambda_CH_07,
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
                                            distr = 'Weibull')

for(sc in scenarios_07){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_complete_07[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./intermediate_results/initial/', sc, '_0.7_complete.RData'))
}


#Generation of data for Figure 14#

scenarios_08 <- c('PH', 'EE', 'ME', 'DE')

lambda_PH_08 <- c(0, 0.015, 0.05, 0.09, 0.15)
lambda_EE_08 <- c(0, 0.018, 0.058, 0.11, 0.175)
lambda_ME_08 <- c(0, 0.015, 0.055, 0.1, 0.165)
lambda_DE_08 <- c(0, 0.015, 0.06, 0.11, 0.19)

shape_exp <- 1
scale_exp <- 2.5/(-log(0.5))^(1/shape_exp)

HR <- 0.8
HR1 <- 2

res_complete_08 <- run_simulations_complete(nsim = nit, scenarios = scenarios_08,
                                            patients = n_patients, ta = ta,
                                            tf = tf, tx_cens = tx_cens, 
                                            lambda_NE = lambda_NE_08, 
                                            lambda_PH = lambda_PH_08,
                                            lambda_EE = lambda_EE_08,
                                            lambda_ME = lambda_ME_08,
                                            lambda_DE = lambda_DE_08,
                                            lambda_CH = lambda_CH_08,
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
                                            distr = 'Weibull')

for(sc in scenarios_08){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_complete_08[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./intermediate_results/initial/', sc, '_0.8_complete.RData'))
}


#Generation of data for Figure 15#

scenarios_DE <- c("NE", "PH", "EE")

res_removeDE <- run_remDE(nsim = nit, scenarios = scenarios_DE,
                               patients = n_patients, ta = ta,
                               tf = tf, tx_cens = tx_cens, 
                               lambda_NE = 0.07, 
                               lambda_PH = 0.03,
                               lambda_EE = 0.05,
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
                               distr = 'Weibull')
mean_res_DE <- calculate_means(res_removeDE)
for(sc in scenarios_DE){
  res <- mean_res_DE[[sc]]
  save(res, file = paste0('./intermediate_results/max_combo', sc, '_DE_remove.RData'))
}


#Generation of data for Figure 16#

scenarios_EE <- c("NE", "ME", "DE")

res_removeEE <- run_remEE(nsim = nit, scenarios = scenarios_EE,
                               patients = n_patients, ta = ta,
                               tf = tf, tx_cens = tx_cens, 
                               lambda_NE = 0.07, 
                               lambda_ME = 0.05,
                               lambda_DE = 0.05,
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
                               distr = 'Weibull')
mean_res_EE <- calculate_means(res_removeEE)
for(sc in scenarios_EE){
  res <- mean_res_EE[[sc]]
  save(res, file = paste0('./intermediate_results/max_combo', sc, '_EE_remove.RData'))
}


#Generation of data for Figure 17#

scenarios <- c('NE', 'PH', 'EE', 'ME', 'DE', 'CH')

res_addMECH <- run_addMECH(nsim = nit, scenarios = scenarios,
                           patients = n_patients, ta = ta,
                           tf = tf, tx_cens = tx_cens, 
                           lambda_NE = 0.07, 
                           lambda_PH = 0.03,
                           lambda_EE = 0.05,
                           lambda_ME = 0.05,
                           lambda_DE = 0.05,
                           lambda_CH = 0.06,
                           HR = HR, HR1 = HR1,
                           shape1 = shape_exp, scale1 = scale_exp,
                           shape0 = shape_cont, scale0 = scale_cont,
                           CP_EE_gen = CP_EE_gen, 
                           CP_ME_gen = CP_ME_gen,
                           CP_DE_gen = CP_DE_gen,
                           CP_CH_gen = CP_CH_gen,
                           CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5,
                           CP_ME1 = 1, CP_ME2 = 4,
                           shape_weib = shape_cont,
                           scale_weib = scale_cont,
                           distr = 'Weibull')
mean_res_ME_CH <- calculate_means(res_addMECH)
for(sc in scenarios){
  res <- mean_res_ME_CH[[sc]]
  save(res, file = paste0('./intermediate_results/max_combo', sc, '_ME_CH_add.RData'))
}

#Generation of data for Figure 18#

scenarios <- c('NE', 'PH', 'EE', 'ME', 'DE', 'CH')

res_addEEDE <- run_addEEDE(nsim = nit, scenarios = scenarios,
                           patients = n_patients, ta = ta,
                           tf = tf, tx_cens = tx_cens, 
                           lambda_NE = 0.07, 
                           lambda_PH = 0.03,
                           lambda_EE = 0.05,
                           lambda_ME = 0.05,
                           lambda_DE = 0.05,
                           lambda_CH = 0.06,
                           HR = HR, HR1 = HR1,
                           shape1 = shape_exp, scale1 = scale_exp,
                           shape0 = shape_cont, scale0 = scale_cont,
                           CP_EE_gen = CP_EE_gen, 
                           CP_ME_gen = CP_ME_gen,
                           CP_DE_gen = CP_DE_gen,
                           CP_CH_gen = CP_CH_gen,
                           CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5,
                           CP5 = 2, CP6 = 1,
                           shape_weib = shape_cont,
                           scale_weib = scale_cont,
                           distr = 'Weibull')
mean_res_EE_DE <- calculate_means(res_addEEDE)
for(sc in scenarios){
  res <- mean_res_EE_DE[[sc]]
  save(res, file = paste0('./intermediate_results/max_combo', sc, '_EE_DE_add.RData'))
}

#Generation of data for Figure 19#

lambda_EE <- c(0, 0.018, 0.05, 0.095, 0.15)
lambda_DE <- c(0, 0.015, 0.05, 0.09, 0.17)

res_complete_misspe_cps <- run_misspe_cps_complete(nsim = nit, scenarios = c('EE', 'DE'),
                                                   patients = n_patients, ta = ta,
                                                   tf = tf, tx_cens = tx_cens, 
                                                   lambda_EE = lambda_EE,
                                                   lambda_DE = lambda_DE,
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
                                                   distr = 'Weibull')
for(sc in c('EE', 'DE')){
  result <- list()
  for(tx in tx_cens){
    result[[tx]] <- calculate_means_simp(res_complete_misspe_cps[[sc]][[tx]])
  }
  res <- result
  save(res, file = paste0('./intermediate_results/misspe_cps/', sc, '_complete.RData'))
}
