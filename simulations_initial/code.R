#######Simulations of Section 5#######

##Load functions##

source("./functions.R")

##Section 5.2##

#nit can be change to reduce the compilation time
#nit <- 100
nit <- 10000

scenarios <- c('NE', 'PH', 'EE', 'ME', 'DE', 'CH')
n_patients  <- c(20, 30, 50, 60, 80, 100, 150, 200)

ta <- 3
tf <- 4

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


#Generation of data for Figure 2#

res_initial <- run_simulations(nsim = nit, scenarios = scenarios,
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
                               distr = 'Weibull')
mean_res <- calculate_means(results = res_initial)
for(sc in scenarios){
  res_15 <- mean_res[[sc]]
  save(res_15, file = paste0('./intermediate_results/initial/', sc, '.RData'))
}

#Generation of data for Figure 3#

res_misspe_cps <- run_misspe_CP(nsim = nit, scenarios = c("EE", "DE"),
                                patients = n_patients, ta = ta, tf = tf,
                                lambda_NE = lambda_NE, lambda_PH = lambda_PH, 
                                lambda_EE = lambda_EE, lambda_ME = lambda_ME,
                                lambda_DE = lambda_DE, lambda_CH = lambda_CH,
                                HR =HR, HR1 = HR1, 
                                shape1 = shape_exp, scale1 = scale_exp,
                                shape0 = shape_cont, scale0 = scale_cont,
                                CP_EE_gen = CP_EE_gen, CP_ME_gen = CP_ME_gen,
                                CP_DE_gen = CP_DE_gen, CP_CH_gen = CP_CH_gen,
                                CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, 
                                shape_weib = shape_cont, scale_weib = scale_cont, 
                                distr = 'Weibull')
mean_res_misspe_cps <- calculate_means(results = res_misspe_cps)
for(sc in c("EE", "DE")){
  res_15 <- mean_res_misspe_cps[[sc]]
  save(res_15, file = paste0('./intermediate_results/misspe_cps/', sc, '.RData'))
}
