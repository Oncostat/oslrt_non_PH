source(file = "Tests_functions.r", echo = FALSE)

##########################
#######Null effect########
##########################
#Parameters for the control group#
shape0 <- 1
m0 <- 2
scale0 <- m0/(-log(0.5))^(1/shape0)
distr0 <- 'Weibull'
#Parameters for the experimental group#
shape1 <- 1
m1 <- 2
scale1 <- m1/(-log(0.5))^(1/shape1)

#Survival curves#
t_cont <- rweibull(n = 3000, shape = shape0, scale = scale0)
delta_cont <- ifelse(t_cont<(2555/365), 1, 0)
t_cont <- ifelse(t_cont>(2555/365), (2555/365), t_cont)
C <- survfit(Surv(t_cont, delta_cont)~1)
t_exp <- rweibull(n = 3000, shape = shape1, scale = scale1)
delta_exp <- ifelse(t_exp<(2555/365), 1, 0)
t_exp <- ifelse(t_exp>(2555/365), (2555/365), t_exp)
T5 <- survfit(Surv(t_exp, delta_exp)~1)
#Survival curves#
plot(C, conf.int = FALSE, xlab = 'Years', ylim = c(0,1), ylab = 'Survival', col = 'black', main = 'Survival')
lines(T5, conf.int = FALSE, col = 'blue')
legend('right', .9, c("Control", "Experimental"), col = c('black','blue'), lty = c(1,1))

set.seed(5)

nit <- 10000

#Censoring rate lambda_cens (0% 5% 15% 25% 35%)#
#HR = 1 (0 0.02 0.07 0.12 0.2)

lambda_cens <- 0.07  #15% of censoring for HR = 0.5

CP_EE <- 4
CP_DE <- 2
CP_ME1 <- 1
CP_ME2 <- 6

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

nit <- 10000

pi1 <- 1
pi3 <- 0.6

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_init_oslrt20 <- rep(0, nit)
err_init_moslrt20 <- rep(0, nit)
err_init_rc20 <- rep(0, nit)
err_init_de20 <- rep(0, nit)
err_init_ee20 <- rep(0, nit)
err_init_me20 <- rep(0, nit)
err_init_max_hoch_20 <- rep(0, nit)
err_init_max_exact_20 <- rep(0, nit)

err_init_oslrt30 <- rep(0, nit)
err_init_moslrt30 <- rep(0, nit)
err_init_rc30 <- rep(0, nit)
err_init_de30 <- rep(0, nit)
err_init_ee30 <- rep(0, nit)
err_init_me30 <- rep(0, nit)
err_init_max_hoch_30 <- rep(0, nit)
err_init_max_exact_30 <- rep(0, nit)

err_init_oslrt50 <- rep(0, nit)
err_init_moslrt50 <- rep(0, nit)
err_init_rc50 <- rep(0, nit)
err_init_de50 <- rep(0, nit)
err_init_ee50 <- rep(0, nit)
err_init_me50 <- rep(0, nit)
err_init_max_hoch_50 <- rep(0, nit)
err_init_max_exact_50 <- rep(0, nit)

err_init_oslrt60 <- rep(0, nit)
err_init_moslrt60 <- rep(0, nit)
err_init_rc60 <- rep(0, nit)
err_init_de60 <- rep(0, nit)
err_init_ee60 <- rep(0, nit)
err_init_me60 <- rep(0, nit)
err_init_max_hoch_60 <- rep(0, nit)
err_init_max_exact_60 <- rep(0, nit)

err_init_oslrt80 <- rep(0, nit)
err_init_moslrt80 <- rep(0, nit)
err_init_rc80 <- rep(0, nit)
err_init_de80 <- rep(0, nit)
err_init_ee80 <- rep(0, nit)
err_init_me80 <- rep(0, nit)
err_init_max_hoch_80 <- rep(0, nit)
err_init_max_exact_80 <- rep(0, nit)

err_init_oslrt100 <- rep(0, nit)
err_init_moslrt100 <- rep(0, nit)
err_init_rc100 <- rep(0, nit)
err_init_de100 <- rep(0, nit)
err_init_ee100 <- rep(0, nit)
err_init_me100 <- rep(0, nit)
err_init_max_hoch_100 <- rep(0, nit)
err_init_max_exact_100 <- rep(0, nit)

err_init_oslrt150 <- rep(0, nit)
err_init_moslrt150 <- rep(0, nit)
err_init_rc150 <- rep(0, nit)
err_init_de150 <- rep(0, nit)
err_init_ee150 <- rep(0, nit)
err_init_me150 <- rep(0, nit)
err_init_max_hoch_150 <- rep(0, nit)
err_init_max_exact_150 <- rep(0, nit)

err_init_oslrt200 <- rep(0, nit)
err_init_moslrt200 <- rep(0, nit)
err_init_rc200 <- rep(0, nit)
err_init_de200 <- rep(0, nit)
err_init_ee200 <- rep(0, nit)
err_init_me200 <- rep(0, nit)
err_init_max_hoch_200 <- rep(0, nit)
err_init_max_exact_200 <- rep(0, nit)

for(i in 1:nit){
  time20 <- rweibull(20, shape1, scale1)
  u20 <- runif(20, 0, ta)
  
  time30 <- rweibull(30, shape1, scale1)
  u30 <- runif(30, 0, ta)
  
  time50 <- rweibull(50, shape1, scale1)
  u50 <- runif(50, 0, ta)
  
  time60 <- rweibull(60, shape1, scale1)
  u60 <- runif(60, 0, ta)
  
  time80 <- rweibull(80, shape1, scale1)
  u80 <- runif(80, 0, ta)
  
  time100 <- rweibull(100, shape1, scale1)
  u100 <- runif(100, 0, ta)
  
  time150 <- rweibull(150, shape1, scale1)
  u150 <- runif(150, 0, ta)
  
  time200 <- rweibull(200, shape1, scale1)
  u200 <- runif(200, 0, ta)
  
  if(lambda_cens==0){  #adm censoring
    del20 <- rep(1, 20)
    del30 <- rep(1, 30)
    del50 <- rep(1, 50)
    del60 <- rep(1, 60)
    del80 <- rep(1, 80)
    del100 <- rep(1, 100)
    del150 <- rep(1, 150)
    del200 <- rep(1, 200)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- stats::model.frame(Surv(t20, delta20)~1)  
    S30 <- stats::model.frame(Surv(t30, delta30)~1) 
    S50 <- stats::model.frame(Surv(t50, delta50)~1)  
    S60 <- stats::model.frame(Surv(t60, delta60)~1)  
    S80 <- stats::model.frame(Surv(t80, delta80)~1)  
    S100 <- stats::model.frame(Surv(t100, delta100)~1)  
    S150 <- stats::model.frame(Surv(t150, delta150)~1)  
    S200 <- stats::model.frame(Surv(t200, delta200)~1)
  }
  else{ #other censoring + adm censoring
    cens20 <- rexp(20, lambda_cens)
    time20 <- ifelse(time20 < cens20, time20, cens20)
    del20 <- ifelse(time20 < cens20, 1, 0)
    
    cens30 <- rexp(30, lambda_cens)
    time30 <- ifelse(time30 < cens30, time30, cens30)
    del30 <- ifelse(time30 < cens30, 1, 0)
    
    cens50 <- rexp(50, lambda_cens)
    time50 <- ifelse(time50 < cens50, time50, cens50)
    del50 <- ifelse(time50 < cens50, 1, 0)
    
    cens60 <- rexp(60, lambda_cens)
    time60 <- ifelse(time60 < cens60, time60, cens60)
    del60 <- ifelse(time60 < cens60, 1, 0)
    
    cens80 <- rexp(80, lambda_cens)
    time80 <- ifelse(time80 < cens80, time80, cens80)
    del80 <- ifelse(time80 < cens80, 1, 0)
    
    cens100 <- rexp(100, lambda_cens)
    time100 <- ifelse(time100 < cens100, time100, cens100)
    del100 <- ifelse(time100 < cens100, 1, 0)
    
    cens150 <- rexp(150, lambda_cens)
    time150 <- ifelse(time150 < cens150, time150, cens150)
    del150 <- ifelse(time150 < cens150, 1, 0)
    
    cens200 <- rexp(200, lambda_cens)
    time200 <- ifelse(time200 < cens200, time200, cens200)
    del200 <- ifelse(time200 < cens200, 1, 0)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- survfit(Surv(t20, delta20) ~ 1)
    S30 <- survfit(Surv(t30, delta30) ~ 1)
    S50 <- survfit(Surv(t50, delta50) ~ 1)
    S60 <- survfit(Surv(t60, delta60) ~ 1)
    S80 <- survfit(Surv(t80, delta80) ~ 1)
    S100 <- survfit(Surv(t100, delta100) ~ 1)
    S150 <- survfit(Surv(t150, delta150) ~ 1)
    S200 <- survfit(Surv(t200, delta200) ~ 1)
  }
  data20 <- ten(S20)
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a20_init <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202_init <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20_init <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20_init <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20_init <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20_init <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a30_init <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302_init <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30_init <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30_init <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30_init <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30_init <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a50_init <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502_init <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50_init <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50_init <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50_init <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50_init <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a60_init <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602_init <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60_init <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60_init <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60_init <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60_init <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a80_init <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802_init <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80_init <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80_init <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80_init <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80_init <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a100_init <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002_init  <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100_init  <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100_init  <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100_init  <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100_init  <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a150_init <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502_init <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150_init <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150_init <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150_init <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150_init <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a200_init <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002_init <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200_init <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200_init <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200_init <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200_init <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)

  err_init_oslrt20[i] <- ifelse((a20_init<0.05), 1, 0)
  err_init_moslrt20[i] <- ifelse((a202_init<0.05), 1, 0)
  err_init_rc20[i] <- ifelse((b20_init<0.05), 1, 0)
  err_init_de20[i] <- ifelse((c20_init<0.05), 1, 0)
  err_init_ee20[i] <- ifelse((d20_init<0.05), 1, 0)
  err_init_me20[i] <- ifelse((e20_init<0.05), 1, 0)
  err_init_max_hoch_20[i] <- ifelse(g20_hoch_init<0.05, 1, 0)
  err_init_max_exact_20[i] <- ifelse(g20_exact_init<0.05, 1, 0)

  err_init_oslrt30[i] <- ifelse((a30_init<0.05), 1, 0)
  err_init_moslrt30[i] <- ifelse((a302_init<0.05), 1, 0)
  err_init_rc30[i] <- ifelse((b30_init<0.05), 1, 0)
  err_init_de30[i] <- ifelse((c30_init<0.05), 1, 0)
  err_init_ee30[i] <- ifelse((d30_init<0.05), 1, 0)
  err_init_me30[i] <- ifelse((e30_init<0.05), 1, 0)
  err_init_max_hoch_30[i] <- ifelse(g30_hoch_init<0.05, 1, 0)
  err_init_max_exact_30[i] <- ifelse(g30_exact_init<0.05, 1, 0)

  err_init_oslrt50[i] <- ifelse((a50_init<0.05), 1, 0)
  err_init_moslrt50[i] <- ifelse((a502_init<0.05), 1, 0)
  err_init_rc50[i] <- ifelse((b50_init<0.05), 1, 0)
  err_init_de50[i] <- ifelse((c50_init<0.05), 1, 0)
  err_init_ee50[i] <- ifelse((d50_init<0.05), 1, 0)
  err_init_me50[i] <- ifelse((e50_init<0.05), 1, 0)
  err_init_max_hoch_50[i] <- ifelse(g50_hoch_init<0.05, 1, 0)
  err_init_max_exact_50[i] <- ifelse(g50_exact_init<0.05, 1, 0)

  err_init_oslrt60[i] <- ifelse((a60_init<0.05), 1, 0)
  err_init_moslrt60[i] <- ifelse((a602_init<0.05), 1, 0)
  err_init_rc60[i] <- ifelse((b60_init<0.05), 1, 0)
  err_init_de60[i] <- ifelse((c60_init<0.05), 1, 0)
  err_init_ee60[i] <- ifelse((d60_init<0.05), 1, 0)
  err_init_me60[i] <- ifelse((e60_init<0.05), 1, 0)
  err_init_max_hoch_60[i] <- ifelse(g60_hoch_init<0.05, 1, 0)
  err_init_max_exact_60[i] <- ifelse(g60_exact_init<0.05, 1, 0)  

  err_init_oslrt80[i] <- ifelse((a80_init<0.05), 1, 0)
  err_init_moslrt80[i] <- ifelse((a802_init<0.05), 1, 0)
  err_init_rc80[i] <- ifelse((b80_init<0.05), 1, 0)
  err_init_de80[i] <- ifelse((c80_init<0.05), 1, 0)
  err_init_ee80[i] <- ifelse((d80_init<0.05), 1, 0)
  err_init_me80[i] <- ifelse((e80_init<0.05), 1, 0)
  err_init_rmst_80[i] <- ifelse((e_init<0.05), 1, 0)
  err_init_max_hoch_80[i] <- ifelse(g80_hoch_init<0.05, 1, 0)
  err_init_max_exact_80[i] <- ifelse(g80_exact_init<0.05, 1, 0)

  err_init_oslrt100[i] <- ifelse((a100_init<0.05), 1, 0)
  err_init_moslrt100[i] <- ifelse((a1002_init<0.05), 1, 0)
  err_init_rc100[i] <- ifelse((b100_init<0.05), 1, 0)
  err_init_de100[i] <- ifelse((c100_init<0.05), 1, 0)
  err_init_ee100[i] <- ifelse((d100_init<0.05), 1, 0)
  err_init_me100[i] <- ifelse((e100_init<0.05), 1, 0)
  err_init_max_hoch_100[i] <- ifelse(g100_hoch_init<0.05, 1, 0)
  err_init_max_exact_100[i] <- ifelse(g100_exact_init<0.05, 1, 0)

  err_init_oslrt150[i] <- ifelse((a150_init<0.05), 1, 0)
  err_init_moslrt150[i] <- ifelse((a1502_init<0.05), 1, 0)
  err_init_rc150[i] <- ifelse((b150_init<0.05), 1, 0)
  err_init_de150[i] <- ifelse((c150_init<0.05), 1, 0)
  err_init_ee150[i] <- ifelse((d150_init<0.05), 1, 0)
  err_init_me150[i] <- ifelse((e150_init<0.05), 1, 0)
  err_init_max_hoch_150[i] <- ifelse(g150_hoch_init<0.05, 1, 0)
  err_init_max_exact_150[i] <- ifelse(g150_exact_init<0.05, 1, 0)

  err_init_oslrt200[i] <- ifelse((a200_init<0.05), 1, 0)
  err_init_moslrt200[i] <- ifelse((a2002_init<0.05), 1, 0)
  err_init_rc200[i] <- ifelse((b200_init<0.05), 1, 0)
  err_init_de200[i] <- ifelse((c200_init<0.05), 1, 0)              
  err_init_ee200[i] <- ifelse((d200_init<0.05), 1, 0)
  err_init_me200[i] <- ifelse((e200_init<0.05), 1, 0)
  err_init_max_hoch_200[i] <- ifelse(g200_hoch_init<0.05, 1, 0)
  err_init_max_exact_200[i] <- ifelse(g200_exact_init<0.05, 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_init <- sum(err_init_oslrt20)/nit
mos20_init <- sum(err_init_moslrt20)/nit
rc20_init <- sum(err_init_rc20)/nit
de20_init <- sum(err_init_de20)/nit
ee20_init <- sum(err_init_ee20)/nit
me20_init <- sum(err_init_me20)/nit
max_hoch20_init <- sum(err_init_max_hoch_20)/nit
max_exact20_init <- sum(err_init_max_exact_20)/nit

diff1_os20 <- ((os20_1-os20_init)/os20_init)*100
diff1_mos20 <- ((mos20_1-mos20_init)/mos20_init)*100
diff1_ee20 <- ((ee20_1-ee20_init)/ee20_init)*100
diff1_me20 <- ((me20_1-me20_init)/me20_init)*100
diff1_de20 <- ((de20_1-de20_init)/de20_init)*100
diff1_rc20 <- ((rc20_1-rc20_init)/rc20_init)*100
diff1_hoch20 <- ((max_hoch20_1-max_hoch20_init)/max_hoch20_init)*100
diff1_exact20 <- ((max_exact20_1-max_exact20_init)/max_exact20_init)*100

diff3_os20 <- ((os20_3-os20_init)/os20_init)*100
diff3_mos20 <- ((mos20_3-mos20_init)/mos20_init)*100
diff3_ee20 <- ((ee20_3-ee20_init)/ee20_init)*100
diff3_me20 <- ((me20_3-me20_init)/me20_init)*100
diff3_de20 <- ((de20_3-de20_init)/de20_init)*100
diff3_rc20 <- ((rc20_3-rc20_init)/rc20_init)*100
diff3_hoch20 <- ((max_hoch20_3-max_hoch20_init)/max_hoch20_init)*100
diff3_exact20 <- ((max_exact20_3-max_exact20_init)/max_exact20_init)*100

os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_init <- sum(err_init_oslrt30)/nit
mos30_init <- sum(err_init_moslrt30)/nit
rc30_init <- sum(err_init_rc30)/nit
de30_init <- sum(err_init_de30)/nit
ee30_init <- sum(err_init_ee30)/nit
me30_init <- sum(err_init_me30)/nit
max_hoch30_init <- sum(err_init_max_hoch_30)/nit
max_exact30_init <- sum(err_init_max_exact_30)/nit

diff1_os30 <- ((os30_1-os30_init)/os30_init)*100
diff1_mos30 <- ((mos30_1-mos30_init)/mos30_init)*100
diff1_ee30 <- ((ee30_1-ee30_init)/ee30_init)*100
diff1_me30 <- ((me30_1-me30_init)/me30_init)*100
diff1_de30 <- ((de30_1-de30_init)/de30_init)*100
diff1_rc30 <- ((rc30_1-rc30_init)/rc30_init)*100
diff1_hoch30 <- ((max_hoch30_1-max_hoch30_init)/max_hoch30_init)*100
diff1_exact30 <- ((max_exact30_1-max_exact30_init)/max_exact30_init)*100

diff3_os30 <- ((os30_3-os30_init)/os30_init)*100
diff3_mos30 <- ((mos30_3-mos30_init)/mos30_init)*100
diff3_ee30 <- ((ee30_3-ee30_init)/ee30_init)*100
diff3_me30 <- ((me30_3-me30_init)/me30_init)*100
diff3_de30 <- ((de30_3-de30_init)/de30_init)*100
diff3_rc30 <- ((rc30_3-rc30_init)/rc30_init)*100
diff3_hoch30 <- ((max_hoch30_3-max_hoch30_init)/max_hoch30_init)*100
diff3_exact30 <- ((max_exact30_3-max_exact30_init)/max_exact30_init)*100


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_init <- sum(err_init_oslrt50)/nit
mos50_init <- sum(err_init_moslrt50)/nit
rc50_init <- sum(err_init_rc50)/nit
de50_init <- sum(err_init_de50)/nit
ee50_init <- sum(err_init_ee50)/nit
me50_init <- sum(err_init_me50)/nit
max_hoch50_init <- sum(err_init_max_hoch_50)/nit
max_exact50_init <- sum(err_init_max_exact_50)/nit

diff1_os50 <- ((os50_1-os50_init)/os50_init)*100
diff1_mos50 <- ((mos50_1-mos50_init)/mos50_init)*100
diff1_ee50 <- ((ee50_1-ee50_init)/ee50_init)*100
diff1_me50 <- ((me50_1-me50_init)/me50_init)*100
diff1_de50 <- ((de50_1-de50_init)/de50_init)*100
diff1_rc50 <- ((rc50_1-rc50_init)/rc50_init)*100
diff1_hoch50 <- ((max_hoch50_1-max_hoch50_init)/max_hoch50_init)*100
diff1_exact50 <- ((max_exact50_1-max_exact50_init)/max_exact50_init)*100

diff3_os50 <- ((os50_3-os50_init)/os50_init)*100
diff3_mos50 <- ((mos50_3-mos50_init)/mos50_init)*100
diff3_ee50 <- ((ee50_3-ee50_init)/ee50_init)*100
diff3_me50 <- ((me50_3-me50_init)/me50_init)*100
diff3_de50 <- ((de50_3-de50_init)/de50_init)*100
diff3_rc50 <- ((rc50_3-rc50_init)/rc50_init)*100
diff3_hoch50 <- ((max_hoch50_3-max_hoch50_init)/max_hoch50_init)*100
diff3_exact50 <- ((max_exact50_3-max_exact50_init)/max_exact50_init)*100


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_init <- sum(err_init_oslrt60)/nit
mos60_init <- sum(err_init_moslrt60)/nit
rc60_init <- sum(err_init_rc60)/nit
de60_init <- sum(err_init_de60)/nit
ee60_init <- sum(err_init_ee60)/nit
me60_init <- sum(err_init_me60)/nit
max_hoch60_init <- sum(err_init_max_hoch_60)/nit
max_exact60_init <- sum(err_init_max_exact_60)/nit

diff1_os60 <- ((os60_1-os60_init)/os60_init)*100
diff1_mos60 <- ((mos60_1-mos60_init)/mos60_init)*100
diff1_ee60 <- ((ee60_1-ee60_init)/ee60_init)*100
diff1_me60 <- ((me60_1-me60_init)/me60_init)*100
diff1_de60 <- ((de60_1-de60_init)/de60_init)*100
diff1_rc60 <- ((rc60_1-rc60_init)/rc60_init)*100
diff1_hoch60 <- ((max_hoch60_1-max_hoch60_init)/max_hoch60_init)*100
diff1_exact60 <- ((max_exact60_1-max_exact60_init)/max_exact60_init)*100

diff3_os60 <- ((os60_3-os60_init)/os60_init)*100
diff3_mos60 <- ((mos60_3-mos60_init)/mos60_init)*100
diff3_ee60 <- ((ee60_3-ee60_init)/ee60_init)*100
diff3_me60 <- ((me60_3-me60_init)/me60_init)*100
diff3_de60 <- ((de60_3-de60_init)/de60_init)*100
diff3_rc60 <- ((rc60_3-rc60_init)/rc60_init)*100
diff3_hoch60 <- ((max_hoch60_3-max_hoch60_init)/max_hoch60_init)*100
diff3_exact60 <- ((max_exact60_3-max_exact60_init)/max_exact60_init)*100


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_init <- sum(err_init_oslrt80)/nit
mos80_init <- sum(err_init_moslrt80)/nit
rc80_init <- sum(err_init_rc80)/nit
de80_init <- sum(err_init_de80)/nit
ee80_init <- sum(err_init_ee80)/nit
me80_init <- sum(err_init_me80)/nit
max_hoch80_init <- sum(err_init_max_hoch_80)/nit
max_exact80_init <- sum(err_init_max_exact_80)/nit

diff1_os80 <- ((os80_1-os80_init)/os80_init)*100
diff1_mos80 <- ((mos80_1-mos80_init)/mos80_init)*100
diff1_ee80 <- ((ee80_1-ee80_init)/ee80_init)*100
diff1_me80 <- ((me80_1-me80_init)/me80_init)*100
diff1_de80 <- ((de80_1-de80_init)/de80_init)*100
diff1_rc80 <- ((rc80_1-rc80_init)/rc80_init)*100
diff1_hoch80 <- ((max_hoch80_1-max_hoch80_init)/max_hoch80_init)*100
diff1_exact80 <- ((max_exact80_1-max_exact80_init)/max_exact80_init)*100

diff3_os80 <- ((os80_3-os80_init)/os80_init)*100
diff3_mos80 <- ((mos80_3-mos80_init)/mos80_init)*100
diff3_ee80 <- ((ee80_3-ee80_init)/ee80_init)*100
diff3_me80 <- ((me80_3-me80_init)/me80_init)*100
diff3_de80 <- ((de80_3-de80_init)/de80_init)*100
diff3_rc80 <- ((rc80_3-rc80_init)/rc80_init)*100
diff3_hoch80 <- ((max_hoch80_3-max_hoch80_init)/max_hoch80_init)*100
diff3_exact80 <- ((max_exact80_3-max_exact80_init)/max_exact80_init)*100


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_init <- sum(err_init_oslrt100)/nit
mos100_init <- sum(err_init_moslrt100)/nit
rc100_init <- sum(err_init_rc100)/nit
de100_init <- sum(err_init_de100)/nit
ee100_init <- sum(err_init_ee100)/nit
me100_init <- sum(err_init_me100)/nit
max_hoch100_init <- sum(err_init_max_hoch_100)/nit
max_exact100_init <- sum(err_init_max_exact_100)/nit

diff1_os100 <- ((os100_1-os100_init)/os100_init)*100
diff1_mos100 <- ((mos100_1-mos100_init)/mos100_init)*100
diff1_ee100 <- ((ee100_1-ee100_init)/ee100_init)*100
diff1_me100 <- ((me100_1-me100_init)/me100_init)*100
diff1_de100 <- ((de100_1-de100_init)/de100_init)*100
diff1_rc100 <- ((rc100_1-rc100_init)/rc100_init)*100
diff1_hoch100 <- ((max_hoch100_1-max_hoch100_init)/max_hoch100_init)*100
diff1_exact100 <- ((max_exact100_1-max_exact100_init)/max_exact100_init)*100

diff3_os100 <- ((os100_3-os100_init)/os100_init)*100
diff3_mos100 <- ((mos100_3-mos100_init)/mos100_init)*100
diff3_ee100 <- ((ee100_3-ee100_init)/ee100_init)*100
diff3_me100 <- ((me100_3-me100_init)/me100_init)*100
diff3_de100 <- ((de100_3-de100_init)/de100_init)*100
diff3_rc100 <- ((rc100_3-rc100_init)/rc100_init)*100
diff3_hoch100 <- ((max_hoch100_3-max_hoch100_init)/max_hoch100_init)*100
diff3_exact100 <- ((max_exact100_3-max_exact100_init)/max_exact100_init)*100


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_init <- sum(err_init_oslrt150)/nit
mos150_init <- sum(err_init_moslrt150)/nit
rc150_init <- sum(err_init_rc150)/nit
de150_init <- sum(err_init_de150)/nit
ee150_init <- sum(err_init_ee150)/nit
me150_init <- sum(err_init_me150)/nit
max_hoch150_init <- sum(err_init_max_hoch_150)/nit
max_exact150_init <- sum(err_init_max_exact_150)/nit

diff1_os150 <- ((os150_1-os150_init)/os150_init)*100
diff1_mos150 <- ((mos150_1-mos150_init)/mos150_init)*100
diff1_ee150 <- ((ee150_1-ee150_init)/ee150_init)*100
diff1_me150 <- ((me150_1-me150_init)/me150_init)*100
diff1_de150 <- ((de150_1-de150_init)/de150_init)*100
diff1_rc150 <- ((rc150_1-rc150_init)/rc150_init)*100
diff1_hoch150 <- ((max_hoch150_1-max_hoch150_init)/max_hoch150_init)*100
diff1_exact150 <- ((max_exact150_1-max_exact150_init)/max_exact150_init)*100

diff3_os150 <- ((os150_3-os150_init)/os150_init)*100
diff3_mos150 <- ((mos150_3-mos150_init)/mos150_init)*100
diff3_ee150 <- ((ee150_3-ee150_init)/ee150_init)*100
diff3_me150 <- ((me150_3-me150_init)/me150_init)*100
diff3_de150 <- ((de150_3-de150_init)/de150_init)*100
diff3_rc150 <- ((rc150_3-rc150_init)/rc150_init)*100
diff3_hoch150 <- ((max_hoch150_3-max_hoch150_init)/max_hoch150_init)*100
diff3_exact150 <- ((max_exact150_3-max_exact150_init)/max_exact150_init)*100


os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_init <- sum(err_init_oslrt200)/nit
mos200_init <- sum(err_init_moslrt200)/nit
rc200_init <- sum(err_init_rc200)/nit
de200_init <- sum(err_init_de200)/nit
ee200_init <- sum(err_init_ee200)/nit
me200_init <- sum(err_init_me200)/nit
r_200_init <- sum(err_init_rmst_200)/nit
max_hoch200_init <- sum(err_init_max_hoch_200)/nit
max_exact200_init <- sum(err_init_max_exact_200)/nit

diff1_os200 <- ((os200_1-os200_init)/os200_init)*100
diff1_mos200 <- ((mos200_1-mos200_init)/mos200_init)*100
diff1_ee200 <- ((ee200_1-ee200_init)/ee200_init)*100
diff1_me200 <- ((me200_1-me200_init)/me200_init)*100
diff1_de200 <- ((de200_1-de200_init)/de200_init)*100
diff1_rc200 <- ((rc200_1-rc200_init)/rc200_init)*100
diff1_hoch200 <- ((max_hoch200_1-max_hoch200_init)/max_hoch200_init)*100
diff1_exact200 <- ((max_exact200_1-max_exact200_init)/max_exact200_init)*100

diff3_os200 <- ((os200_3-os200_init)/os200_init)*100
diff3_mos200 <- ((mos200_3-mos200_init)/mos200_init)*100
diff3_ee200 <- ((ee200_3-ee200_init)/ee200_init)*100
diff3_me200 <- ((me200_3-me200_init)/me200_init)*100
diff3_de200 <- ((de200_3-de200_init)/de200_init)*100
diff3_rc200 <- ((rc200_3-rc200_init)/rc200_init)*100
diff3_hoch200 <- ((max_hoch200_3-max_hoch200_init)/max_hoch200_init)*100
diff3_exact200 <- ((max_exact200_3-max_exact200_init)/max_exact200_init)*100


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
diff1_osl <- c(diff1_os20, diff1_os30, diff1_os50, diff1_os60, diff1_os80, diff1_os100, diff1_os150, diff1_os200)
diff1_mosl <- c(diff1_mos20, diff1_mos30, diff1_mos50, diff1_mos60, diff1_mos80, diff1_mos100, diff1_mos150, diff1_mos200)
diff1_rc <- c(diff1_rc20, diff1_rc30, diff1_rc50, diff1_rc60, diff1_rc80, diff1_rc100, diff1_rc150, diff1_rc200)
diff1_de <- c(diff1_de20, diff1_de30, diff1_de50, diff1_de60, diff1_de80, diff1_de100, diff1_de150, diff1_de200)
diff1_ee <- c(diff1_ee20, diff1_ee30, diff1_ee50, diff1_ee60, diff1_ee80, diff1_ee100, diff1_ee150, diff1_ee200)
diff1_me <- c(diff1_me20, diff1_me30, diff1_me50, diff1_me60, diff1_me80, diff1_me100, diff1_me150, diff1_me200)
diff1_hoch <- c(diff1_hoch20, diff1_hoch30, diff1_hoch50, diff1_hoch60, diff1_hoch80, diff1_hoch100, diff1_hoch150, diff1_hoch200)
diff1_exact <- c(diff1_exact20, diff1_exact30, diff1_exact50, diff1_exact60, diff1_exact80, diff1_exact100, diff1_exact150, diff1_exact200)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
diff3_osl <- c(diff3_os20, diff3_os30, diff3_os50, diff3_os60, diff3_os80, diff3_os100, diff3_os150, diff3_os200)
diff3_mosl <- c(diff3_mos20, diff3_mos30, diff3_mos50, diff3_mos60, diff3_mos80, diff3_mos100, diff3_mos150, diff3_mos200)
diff3_rc <- c(diff3_rc20, diff3_rc30, diff3_rc50, diff3_rc60, diff3_rc80, diff3_rc100, diff3_rc150, diff3_rc200)
diff3_de <- c(diff3_de20, diff3_de30, diff3_de50, diff3_de60, diff3_de80, diff3_de100, diff3_de150, diff3_de200)
diff3_ee <- c(diff3_ee20, diff3_ee30, diff3_ee50, diff3_ee60, diff3_ee80, diff3_ee100, diff3_ee150, diff3_ee200)
diff3_me <- c(diff3_me20, diff3_me30, diff3_me50, diff3_me60, diff3_me80, diff3_me100, diff3_me150, diff3_me200)
diff3_hoch <- c(diff3_hoch20, diff3_hoch30, diff3_hoch50, diff3_hoch60, diff3_hoch80, diff3_hoch100, diff3_hoch150, diff3_hoch200)
diff3_exact <- c(diff3_exact20, diff3_exact30, diff3_exact50, diff3_exact60, diff3_exact80, diff3_exact100, diff3_exact150, diff3_exact200)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

Neff_1 <- ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 1: null effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0.05, size = 0.2)+
  ylim(0, 0.1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Neff_1

d2_1 <- data.frame(Sample.size = n, Error = c(diff1_osl, diff1_mosl, diff1_ee, diff1_me, diff1_de, diff1_rc, diff1_hoch, diff1_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_1$Test <- as.factor(d2_1$Test)
d2_1$Test <- factor(d2_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_Neff_1 <- ggplot(d2_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_1$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 1: null effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_Neff_1


d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

Neff_3 <- ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 1: null effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0.05, size = 0.2)+
  ylim(0,0.1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
Neff_3

d2_3 <- data.frame(Sample.size = n, Error = c(diff3_osl, diff3_mosl, diff3_ee, diff3_me, diff3_de, diff3_rc, diff3_hoch, diff3_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_3$Test <- as.factor(d2_3$Test)
d2_3$Test <- factor(d2_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_Neff_3 <- ggplot(d2_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_3$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 1: null effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_Neff_3


##########################
###Proportional hazards###
##########################
#Parameters for the control group#
shape0 <- 1
m0 <- 2
scale0 <- m0/(-log(0.5))^(1/shape0)
distr0 <- 'Weibull'
#Parameters for the experimental group#
shape1 <- 1
m1 <- 4   #HR = 0.5
#m1 <- 2.84   #HR = 0.7
#m1 <- 2.5   #HR = 0.8
scale1 <- m1/(-log(0.5))^(1/shape1)

#Survival curves#
t_cont <- rweibull(n = 3000, shape = shape0, scale = scale0)
delta_cont <- ifelse(t_cont<(2555/365), 1, 0)
t_cont <- ifelse(t_cont>(2555/365), (2555/365), t_cont)
C <- survfit(Surv(t_cont, delta_cont)~1)
t_exp <- rweibull(n = 3000, shape = shape1, scale = scale1)
delta_exp <- ifelse(t_exp<(2555/365), 1, 0)
t_exp <- ifelse(t_exp>(2555/365), (2555/365), t_exp)
T5 <- survfit(Surv(t_exp, delta_exp)~1)
#Survival curves#
plot(C, conf.int = FALSE, xlab = 'Years', ylim = c(0,1), ylab = 'Survival', col = 'black', main = 'Survival')
lines(T5, conf.int = FALSE, col = 'blue')
legend('right', .9, c("Control", "Experimental"), col = c('black','blue'), lty = c(1,1))

set.seed(5)

nit <- 10000

#Censoring rate lambda_cens (0% 5% 15% 25% 35%)#
#HR = 0.5 (0 0.01 0.03 0.06 0.1)
#HR = 0.7 (0 0.015 0.04 0.08 0.13)
#HR = 0.8 (0 0.015 0.05 0.099 0.15)

lambda_cens <- 0.03  #15% of censoring for HR = 0.5

CP_EE <- 4
CP_DE <- 2
CP_ME1 <- 1
CP_ME2 <- 6

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

nit <- 10000

pi1 <- 1
pi3 <- 0.6

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_init_oslrt20 <- rep(0, nit)
err_init_moslrt20 <- rep(0, nit)
err_init_rc20 <- rep(0, nit)
err_init_de20 <- rep(0, nit)
err_init_ee20 <- rep(0, nit)
err_init_me20 <- rep(0, nit)
err_init_max_hoch_20 <- rep(0, nit)
err_init_max_exact_20 <- rep(0, nit)

err_init_oslrt30 <- rep(0, nit)
err_init_moslrt30 <- rep(0, nit)
err_init_rc30 <- rep(0, nit)
err_init_de30 <- rep(0, nit)
err_init_ee30 <- rep(0, nit)
err_init_me30 <- rep(0, nit)
err_init_max_hoch_30 <- rep(0, nit)
err_init_max_exact_30 <- rep(0, nit)

err_init_oslrt50 <- rep(0, nit)
err_init_moslrt50 <- rep(0, nit)
err_init_rc50 <- rep(0, nit)
err_init_de50 <- rep(0, nit)
err_init_ee50 <- rep(0, nit)
err_init_me50 <- rep(0, nit)
err_init_max_hoch_50 <- rep(0, nit)
err_init_max_exact_50 <- rep(0, nit)

err_init_oslrt60 <- rep(0, nit)
err_init_moslrt60 <- rep(0, nit)
err_init_rc60 <- rep(0, nit)
err_init_de60 <- rep(0, nit)
err_init_ee60 <- rep(0, nit)
err_init_me60 <- rep(0, nit)
err_init_max_hoch_60 <- rep(0, nit)
err_init_max_exact_60 <- rep(0, nit)

err_init_oslrt80 <- rep(0, nit)
err_init_moslrt80 <- rep(0, nit)
err_init_rc80 <- rep(0, nit)
err_init_de80 <- rep(0, nit)
err_init_ee80 <- rep(0, nit)
err_init_me80 <- rep(0, nit)
err_init_max_hoch_80 <- rep(0, nit)
err_init_max_exact_80 <- rep(0, nit)

err_init_oslrt100 <- rep(0, nit)
err_init_moslrt100 <- rep(0, nit)
err_init_rc100 <- rep(0, nit)
err_init_de100 <- rep(0, nit)
err_init_ee100 <- rep(0, nit)
err_init_me100 <- rep(0, nit)
err_init_max_hoch_100 <- rep(0, nit)
err_init_max_exact_100 <- rep(0, nit)

err_init_oslrt150 <- rep(0, nit)
err_init_moslrt150 <- rep(0, nit)
err_init_rc150 <- rep(0, nit)
err_init_de150 <- rep(0, nit)
err_init_ee150 <- rep(0, nit)
err_init_me150 <- rep(0, nit)
err_init_max_hoch_150 <- rep(0, nit)
err_init_max_exact_150 <- rep(0, nit)

err_init_oslrt200 <- rep(0, nit)
err_init_moslrt200 <- rep(0, nit)
err_init_rc200 <- rep(0, nit)
err_init_de200 <- rep(0, nit)
err_init_ee200 <- rep(0, nit)
err_init_me200 <- rep(0, nit)
err_init_max_hoch_200 <- rep(0, nit)
err_init_max_exact_200 <- rep(0, nit)

for(i in 1:nit){
  time20 <- rweibull(20, shape1, scale1)
  u20 <- runif(20, 0, ta)
  
  time30 <- rweibull(30, shape1, scale1)
  u30 <- runif(30, 0, ta)
  
  time50 <- rweibull(50, shape1, scale1)
  u50 <- runif(50, 0, ta)
  
  time60 <- rweibull(60, shape1, scale1)
  u60 <- runif(60, 0, ta)
  
  time80 <- rweibull(80, shape1, scale1)
  u80 <- runif(80, 0, ta)
  
  time100 <- rweibull(100, shape1, scale1)
  u100 <- runif(100, 0, ta)
  
  time150 <- rweibull(150, shape1, scale1)
  u150 <- runif(150, 0, ta)
  
  time200 <- rweibull(200, shape1, scale1)
  u200 <- runif(200, 0, ta)
  
  if(lambda_cens==0){  #adm censoring
    del20 <- rep(1, 20)
    del30 <- rep(1, 30)
    del50 <- rep(1, 50)
    del60 <- rep(1, 60)
    del80 <- rep(1, 80)
    del100 <- rep(1, 100)
    del150 <- rep(1, 150)
    del200 <- rep(1, 200)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- stats::model.frame(Surv(t20, delta20)~1)  
    S30 <- stats::model.frame(Surv(t30, delta30)~1) 
    S50 <- stats::model.frame(Surv(t50, delta50)~1)  
    S60 <- stats::model.frame(Surv(t60, delta60)~1)  
    S80 <- stats::model.frame(Surv(t80, delta80)~1)  
    S100 <- stats::model.frame(Surv(t100, delta100)~1)  
    S150 <- stats::model.frame(Surv(t150, delta150)~1)  
    S200 <- stats::model.frame(Surv(t200, delta200)~1)
  }
  else{ #other censoring + adm censoring
    cens20 <- rexp(20, lambda_cens)
    time20 <- ifelse(time20 < cens20, time20, cens20)
    del20 <- ifelse(time20 < cens20, 1, 0)
    
    cens30 <- rexp(30, lambda_cens)
    time30 <- ifelse(time30 < cens30, time30, cens30)
    del30 <- ifelse(time30 < cens30, 1, 0)
    
    cens50 <- rexp(50, lambda_cens)
    time50 <- ifelse(time50 < cens50, time50, cens50)
    del50 <- ifelse(time50 < cens50, 1, 0)
    
    cens60 <- rexp(60, lambda_cens)
    time60 <- ifelse(time60 < cens60, time60, cens60)
    del60 <- ifelse(time60 < cens60, 1, 0)
    
    cens80 <- rexp(80, lambda_cens)
    time80 <- ifelse(time80 < cens80, time80, cens80)
    del80 <- ifelse(time80 < cens80, 1, 0)
    
    cens100 <- rexp(100, lambda_cens)
    time100 <- ifelse(time100 < cens100, time100, cens100)
    del100 <- ifelse(time100 < cens100, 1, 0)
    
    cens150 <- rexp(150, lambda_cens)
    time150 <- ifelse(time150 < cens150, time150, cens150)
    del150 <- ifelse(time150 < cens150, 1, 0)
    
    cens200 <- rexp(200, lambda_cens)
    time200 <- ifelse(time200 < cens200, time200, cens200)
    del200 <- ifelse(time200 < cens200, 1, 0)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- survfit(Surv(t20, delta20) ~ 1)
    S30 <- survfit(Surv(t30, delta30) ~ 1)
    S50 <- survfit(Surv(t50, delta50) ~ 1)
    S60 <- survfit(Surv(t60, delta60) ~ 1)
    S80 <- survfit(Surv(t80, delta80) ~ 1)
    S100 <- survfit(Surv(t100, delta100) ~ 1)
    S150 <- survfit(Surv(t150, delta150) ~ 1)
    S200 <- survfit(Surv(t200, delta200) ~ 1)
  }
  data20 <- ten(S20)
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a20_init <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202_init <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20_init <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20_init <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20_init <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20_init <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a30_init <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302_init <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30_init <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30_init <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30_init <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30_init <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a50_init <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502_init <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50_init <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50_init <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50_init <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50_init <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a60_init <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602_init <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60_init <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60_init <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60_init <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60_init <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a80_init <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802_init <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80_init <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80_init <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80_init <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80_init <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a100_init <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002_init  <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100_init  <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100_init  <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100_init  <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100_init  <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a150_init <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502_init <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150_init <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150_init <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150_init <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150_init <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a200_init <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002_init <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200_init <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200_init <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200_init <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200_init <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)

  err_init_oslrt20[i] <- ifelse((a20_init<0.05), 1, 0)
  err_init_moslrt20[i] <- ifelse((a202_init<0.05), 1, 0)
  err_init_rc20[i] <- ifelse((b20_init<0.05), 1, 0)
  err_init_de20[i] <- ifelse((c20_init<0.05), 1, 0)
  err_init_ee20[i] <- ifelse((d20_init<0.05), 1, 0)
  err_init_me20[i] <- ifelse((e20_init<0.05), 1, 0)
  err_init_max_hoch_20[i] <- ifelse(g20_hoch_init<0.05, 1, 0)
  err_init_max_exact_20[i] <- ifelse(g20_exact_init<0.05, 1, 0)

  err_init_oslrt30[i] <- ifelse((a30_init<0.05), 1, 0)
  err_init_moslrt30[i] <- ifelse((a302_init<0.05), 1, 0)
  err_init_rc30[i] <- ifelse((b30_init<0.05), 1, 0)
  err_init_de30[i] <- ifelse((c30_init<0.05), 1, 0)
  err_init_ee30[i] <- ifelse((d30_init<0.05), 1, 0)
  err_init_me30[i] <- ifelse((e30_init<0.05), 1, 0)
  err_init_max_hoch_30[i] <- ifelse(g30_hoch_init<0.05, 1, 0)
  err_init_max_exact_30[i] <- ifelse(g30_exact_init<0.05, 1, 0)

  err_init_oslrt50[i] <- ifelse((a50_init<0.05), 1, 0)
  err_init_moslrt50[i] <- ifelse((a502_init<0.05), 1, 0)
  err_init_rc50[i] <- ifelse((b50_init<0.05), 1, 0)
  err_init_de50[i] <- ifelse((c50_init<0.05), 1, 0)
  err_init_ee50[i] <- ifelse((d50_init<0.05), 1, 0)
  err_init_me50[i] <- ifelse((e50_init<0.05), 1, 0)
  err_init_max_hoch_50[i] <- ifelse(g50_hoch_init<0.05, 1, 0)
  err_init_max_exact_50[i] <- ifelse(g50_exact_init<0.05, 1, 0)

  err_init_oslrt60[i] <- ifelse((a60_init<0.05), 1, 0)
  err_init_moslrt60[i] <- ifelse((a602_init<0.05), 1, 0)
  err_init_rc60[i] <- ifelse((b60_init<0.05), 1, 0)
  err_init_de60[i] <- ifelse((c60_init<0.05), 1, 0)
  err_init_ee60[i] <- ifelse((d60_init<0.05), 1, 0)
  err_init_me60[i] <- ifelse((e60_init<0.05), 1, 0)
  err_init_max_hoch_60[i] <- ifelse(g60_hoch_init<0.05, 1, 0)
  err_init_max_exact_60[i] <- ifelse(g60_exact_init<0.05, 1, 0)  

  err_init_oslrt80[i] <- ifelse((a80_init<0.05), 1, 0)
  err_init_moslrt80[i] <- ifelse((a802_init<0.05), 1, 0)
  err_init_rc80[i] <- ifelse((b80_init<0.05), 1, 0)
  err_init_de80[i] <- ifelse((c80_init<0.05), 1, 0)
  err_init_ee80[i] <- ifelse((d80_init<0.05), 1, 0)
  err_init_me80[i] <- ifelse((e80_init<0.05), 1, 0)
  err_init_rmst_80[i] <- ifelse((e_init<0.05), 1, 0)
  err_init_max_hoch_80[i] <- ifelse(g80_hoch_init<0.05, 1, 0)
  err_init_max_exact_80[i] <- ifelse(g80_exact_init<0.05, 1, 0)

  err_init_oslrt100[i] <- ifelse((a100_init<0.05), 1, 0)
  err_init_moslrt100[i] <- ifelse((a1002_init<0.05), 1, 0)
  err_init_rc100[i] <- ifelse((b100_init<0.05), 1, 0)
  err_init_de100[i] <- ifelse((c100_init<0.05), 1, 0)
  err_init_ee100[i] <- ifelse((d100_init<0.05), 1, 0)
  err_init_me100[i] <- ifelse((e100_init<0.05), 1, 0)
  err_init_max_hoch_100[i] <- ifelse(g100_hoch_init<0.05, 1, 0)
  err_init_max_exact_100[i] <- ifelse(g100_exact_init<0.05, 1, 0)

  err_init_oslrt150[i] <- ifelse((a150_init<0.05), 1, 0)
  err_init_moslrt150[i] <- ifelse((a1502_init<0.05), 1, 0)
  err_init_rc150[i] <- ifelse((b150_init<0.05), 1, 0)
  err_init_de150[i] <- ifelse((c150_init<0.05), 1, 0)
  err_init_ee150[i] <- ifelse((d150_init<0.05), 1, 0)
  err_init_me150[i] <- ifelse((e150_init<0.05), 1, 0)
  err_init_max_hoch_150[i] <- ifelse(g150_hoch_init<0.05, 1, 0)
  err_init_max_exact_150[i] <- ifelse(g150_exact_init<0.05, 1, 0)

  err_init_oslrt200[i] <- ifelse((a200_init<0.05), 1, 0)
  err_init_moslrt200[i] <- ifelse((a2002_init<0.05), 1, 0)
  err_init_rc200[i] <- ifelse((b200_init<0.05), 1, 0)
  err_init_de200[i] <- ifelse((c200_init<0.05), 1, 0)              
  err_init_ee200[i] <- ifelse((d200_init<0.05), 1, 0)
  err_init_me200[i] <- ifelse((e200_init<0.05), 1, 0)
  err_init_max_hoch_200[i] <- ifelse(g200_hoch_init<0.05, 1, 0)
  err_init_max_exact_200[i] <- ifelse(g200_exact_init<0.05, 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_init <- sum(err_init_oslrt20)/nit
mos20_init <- sum(err_init_moslrt20)/nit
rc20_init <- sum(err_init_rc20)/nit
de20_init <- sum(err_init_de20)/nit
ee20_init <- sum(err_init_ee20)/nit
me20_init <- sum(err_init_me20)/nit
max_hoch20_init <- sum(err_init_max_hoch_20)/nit
max_exact20_init <- sum(err_init_max_exact_20)/nit

diff1_os20 <- ((os20_1-os20_init)/os20_init)*100
diff1_mos20 <- ((mos20_1-mos20_init)/mos20_init)*100
diff1_ee20 <- ((ee20_1-ee20_init)/ee20_init)*100
diff1_me20 <- ((me20_1-me20_init)/me20_init)*100
diff1_de20 <- ((de20_1-de20_init)/de20_init)*100
diff1_rc20 <- ((rc20_1-rc20_init)/rc20_init)*100
diff1_hoch20 <- ((max_hoch20_1-max_hoch20_init)/max_hoch20_init)*100
diff1_exact20 <- ((max_exact20_1-max_exact20_init)/max_exact20_init)*100

diff3_os20 <- ((os20_3-os20_init)/os20_init)*100
diff3_mos20 <- ((mos20_3-mos20_init)/mos20_init)*100
diff3_ee20 <- ((ee20_3-ee20_init)/ee20_init)*100
diff3_me20 <- ((me20_3-me20_init)/me20_init)*100
diff3_de20 <- ((de20_3-de20_init)/de20_init)*100
diff3_rc20 <- ((rc20_3-rc20_init)/rc20_init)*100
diff3_hoch20 <- ((max_hoch20_3-max_hoch20_init)/max_hoch20_init)*100
diff3_exact20 <- ((max_exact20_3-max_exact20_init)/max_exact20_init)*100

os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_init <- sum(err_init_oslrt30)/nit
mos30_init <- sum(err_init_moslrt30)/nit
rc30_init <- sum(err_init_rc30)/nit
de30_init <- sum(err_init_de30)/nit
ee30_init <- sum(err_init_ee30)/nit
me30_init <- sum(err_init_me30)/nit
max_hoch30_init <- sum(err_init_max_hoch_30)/nit
max_exact30_init <- sum(err_init_max_exact_30)/nit

diff1_os30 <- ((os30_1-os30_init)/os30_init)*100
diff1_mos30 <- ((mos30_1-mos30_init)/mos30_init)*100
diff1_ee30 <- ((ee30_1-ee30_init)/ee30_init)*100
diff1_me30 <- ((me30_1-me30_init)/me30_init)*100
diff1_de30 <- ((de30_1-de30_init)/de30_init)*100
diff1_rc30 <- ((rc30_1-rc30_init)/rc30_init)*100
diff1_hoch30 <- ((max_hoch30_1-max_hoch30_init)/max_hoch30_init)*100
diff1_exact30 <- ((max_exact30_1-max_exact30_init)/max_exact30_init)*100

diff3_os30 <- ((os30_3-os30_init)/os30_init)*100
diff3_mos30 <- ((mos30_3-mos30_init)/mos30_init)*100
diff3_ee30 <- ((ee30_3-ee30_init)/ee30_init)*100
diff3_me30 <- ((me30_3-me30_init)/me30_init)*100
diff3_de30 <- ((de30_3-de30_init)/de30_init)*100
diff3_rc30 <- ((rc30_3-rc30_init)/rc30_init)*100
diff3_hoch30 <- ((max_hoch30_3-max_hoch30_init)/max_hoch30_init)*100
diff3_exact30 <- ((max_exact30_3-max_exact30_init)/max_exact30_init)*100


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_init <- sum(err_init_oslrt50)/nit
mos50_init <- sum(err_init_moslrt50)/nit
rc50_init <- sum(err_init_rc50)/nit
de50_init <- sum(err_init_de50)/nit
ee50_init <- sum(err_init_ee50)/nit
me50_init <- sum(err_init_me50)/nit
max_hoch50_init <- sum(err_init_max_hoch_50)/nit
max_exact50_init <- sum(err_init_max_exact_50)/nit

diff1_os50 <- ((os50_1-os50_init)/os50_init)*100
diff1_mos50 <- ((mos50_1-mos50_init)/mos50_init)*100
diff1_ee50 <- ((ee50_1-ee50_init)/ee50_init)*100
diff1_me50 <- ((me50_1-me50_init)/me50_init)*100
diff1_de50 <- ((de50_1-de50_init)/de50_init)*100
diff1_rc50 <- ((rc50_1-rc50_init)/rc50_init)*100
diff1_hoch50 <- ((max_hoch50_1-max_hoch50_init)/max_hoch50_init)*100
diff1_exact50 <- ((max_exact50_1-max_exact50_init)/max_exact50_init)*100

diff3_os50 <- ((os50_3-os50_init)/os50_init)*100
diff3_mos50 <- ((mos50_3-mos50_init)/mos50_init)*100
diff3_ee50 <- ((ee50_3-ee50_init)/ee50_init)*100
diff3_me50 <- ((me50_3-me50_init)/me50_init)*100
diff3_de50 <- ((de50_3-de50_init)/de50_init)*100
diff3_rc50 <- ((rc50_3-rc50_init)/rc50_init)*100
diff3_hoch50 <- ((max_hoch50_3-max_hoch50_init)/max_hoch50_init)*100
diff3_exact50 <- ((max_exact50_3-max_exact50_init)/max_exact50_init)*100


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_init <- sum(err_init_oslrt60)/nit
mos60_init <- sum(err_init_moslrt60)/nit
rc60_init <- sum(err_init_rc60)/nit
de60_init <- sum(err_init_de60)/nit
ee60_init <- sum(err_init_ee60)/nit
me60_init <- sum(err_init_me60)/nit
max_hoch60_init <- sum(err_init_max_hoch_60)/nit
max_exact60_init <- sum(err_init_max_exact_60)/nit

diff1_os60 <- ((os60_1-os60_init)/os60_init)*100
diff1_mos60 <- ((mos60_1-mos60_init)/mos60_init)*100
diff1_ee60 <- ((ee60_1-ee60_init)/ee60_init)*100
diff1_me60 <- ((me60_1-me60_init)/me60_init)*100
diff1_de60 <- ((de60_1-de60_init)/de60_init)*100
diff1_rc60 <- ((rc60_1-rc60_init)/rc60_init)*100
diff1_hoch60 <- ((max_hoch60_1-max_hoch60_init)/max_hoch60_init)*100
diff1_exact60 <- ((max_exact60_1-max_exact60_init)/max_exact60_init)*100

diff3_os60 <- ((os60_3-os60_init)/os60_init)*100
diff3_mos60 <- ((mos60_3-mos60_init)/mos60_init)*100
diff3_ee60 <- ((ee60_3-ee60_init)/ee60_init)*100
diff3_me60 <- ((me60_3-me60_init)/me60_init)*100
diff3_de60 <- ((de60_3-de60_init)/de60_init)*100
diff3_rc60 <- ((rc60_3-rc60_init)/rc60_init)*100
diff3_hoch60 <- ((max_hoch60_3-max_hoch60_init)/max_hoch60_init)*100
diff3_exact60 <- ((max_exact60_3-max_exact60_init)/max_exact60_init)*100


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_init <- sum(err_init_oslrt80)/nit
mos80_init <- sum(err_init_moslrt80)/nit
rc80_init <- sum(err_init_rc80)/nit
de80_init <- sum(err_init_de80)/nit
ee80_init <- sum(err_init_ee80)/nit
me80_init <- sum(err_init_me80)/nit
max_hoch80_init <- sum(err_init_max_hoch_80)/nit
max_exact80_init <- sum(err_init_max_exact_80)/nit

diff1_os80 <- ((os80_1-os80_init)/os80_init)*100
diff1_mos80 <- ((mos80_1-mos80_init)/mos80_init)*100
diff1_ee80 <- ((ee80_1-ee80_init)/ee80_init)*100
diff1_me80 <- ((me80_1-me80_init)/me80_init)*100
diff1_de80 <- ((de80_1-de80_init)/de80_init)*100
diff1_rc80 <- ((rc80_1-rc80_init)/rc80_init)*100
diff1_hoch80 <- ((max_hoch80_1-max_hoch80_init)/max_hoch80_init)*100
diff1_exact80 <- ((max_exact80_1-max_exact80_init)/max_exact80_init)*100

diff3_os80 <- ((os80_3-os80_init)/os80_init)*100
diff3_mos80 <- ((mos80_3-mos80_init)/mos80_init)*100
diff3_ee80 <- ((ee80_3-ee80_init)/ee80_init)*100
diff3_me80 <- ((me80_3-me80_init)/me80_init)*100
diff3_de80 <- ((de80_3-de80_init)/de80_init)*100
diff3_rc80 <- ((rc80_3-rc80_init)/rc80_init)*100
diff3_hoch80 <- ((max_hoch80_3-max_hoch80_init)/max_hoch80_init)*100
diff3_exact80 <- ((max_exact80_3-max_exact80_init)/max_exact80_init)*100


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_init <- sum(err_init_oslrt100)/nit
mos100_init <- sum(err_init_moslrt100)/nit
rc100_init <- sum(err_init_rc100)/nit
de100_init <- sum(err_init_de100)/nit
ee100_init <- sum(err_init_ee100)/nit
me100_init <- sum(err_init_me100)/nit
max_hoch100_init <- sum(err_init_max_hoch_100)/nit
max_exact100_init <- sum(err_init_max_exact_100)/nit

diff1_os100 <- ((os100_1-os100_init)/os100_init)*100
diff1_mos100 <- ((mos100_1-mos100_init)/mos100_init)*100
diff1_ee100 <- ((ee100_1-ee100_init)/ee100_init)*100
diff1_me100 <- ((me100_1-me100_init)/me100_init)*100
diff1_de100 <- ((de100_1-de100_init)/de100_init)*100
diff1_rc100 <- ((rc100_1-rc100_init)/rc100_init)*100
diff1_hoch100 <- ((max_hoch100_1-max_hoch100_init)/max_hoch100_init)*100
diff1_exact100 <- ((max_exact100_1-max_exact100_init)/max_exact100_init)*100

diff3_os100 <- ((os100_3-os100_init)/os100_init)*100
diff3_mos100 <- ((mos100_3-mos100_init)/mos100_init)*100
diff3_ee100 <- ((ee100_3-ee100_init)/ee100_init)*100
diff3_me100 <- ((me100_3-me100_init)/me100_init)*100
diff3_de100 <- ((de100_3-de100_init)/de100_init)*100
diff3_rc100 <- ((rc100_3-rc100_init)/rc100_init)*100
diff3_hoch100 <- ((max_hoch100_3-max_hoch100_init)/max_hoch100_init)*100
diff3_exact100 <- ((max_exact100_3-max_exact100_init)/max_exact100_init)*100


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_init <- sum(err_init_oslrt150)/nit
mos150_init <- sum(err_init_moslrt150)/nit
rc150_init <- sum(err_init_rc150)/nit
de150_init <- sum(err_init_de150)/nit
ee150_init <- sum(err_init_ee150)/nit
me150_init <- sum(err_init_me150)/nit
max_hoch150_init <- sum(err_init_max_hoch_150)/nit
max_exact150_init <- sum(err_init_max_exact_150)/nit

diff1_os150 <- ((os150_1-os150_init)/os150_init)*100
diff1_mos150 <- ((mos150_1-mos150_init)/mos150_init)*100
diff1_ee150 <- ((ee150_1-ee150_init)/ee150_init)*100
diff1_me150 <- ((me150_1-me150_init)/me150_init)*100
diff1_de150 <- ((de150_1-de150_init)/de150_init)*100
diff1_rc150 <- ((rc150_1-rc150_init)/rc150_init)*100
diff1_hoch150 <- ((max_hoch150_1-max_hoch150_init)/max_hoch150_init)*100
diff1_exact150 <- ((max_exact150_1-max_exact150_init)/max_exact150_init)*100

diff3_os150 <- ((os150_3-os150_init)/os150_init)*100
diff3_mos150 <- ((mos150_3-mos150_init)/mos150_init)*100
diff3_ee150 <- ((ee150_3-ee150_init)/ee150_init)*100
diff3_me150 <- ((me150_3-me150_init)/me150_init)*100
diff3_de150 <- ((de150_3-de150_init)/de150_init)*100
diff3_rc150 <- ((rc150_3-rc150_init)/rc150_init)*100
diff3_hoch150 <- ((max_hoch150_3-max_hoch150_init)/max_hoch150_init)*100
diff3_exact150 <- ((max_exact150_3-max_exact150_init)/max_exact150_init)*100


os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_init <- sum(err_init_oslrt200)/nit
mos200_init <- sum(err_init_moslrt200)/nit
rc200_init <- sum(err_init_rc200)/nit
de200_init <- sum(err_init_de200)/nit
ee200_init <- sum(err_init_ee200)/nit
me200_init <- sum(err_init_me200)/nit
r_200_init <- sum(err_init_rmst_200)/nit
max_hoch200_init <- sum(err_init_max_hoch_200)/nit
max_exact200_init <- sum(err_init_max_exact_200)/nit

diff1_os200 <- ((os200_1-os200_init)/os200_init)*100
diff1_mos200 <- ((mos200_1-mos200_init)/mos200_init)*100
diff1_ee200 <- ((ee200_1-ee200_init)/ee200_init)*100
diff1_me200 <- ((me200_1-me200_init)/me200_init)*100
diff1_de200 <- ((de200_1-de200_init)/de200_init)*100
diff1_rc200 <- ((rc200_1-rc200_init)/rc200_init)*100
diff1_hoch200 <- ((max_hoch200_1-max_hoch200_init)/max_hoch200_init)*100
diff1_exact200 <- ((max_exact200_1-max_exact200_init)/max_exact200_init)*100

diff3_os200 <- ((os200_3-os200_init)/os200_init)*100
diff3_mos200 <- ((mos200_3-mos200_init)/mos200_init)*100
diff3_ee200 <- ((ee200_3-ee200_init)/ee200_init)*100
diff3_me200 <- ((me200_3-me200_init)/me200_init)*100
diff3_de200 <- ((de200_3-de200_init)/de200_init)*100
diff3_rc200 <- ((rc200_3-rc200_init)/rc200_init)*100
diff3_hoch200 <- ((max_hoch200_3-max_hoch200_init)/max_hoch200_init)*100
diff3_exact200 <- ((max_exact200_3-max_exact200_init)/max_exact200_init)*100


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
diff1_osl <- c(diff1_os20, diff1_os30, diff1_os50, diff1_os60, diff1_os80, diff1_os100, diff1_os150, diff1_os200)
diff1_mosl <- c(diff1_mos20, diff1_mos30, diff1_mos50, diff1_mos60, diff1_mos80, diff1_mos100, diff1_mos150, diff1_mos200)
diff1_rc <- c(diff1_rc20, diff1_rc30, diff1_rc50, diff1_rc60, diff1_rc80, diff1_rc100, diff1_rc150, diff1_rc200)
diff1_de <- c(diff1_de20, diff1_de30, diff1_de50, diff1_de60, diff1_de80, diff1_de100, diff1_de150, diff1_de200)
diff1_ee <- c(diff1_ee20, diff1_ee30, diff1_ee50, diff1_ee60, diff1_ee80, diff1_ee100, diff1_ee150, diff1_ee200)
diff1_me <- c(diff1_me20, diff1_me30, diff1_me50, diff1_me60, diff1_me80, diff1_me100, diff1_me150, diff1_me200)
diff1_hoch <- c(diff1_hoch20, diff1_hoch30, diff1_hoch50, diff1_hoch60, diff1_hoch80, diff1_hoch100, diff1_hoch150, diff1_hoch200)
diff1_exact <- c(diff1_exact20, diff1_exact30, diff1_exact50, diff1_exact60, diff1_exact80, diff1_exact100, diff1_exact150, diff1_exact200)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
diff3_osl <- c(diff3_os20, diff3_os30, diff3_os50, diff3_os60, diff3_os80, diff3_os100, diff3_os150, diff3_os200)
diff3_mosl <- c(diff3_mos20, diff3_mos30, diff3_mos50, diff3_mos60, diff3_mos80, diff3_mos100, diff3_mos150, diff3_mos200)
diff3_rc <- c(diff3_rc20, diff3_rc30, diff3_rc50, diff3_rc60, diff3_rc80, diff3_rc100, diff3_rc150, diff3_rc200)
diff3_de <- c(diff3_de20, diff3_de30, diff3_de50, diff3_de60, diff3_de80, diff3_de100, diff3_de150, diff3_de200)
diff3_ee <- c(diff3_ee20, diff3_ee30, diff3_ee50, diff3_ee60, diff3_ee80, diff3_ee100, diff3_ee150, diff3_ee200)
diff3_me <- c(diff3_me20, diff3_me30, diff3_me50, diff3_me60, diff3_me80, diff3_me100, diff3_me150, diff3_me200)
diff3_hoch <- c(diff3_hoch20, diff3_hoch30, diff3_hoch50, diff3_hoch60, diff3_hoch80, diff3_hoch100, diff3_hoch150, diff3_hoch200)
diff3_exact <- c(diff3_exact20, diff3_exact30, diff3_exact50, diff3_exact60, diff3_exact80, diff3_exact100, diff3_exact150, diff3_exact200)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

PH_1 <- ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 2: proportional hazards',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
PH_1

d2_1 <- data.frame(Sample.size = n, Error = c(diff1_osl, diff1_mosl, diff1_ee, diff1_me, diff1_de, diff1_rc, diff1_hoch, diff1_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_1$Test <- as.factor(d2_1$Test)
d2_1$Test <- factor(d2_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_PH_1 <- ggplot(d2_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_1$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 2: proportional hazards',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_PH_1


d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

Neff_3 <- ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 2: proportional hazards',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
PH_3

d2_3 <- data.frame(Sample.size = n, Error = c(diff3_osl, diff3_mosl, diff3_ee, diff3_me, diff3_de, diff3_rc, diff3_hoch, diff3_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_3$Test <- as.factor(d2_3$Test)
d2_3$Test <- factor(d2_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_PH_3 <- ggplot(d2_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_3$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 2: proportional hazards',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_PH_3





##########################
#######Early effect#######
##########################
#Parameters for the control group#
shape0 <- 1
m0 <- 2
scale0 <- m0/(-log(0.5))^(1/shape0)
distr0 <- 'Weibull'
#Parameters for the experimental group#
CP <- 1
t <- rpwexp(n = 3000, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
#t <- rpwexp(n = 3000, rate = c(1/scale0*0.7, 1/scale0*1), intervals = CP, cumulative = FALSE)
#t <- rpwexp(n = 3000, rate = c(1/scale0*0.8, 1/scale0*1), intervals = CP, cumulative = FALSE)
delta <- ifelse(t<(2555/365), 1, 0)
t <- ifelse(t>(2555/365), (2555/365), t)

#Survival curves#
t_cont <- rweibull(n = 3000, shape = shape0, scale = scale0)
delta_cont <- ifelse(t_cont<(2555/365), 1, 0)
t_cont <- ifelse(t_cont>(2555/365), (2555/365), t_cont)
C <- survfit(Surv(t_cont, delta_cont)~1)
S2 <- survfit(Surv(t, delta)~1)
plot(C, conf.int = FALSE, xlab = 'Years', ylab = 'Survival', col = 'black', main = "Survival")
lines(S2, conf.int = FALSE, col = 'blue')
legend('right', .9, c("Control", "Experimental"), col = c('black','blue'), lty = c(1,1))


set.seed(5)

nit <- 10000

#Censoring rate lambda_cens (0% 5% 15% 25% 35%)#
#HR1 = 0.5 (0 0.018 0.05 0.095 0.15)
#HR1 = 0.7 (0 0.018 0.055 0.105 0.17)
#HR1 = 0.8 (0 0.018 0.058 0.11 0.175)
lambda_cens <- 0.05   #15% of censoring for HR1 = 0.5

CP_EE <- 1
CP_DE <- 1
CP_ME1 <- 1
CP_ME2 <- 7

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

pi1 <- 1
pi3 <- 0.6

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_init_oslrt20 <- rep(0, nit)
err_init_moslrt20 <- rep(0, nit)
err_init_rc20 <- rep(0, nit)
err_init_de20 <- rep(0, nit)
err_init_ee20 <- rep(0, nit)
err_init_me20 <- rep(0, nit)
err_init_max_hoch_20 <- rep(0, nit)
err_init_max_exact_20 <- rep(0, nit)

err_init_oslrt30 <- rep(0, nit)
err_init_moslrt30 <- rep(0, nit)
err_init_rc30 <- rep(0, nit)
err_init_de30 <- rep(0, nit)
err_init_ee30 <- rep(0, nit)
err_init_me30 <- rep(0, nit)
err_init_max_hoch_30 <- rep(0, nit)
err_init_max_exact_30 <- rep(0, nit)

err_init_oslrt50 <- rep(0, nit)
err_init_moslrt50 <- rep(0, nit)
err_init_rc50 <- rep(0, nit)
err_init_de50 <- rep(0, nit)
err_init_ee50 <- rep(0, nit)
err_init_me50 <- rep(0, nit)
err_init_max_hoch_50 <- rep(0, nit)
err_init_max_exact_50 <- rep(0, nit)

err_init_oslrt60 <- rep(0, nit)
err_init_moslrt60 <- rep(0, nit)
err_init_rc60 <- rep(0, nit)
err_init_de60 <- rep(0, nit)
err_init_ee60 <- rep(0, nit)
err_init_me60 <- rep(0, nit)
err_init_max_hoch_60 <- rep(0, nit)
err_init_max_exact_60 <- rep(0, nit)

err_init_oslrt80 <- rep(0, nit)
err_init_moslrt80 <- rep(0, nit)
err_init_rc80 <- rep(0, nit)
err_init_de80 <- rep(0, nit)
err_init_ee80 <- rep(0, nit)
err_init_me80 <- rep(0, nit)
err_init_max_hoch_80 <- rep(0, nit)
err_init_max_exact_80 <- rep(0, nit)

err_init_oslrt100 <- rep(0, nit)
err_init_moslrt100 <- rep(0, nit)
err_init_rc100 <- rep(0, nit)
err_init_de100 <- rep(0, nit)
err_init_ee100 <- rep(0, nit)
err_init_me100 <- rep(0, nit)
err_init_max_hoch_100 <- rep(0, nit)
err_init_max_exact_100 <- rep(0, nit)

err_init_oslrt150 <- rep(0, nit)
err_init_moslrt150 <- rep(0, nit)
err_init_rc150 <- rep(0, nit)
err_init_de150 <- rep(0, nit)
err_init_ee150 <- rep(0, nit)
err_init_me150 <- rep(0, nit)
err_init_max_hoch_150 <- rep(0, nit)
err_init_max_exact_150 <- rep(0, nit)

err_init_oslrt200 <- rep(0, nit)
err_init_moslrt200 <- rep(0, nit)
err_init_rc200 <- rep(0, nit)
err_init_de200 <- rep(0, nit)
err_init_ee200 <- rep(0, nit)
err_init_me200 <- rep(0, nit)
err_init_max_hoch_200 <- rep(0, nit)
err_init_max_exact_200 <- rep(0, nit)

for(i in 1:nit){
  time20 <- rpwexp(n = 20, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u20 <- runif(20, 0, ta)
  
  time30 <- rpwexp(n = 30, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u30 <- runif(30, 0, ta)
  
  time50 <- rpwexp(n = 50, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u50 <- runif(50, 0, ta)
  
  time60 <- rpwexp(n = 60, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u60 <- runif(60, 0, ta)
  
  time80 <- rpwexp(n = 80, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u80 <- runif(80, 0, ta)
  
  time100 <- rpwexp(n = 100, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u100 <- runif(100, 0, ta)
  
  time150 <- rpwexp(n = 150, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u150 <- runif(150, 0, ta)
  
  time200 <- rpwexp(n = 200, rate = c(1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u200 <- runif(200, 0, ta)
  
  if(lambda_cens==0){  #adm censoring
    del20 <- rep(1, 20)
    del30 <- rep(1, 30)
    del50 <- rep(1, 50)
    del60 <- rep(1, 60)
    del80 <- rep(1, 80)
    del100 <- rep(1, 100)
    del150 <- rep(1, 150)
    del200 <- rep(1, 200)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- stats::model.frame(Surv(t20, delta20)~1)  
    S30 <- stats::model.frame(Surv(t30, delta30)~1) 
    S50 <- stats::model.frame(Surv(t50, delta50)~1)  
    S60 <- stats::model.frame(Surv(t60, delta60)~1)  
    S80 <- stats::model.frame(Surv(t80, delta80)~1)  
    S100 <- stats::model.frame(Surv(t100, delta100)~1)  
    S150 <- stats::model.frame(Surv(t150, delta150)~1)  
    S200 <- stats::model.frame(Surv(t200, delta200)~1)
  }
  else{ #other censoring + adm censoring
    cens20 <- rexp(20, lambda_cens)
    time20 <- ifelse(time20 < cens20, time20, cens20)
    del20 <- ifelse(time20 < cens20, 1, 0)
    
    cens30 <- rexp(30, lambda_cens)
    time30 <- ifelse(time30 < cens30, time30, cens30)
    del30 <- ifelse(time30 < cens30, 1, 0)
    
    cens50 <- rexp(50, lambda_cens)
    time50 <- ifelse(time50 < cens50, time50, cens50)
    del50 <- ifelse(time50 < cens50, 1, 0)
    
    cens60 <- rexp(60, lambda_cens)
    time60 <- ifelse(time60 < cens60, time60, cens60)
    del60 <- ifelse(time60 < cens60, 1, 0)
    
    cens80 <- rexp(80, lambda_cens)
    time80 <- ifelse(time80 < cens80, time80, cens80)
    del80 <- ifelse(time80 < cens80, 1, 0)
    
    cens100 <- rexp(100, lambda_cens)
    time100 <- ifelse(time100 < cens100, time100, cens100)
    del100 <- ifelse(time100 < cens100, 1, 0)
    
    cens150 <- rexp(150, lambda_cens)
    time150 <- ifelse(time150 < cens150, time150, cens150)
    del150 <- ifelse(time150 < cens150, 1, 0)
    
    cens200 <- rexp(200, lambda_cens)
    time200 <- ifelse(time200 < cens200, time200, cens200)
    del200 <- ifelse(time200 < cens200, 1, 0)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- survfit(Surv(t20, delta20) ~ 1)
    S30 <- survfit(Surv(t30, delta30) ~ 1)
    S50 <- survfit(Surv(t50, delta50) ~ 1)
    S60 <- survfit(Surv(t60, delta60) ~ 1)
    S80 <- survfit(Surv(t80, delta80) ~ 1)
    S100 <- survfit(Surv(t100, delta100) ~ 1)
    S150 <- survfit(Surv(t150, delta150) ~ 1)
    S200 <- survfit(Surv(t200, delta200) ~ 1)
  }
  data20 <- ten(S20)
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a20_init <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202_init <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20_init <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20_init <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20_init <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20_init <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a30_init <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302_init <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30_init <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30_init <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30_init <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30_init <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a50_init <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502_init <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50_init <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50_init <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50_init <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50_init <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a60_init <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602_init <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60_init <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60_init <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60_init <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60_init <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a80_init <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802_init <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80_init <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80_init <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80_init <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80_init <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a100_init <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002_init  <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100_init  <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100_init  <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100_init  <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100_init  <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a150_init <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502_init <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150_init <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150_init <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150_init <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150_init <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a200_init <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002_init <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200_init <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200_init <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200_init <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200_init <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)

  err_init_oslrt20[i] <- ifelse((a20_init<0.05), 1, 0)
  err_init_moslrt20[i] <- ifelse((a202_init<0.05), 1, 0)
  err_init_rc20[i] <- ifelse((b20_init<0.05), 1, 0)
  err_init_de20[i] <- ifelse((c20_init<0.05), 1, 0)
  err_init_ee20[i] <- ifelse((d20_init<0.05), 1, 0)
  err_init_me20[i] <- ifelse((e20_init<0.05), 1, 0)
  err_init_max_hoch_20[i] <- ifelse(g20_hoch_init<0.05, 1, 0)
  err_init_max_exact_20[i] <- ifelse(g20_exact_init<0.05, 1, 0)

  err_init_oslrt30[i] <- ifelse((a30_init<0.05), 1, 0)
  err_init_moslrt30[i] <- ifelse((a302_init<0.05), 1, 0)
  err_init_rc30[i] <- ifelse((b30_init<0.05), 1, 0)
  err_init_de30[i] <- ifelse((c30_init<0.05), 1, 0)
  err_init_ee30[i] <- ifelse((d30_init<0.05), 1, 0)
  err_init_me30[i] <- ifelse((e30_init<0.05), 1, 0)
  err_init_max_hoch_30[i] <- ifelse(g30_hoch_init<0.05, 1, 0)
  err_init_max_exact_30[i] <- ifelse(g30_exact_init<0.05, 1, 0)

  err_init_oslrt50[i] <- ifelse((a50_init<0.05), 1, 0)
  err_init_moslrt50[i] <- ifelse((a502_init<0.05), 1, 0)
  err_init_rc50[i] <- ifelse((b50_init<0.05), 1, 0)
  err_init_de50[i] <- ifelse((c50_init<0.05), 1, 0)
  err_init_ee50[i] <- ifelse((d50_init<0.05), 1, 0)
  err_init_me50[i] <- ifelse((e50_init<0.05), 1, 0)
  err_init_max_hoch_50[i] <- ifelse(g50_hoch_init<0.05, 1, 0)
  err_init_max_exact_50[i] <- ifelse(g50_exact_init<0.05, 1, 0)

  err_init_oslrt60[i] <- ifelse((a60_init<0.05), 1, 0)
  err_init_moslrt60[i] <- ifelse((a602_init<0.05), 1, 0)
  err_init_rc60[i] <- ifelse((b60_init<0.05), 1, 0)
  err_init_de60[i] <- ifelse((c60_init<0.05), 1, 0)
  err_init_ee60[i] <- ifelse((d60_init<0.05), 1, 0)
  err_init_me60[i] <- ifelse((e60_init<0.05), 1, 0)
  err_init_max_hoch_60[i] <- ifelse(g60_hoch_init<0.05, 1, 0)
  err_init_max_exact_60[i] <- ifelse(g60_exact_init<0.05, 1, 0)  

  err_init_oslrt80[i] <- ifelse((a80_init<0.05), 1, 0)
  err_init_moslrt80[i] <- ifelse((a802_init<0.05), 1, 0)
  err_init_rc80[i] <- ifelse((b80_init<0.05), 1, 0)
  err_init_de80[i] <- ifelse((c80_init<0.05), 1, 0)
  err_init_ee80[i] <- ifelse((d80_init<0.05), 1, 0)
  err_init_me80[i] <- ifelse((e80_init<0.05), 1, 0)
  err_init_rmst_80[i] <- ifelse((e_init<0.05), 1, 0)
  err_init_max_hoch_80[i] <- ifelse(g80_hoch_init<0.05, 1, 0)
  err_init_max_exact_80[i] <- ifelse(g80_exact_init<0.05, 1, 0)

  err_init_oslrt100[i] <- ifelse((a100_init<0.05), 1, 0)
  err_init_moslrt100[i] <- ifelse((a1002_init<0.05), 1, 0)
  err_init_rc100[i] <- ifelse((b100_init<0.05), 1, 0)
  err_init_de100[i] <- ifelse((c100_init<0.05), 1, 0)
  err_init_ee100[i] <- ifelse((d100_init<0.05), 1, 0)
  err_init_me100[i] <- ifelse((e100_init<0.05), 1, 0)
  err_init_max_hoch_100[i] <- ifelse(g100_hoch_init<0.05, 1, 0)
  err_init_max_exact_100[i] <- ifelse(g100_exact_init<0.05, 1, 0)

  err_init_oslrt150[i] <- ifelse((a150_init<0.05), 1, 0)
  err_init_moslrt150[i] <- ifelse((a1502_init<0.05), 1, 0)
  err_init_rc150[i] <- ifelse((b150_init<0.05), 1, 0)
  err_init_de150[i] <- ifelse((c150_init<0.05), 1, 0)
  err_init_ee150[i] <- ifelse((d150_init<0.05), 1, 0)
  err_init_me150[i] <- ifelse((e150_init<0.05), 1, 0)
  err_init_max_hoch_150[i] <- ifelse(g150_hoch_init<0.05, 1, 0)
  err_init_max_exact_150[i] <- ifelse(g150_exact_init<0.05, 1, 0)

  err_init_oslrt200[i] <- ifelse((a200_init<0.05), 1, 0)
  err_init_moslrt200[i] <- ifelse((a2002_init<0.05), 1, 0)
  err_init_rc200[i] <- ifelse((b200_init<0.05), 1, 0)
  err_init_de200[i] <- ifelse((c200_init<0.05), 1, 0)              
  err_init_ee200[i] <- ifelse((d200_init<0.05), 1, 0)
  err_init_me200[i] <- ifelse((e200_init<0.05), 1, 0)
  err_init_max_hoch_200[i] <- ifelse(g200_hoch_init<0.05, 1, 0)
  err_init_max_exact_200[i] <- ifelse(g200_exact_init<0.05, 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_init <- sum(err_init_oslrt20)/nit
mos20_init <- sum(err_init_moslrt20)/nit
rc20_init <- sum(err_init_rc20)/nit
de20_init <- sum(err_init_de20)/nit
ee20_init <- sum(err_init_ee20)/nit
me20_init <- sum(err_init_me20)/nit
max_hoch20_init <- sum(err_init_max_hoch_20)/nit
max_exact20_init <- sum(err_init_max_exact_20)/nit

diff1_os20 <- ((os20_1-os20_init)/os20_init)*100
diff1_mos20 <- ((mos20_1-mos20_init)/mos20_init)*100
diff1_ee20 <- ((ee20_1-ee20_init)/ee20_init)*100
diff1_me20 <- ((me20_1-me20_init)/me20_init)*100
diff1_de20 <- ((de20_1-de20_init)/de20_init)*100
diff1_rc20 <- ((rc20_1-rc20_init)/rc20_init)*100
diff1_hoch20 <- ((max_hoch20_1-max_hoch20_init)/max_hoch20_init)*100
diff1_exact20 <- ((max_exact20_1-max_exact20_init)/max_exact20_init)*100

diff3_os20 <- ((os20_3-os20_init)/os20_init)*100
diff3_mos20 <- ((mos20_3-mos20_init)/mos20_init)*100
diff3_ee20 <- ((ee20_3-ee20_init)/ee20_init)*100
diff3_me20 <- ((me20_3-me20_init)/me20_init)*100
diff3_de20 <- ((de20_3-de20_init)/de20_init)*100
diff3_rc20 <- ((rc20_3-rc20_init)/rc20_init)*100
diff3_hoch20 <- ((max_hoch20_3-max_hoch20_init)/max_hoch20_init)*100
diff3_exact20 <- ((max_exact20_3-max_exact20_init)/max_exact20_init)*100

os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_init <- sum(err_init_oslrt30)/nit
mos30_init <- sum(err_init_moslrt30)/nit
rc30_init <- sum(err_init_rc30)/nit
de30_init <- sum(err_init_de30)/nit
ee30_init <- sum(err_init_ee30)/nit
me30_init <- sum(err_init_me30)/nit
max_hoch30_init <- sum(err_init_max_hoch_30)/nit
max_exact30_init <- sum(err_init_max_exact_30)/nit

diff1_os30 <- ((os30_1-os30_init)/os30_init)*100
diff1_mos30 <- ((mos30_1-mos30_init)/mos30_init)*100
diff1_ee30 <- ((ee30_1-ee30_init)/ee30_init)*100
diff1_me30 <- ((me30_1-me30_init)/me30_init)*100
diff1_de30 <- ((de30_1-de30_init)/de30_init)*100
diff1_rc30 <- ((rc30_1-rc30_init)/rc30_init)*100
diff1_hoch30 <- ((max_hoch30_1-max_hoch30_init)/max_hoch30_init)*100
diff1_exact30 <- ((max_exact30_1-max_exact30_init)/max_exact30_init)*100

diff3_os30 <- ((os30_3-os30_init)/os30_init)*100
diff3_mos30 <- ((mos30_3-mos30_init)/mos30_init)*100
diff3_ee30 <- ((ee30_3-ee30_init)/ee30_init)*100
diff3_me30 <- ((me30_3-me30_init)/me30_init)*100
diff3_de30 <- ((de30_3-de30_init)/de30_init)*100
diff3_rc30 <- ((rc30_3-rc30_init)/rc30_init)*100
diff3_hoch30 <- ((max_hoch30_3-max_hoch30_init)/max_hoch30_init)*100
diff3_exact30 <- ((max_exact30_3-max_exact30_init)/max_exact30_init)*100


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_init <- sum(err_init_oslrt50)/nit
mos50_init <- sum(err_init_moslrt50)/nit
rc50_init <- sum(err_init_rc50)/nit
de50_init <- sum(err_init_de50)/nit
ee50_init <- sum(err_init_ee50)/nit
me50_init <- sum(err_init_me50)/nit
max_hoch50_init <- sum(err_init_max_hoch_50)/nit
max_exact50_init <- sum(err_init_max_exact_50)/nit

diff1_os50 <- ((os50_1-os50_init)/os50_init)*100
diff1_mos50 <- ((mos50_1-mos50_init)/mos50_init)*100
diff1_ee50 <- ((ee50_1-ee50_init)/ee50_init)*100
diff1_me50 <- ((me50_1-me50_init)/me50_init)*100
diff1_de50 <- ((de50_1-de50_init)/de50_init)*100
diff1_rc50 <- ((rc50_1-rc50_init)/rc50_init)*100
diff1_hoch50 <- ((max_hoch50_1-max_hoch50_init)/max_hoch50_init)*100
diff1_exact50 <- ((max_exact50_1-max_exact50_init)/max_exact50_init)*100

diff3_os50 <- ((os50_3-os50_init)/os50_init)*100
diff3_mos50 <- ((mos50_3-mos50_init)/mos50_init)*100
diff3_ee50 <- ((ee50_3-ee50_init)/ee50_init)*100
diff3_me50 <- ((me50_3-me50_init)/me50_init)*100
diff3_de50 <- ((de50_3-de50_init)/de50_init)*100
diff3_rc50 <- ((rc50_3-rc50_init)/rc50_init)*100
diff3_hoch50 <- ((max_hoch50_3-max_hoch50_init)/max_hoch50_init)*100
diff3_exact50 <- ((max_exact50_3-max_exact50_init)/max_exact50_init)*100


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_init <- sum(err_init_oslrt60)/nit
mos60_init <- sum(err_init_moslrt60)/nit
rc60_init <- sum(err_init_rc60)/nit
de60_init <- sum(err_init_de60)/nit
ee60_init <- sum(err_init_ee60)/nit
me60_init <- sum(err_init_me60)/nit
max_hoch60_init <- sum(err_init_max_hoch_60)/nit
max_exact60_init <- sum(err_init_max_exact_60)/nit

diff1_os60 <- ((os60_1-os60_init)/os60_init)*100
diff1_mos60 <- ((mos60_1-mos60_init)/mos60_init)*100
diff1_ee60 <- ((ee60_1-ee60_init)/ee60_init)*100
diff1_me60 <- ((me60_1-me60_init)/me60_init)*100
diff1_de60 <- ((de60_1-de60_init)/de60_init)*100
diff1_rc60 <- ((rc60_1-rc60_init)/rc60_init)*100
diff1_hoch60 <- ((max_hoch60_1-max_hoch60_init)/max_hoch60_init)*100
diff1_exact60 <- ((max_exact60_1-max_exact60_init)/max_exact60_init)*100

diff3_os60 <- ((os60_3-os60_init)/os60_init)*100
diff3_mos60 <- ((mos60_3-mos60_init)/mos60_init)*100
diff3_ee60 <- ((ee60_3-ee60_init)/ee60_init)*100
diff3_me60 <- ((me60_3-me60_init)/me60_init)*100
diff3_de60 <- ((de60_3-de60_init)/de60_init)*100
diff3_rc60 <- ((rc60_3-rc60_init)/rc60_init)*100
diff3_hoch60 <- ((max_hoch60_3-max_hoch60_init)/max_hoch60_init)*100
diff3_exact60 <- ((max_exact60_3-max_exact60_init)/max_exact60_init)*100


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_init <- sum(err_init_oslrt80)/nit
mos80_init <- sum(err_init_moslrt80)/nit
rc80_init <- sum(err_init_rc80)/nit
de80_init <- sum(err_init_de80)/nit
ee80_init <- sum(err_init_ee80)/nit
me80_init <- sum(err_init_me80)/nit
max_hoch80_init <- sum(err_init_max_hoch_80)/nit
max_exact80_init <- sum(err_init_max_exact_80)/nit

diff1_os80 <- ((os80_1-os80_init)/os80_init)*100
diff1_mos80 <- ((mos80_1-mos80_init)/mos80_init)*100
diff1_ee80 <- ((ee80_1-ee80_init)/ee80_init)*100
diff1_me80 <- ((me80_1-me80_init)/me80_init)*100
diff1_de80 <- ((de80_1-de80_init)/de80_init)*100
diff1_rc80 <- ((rc80_1-rc80_init)/rc80_init)*100
diff1_hoch80 <- ((max_hoch80_1-max_hoch80_init)/max_hoch80_init)*100
diff1_exact80 <- ((max_exact80_1-max_exact80_init)/max_exact80_init)*100

diff3_os80 <- ((os80_3-os80_init)/os80_init)*100
diff3_mos80 <- ((mos80_3-mos80_init)/mos80_init)*100
diff3_ee80 <- ((ee80_3-ee80_init)/ee80_init)*100
diff3_me80 <- ((me80_3-me80_init)/me80_init)*100
diff3_de80 <- ((de80_3-de80_init)/de80_init)*100
diff3_rc80 <- ((rc80_3-rc80_init)/rc80_init)*100
diff3_hoch80 <- ((max_hoch80_3-max_hoch80_init)/max_hoch80_init)*100
diff3_exact80 <- ((max_exact80_3-max_exact80_init)/max_exact80_init)*100


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_init <- sum(err_init_oslrt100)/nit
mos100_init <- sum(err_init_moslrt100)/nit
rc100_init <- sum(err_init_rc100)/nit
de100_init <- sum(err_init_de100)/nit
ee100_init <- sum(err_init_ee100)/nit
me100_init <- sum(err_init_me100)/nit
max_hoch100_init <- sum(err_init_max_hoch_100)/nit
max_exact100_init <- sum(err_init_max_exact_100)/nit

diff1_os100 <- ((os100_1-os100_init)/os100_init)*100
diff1_mos100 <- ((mos100_1-mos100_init)/mos100_init)*100
diff1_ee100 <- ((ee100_1-ee100_init)/ee100_init)*100
diff1_me100 <- ((me100_1-me100_init)/me100_init)*100
diff1_de100 <- ((de100_1-de100_init)/de100_init)*100
diff1_rc100 <- ((rc100_1-rc100_init)/rc100_init)*100
diff1_hoch100 <- ((max_hoch100_1-max_hoch100_init)/max_hoch100_init)*100
diff1_exact100 <- ((max_exact100_1-max_exact100_init)/max_exact100_init)*100

diff3_os100 <- ((os100_3-os100_init)/os100_init)*100
diff3_mos100 <- ((mos100_3-mos100_init)/mos100_init)*100
diff3_ee100 <- ((ee100_3-ee100_init)/ee100_init)*100
diff3_me100 <- ((me100_3-me100_init)/me100_init)*100
diff3_de100 <- ((de100_3-de100_init)/de100_init)*100
diff3_rc100 <- ((rc100_3-rc100_init)/rc100_init)*100
diff3_hoch100 <- ((max_hoch100_3-max_hoch100_init)/max_hoch100_init)*100
diff3_exact100 <- ((max_exact100_3-max_exact100_init)/max_exact100_init)*100


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_init <- sum(err_init_oslrt150)/nit
mos150_init <- sum(err_init_moslrt150)/nit
rc150_init <- sum(err_init_rc150)/nit
de150_init <- sum(err_init_de150)/nit
ee150_init <- sum(err_init_ee150)/nit
me150_init <- sum(err_init_me150)/nit
max_hoch150_init <- sum(err_init_max_hoch_150)/nit
max_exact150_init <- sum(err_init_max_exact_150)/nit

diff1_os150 <- ((os150_1-os150_init)/os150_init)*100
diff1_mos150 <- ((mos150_1-mos150_init)/mos150_init)*100
diff1_ee150 <- ((ee150_1-ee150_init)/ee150_init)*100
diff1_me150 <- ((me150_1-me150_init)/me150_init)*100
diff1_de150 <- ((de150_1-de150_init)/de150_init)*100
diff1_rc150 <- ((rc150_1-rc150_init)/rc150_init)*100
diff1_hoch150 <- ((max_hoch150_1-max_hoch150_init)/max_hoch150_init)*100
diff1_exact150 <- ((max_exact150_1-max_exact150_init)/max_exact150_init)*100

diff3_os150 <- ((os150_3-os150_init)/os150_init)*100
diff3_mos150 <- ((mos150_3-mos150_init)/mos150_init)*100
diff3_ee150 <- ((ee150_3-ee150_init)/ee150_init)*100
diff3_me150 <- ((me150_3-me150_init)/me150_init)*100
diff3_de150 <- ((de150_3-de150_init)/de150_init)*100
diff3_rc150 <- ((rc150_3-rc150_init)/rc150_init)*100
diff3_hoch150 <- ((max_hoch150_3-max_hoch150_init)/max_hoch150_init)*100
diff3_exact150 <- ((max_exact150_3-max_exact150_init)/max_exact150_init)*100


os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_init <- sum(err_init_oslrt200)/nit
mos200_init <- sum(err_init_moslrt200)/nit
rc200_init <- sum(err_init_rc200)/nit
de200_init <- sum(err_init_de200)/nit
ee200_init <- sum(err_init_ee200)/nit
me200_init <- sum(err_init_me200)/nit
r_200_init <- sum(err_init_rmst_200)/nit
max_hoch200_init <- sum(err_init_max_hoch_200)/nit
max_exact200_init <- sum(err_init_max_exact_200)/nit

diff1_os200 <- ((os200_1-os200_init)/os200_init)*100
diff1_mos200 <- ((mos200_1-mos200_init)/mos200_init)*100
diff1_ee200 <- ((ee200_1-ee200_init)/ee200_init)*100
diff1_me200 <- ((me200_1-me200_init)/me200_init)*100
diff1_de200 <- ((de200_1-de200_init)/de200_init)*100
diff1_rc200 <- ((rc200_1-rc200_init)/rc200_init)*100
diff1_hoch200 <- ((max_hoch200_1-max_hoch200_init)/max_hoch200_init)*100
diff1_exact200 <- ((max_exact200_1-max_exact200_init)/max_exact200_init)*100

diff3_os200 <- ((os200_3-os200_init)/os200_init)*100
diff3_mos200 <- ((mos200_3-mos200_init)/mos200_init)*100
diff3_ee200 <- ((ee200_3-ee200_init)/ee200_init)*100
diff3_me200 <- ((me200_3-me200_init)/me200_init)*100
diff3_de200 <- ((de200_3-de200_init)/de200_init)*100
diff3_rc200 <- ((rc200_3-rc200_init)/rc200_init)*100
diff3_hoch200 <- ((max_hoch200_3-max_hoch200_init)/max_hoch200_init)*100
diff3_exact200 <- ((max_exact200_3-max_exact200_init)/max_exact200_init)*100


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
diff1_osl <- c(diff1_os20, diff1_os30, diff1_os50, diff1_os60, diff1_os80, diff1_os100, diff1_os150, diff1_os200)
diff1_mosl <- c(diff1_mos20, diff1_mos30, diff1_mos50, diff1_mos60, diff1_mos80, diff1_mos100, diff1_mos150, diff1_mos200)
diff1_rc <- c(diff1_rc20, diff1_rc30, diff1_rc50, diff1_rc60, diff1_rc80, diff1_rc100, diff1_rc150, diff1_rc200)
diff1_de <- c(diff1_de20, diff1_de30, diff1_de50, diff1_de60, diff1_de80, diff1_de100, diff1_de150, diff1_de200)
diff1_ee <- c(diff1_ee20, diff1_ee30, diff1_ee50, diff1_ee60, diff1_ee80, diff1_ee100, diff1_ee150, diff1_ee200)
diff1_me <- c(diff1_me20, diff1_me30, diff1_me50, diff1_me60, diff1_me80, diff1_me100, diff1_me150, diff1_me200)
diff1_hoch <- c(diff1_hoch20, diff1_hoch30, diff1_hoch50, diff1_hoch60, diff1_hoch80, diff1_hoch100, diff1_hoch150, diff1_hoch200)
diff1_exact <- c(diff1_exact20, diff1_exact30, diff1_exact50, diff1_exact60, diff1_exact80, diff1_exact100, diff1_exact150, diff1_exact200)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
diff3_osl <- c(diff3_os20, diff3_os30, diff3_os50, diff3_os60, diff3_os80, diff3_os100, diff3_os150, diff3_os200)
diff3_mosl <- c(diff3_mos20, diff3_mos30, diff3_mos50, diff3_mos60, diff3_mos80, diff3_mos100, diff3_mos150, diff3_mos200)
diff3_rc <- c(diff3_rc20, diff3_rc30, diff3_rc50, diff3_rc60, diff3_rc80, diff3_rc100, diff3_rc150, diff3_rc200)
diff3_de <- c(diff3_de20, diff3_de30, diff3_de50, diff3_de60, diff3_de80, diff3_de100, diff3_de150, diff3_de200)
diff3_ee <- c(diff3_ee20, diff3_ee30, diff3_ee50, diff3_ee60, diff3_ee80, diff3_ee100, diff3_ee150, diff3_ee200)
diff3_me <- c(diff3_me20, diff3_me30, diff3_me50, diff3_me60, diff3_me80, diff3_me100, diff3_me150, diff3_me200)
diff3_hoch <- c(diff3_hoch20, diff3_hoch30, diff3_hoch50, diff3_hoch60, diff3_hoch80, diff3_hoch100, diff3_hoch150, diff3_hoch200)
diff3_exact <- c(diff3_exact20, diff3_exact30, diff3_exact50, diff3_exact60, diff3_exact80, diff3_exact100, diff3_exact150, diff3_exact200)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

EE_1 <- ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 3: early effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
EE_1

d2_1 <- data.frame(Sample.size = n, Error = c(diff1_osl, diff1_mosl, diff1_ee, diff1_me, diff1_de, diff1_rc, diff1_hoch, diff1_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_1$Test <- as.factor(d2_1$Test)
d2_1$Test <- factor(d2_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_EE_1 <- ggplot(d2_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_1$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 3: early effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_EE_1


d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

EE_3 <- ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 3: early effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
EE_3

d2_3 <- data.frame(Sample.size = n, Error = c(diff3_osl, diff3_mosl, diff3_ee, diff3_me, diff3_de, diff3_rc, diff3_hoch, diff3_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_3$Test <- as.factor(d2_3$Test)
d2_3$Test <- factor(d2_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_EE_3 <- ggplot(d2_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_3$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 3: early effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_EE_3



##########################
#######Middle effect######
##########################
#Parameters for the control group#
shape0 <- 1
m0 <- 2
scale0 <- m0/(-log(0.5))^(1/shape0)
distr0 <- 'Weibull'
#Parameters for the experimental group#
CP1 <- 1
CP2 <- 3
CP <- c(CP1, CP2)
t <- rpwexp(n = 3000, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
# t <- rpwexp(n = 3000, rate = c(1/scale0*1, 1/scale0*0.7, 1/scale0*1), intervals = CP, cumulative = FALSE)
# t <- rpwexp(n = 3000, rate = c(1/scale0*1, 1/scale0*0.8, 1/scale0*1), intervals = CP, cumulative = FALSE)
delta <- ifelse(t<(2555/365), 1, 0)
t <- ifelse(t>(2555/365), (2555/365), t)

#Survival curves#
t_cont <- rweibull(n = 3000, shape = shape0, scale = scale0)
delta_cont <- ifelse(t_cont<(2555/365), 1, 0)
t_cont <- ifelse(t_cont>(2555/365), (2555/365), t_cont)
C <- survfit(Surv(t_cont, delta_cont)~1)
S2 <- survfit(Surv(t, delta)~1)
plot(C, conf.int = FALSE, xlab = 'Years', ylab = 'Survival', col = 'black', main = "Survival")
lines(S2, conf.int = FALSE, col = 'blue')
legend('right', .9, c("Control", "Experimental"), col = c('black','blue'), lty = c(1,1))


set.seed(5)

nit <- 10000

#Censoring rate lambda_cens (0% 5% 15% 25% 35%)#
#HR2 = 0.5 (0 0.015 0.05 0.09 0.14)
#HR2 = 0.7 (0 0.015 0.05 0.1 0.16)
#HR2 = 0.8 (0 0.015 0.055 0.1 0.165)

lambda_cens <- 0.05   #15% of censoring for HR1 = 0.5

CP_EE <- 4
CP_DE <- 1
CP_ME1 <- 1
CP_ME2 <- 4

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

pi1 <- 1
pi3 <- 0.6

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_init_oslrt20 <- rep(0, nit)
err_init_moslrt20 <- rep(0, nit)
err_init_rc20 <- rep(0, nit)
err_init_de20 <- rep(0, nit)
err_init_ee20 <- rep(0, nit)
err_init_me20 <- rep(0, nit)
err_init_max_hoch_20 <- rep(0, nit)
err_init_max_exact_20 <- rep(0, nit)

err_init_oslrt30 <- rep(0, nit)
err_init_moslrt30 <- rep(0, nit)
err_init_rc30 <- rep(0, nit)
err_init_de30 <- rep(0, nit)
err_init_ee30 <- rep(0, nit)
err_init_me30 <- rep(0, nit)
err_init_max_hoch_30 <- rep(0, nit)
err_init_max_exact_30 <- rep(0, nit)

err_init_oslrt50 <- rep(0, nit)
err_init_moslrt50 <- rep(0, nit)
err_init_rc50 <- rep(0, nit)
err_init_de50 <- rep(0, nit)
err_init_ee50 <- rep(0, nit)
err_init_me50 <- rep(0, nit)
err_init_max_hoch_50 <- rep(0, nit)
err_init_max_exact_50 <- rep(0, nit)

err_init_oslrt60 <- rep(0, nit)
err_init_moslrt60 <- rep(0, nit)
err_init_rc60 <- rep(0, nit)
err_init_de60 <- rep(0, nit)
err_init_ee60 <- rep(0, nit)
err_init_me60 <- rep(0, nit)
err_init_max_hoch_60 <- rep(0, nit)
err_init_max_exact_60 <- rep(0, nit)

err_init_oslrt80 <- rep(0, nit)
err_init_moslrt80 <- rep(0, nit)
err_init_rc80 <- rep(0, nit)
err_init_de80 <- rep(0, nit)
err_init_ee80 <- rep(0, nit)
err_init_me80 <- rep(0, nit)
err_init_max_hoch_80 <- rep(0, nit)
err_init_max_exact_80 <- rep(0, nit)

err_init_oslrt100 <- rep(0, nit)
err_init_moslrt100 <- rep(0, nit)
err_init_rc100 <- rep(0, nit)
err_init_de100 <- rep(0, nit)
err_init_ee100 <- rep(0, nit)
err_init_me100 <- rep(0, nit)
err_init_max_hoch_100 <- rep(0, nit)
err_init_max_exact_100 <- rep(0, nit)

err_init_oslrt150 <- rep(0, nit)
err_init_moslrt150 <- rep(0, nit)
err_init_rc150 <- rep(0, nit)
err_init_de150 <- rep(0, nit)
err_init_ee150 <- rep(0, nit)
err_init_me150 <- rep(0, nit)
err_init_max_hoch_150 <- rep(0, nit)
err_init_max_exact_150 <- rep(0, nit)

err_init_oslrt200 <- rep(0, nit)
err_init_moslrt200 <- rep(0, nit)
err_init_rc200 <- rep(0, nit)
err_init_de200 <- rep(0, nit)
err_init_ee200 <- rep(0, nit)
err_init_me200 <- rep(0, nit)
err_init_max_hoch_200 <- rep(0, nit)
err_init_max_exact_200 <- rep(0, nit)

for(i in 1:nit){
  time20 <- rpwexp(n = 20, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u20 <- runif(20, 0, ta)
  
  time30 <- rpwexp(n = 30, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u30 <- runif(30, 0, ta)
  
  time50 <- rpwexp(n = 50, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u50 <- runif(50, 0, ta)
  
  time60 <- rpwexp(n = 60, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u60 <- runif(60, 0, ta)
  
  time80 <- rpwexp(n = 80, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u80 <- runif(80, 0, ta)
  
  time100 <- rpwexp(n = 100, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u100 <- runif(100, 0, ta)
  
  time150 <- rpwexp(n = 150, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u150 <- runif(150, 0, ta)
  
  time200 <- rpwexp(n = 200, rate = c(1/scale0*1, 1/scale0*0.5, 1/scale0*1), intervals = CP, cumulative = FALSE)
  u200 <- runif(200, 0, ta)
  
  if(lambda_cens==0){  #adm censoring
    del20 <- rep(1, 20)
    del30 <- rep(1, 30)
    del50 <- rep(1, 50)
    del60 <- rep(1, 60)
    del80 <- rep(1, 80)
    del100 <- rep(1, 100)
    del150 <- rep(1, 150)
    del200 <- rep(1, 200)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- stats::model.frame(Surv(t20, delta20)~1)  
    S30 <- stats::model.frame(Surv(t30, delta30)~1) 
    S50 <- stats::model.frame(Surv(t50, delta50)~1)  
    S60 <- stats::model.frame(Surv(t60, delta60)~1)  
    S80 <- stats::model.frame(Surv(t80, delta80)~1)  
    S100 <- stats::model.frame(Surv(t100, delta100)~1)  
    S150 <- stats::model.frame(Surv(t150, delta150)~1)  
    S200 <- stats::model.frame(Surv(t200, delta200)~1)
  }
  else{ #other censoring + adm censoring
    cens20 <- rexp(20, lambda_cens)
    time20 <- ifelse(time20 < cens20, time20, cens20)
    del20 <- ifelse(time20 < cens20, 1, 0)
    
    cens30 <- rexp(30, lambda_cens)
    time30 <- ifelse(time30 < cens30, time30, cens30)
    del30 <- ifelse(time30 < cens30, 1, 0)
    
    cens50 <- rexp(50, lambda_cens)
    time50 <- ifelse(time50 < cens50, time50, cens50)
    del50 <- ifelse(time50 < cens50, 1, 0)
    
    cens60 <- rexp(60, lambda_cens)
    time60 <- ifelse(time60 < cens60, time60, cens60)
    del60 <- ifelse(time60 < cens60, 1, 0)
    
    cens80 <- rexp(80, lambda_cens)
    time80 <- ifelse(time80 < cens80, time80, cens80)
    del80 <- ifelse(time80 < cens80, 1, 0)
    
    cens100 <- rexp(100, lambda_cens)
    time100 <- ifelse(time100 < cens100, time100, cens100)
    del100 <- ifelse(time100 < cens100, 1, 0)
    
    cens150 <- rexp(150, lambda_cens)
    time150 <- ifelse(time150 < cens150, time150, cens150)
    del150 <- ifelse(time150 < cens150, 1, 0)
    
    cens200 <- rexp(200, lambda_cens)
    time200 <- ifelse(time200 < cens200, time200, cens200)
    del200 <- ifelse(time200 < cens200, 1, 0)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- survfit(Surv(t20, delta20) ~ 1)
    S30 <- survfit(Surv(t30, delta30) ~ 1)
    S50 <- survfit(Surv(t50, delta50) ~ 1)
    S60 <- survfit(Surv(t60, delta60) ~ 1)
    S80 <- survfit(Surv(t80, delta80) ~ 1)
    S100 <- survfit(Surv(t100, delta100) ~ 1)
    S150 <- survfit(Surv(t150, delta150) ~ 1)
    S200 <- survfit(Surv(t200, delta200) ~ 1)
  }
  data20 <- ten(S20)
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a20_init <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202_init <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20_init <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20_init <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20_init <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20_init <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a30_init <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302_init <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30_init <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30_init <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30_init <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30_init <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a50_init <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502_init <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50_init <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50_init <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50_init <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50_init <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a60_init <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602_init <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60_init <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60_init <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60_init <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60_init <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a80_init <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802_init <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80_init <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80_init <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80_init <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80_init <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a100_init <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002_init  <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100_init  <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100_init  <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100_init  <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100_init  <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a150_init <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502_init <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150_init <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150_init <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150_init <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150_init <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a200_init <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002_init <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200_init <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200_init <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200_init <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200_init <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)

  err_init_oslrt20[i] <- ifelse((a20_init<0.05), 1, 0)
  err_init_moslrt20[i] <- ifelse((a202_init<0.05), 1, 0)
  err_init_rc20[i] <- ifelse((b20_init<0.05), 1, 0)
  err_init_de20[i] <- ifelse((c20_init<0.05), 1, 0)
  err_init_ee20[i] <- ifelse((d20_init<0.05), 1, 0)
  err_init_me20[i] <- ifelse((e20_init<0.05), 1, 0)
  err_init_max_hoch_20[i] <- ifelse(g20_hoch_init<0.05, 1, 0)
  err_init_max_exact_20[i] <- ifelse(g20_exact_init<0.05, 1, 0)

  err_init_oslrt30[i] <- ifelse((a30_init<0.05), 1, 0)
  err_init_moslrt30[i] <- ifelse((a302_init<0.05), 1, 0)
  err_init_rc30[i] <- ifelse((b30_init<0.05), 1, 0)
  err_init_de30[i] <- ifelse((c30_init<0.05), 1, 0)
  err_init_ee30[i] <- ifelse((d30_init<0.05), 1, 0)
  err_init_me30[i] <- ifelse((e30_init<0.05), 1, 0)
  err_init_max_hoch_30[i] <- ifelse(g30_hoch_init<0.05, 1, 0)
  err_init_max_exact_30[i] <- ifelse(g30_exact_init<0.05, 1, 0)

  err_init_oslrt50[i] <- ifelse((a50_init<0.05), 1, 0)
  err_init_moslrt50[i] <- ifelse((a502_init<0.05), 1, 0)
  err_init_rc50[i] <- ifelse((b50_init<0.05), 1, 0)
  err_init_de50[i] <- ifelse((c50_init<0.05), 1, 0)
  err_init_ee50[i] <- ifelse((d50_init<0.05), 1, 0)
  err_init_me50[i] <- ifelse((e50_init<0.05), 1, 0)
  err_init_max_hoch_50[i] <- ifelse(g50_hoch_init<0.05, 1, 0)
  err_init_max_exact_50[i] <- ifelse(g50_exact_init<0.05, 1, 0)

  err_init_oslrt60[i] <- ifelse((a60_init<0.05), 1, 0)
  err_init_moslrt60[i] <- ifelse((a602_init<0.05), 1, 0)
  err_init_rc60[i] <- ifelse((b60_init<0.05), 1, 0)
  err_init_de60[i] <- ifelse((c60_init<0.05), 1, 0)
  err_init_ee60[i] <- ifelse((d60_init<0.05), 1, 0)
  err_init_me60[i] <- ifelse((e60_init<0.05), 1, 0)
  err_init_max_hoch_60[i] <- ifelse(g60_hoch_init<0.05, 1, 0)
  err_init_max_exact_60[i] <- ifelse(g60_exact_init<0.05, 1, 0)  

  err_init_oslrt80[i] <- ifelse((a80_init<0.05), 1, 0)
  err_init_moslrt80[i] <- ifelse((a802_init<0.05), 1, 0)
  err_init_rc80[i] <- ifelse((b80_init<0.05), 1, 0)
  err_init_de80[i] <- ifelse((c80_init<0.05), 1, 0)
  err_init_ee80[i] <- ifelse((d80_init<0.05), 1, 0)
  err_init_me80[i] <- ifelse((e80_init<0.05), 1, 0)
  err_init_rmst_80[i] <- ifelse((e_init<0.05), 1, 0)
  err_init_max_hoch_80[i] <- ifelse(g80_hoch_init<0.05, 1, 0)
  err_init_max_exact_80[i] <- ifelse(g80_exact_init<0.05, 1, 0)

  err_init_oslrt100[i] <- ifelse((a100_init<0.05), 1, 0)
  err_init_moslrt100[i] <- ifelse((a1002_init<0.05), 1, 0)
  err_init_rc100[i] <- ifelse((b100_init<0.05), 1, 0)
  err_init_de100[i] <- ifelse((c100_init<0.05), 1, 0)
  err_init_ee100[i] <- ifelse((d100_init<0.05), 1, 0)
  err_init_me100[i] <- ifelse((e100_init<0.05), 1, 0)
  err_init_max_hoch_100[i] <- ifelse(g100_hoch_init<0.05, 1, 0)
  err_init_max_exact_100[i] <- ifelse(g100_exact_init<0.05, 1, 0)

  err_init_oslrt150[i] <- ifelse((a150_init<0.05), 1, 0)
  err_init_moslrt150[i] <- ifelse((a1502_init<0.05), 1, 0)
  err_init_rc150[i] <- ifelse((b150_init<0.05), 1, 0)
  err_init_de150[i] <- ifelse((c150_init<0.05), 1, 0)
  err_init_ee150[i] <- ifelse((d150_init<0.05), 1, 0)
  err_init_me150[i] <- ifelse((e150_init<0.05), 1, 0)
  err_init_max_hoch_150[i] <- ifelse(g150_hoch_init<0.05, 1, 0)
  err_init_max_exact_150[i] <- ifelse(g150_exact_init<0.05, 1, 0)

  err_init_oslrt200[i] <- ifelse((a200_init<0.05), 1, 0)
  err_init_moslrt200[i] <- ifelse((a2002_init<0.05), 1, 0)
  err_init_rc200[i] <- ifelse((b200_init<0.05), 1, 0)
  err_init_de200[i] <- ifelse((c200_init<0.05), 1, 0)              
  err_init_ee200[i] <- ifelse((d200_init<0.05), 1, 0)
  err_init_me200[i] <- ifelse((e200_init<0.05), 1, 0)
  err_init_max_hoch_200[i] <- ifelse(g200_hoch_init<0.05, 1, 0)
  err_init_max_exact_200[i] <- ifelse(g200_exact_init<0.05, 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_init <- sum(err_init_oslrt20)/nit
mos20_init <- sum(err_init_moslrt20)/nit
rc20_init <- sum(err_init_rc20)/nit
de20_init <- sum(err_init_de20)/nit
ee20_init <- sum(err_init_ee20)/nit
me20_init <- sum(err_init_me20)/nit
max_hoch20_init <- sum(err_init_max_hoch_20)/nit
max_exact20_init <- sum(err_init_max_exact_20)/nit

diff1_os20 <- ((os20_1-os20_init)/os20_init)*100
diff1_mos20 <- ((mos20_1-mos20_init)/mos20_init)*100
diff1_ee20 <- ((ee20_1-ee20_init)/ee20_init)*100
diff1_me20 <- ((me20_1-me20_init)/me20_init)*100
diff1_de20 <- ((de20_1-de20_init)/de20_init)*100
diff1_rc20 <- ((rc20_1-rc20_init)/rc20_init)*100
diff1_hoch20 <- ((max_hoch20_1-max_hoch20_init)/max_hoch20_init)*100
diff1_exact20 <- ((max_exact20_1-max_exact20_init)/max_exact20_init)*100

diff3_os20 <- ((os20_3-os20_init)/os20_init)*100
diff3_mos20 <- ((mos20_3-mos20_init)/mos20_init)*100
diff3_ee20 <- ((ee20_3-ee20_init)/ee20_init)*100
diff3_me20 <- ((me20_3-me20_init)/me20_init)*100
diff3_de20 <- ((de20_3-de20_init)/de20_init)*100
diff3_rc20 <- ((rc20_3-rc20_init)/rc20_init)*100
diff3_hoch20 <- ((max_hoch20_3-max_hoch20_init)/max_hoch20_init)*100
diff3_exact20 <- ((max_exact20_3-max_exact20_init)/max_exact20_init)*100

os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_init <- sum(err_init_oslrt30)/nit
mos30_init <- sum(err_init_moslrt30)/nit
rc30_init <- sum(err_init_rc30)/nit
de30_init <- sum(err_init_de30)/nit
ee30_init <- sum(err_init_ee30)/nit
me30_init <- sum(err_init_me30)/nit
max_hoch30_init <- sum(err_init_max_hoch_30)/nit
max_exact30_init <- sum(err_init_max_exact_30)/nit

diff1_os30 <- ((os30_1-os30_init)/os30_init)*100
diff1_mos30 <- ((mos30_1-mos30_init)/mos30_init)*100
diff1_ee30 <- ((ee30_1-ee30_init)/ee30_init)*100
diff1_me30 <- ((me30_1-me30_init)/me30_init)*100
diff1_de30 <- ((de30_1-de30_init)/de30_init)*100
diff1_rc30 <- ((rc30_1-rc30_init)/rc30_init)*100
diff1_hoch30 <- ((max_hoch30_1-max_hoch30_init)/max_hoch30_init)*100
diff1_exact30 <- ((max_exact30_1-max_exact30_init)/max_exact30_init)*100

diff3_os30 <- ((os30_3-os30_init)/os30_init)*100
diff3_mos30 <- ((mos30_3-mos30_init)/mos30_init)*100
diff3_ee30 <- ((ee30_3-ee30_init)/ee30_init)*100
diff3_me30 <- ((me30_3-me30_init)/me30_init)*100
diff3_de30 <- ((de30_3-de30_init)/de30_init)*100
diff3_rc30 <- ((rc30_3-rc30_init)/rc30_init)*100
diff3_hoch30 <- ((max_hoch30_3-max_hoch30_init)/max_hoch30_init)*100
diff3_exact30 <- ((max_exact30_3-max_exact30_init)/max_exact30_init)*100


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_init <- sum(err_init_oslrt50)/nit
mos50_init <- sum(err_init_moslrt50)/nit
rc50_init <- sum(err_init_rc50)/nit
de50_init <- sum(err_init_de50)/nit
ee50_init <- sum(err_init_ee50)/nit
me50_init <- sum(err_init_me50)/nit
max_hoch50_init <- sum(err_init_max_hoch_50)/nit
max_exact50_init <- sum(err_init_max_exact_50)/nit

diff1_os50 <- ((os50_1-os50_init)/os50_init)*100
diff1_mos50 <- ((mos50_1-mos50_init)/mos50_init)*100
diff1_ee50 <- ((ee50_1-ee50_init)/ee50_init)*100
diff1_me50 <- ((me50_1-me50_init)/me50_init)*100
diff1_de50 <- ((de50_1-de50_init)/de50_init)*100
diff1_rc50 <- ((rc50_1-rc50_init)/rc50_init)*100
diff1_hoch50 <- ((max_hoch50_1-max_hoch50_init)/max_hoch50_init)*100
diff1_exact50 <- ((max_exact50_1-max_exact50_init)/max_exact50_init)*100

diff3_os50 <- ((os50_3-os50_init)/os50_init)*100
diff3_mos50 <- ((mos50_3-mos50_init)/mos50_init)*100
diff3_ee50 <- ((ee50_3-ee50_init)/ee50_init)*100
diff3_me50 <- ((me50_3-me50_init)/me50_init)*100
diff3_de50 <- ((de50_3-de50_init)/de50_init)*100
diff3_rc50 <- ((rc50_3-rc50_init)/rc50_init)*100
diff3_hoch50 <- ((max_hoch50_3-max_hoch50_init)/max_hoch50_init)*100
diff3_exact50 <- ((max_exact50_3-max_exact50_init)/max_exact50_init)*100


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_init <- sum(err_init_oslrt60)/nit
mos60_init <- sum(err_init_moslrt60)/nit
rc60_init <- sum(err_init_rc60)/nit
de60_init <- sum(err_init_de60)/nit
ee60_init <- sum(err_init_ee60)/nit
me60_init <- sum(err_init_me60)/nit
max_hoch60_init <- sum(err_init_max_hoch_60)/nit
max_exact60_init <- sum(err_init_max_exact_60)/nit

diff1_os60 <- ((os60_1-os60_init)/os60_init)*100
diff1_mos60 <- ((mos60_1-mos60_init)/mos60_init)*100
diff1_ee60 <- ((ee60_1-ee60_init)/ee60_init)*100
diff1_me60 <- ((me60_1-me60_init)/me60_init)*100
diff1_de60 <- ((de60_1-de60_init)/de60_init)*100
diff1_rc60 <- ((rc60_1-rc60_init)/rc60_init)*100
diff1_hoch60 <- ((max_hoch60_1-max_hoch60_init)/max_hoch60_init)*100
diff1_exact60 <- ((max_exact60_1-max_exact60_init)/max_exact60_init)*100

diff3_os60 <- ((os60_3-os60_init)/os60_init)*100
diff3_mos60 <- ((mos60_3-mos60_init)/mos60_init)*100
diff3_ee60 <- ((ee60_3-ee60_init)/ee60_init)*100
diff3_me60 <- ((me60_3-me60_init)/me60_init)*100
diff3_de60 <- ((de60_3-de60_init)/de60_init)*100
diff3_rc60 <- ((rc60_3-rc60_init)/rc60_init)*100
diff3_hoch60 <- ((max_hoch60_3-max_hoch60_init)/max_hoch60_init)*100
diff3_exact60 <- ((max_exact60_3-max_exact60_init)/max_exact60_init)*100


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_init <- sum(err_init_oslrt80)/nit
mos80_init <- sum(err_init_moslrt80)/nit
rc80_init <- sum(err_init_rc80)/nit
de80_init <- sum(err_init_de80)/nit
ee80_init <- sum(err_init_ee80)/nit
me80_init <- sum(err_init_me80)/nit
max_hoch80_init <- sum(err_init_max_hoch_80)/nit
max_exact80_init <- sum(err_init_max_exact_80)/nit

diff1_os80 <- ((os80_1-os80_init)/os80_init)*100
diff1_mos80 <- ((mos80_1-mos80_init)/mos80_init)*100
diff1_ee80 <- ((ee80_1-ee80_init)/ee80_init)*100
diff1_me80 <- ((me80_1-me80_init)/me80_init)*100
diff1_de80 <- ((de80_1-de80_init)/de80_init)*100
diff1_rc80 <- ((rc80_1-rc80_init)/rc80_init)*100
diff1_hoch80 <- ((max_hoch80_1-max_hoch80_init)/max_hoch80_init)*100
diff1_exact80 <- ((max_exact80_1-max_exact80_init)/max_exact80_init)*100

diff3_os80 <- ((os80_3-os80_init)/os80_init)*100
diff3_mos80 <- ((mos80_3-mos80_init)/mos80_init)*100
diff3_ee80 <- ((ee80_3-ee80_init)/ee80_init)*100
diff3_me80 <- ((me80_3-me80_init)/me80_init)*100
diff3_de80 <- ((de80_3-de80_init)/de80_init)*100
diff3_rc80 <- ((rc80_3-rc80_init)/rc80_init)*100
diff3_hoch80 <- ((max_hoch80_3-max_hoch80_init)/max_hoch80_init)*100
diff3_exact80 <- ((max_exact80_3-max_exact80_init)/max_exact80_init)*100


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_init <- sum(err_init_oslrt100)/nit
mos100_init <- sum(err_init_moslrt100)/nit
rc100_init <- sum(err_init_rc100)/nit
de100_init <- sum(err_init_de100)/nit
ee100_init <- sum(err_init_ee100)/nit
me100_init <- sum(err_init_me100)/nit
max_hoch100_init <- sum(err_init_max_hoch_100)/nit
max_exact100_init <- sum(err_init_max_exact_100)/nit

diff1_os100 <- ((os100_1-os100_init)/os100_init)*100
diff1_mos100 <- ((mos100_1-mos100_init)/mos100_init)*100
diff1_ee100 <- ((ee100_1-ee100_init)/ee100_init)*100
diff1_me100 <- ((me100_1-me100_init)/me100_init)*100
diff1_de100 <- ((de100_1-de100_init)/de100_init)*100
diff1_rc100 <- ((rc100_1-rc100_init)/rc100_init)*100
diff1_hoch100 <- ((max_hoch100_1-max_hoch100_init)/max_hoch100_init)*100
diff1_exact100 <- ((max_exact100_1-max_exact100_init)/max_exact100_init)*100

diff3_os100 <- ((os100_3-os100_init)/os100_init)*100
diff3_mos100 <- ((mos100_3-mos100_init)/mos100_init)*100
diff3_ee100 <- ((ee100_3-ee100_init)/ee100_init)*100
diff3_me100 <- ((me100_3-me100_init)/me100_init)*100
diff3_de100 <- ((de100_3-de100_init)/de100_init)*100
diff3_rc100 <- ((rc100_3-rc100_init)/rc100_init)*100
diff3_hoch100 <- ((max_hoch100_3-max_hoch100_init)/max_hoch100_init)*100
diff3_exact100 <- ((max_exact100_3-max_exact100_init)/max_exact100_init)*100


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_init <- sum(err_init_oslrt150)/nit
mos150_init <- sum(err_init_moslrt150)/nit
rc150_init <- sum(err_init_rc150)/nit
de150_init <- sum(err_init_de150)/nit
ee150_init <- sum(err_init_ee150)/nit
me150_init <- sum(err_init_me150)/nit
max_hoch150_init <- sum(err_init_max_hoch_150)/nit
max_exact150_init <- sum(err_init_max_exact_150)/nit

diff1_os150 <- ((os150_1-os150_init)/os150_init)*100
diff1_mos150 <- ((mos150_1-mos150_init)/mos150_init)*100
diff1_ee150 <- ((ee150_1-ee150_init)/ee150_init)*100
diff1_me150 <- ((me150_1-me150_init)/me150_init)*100
diff1_de150 <- ((de150_1-de150_init)/de150_init)*100
diff1_rc150 <- ((rc150_1-rc150_init)/rc150_init)*100
diff1_hoch150 <- ((max_hoch150_1-max_hoch150_init)/max_hoch150_init)*100
diff1_exact150 <- ((max_exact150_1-max_exact150_init)/max_exact150_init)*100

diff3_os150 <- ((os150_3-os150_init)/os150_init)*100
diff3_mos150 <- ((mos150_3-mos150_init)/mos150_init)*100
diff3_ee150 <- ((ee150_3-ee150_init)/ee150_init)*100
diff3_me150 <- ((me150_3-me150_init)/me150_init)*100
diff3_de150 <- ((de150_3-de150_init)/de150_init)*100
diff3_rc150 <- ((rc150_3-rc150_init)/rc150_init)*100
diff3_hoch150 <- ((max_hoch150_3-max_hoch150_init)/max_hoch150_init)*100
diff3_exact150 <- ((max_exact150_3-max_exact150_init)/max_exact150_init)*100


os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_init <- sum(err_init_oslrt200)/nit
mos200_init <- sum(err_init_moslrt200)/nit
rc200_init <- sum(err_init_rc200)/nit
de200_init <- sum(err_init_de200)/nit
ee200_init <- sum(err_init_ee200)/nit
me200_init <- sum(err_init_me200)/nit
r_200_init <- sum(err_init_rmst_200)/nit
max_hoch200_init <- sum(err_init_max_hoch_200)/nit
max_exact200_init <- sum(err_init_max_exact_200)/nit

diff1_os200 <- ((os200_1-os200_init)/os200_init)*100
diff1_mos200 <- ((mos200_1-mos200_init)/mos200_init)*100
diff1_ee200 <- ((ee200_1-ee200_init)/ee200_init)*100
diff1_me200 <- ((me200_1-me200_init)/me200_init)*100
diff1_de200 <- ((de200_1-de200_init)/de200_init)*100
diff1_rc200 <- ((rc200_1-rc200_init)/rc200_init)*100
diff1_hoch200 <- ((max_hoch200_1-max_hoch200_init)/max_hoch200_init)*100
diff1_exact200 <- ((max_exact200_1-max_exact200_init)/max_exact200_init)*100

diff3_os200 <- ((os200_3-os200_init)/os200_init)*100
diff3_mos200 <- ((mos200_3-mos200_init)/mos200_init)*100
diff3_ee200 <- ((ee200_3-ee200_init)/ee200_init)*100
diff3_me200 <- ((me200_3-me200_init)/me200_init)*100
diff3_de200 <- ((de200_3-de200_init)/de200_init)*100
diff3_rc200 <- ((rc200_3-rc200_init)/rc200_init)*100
diff3_hoch200 <- ((max_hoch200_3-max_hoch200_init)/max_hoch200_init)*100
diff3_exact200 <- ((max_exact200_3-max_exact200_init)/max_exact200_init)*100


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
diff1_osl <- c(diff1_os20, diff1_os30, diff1_os50, diff1_os60, diff1_os80, diff1_os100, diff1_os150, diff1_os200)
diff1_mosl <- c(diff1_mos20, diff1_mos30, diff1_mos50, diff1_mos60, diff1_mos80, diff1_mos100, diff1_mos150, diff1_mos200)
diff1_rc <- c(diff1_rc20, diff1_rc30, diff1_rc50, diff1_rc60, diff1_rc80, diff1_rc100, diff1_rc150, diff1_rc200)
diff1_de <- c(diff1_de20, diff1_de30, diff1_de50, diff1_de60, diff1_de80, diff1_de100, diff1_de150, diff1_de200)
diff1_ee <- c(diff1_ee20, diff1_ee30, diff1_ee50, diff1_ee60, diff1_ee80, diff1_ee100, diff1_ee150, diff1_ee200)
diff1_me <- c(diff1_me20, diff1_me30, diff1_me50, diff1_me60, diff1_me80, diff1_me100, diff1_me150, diff1_me200)
diff1_hoch <- c(diff1_hoch20, diff1_hoch30, diff1_hoch50, diff1_hoch60, diff1_hoch80, diff1_hoch100, diff1_hoch150, diff1_hoch200)
diff1_exact <- c(diff1_exact20, diff1_exact30, diff1_exact50, diff1_exact60, diff1_exact80, diff1_exact100, diff1_exact150, diff1_exact200)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
diff3_osl <- c(diff3_os20, diff3_os30, diff3_os50, diff3_os60, diff3_os80, diff3_os100, diff3_os150, diff3_os200)
diff3_mosl <- c(diff3_mos20, diff3_mos30, diff3_mos50, diff3_mos60, diff3_mos80, diff3_mos100, diff3_mos150, diff3_mos200)
diff3_rc <- c(diff3_rc20, diff3_rc30, diff3_rc50, diff3_rc60, diff3_rc80, diff3_rc100, diff3_rc150, diff3_rc200)
diff3_de <- c(diff3_de20, diff3_de30, diff3_de50, diff3_de60, diff3_de80, diff3_de100, diff3_de150, diff3_de200)
diff3_ee <- c(diff3_ee20, diff3_ee30, diff3_ee50, diff3_ee60, diff3_ee80, diff3_ee100, diff3_ee150, diff3_ee200)
diff3_me <- c(diff3_me20, diff3_me30, diff3_me50, diff3_me60, diff3_me80, diff3_me100, diff3_me150, diff3_me200)
diff3_hoch <- c(diff3_hoch20, diff3_hoch30, diff3_hoch50, diff3_hoch60, diff3_hoch80, diff3_hoch100, diff3_hoch150, diff3_hoch200)
diff3_exact <- c(diff3_exact20, diff3_exact30, diff3_exact50, diff3_exact60, diff3_exact80, diff3_exact100, diff3_exact150, diff3_exact200)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ME_1 <- ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 4: middle effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
ME_1

d2_1 <- data.frame(Sample.size = n, Error = c(diff1_osl, diff1_mosl, diff1_ee, diff1_me, diff1_de, diff1_rc, diff1_hoch, diff1_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_1$Test <- as.factor(d2_1$Test)
d2_1$Test <- factor(d2_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_ME_1 <- ggplot(d2_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_1$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 4: middle effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_ME_1


d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ME_3 <- ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 4: middle effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
ME_3

d2_3 <- data.frame(Sample.size = n, Error = c(diff3_osl, diff3_mosl, diff3_ee, diff3_me, diff3_de, diff3_rc, diff3_hoch, diff3_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_3$Test <- as.factor(d2_3$Test)
d2_3$Test <- factor(d2_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_ME_3 <- ggplot(d2_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_3$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 4: middle effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_ME_3



##########################
######Delayed effect######
##########################
#Parameters for the control group#
shape0 <- 1
m0 <- 2
scale0 <- m0/(-log(0.5))^(1/shape0)
distr0 <- 'Weibull'
#Parameters for the experimental group#
CP <- 3
t <- rpwexp(n = 3000, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
#t <- rpwexp(n = 3000, rate = c(1/scale0*1, 1/scale0*0.7), intervals = CP, cumulative = FALSE)
#t <- rpwexp(n = 3000, rate = c(1/scale0*1, 1/scale0*0.8), intervals = CP, cumulative = FALSE)
delta <- ifelse(t<(2555/365), 1, 0)
t <- ifelse(t>(2555/365), (2555/365), t)

#Survival curves#
t_cont <- rweibull(n = 3000, shape = shape0, scale = scale0)
delta_cont <- ifelse(t_cont<(2555/365), 1, 0)
t_cont <- ifelse(t_cont>(2555/365), (2555/365), t_cont)
C <- survfit(Surv(t_cont, delta_cont)~1)
S2 <- survfit(Surv(t, delta)~1)
plot(C, conf.int = FALSE, xlab = 'Years', ylab = 'Survival', col = 'black', main = "Survival")
lines(S2, conf.int = FALSE, col = 'blue')
legend('right', .9, c("Control", "Experimental"), col = c('black','blue'), lty = c(1,1))


set.seed(5)

nit <- 10000

#Censoring rate lambda_cens (0% 5% 15% 25% 35%)#
#HR1 = 0.5 (0 0.015 0.05 0.09 0.17)
#HR1 = 0.7 (0 0.015 0.06 0.1 0.17)
#HR1 = 0.8 (0 0.015 0.06 0.11 0.19)

lambda_cens <- 0.05   #15% of censoring for HR1 = 0.5

CP_EE <- 3
CP_DE <- 3
CP_ME1 <- 0
CP_ME2 <- 3

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

pi1 <- 1
pi3 <- 0.6

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_init_oslrt20 <- rep(0, nit)
err_init_moslrt20 <- rep(0, nit)
err_init_rc20 <- rep(0, nit)
err_init_de20 <- rep(0, nit)
err_init_ee20 <- rep(0, nit)
err_init_me20 <- rep(0, nit)
err_init_max_hoch_20 <- rep(0, nit)
err_init_max_exact_20 <- rep(0, nit)

err_init_oslrt30 <- rep(0, nit)
err_init_moslrt30 <- rep(0, nit)
err_init_rc30 <- rep(0, nit)
err_init_de30 <- rep(0, nit)
err_init_ee30 <- rep(0, nit)
err_init_me30 <- rep(0, nit)
err_init_max_hoch_30 <- rep(0, nit)
err_init_max_exact_30 <- rep(0, nit)

err_init_oslrt50 <- rep(0, nit)
err_init_moslrt50 <- rep(0, nit)
err_init_rc50 <- rep(0, nit)
err_init_de50 <- rep(0, nit)
err_init_ee50 <- rep(0, nit)
err_init_me50 <- rep(0, nit)
err_init_max_hoch_50 <- rep(0, nit)
err_init_max_exact_50 <- rep(0, nit)

err_init_oslrt60 <- rep(0, nit)
err_init_moslrt60 <- rep(0, nit)
err_init_rc60 <- rep(0, nit)
err_init_de60 <- rep(0, nit)
err_init_ee60 <- rep(0, nit)
err_init_me60 <- rep(0, nit)
err_init_max_hoch_60 <- rep(0, nit)
err_init_max_exact_60 <- rep(0, nit)

err_init_oslrt80 <- rep(0, nit)
err_init_moslrt80 <- rep(0, nit)
err_init_rc80 <- rep(0, nit)
err_init_de80 <- rep(0, nit)
err_init_ee80 <- rep(0, nit)
err_init_me80 <- rep(0, nit)
err_init_max_hoch_80 <- rep(0, nit)
err_init_max_exact_80 <- rep(0, nit)

err_init_oslrt100 <- rep(0, nit)
err_init_moslrt100 <- rep(0, nit)
err_init_rc100 <- rep(0, nit)
err_init_de100 <- rep(0, nit)
err_init_ee100 <- rep(0, nit)
err_init_me100 <- rep(0, nit)
err_init_max_hoch_100 <- rep(0, nit)
err_init_max_exact_100 <- rep(0, nit)

err_init_oslrt150 <- rep(0, nit)
err_init_moslrt150 <- rep(0, nit)
err_init_rc150 <- rep(0, nit)
err_init_de150 <- rep(0, nit)
err_init_ee150 <- rep(0, nit)
err_init_me150 <- rep(0, nit)
err_init_max_hoch_150 <- rep(0, nit)
err_init_max_exact_150 <- rep(0, nit)

err_init_oslrt200 <- rep(0, nit)
err_init_moslrt200 <- rep(0, nit)
err_init_rc200 <- rep(0, nit)
err_init_de200 <- rep(0, nit)
err_init_ee200 <- rep(0, nit)
err_init_me200 <- rep(0, nit)
err_init_max_hoch_200 <- rep(0, nit)
err_init_max_exact_200 <- rep(0, nit)

for(i in 1:nit){
  time20 <- rpwexp(n = 20, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u20 <- runif(20, 0, ta)
  
  time30 <- rpwexp(n = 30, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u30 <- runif(30, 0, ta)
  
  time50 <- rpwexp(n = 50, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u50 <- runif(50, 0, ta)
  
  time60 <- rpwexp(n = 60, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u60 <- runif(60, 0, ta)
  
  time80 <- rpwexp(n = 80, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u80 <- runif(80, 0, ta)
  
  time100 <- rpwexp(n = 100, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u100 <- runif(100, 0, ta)
  
  time150 <- rpwexp(n = 150, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u150 <- runif(150, 0, ta)
  
  time200 <- rpwexp(n = 200, rate = c(1/scale0*1, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u200 <- runif(200, 0, ta)
  
  if(lambda_cens==0){  #adm censoring
    del20 <- rep(1, 20)
    del30 <- rep(1, 30)
    del50 <- rep(1, 50)
    del60 <- rep(1, 60)
    del80 <- rep(1, 80)
    del100 <- rep(1, 100)
    del150 <- rep(1, 150)
    del200 <- rep(1, 200)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- stats::model.frame(Surv(t20, delta20)~1)  
    S30 <- stats::model.frame(Surv(t30, delta30)~1) 
    S50 <- stats::model.frame(Surv(t50, delta50)~1)  
    S60 <- stats::model.frame(Surv(t60, delta60)~1)  
    S80 <- stats::model.frame(Surv(t80, delta80)~1)  
    S100 <- stats::model.frame(Surv(t100, delta100)~1)  
    S150 <- stats::model.frame(Surv(t150, delta150)~1)  
    S200 <- stats::model.frame(Surv(t200, delta200)~1)
  }
  else{ #other censoring + adm censoring
    cens20 <- rexp(20, lambda_cens)
    time20 <- ifelse(time20 < cens20, time20, cens20)
    del20 <- ifelse(time20 < cens20, 1, 0)
    
    cens30 <- rexp(30, lambda_cens)
    time30 <- ifelse(time30 < cens30, time30, cens30)
    del30 <- ifelse(time30 < cens30, 1, 0)
    
    cens50 <- rexp(50, lambda_cens)
    time50 <- ifelse(time50 < cens50, time50, cens50)
    del50 <- ifelse(time50 < cens50, 1, 0)
    
    cens60 <- rexp(60, lambda_cens)
    time60 <- ifelse(time60 < cens60, time60, cens60)
    del60 <- ifelse(time60 < cens60, 1, 0)
    
    cens80 <- rexp(80, lambda_cens)
    time80 <- ifelse(time80 < cens80, time80, cens80)
    del80 <- ifelse(time80 < cens80, 1, 0)
    
    cens100 <- rexp(100, lambda_cens)
    time100 <- ifelse(time100 < cens100, time100, cens100)
    del100 <- ifelse(time100 < cens100, 1, 0)
    
    cens150 <- rexp(150, lambda_cens)
    time150 <- ifelse(time150 < cens150, time150, cens150)
    del150 <- ifelse(time150 < cens150, 1, 0)
    
    cens200 <- rexp(200, lambda_cens)
    time200 <- ifelse(time200 < cens200, time200, cens200)
    del200 <- ifelse(time200 < cens200, 1, 0)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- survfit(Surv(t20, delta20) ~ 1)
    S30 <- survfit(Surv(t30, delta30) ~ 1)
    S50 <- survfit(Surv(t50, delta50) ~ 1)
    S60 <- survfit(Surv(t60, delta60) ~ 1)
    S80 <- survfit(Surv(t80, delta80) ~ 1)
    S100 <- survfit(Surv(t100, delta100) ~ 1)
    S150 <- survfit(Surv(t150, delta150) ~ 1)
    S200 <- survfit(Surv(t200, delta200) ~ 1)
  }
  data20 <- ten(S20)
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a20_init <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202_init <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20_init <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20_init <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20_init <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20_init <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a30_init <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302_init <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30_init <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30_init <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30_init <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30_init <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a50_init <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502_init <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50_init <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50_init <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50_init <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50_init <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a60_init <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602_init <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60_init <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60_init <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60_init <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60_init <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a80_init <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802_init <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80_init <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80_init <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80_init <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80_init <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a100_init <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002_init  <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100_init  <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100_init  <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100_init  <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100_init  <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a150_init <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502_init <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150_init <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150_init <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150_init <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150_init <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a200_init <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002_init <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200_init <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200_init <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200_init <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200_init <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)

  err_init_oslrt20[i] <- ifelse((a20_init<0.05), 1, 0)
  err_init_moslrt20[i] <- ifelse((a202_init<0.05), 1, 0)
  err_init_rc20[i] <- ifelse((b20_init<0.05), 1, 0)
  err_init_de20[i] <- ifelse((c20_init<0.05), 1, 0)
  err_init_ee20[i] <- ifelse((d20_init<0.05), 1, 0)
  err_init_me20[i] <- ifelse((e20_init<0.05), 1, 0)
  err_init_max_hoch_20[i] <- ifelse(g20_hoch_init<0.05, 1, 0)
  err_init_max_exact_20[i] <- ifelse(g20_exact_init<0.05, 1, 0)

  err_init_oslrt30[i] <- ifelse((a30_init<0.05), 1, 0)
  err_init_moslrt30[i] <- ifelse((a302_init<0.05), 1, 0)
  err_init_rc30[i] <- ifelse((b30_init<0.05), 1, 0)
  err_init_de30[i] <- ifelse((c30_init<0.05), 1, 0)
  err_init_ee30[i] <- ifelse((d30_init<0.05), 1, 0)
  err_init_me30[i] <- ifelse((e30_init<0.05), 1, 0)
  err_init_max_hoch_30[i] <- ifelse(g30_hoch_init<0.05, 1, 0)
  err_init_max_exact_30[i] <- ifelse(g30_exact_init<0.05, 1, 0)

  err_init_oslrt50[i] <- ifelse((a50_init<0.05), 1, 0)
  err_init_moslrt50[i] <- ifelse((a502_init<0.05), 1, 0)
  err_init_rc50[i] <- ifelse((b50_init<0.05), 1, 0)
  err_init_de50[i] <- ifelse((c50_init<0.05), 1, 0)
  err_init_ee50[i] <- ifelse((d50_init<0.05), 1, 0)
  err_init_me50[i] <- ifelse((e50_init<0.05), 1, 0)
  err_init_max_hoch_50[i] <- ifelse(g50_hoch_init<0.05, 1, 0)
  err_init_max_exact_50[i] <- ifelse(g50_exact_init<0.05, 1, 0)

  err_init_oslrt60[i] <- ifelse((a60_init<0.05), 1, 0)
  err_init_moslrt60[i] <- ifelse((a602_init<0.05), 1, 0)
  err_init_rc60[i] <- ifelse((b60_init<0.05), 1, 0)
  err_init_de60[i] <- ifelse((c60_init<0.05), 1, 0)
  err_init_ee60[i] <- ifelse((d60_init<0.05), 1, 0)
  err_init_me60[i] <- ifelse((e60_init<0.05), 1, 0)
  err_init_max_hoch_60[i] <- ifelse(g60_hoch_init<0.05, 1, 0)
  err_init_max_exact_60[i] <- ifelse(g60_exact_init<0.05, 1, 0)  

  err_init_oslrt80[i] <- ifelse((a80_init<0.05), 1, 0)
  err_init_moslrt80[i] <- ifelse((a802_init<0.05), 1, 0)
  err_init_rc80[i] <- ifelse((b80_init<0.05), 1, 0)
  err_init_de80[i] <- ifelse((c80_init<0.05), 1, 0)
  err_init_ee80[i] <- ifelse((d80_init<0.05), 1, 0)
  err_init_me80[i] <- ifelse((e80_init<0.05), 1, 0)
  err_init_rmst_80[i] <- ifelse((e_init<0.05), 1, 0)
  err_init_max_hoch_80[i] <- ifelse(g80_hoch_init<0.05, 1, 0)
  err_init_max_exact_80[i] <- ifelse(g80_exact_init<0.05, 1, 0)

  err_init_oslrt100[i] <- ifelse((a100_init<0.05), 1, 0)
  err_init_moslrt100[i] <- ifelse((a1002_init<0.05), 1, 0)
  err_init_rc100[i] <- ifelse((b100_init<0.05), 1, 0)
  err_init_de100[i] <- ifelse((c100_init<0.05), 1, 0)
  err_init_ee100[i] <- ifelse((d100_init<0.05), 1, 0)
  err_init_me100[i] <- ifelse((e100_init<0.05), 1, 0)
  err_init_max_hoch_100[i] <- ifelse(g100_hoch_init<0.05, 1, 0)
  err_init_max_exact_100[i] <- ifelse(g100_exact_init<0.05, 1, 0)

  err_init_oslrt150[i] <- ifelse((a150_init<0.05), 1, 0)
  err_init_moslrt150[i] <- ifelse((a1502_init<0.05), 1, 0)
  err_init_rc150[i] <- ifelse((b150_init<0.05), 1, 0)
  err_init_de150[i] <- ifelse((c150_init<0.05), 1, 0)
  err_init_ee150[i] <- ifelse((d150_init<0.05), 1, 0)
  err_init_me150[i] <- ifelse((e150_init<0.05), 1, 0)
  err_init_max_hoch_150[i] <- ifelse(g150_hoch_init<0.05, 1, 0)
  err_init_max_exact_150[i] <- ifelse(g150_exact_init<0.05, 1, 0)

  err_init_oslrt200[i] <- ifelse((a200_init<0.05), 1, 0)
  err_init_moslrt200[i] <- ifelse((a2002_init<0.05), 1, 0)
  err_init_rc200[i] <- ifelse((b200_init<0.05), 1, 0)
  err_init_de200[i] <- ifelse((c200_init<0.05), 1, 0)              
  err_init_ee200[i] <- ifelse((d200_init<0.05), 1, 0)
  err_init_me200[i] <- ifelse((e200_init<0.05), 1, 0)
  err_init_max_hoch_200[i] <- ifelse(g200_hoch_init<0.05, 1, 0)
  err_init_max_exact_200[i] <- ifelse(g200_exact_init<0.05, 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_init <- sum(err_init_oslrt20)/nit
mos20_init <- sum(err_init_moslrt20)/nit
rc20_init <- sum(err_init_rc20)/nit
de20_init <- sum(err_init_de20)/nit
ee20_init <- sum(err_init_ee20)/nit
me20_init <- sum(err_init_me20)/nit
max_hoch20_init <- sum(err_init_max_hoch_20)/nit
max_exact20_init <- sum(err_init_max_exact_20)/nit

diff1_os20 <- ((os20_1-os20_init)/os20_init)*100
diff1_mos20 <- ((mos20_1-mos20_init)/mos20_init)*100
diff1_ee20 <- ((ee20_1-ee20_init)/ee20_init)*100
diff1_me20 <- ((me20_1-me20_init)/me20_init)*100
diff1_de20 <- ((de20_1-de20_init)/de20_init)*100
diff1_rc20 <- ((rc20_1-rc20_init)/rc20_init)*100
diff1_hoch20 <- ((max_hoch20_1-max_hoch20_init)/max_hoch20_init)*100
diff1_exact20 <- ((max_exact20_1-max_exact20_init)/max_exact20_init)*100

diff3_os20 <- ((os20_3-os20_init)/os20_init)*100
diff3_mos20 <- ((mos20_3-mos20_init)/mos20_init)*100
diff3_ee20 <- ((ee20_3-ee20_init)/ee20_init)*100
diff3_me20 <- ((me20_3-me20_init)/me20_init)*100
diff3_de20 <- ((de20_3-de20_init)/de20_init)*100
diff3_rc20 <- ((rc20_3-rc20_init)/rc20_init)*100
diff3_hoch20 <- ((max_hoch20_3-max_hoch20_init)/max_hoch20_init)*100
diff3_exact20 <- ((max_exact20_3-max_exact20_init)/max_exact20_init)*100

os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_init <- sum(err_init_oslrt30)/nit
mos30_init <- sum(err_init_moslrt30)/nit
rc30_init <- sum(err_init_rc30)/nit
de30_init <- sum(err_init_de30)/nit
ee30_init <- sum(err_init_ee30)/nit
me30_init <- sum(err_init_me30)/nit
max_hoch30_init <- sum(err_init_max_hoch_30)/nit
max_exact30_init <- sum(err_init_max_exact_30)/nit

diff1_os30 <- ((os30_1-os30_init)/os30_init)*100
diff1_mos30 <- ((mos30_1-mos30_init)/mos30_init)*100
diff1_ee30 <- ((ee30_1-ee30_init)/ee30_init)*100
diff1_me30 <- ((me30_1-me30_init)/me30_init)*100
diff1_de30 <- ((de30_1-de30_init)/de30_init)*100
diff1_rc30 <- ((rc30_1-rc30_init)/rc30_init)*100
diff1_hoch30 <- ((max_hoch30_1-max_hoch30_init)/max_hoch30_init)*100
diff1_exact30 <- ((max_exact30_1-max_exact30_init)/max_exact30_init)*100

diff3_os30 <- ((os30_3-os30_init)/os30_init)*100
diff3_mos30 <- ((mos30_3-mos30_init)/mos30_init)*100
diff3_ee30 <- ((ee30_3-ee30_init)/ee30_init)*100
diff3_me30 <- ((me30_3-me30_init)/me30_init)*100
diff3_de30 <- ((de30_3-de30_init)/de30_init)*100
diff3_rc30 <- ((rc30_3-rc30_init)/rc30_init)*100
diff3_hoch30 <- ((max_hoch30_3-max_hoch30_init)/max_hoch30_init)*100
diff3_exact30 <- ((max_exact30_3-max_exact30_init)/max_exact30_init)*100


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_init <- sum(err_init_oslrt50)/nit
mos50_init <- sum(err_init_moslrt50)/nit
rc50_init <- sum(err_init_rc50)/nit
de50_init <- sum(err_init_de50)/nit
ee50_init <- sum(err_init_ee50)/nit
me50_init <- sum(err_init_me50)/nit
max_hoch50_init <- sum(err_init_max_hoch_50)/nit
max_exact50_init <- sum(err_init_max_exact_50)/nit

diff1_os50 <- ((os50_1-os50_init)/os50_init)*100
diff1_mos50 <- ((mos50_1-mos50_init)/mos50_init)*100
diff1_ee50 <- ((ee50_1-ee50_init)/ee50_init)*100
diff1_me50 <- ((me50_1-me50_init)/me50_init)*100
diff1_de50 <- ((de50_1-de50_init)/de50_init)*100
diff1_rc50 <- ((rc50_1-rc50_init)/rc50_init)*100
diff1_hoch50 <- ((max_hoch50_1-max_hoch50_init)/max_hoch50_init)*100
diff1_exact50 <- ((max_exact50_1-max_exact50_init)/max_exact50_init)*100

diff3_os50 <- ((os50_3-os50_init)/os50_init)*100
diff3_mos50 <- ((mos50_3-mos50_init)/mos50_init)*100
diff3_ee50 <- ((ee50_3-ee50_init)/ee50_init)*100
diff3_me50 <- ((me50_3-me50_init)/me50_init)*100
diff3_de50 <- ((de50_3-de50_init)/de50_init)*100
diff3_rc50 <- ((rc50_3-rc50_init)/rc50_init)*100
diff3_hoch50 <- ((max_hoch50_3-max_hoch50_init)/max_hoch50_init)*100
diff3_exact50 <- ((max_exact50_3-max_exact50_init)/max_exact50_init)*100


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_init <- sum(err_init_oslrt60)/nit
mos60_init <- sum(err_init_moslrt60)/nit
rc60_init <- sum(err_init_rc60)/nit
de60_init <- sum(err_init_de60)/nit
ee60_init <- sum(err_init_ee60)/nit
me60_init <- sum(err_init_me60)/nit
max_hoch60_init <- sum(err_init_max_hoch_60)/nit
max_exact60_init <- sum(err_init_max_exact_60)/nit

diff1_os60 <- ((os60_1-os60_init)/os60_init)*100
diff1_mos60 <- ((mos60_1-mos60_init)/mos60_init)*100
diff1_ee60 <- ((ee60_1-ee60_init)/ee60_init)*100
diff1_me60 <- ((me60_1-me60_init)/me60_init)*100
diff1_de60 <- ((de60_1-de60_init)/de60_init)*100
diff1_rc60 <- ((rc60_1-rc60_init)/rc60_init)*100
diff1_hoch60 <- ((max_hoch60_1-max_hoch60_init)/max_hoch60_init)*100
diff1_exact60 <- ((max_exact60_1-max_exact60_init)/max_exact60_init)*100

diff3_os60 <- ((os60_3-os60_init)/os60_init)*100
diff3_mos60 <- ((mos60_3-mos60_init)/mos60_init)*100
diff3_ee60 <- ((ee60_3-ee60_init)/ee60_init)*100
diff3_me60 <- ((me60_3-me60_init)/me60_init)*100
diff3_de60 <- ((de60_3-de60_init)/de60_init)*100
diff3_rc60 <- ((rc60_3-rc60_init)/rc60_init)*100
diff3_hoch60 <- ((max_hoch60_3-max_hoch60_init)/max_hoch60_init)*100
diff3_exact60 <- ((max_exact60_3-max_exact60_init)/max_exact60_init)*100


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_init <- sum(err_init_oslrt80)/nit
mos80_init <- sum(err_init_moslrt80)/nit
rc80_init <- sum(err_init_rc80)/nit
de80_init <- sum(err_init_de80)/nit
ee80_init <- sum(err_init_ee80)/nit
me80_init <- sum(err_init_me80)/nit
max_hoch80_init <- sum(err_init_max_hoch_80)/nit
max_exact80_init <- sum(err_init_max_exact_80)/nit

diff1_os80 <- ((os80_1-os80_init)/os80_init)*100
diff1_mos80 <- ((mos80_1-mos80_init)/mos80_init)*100
diff1_ee80 <- ((ee80_1-ee80_init)/ee80_init)*100
diff1_me80 <- ((me80_1-me80_init)/me80_init)*100
diff1_de80 <- ((de80_1-de80_init)/de80_init)*100
diff1_rc80 <- ((rc80_1-rc80_init)/rc80_init)*100
diff1_hoch80 <- ((max_hoch80_1-max_hoch80_init)/max_hoch80_init)*100
diff1_exact80 <- ((max_exact80_1-max_exact80_init)/max_exact80_init)*100

diff3_os80 <- ((os80_3-os80_init)/os80_init)*100
diff3_mos80 <- ((mos80_3-mos80_init)/mos80_init)*100
diff3_ee80 <- ((ee80_3-ee80_init)/ee80_init)*100
diff3_me80 <- ((me80_3-me80_init)/me80_init)*100
diff3_de80 <- ((de80_3-de80_init)/de80_init)*100
diff3_rc80 <- ((rc80_3-rc80_init)/rc80_init)*100
diff3_hoch80 <- ((max_hoch80_3-max_hoch80_init)/max_hoch80_init)*100
diff3_exact80 <- ((max_exact80_3-max_exact80_init)/max_exact80_init)*100


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_init <- sum(err_init_oslrt100)/nit
mos100_init <- sum(err_init_moslrt100)/nit
rc100_init <- sum(err_init_rc100)/nit
de100_init <- sum(err_init_de100)/nit
ee100_init <- sum(err_init_ee100)/nit
me100_init <- sum(err_init_me100)/nit
max_hoch100_init <- sum(err_init_max_hoch_100)/nit
max_exact100_init <- sum(err_init_max_exact_100)/nit

diff1_os100 <- ((os100_1-os100_init)/os100_init)*100
diff1_mos100 <- ((mos100_1-mos100_init)/mos100_init)*100
diff1_ee100 <- ((ee100_1-ee100_init)/ee100_init)*100
diff1_me100 <- ((me100_1-me100_init)/me100_init)*100
diff1_de100 <- ((de100_1-de100_init)/de100_init)*100
diff1_rc100 <- ((rc100_1-rc100_init)/rc100_init)*100
diff1_hoch100 <- ((max_hoch100_1-max_hoch100_init)/max_hoch100_init)*100
diff1_exact100 <- ((max_exact100_1-max_exact100_init)/max_exact100_init)*100

diff3_os100 <- ((os100_3-os100_init)/os100_init)*100
diff3_mos100 <- ((mos100_3-mos100_init)/mos100_init)*100
diff3_ee100 <- ((ee100_3-ee100_init)/ee100_init)*100
diff3_me100 <- ((me100_3-me100_init)/me100_init)*100
diff3_de100 <- ((de100_3-de100_init)/de100_init)*100
diff3_rc100 <- ((rc100_3-rc100_init)/rc100_init)*100
diff3_hoch100 <- ((max_hoch100_3-max_hoch100_init)/max_hoch100_init)*100
diff3_exact100 <- ((max_exact100_3-max_exact100_init)/max_exact100_init)*100


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_init <- sum(err_init_oslrt150)/nit
mos150_init <- sum(err_init_moslrt150)/nit
rc150_init <- sum(err_init_rc150)/nit
de150_init <- sum(err_init_de150)/nit
ee150_init <- sum(err_init_ee150)/nit
me150_init <- sum(err_init_me150)/nit
max_hoch150_init <- sum(err_init_max_hoch_150)/nit
max_exact150_init <- sum(err_init_max_exact_150)/nit

diff1_os150 <- ((os150_1-os150_init)/os150_init)*100
diff1_mos150 <- ((mos150_1-mos150_init)/mos150_init)*100
diff1_ee150 <- ((ee150_1-ee150_init)/ee150_init)*100
diff1_me150 <- ((me150_1-me150_init)/me150_init)*100
diff1_de150 <- ((de150_1-de150_init)/de150_init)*100
diff1_rc150 <- ((rc150_1-rc150_init)/rc150_init)*100
diff1_hoch150 <- ((max_hoch150_1-max_hoch150_init)/max_hoch150_init)*100
diff1_exact150 <- ((max_exact150_1-max_exact150_init)/max_exact150_init)*100

diff3_os150 <- ((os150_3-os150_init)/os150_init)*100
diff3_mos150 <- ((mos150_3-mos150_init)/mos150_init)*100
diff3_ee150 <- ((ee150_3-ee150_init)/ee150_init)*100
diff3_me150 <- ((me150_3-me150_init)/me150_init)*100
diff3_de150 <- ((de150_3-de150_init)/de150_init)*100
diff3_rc150 <- ((rc150_3-rc150_init)/rc150_init)*100
diff3_hoch150 <- ((max_hoch150_3-max_hoch150_init)/max_hoch150_init)*100
diff3_exact150 <- ((max_exact150_3-max_exact150_init)/max_exact150_init)*100


os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_init <- sum(err_init_oslrt200)/nit
mos200_init <- sum(err_init_moslrt200)/nit
rc200_init <- sum(err_init_rc200)/nit
de200_init <- sum(err_init_de200)/nit
ee200_init <- sum(err_init_ee200)/nit
me200_init <- sum(err_init_me200)/nit
r_200_init <- sum(err_init_rmst_200)/nit
max_hoch200_init <- sum(err_init_max_hoch_200)/nit
max_exact200_init <- sum(err_init_max_exact_200)/nit

diff1_os200 <- ((os200_1-os200_init)/os200_init)*100
diff1_mos200 <- ((mos200_1-mos200_init)/mos200_init)*100
diff1_ee200 <- ((ee200_1-ee200_init)/ee200_init)*100
diff1_me200 <- ((me200_1-me200_init)/me200_init)*100
diff1_de200 <- ((de200_1-de200_init)/de200_init)*100
diff1_rc200 <- ((rc200_1-rc200_init)/rc200_init)*100
diff1_hoch200 <- ((max_hoch200_1-max_hoch200_init)/max_hoch200_init)*100
diff1_exact200 <- ((max_exact200_1-max_exact200_init)/max_exact200_init)*100

diff3_os200 <- ((os200_3-os200_init)/os200_init)*100
diff3_mos200 <- ((mos200_3-mos200_init)/mos200_init)*100
diff3_ee200 <- ((ee200_3-ee200_init)/ee200_init)*100
diff3_me200 <- ((me200_3-me200_init)/me200_init)*100
diff3_de200 <- ((de200_3-de200_init)/de200_init)*100
diff3_rc200 <- ((rc200_3-rc200_init)/rc200_init)*100
diff3_hoch200 <- ((max_hoch200_3-max_hoch200_init)/max_hoch200_init)*100
diff3_exact200 <- ((max_exact200_3-max_exact200_init)/max_exact200_init)*100


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
diff1_osl <- c(diff1_os20, diff1_os30, diff1_os50, diff1_os60, diff1_os80, diff1_os100, diff1_os150, diff1_os200)
diff1_mosl <- c(diff1_mos20, diff1_mos30, diff1_mos50, diff1_mos60, diff1_mos80, diff1_mos100, diff1_mos150, diff1_mos200)
diff1_rc <- c(diff1_rc20, diff1_rc30, diff1_rc50, diff1_rc60, diff1_rc80, diff1_rc100, diff1_rc150, diff1_rc200)
diff1_de <- c(diff1_de20, diff1_de30, diff1_de50, diff1_de60, diff1_de80, diff1_de100, diff1_de150, diff1_de200)
diff1_ee <- c(diff1_ee20, diff1_ee30, diff1_ee50, diff1_ee60, diff1_ee80, diff1_ee100, diff1_ee150, diff1_ee200)
diff1_me <- c(diff1_me20, diff1_me30, diff1_me50, diff1_me60, diff1_me80, diff1_me100, diff1_me150, diff1_me200)
diff1_hoch <- c(diff1_hoch20, diff1_hoch30, diff1_hoch50, diff1_hoch60, diff1_hoch80, diff1_hoch100, diff1_hoch150, diff1_hoch200)
diff1_exact <- c(diff1_exact20, diff1_exact30, diff1_exact50, diff1_exact60, diff1_exact80, diff1_exact100, diff1_exact150, diff1_exact200)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
diff3_osl <- c(diff3_os20, diff3_os30, diff3_os50, diff3_os60, diff3_os80, diff3_os100, diff3_os150, diff3_os200)
diff3_mosl <- c(diff3_mos20, diff3_mos30, diff3_mos50, diff3_mos60, diff3_mos80, diff3_mos100, diff3_mos150, diff3_mos200)
diff3_rc <- c(diff3_rc20, diff3_rc30, diff3_rc50, diff3_rc60, diff3_rc80, diff3_rc100, diff3_rc150, diff3_rc200)
diff3_de <- c(diff3_de20, diff3_de30, diff3_de50, diff3_de60, diff3_de80, diff3_de100, diff3_de150, diff3_de200)
diff3_ee <- c(diff3_ee20, diff3_ee30, diff3_ee50, diff3_ee60, diff3_ee80, diff3_ee100, diff3_ee150, diff3_ee200)
diff3_me <- c(diff3_me20, diff3_me30, diff3_me50, diff3_me60, diff3_me80, diff3_me100, diff3_me150, diff3_me200)
diff3_hoch <- c(diff3_hoch20, diff3_hoch30, diff3_hoch50, diff3_hoch60, diff3_hoch80, diff3_hoch100, diff3_hoch150, diff3_hoch200)
diff3_exact <- c(diff3_exact20, diff3_exact30, diff3_exact50, diff3_exact60, diff3_exact80, diff3_exact100, diff3_exact150, diff3_exact200)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

DE_1 <- ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 5: delayed effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
DE_1

d2_1 <- data.frame(Sample.size = n, Error = c(diff1_osl, diff1_mosl, diff1_ee, diff1_me, diff1_de, diff1_rc, diff1_hoch, diff1_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_1$Test <- as.factor(d2_1$Test)
d2_1$Test <- factor(d2_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_DE_1 <- ggplot(d2_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_1$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 5: delayed effect',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_DE_1


d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

DE_3 <- ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 5: delayed effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
DE_3

d2_3 <- data.frame(Sample.size = n, Error = c(diff3_osl, diff3_mosl, diff3_ee, diff3_me, diff3_de, diff3_rc, diff3_hoch, diff3_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_3$Test <- as.factor(d2_3$Test)
d2_3$Test <- factor(d2_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_DE_3 <- ggplot(d2_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_3$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 5: delayed effect',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_DE_3



##########################
#####Crossing hazards#####
##########################
#Parameters for the control group#
shape0 <- 1
m0 <- 2
scale0 <- m0/(-log(0.5))^(1/shape0)
distr0 <- 'Weibull'
#Parameters for the experimental group#
CP <- 1
t <- rpwexp(n = 3000, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
delta <- ifelse(t<(2555/365), 1, 0)
t <- ifelse(t>(2555/365), (2555/365), t)

#Survival curves#
t_cont <- rweibull(n = 3000, shape = shape0, scale = scale0)
delta_cont <- ifelse(t_cont<(2555/365), 1, 0)
t_cont <- ifelse(t_cont>(2555/365), (2555/365), t_cont)
C <- survfit(Surv(t_cont, delta_cont)~1)
S2 <- survfit(Surv(t, delta)~1)
plot(C, conf.int = FALSE, xlab = 'Years', ylab = 'Survival', col = 'black', main = "Survival")
lines(S2, conf.int = FALSE, col = 'blue')
legend('right', .9, c("Control", "Experimental"), col = c('black','blue'), lty = c(1,1))


set.seed(5)

nit <- 10000

#Censoring rate lambda_cens (0% 5% 15% 25% 35%)#
#HR1 = 2 & HR2 = 0.5 (0 0.018 0.06 0.11 0.19)

lambda_cens <- 0.06   #15% of censoring for HR1 = 2 & HR2 = 0.5

CP_EE <- 1
CP_DE <- 1
CP_ME1 <- 1
CP_ME2 <- 4

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

pi1 <- 1
pi3 <- 0.6

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_init_oslrt20 <- rep(0, nit)
err_init_moslrt20 <- rep(0, nit)
err_init_rc20 <- rep(0, nit)
err_init_de20 <- rep(0, nit)
err_init_ee20 <- rep(0, nit)
err_init_me20 <- rep(0, nit)
err_init_max_hoch_20 <- rep(0, nit)
err_init_max_exact_20 <- rep(0, nit)

err_init_oslrt30 <- rep(0, nit)
err_init_moslrt30 <- rep(0, nit)
err_init_rc30 <- rep(0, nit)
err_init_de30 <- rep(0, nit)
err_init_ee30 <- rep(0, nit)
err_init_me30 <- rep(0, nit)
err_init_max_hoch_30 <- rep(0, nit)
err_init_max_exact_30 <- rep(0, nit)

err_init_oslrt50 <- rep(0, nit)
err_init_moslrt50 <- rep(0, nit)
err_init_rc50 <- rep(0, nit)
err_init_de50 <- rep(0, nit)
err_init_ee50 <- rep(0, nit)
err_init_me50 <- rep(0, nit)
err_init_max_hoch_50 <- rep(0, nit)
err_init_max_exact_50 <- rep(0, nit)

err_init_oslrt60 <- rep(0, nit)
err_init_moslrt60 <- rep(0, nit)
err_init_rc60 <- rep(0, nit)
err_init_de60 <- rep(0, nit)
err_init_ee60 <- rep(0, nit)
err_init_me60 <- rep(0, nit)
err_init_max_hoch_60 <- rep(0, nit)
err_init_max_exact_60 <- rep(0, nit)

err_init_oslrt80 <- rep(0, nit)
err_init_moslrt80 <- rep(0, nit)
err_init_rc80 <- rep(0, nit)
err_init_de80 <- rep(0, nit)
err_init_ee80 <- rep(0, nit)
err_init_me80 <- rep(0, nit)
err_init_max_hoch_80 <- rep(0, nit)
err_init_max_exact_80 <- rep(0, nit)

err_init_oslrt100 <- rep(0, nit)
err_init_moslrt100 <- rep(0, nit)
err_init_rc100 <- rep(0, nit)
err_init_de100 <- rep(0, nit)
err_init_ee100 <- rep(0, nit)
err_init_me100 <- rep(0, nit)
err_init_max_hoch_100 <- rep(0, nit)
err_init_max_exact_100 <- rep(0, nit)

err_init_oslrt150 <- rep(0, nit)
err_init_moslrt150 <- rep(0, nit)
err_init_rc150 <- rep(0, nit)
err_init_de150 <- rep(0, nit)
err_init_ee150 <- rep(0, nit)
err_init_me150 <- rep(0, nit)
err_init_max_hoch_150 <- rep(0, nit)
err_init_max_exact_150 <- rep(0, nit)

err_init_oslrt200 <- rep(0, nit)
err_init_moslrt200 <- rep(0, nit)
err_init_rc200 <- rep(0, nit)
err_init_de200 <- rep(0, nit)
err_init_ee200 <- rep(0, nit)
err_init_me200 <- rep(0, nit)
err_init_max_hoch_200 <- rep(0, nit)
err_init_max_exact_200 <- rep(0, nit)

for(i in 1:nit){
  time20 <- rpwexp(n = 20, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u20 <- runif(20, 0, ta)
  
  time30 <- rpwexp(n = 30, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u30 <- runif(30, 0, ta)
  
  time50 <- rpwexp(n = 50, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u50 <- runif(50, 0, ta)
  
  time60 <- rpwexp(n = 60, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u60 <- runif(60, 0, ta)
  
  time80 <- rpwexp(n = 80, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u80 <- runif(80, 0, ta)
  
  time100 <- rpwexp(n = 100, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u100 <- runif(100, 0, ta)
  
  time150 <- rpwexp(n = 150, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u150 <- runif(150, 0, ta)
  
  time200 <- rpwexp(n = 200, rate = c(1/scale0*2, 1/scale0*0.5), intervals = CP, cumulative = FALSE)
  u200 <- runif(200, 0, ta)
  
  if(lambda_cens==0){  #adm censoring
    del20 <- rep(1, 20)
    del30 <- rep(1, 30)
    del50 <- rep(1, 50)
    del60 <- rep(1, 60)
    del80 <- rep(1, 80)
    del100 <- rep(1, 100)
    del150 <- rep(1, 150)
    del200 <- rep(1, 200)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- stats::model.frame(Surv(t20, delta20)~1)  
    S30 <- stats::model.frame(Surv(t30, delta30)~1) 
    S50 <- stats::model.frame(Surv(t50, delta50)~1)  
    S60 <- stats::model.frame(Surv(t60, delta60)~1)  
    S80 <- stats::model.frame(Surv(t80, delta80)~1)  
    S100 <- stats::model.frame(Surv(t100, delta100)~1)  
    S150 <- stats::model.frame(Surv(t150, delta150)~1)  
    S200 <- stats::model.frame(Surv(t200, delta200)~1)
  }
  else{ #other censoring + adm censoring
    cens20 <- rexp(20, lambda_cens)
    time20 <- ifelse(time20 < cens20, time20, cens20)
    del20 <- ifelse(time20 < cens20, 1, 0)
    
    cens30 <- rexp(30, lambda_cens)
    time30 <- ifelse(time30 < cens30, time30, cens30)
    del30 <- ifelse(time30 < cens30, 1, 0)
    
    cens50 <- rexp(50, lambda_cens)
    time50 <- ifelse(time50 < cens50, time50, cens50)
    del50 <- ifelse(time50 < cens50, 1, 0)
    
    cens60 <- rexp(60, lambda_cens)
    time60 <- ifelse(time60 < cens60, time60, cens60)
    del60 <- ifelse(time60 < cens60, 1, 0)
    
    cens80 <- rexp(80, lambda_cens)
    time80 <- ifelse(time80 < cens80, time80, cens80)
    del80 <- ifelse(time80 < cens80, 1, 0)
    
    cens100 <- rexp(100, lambda_cens)
    time100 <- ifelse(time100 < cens100, time100, cens100)
    del100 <- ifelse(time100 < cens100, 1, 0)
    
    cens150 <- rexp(150, lambda_cens)
    time150 <- ifelse(time150 < cens150, time150, cens150)
    del150 <- ifelse(time150 < cens150, 1, 0)
    
    cens200 <- rexp(200, lambda_cens)
    time200 <- ifelse(time200 < cens200, time200, cens200)
    del200 <- ifelse(time200 < cens200, 1, 0)
    
    t20 <- pmin(time20, ta+tf-u20)
    t30 <- pmin(time30, ta+tf-u30)
    t50 <- pmin(time50, ta+tf-u50)
    t60 <- pmin(time60, ta+tf-u60)
    t80 <- pmin(time80, ta+tf-u80)
    t100 <- pmin(time100, ta+tf-u100)
    t150 <- pmin(time150, ta+tf-u150)
    t200 <- pmin(time200, ta+tf-u200)
    
    delta20 <- ifelse(time20 < ta+tf-u20, del20, 0)
    delta30 <- ifelse(time30 < ta+tf-u30, del30, 0)
    delta50 <- ifelse(time50 < ta+tf-u50, del50, 0)
    delta60 <- ifelse(time60 < ta+tf-u60, del60, 0)
    delta80 <- ifelse(time80 < ta+tf-u80, del80, 0)
    delta100 <- ifelse(time100 < ta+tf-u100, del100, 0)
    delta150 <- ifelse(time150 < ta+tf-u150, del150, 0)
    delta200 <- ifelse(time200 < ta+tf-u200, del200, 0)
    
    S20 <- survfit(Surv(t20, delta20) ~ 1)
    S30 <- survfit(Surv(t30, delta30) ~ 1)
    S50 <- survfit(Surv(t50, delta50) ~ 1)
    S60 <- survfit(Surv(t60, delta60) ~ 1)
    S80 <- survfit(Surv(t80, delta80) ~ 1)
    S100 <- survfit(Surv(t100, delta100) ~ 1)
    S150 <- survfit(Surv(t150, delta150) ~ 1)
    S200 <- survfit(Surv(t200, delta200) ~ 1)
  }
  data20 <- ten(S20)
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a20_init <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202_init <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20_init <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20_init <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20_init <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20_init <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact_init <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a30_init <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302_init <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30_init <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30_init <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30_init <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30_init <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact_init <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a50_init <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502_init <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50_init <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50_init <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50_init <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50_init <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact_init <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a60_init <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602_init <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60_init <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60_init <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60_init <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60_init <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact_init <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a80_init <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802_init <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80_init <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80_init <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80_init <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80_init <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact_init <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a100_init <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002_init  <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100_init  <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100_init  <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100_init  <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100_init  <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact_init  <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a150_init <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502_init <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150_init <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150_init <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150_init <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150_init <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact_init <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, distr = distr0, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, distr = distr0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)

  a200_init <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002_init <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200_init <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200_init <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200_init <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200_init <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact_init <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)

  err_init_oslrt20[i] <- ifelse((a20_init<0.05), 1, 0)
  err_init_moslrt20[i] <- ifelse((a202_init<0.05), 1, 0)
  err_init_rc20[i] <- ifelse((b20_init<0.05), 1, 0)
  err_init_de20[i] <- ifelse((c20_init<0.05), 1, 0)
  err_init_ee20[i] <- ifelse((d20_init<0.05), 1, 0)
  err_init_me20[i] <- ifelse((e20_init<0.05), 1, 0)
  err_init_max_hoch_20[i] <- ifelse(g20_hoch_init<0.05, 1, 0)
  err_init_max_exact_20[i] <- ifelse(g20_exact_init<0.05, 1, 0)

  err_init_oslrt30[i] <- ifelse((a30_init<0.05), 1, 0)
  err_init_moslrt30[i] <- ifelse((a302_init<0.05), 1, 0)
  err_init_rc30[i] <- ifelse((b30_init<0.05), 1, 0)
  err_init_de30[i] <- ifelse((c30_init<0.05), 1, 0)
  err_init_ee30[i] <- ifelse((d30_init<0.05), 1, 0)
  err_init_me30[i] <- ifelse((e30_init<0.05), 1, 0)
  err_init_max_hoch_30[i] <- ifelse(g30_hoch_init<0.05, 1, 0)
  err_init_max_exact_30[i] <- ifelse(g30_exact_init<0.05, 1, 0)

  err_init_oslrt50[i] <- ifelse((a50_init<0.05), 1, 0)
  err_init_moslrt50[i] <- ifelse((a502_init<0.05), 1, 0)
  err_init_rc50[i] <- ifelse((b50_init<0.05), 1, 0)
  err_init_de50[i] <- ifelse((c50_init<0.05), 1, 0)
  err_init_ee50[i] <- ifelse((d50_init<0.05), 1, 0)
  err_init_me50[i] <- ifelse((e50_init<0.05), 1, 0)
  err_init_max_hoch_50[i] <- ifelse(g50_hoch_init<0.05, 1, 0)
  err_init_max_exact_50[i] <- ifelse(g50_exact_init<0.05, 1, 0)

  err_init_oslrt60[i] <- ifelse((a60_init<0.05), 1, 0)
  err_init_moslrt60[i] <- ifelse((a602_init<0.05), 1, 0)
  err_init_rc60[i] <- ifelse((b60_init<0.05), 1, 0)
  err_init_de60[i] <- ifelse((c60_init<0.05), 1, 0)
  err_init_ee60[i] <- ifelse((d60_init<0.05), 1, 0)
  err_init_me60[i] <- ifelse((e60_init<0.05), 1, 0)
  err_init_max_hoch_60[i] <- ifelse(g60_hoch_init<0.05, 1, 0)
  err_init_max_exact_60[i] <- ifelse(g60_exact_init<0.05, 1, 0)  

  err_init_oslrt80[i] <- ifelse((a80_init<0.05), 1, 0)
  err_init_moslrt80[i] <- ifelse((a802_init<0.05), 1, 0)
  err_init_rc80[i] <- ifelse((b80_init<0.05), 1, 0)
  err_init_de80[i] <- ifelse((c80_init<0.05), 1, 0)
  err_init_ee80[i] <- ifelse((d80_init<0.05), 1, 0)
  err_init_me80[i] <- ifelse((e80_init<0.05), 1, 0)
  err_init_rmst_80[i] <- ifelse((e_init<0.05), 1, 0)
  err_init_max_hoch_80[i] <- ifelse(g80_hoch_init<0.05, 1, 0)
  err_init_max_exact_80[i] <- ifelse(g80_exact_init<0.05, 1, 0)

  err_init_oslrt100[i] <- ifelse((a100_init<0.05), 1, 0)
  err_init_moslrt100[i] <- ifelse((a1002_init<0.05), 1, 0)
  err_init_rc100[i] <- ifelse((b100_init<0.05), 1, 0)
  err_init_de100[i] <- ifelse((c100_init<0.05), 1, 0)
  err_init_ee100[i] <- ifelse((d100_init<0.05), 1, 0)
  err_init_me100[i] <- ifelse((e100_init<0.05), 1, 0)
  err_init_max_hoch_100[i] <- ifelse(g100_hoch_init<0.05, 1, 0)
  err_init_max_exact_100[i] <- ifelse(g100_exact_init<0.05, 1, 0)

  err_init_oslrt150[i] <- ifelse((a150_init<0.05), 1, 0)
  err_init_moslrt150[i] <- ifelse((a1502_init<0.05), 1, 0)
  err_init_rc150[i] <- ifelse((b150_init<0.05), 1, 0)
  err_init_de150[i] <- ifelse((c150_init<0.05), 1, 0)
  err_init_ee150[i] <- ifelse((d150_init<0.05), 1, 0)
  err_init_me150[i] <- ifelse((e150_init<0.05), 1, 0)
  err_init_max_hoch_150[i] <- ifelse(g150_hoch_init<0.05, 1, 0)
  err_init_max_exact_150[i] <- ifelse(g150_exact_init<0.05, 1, 0)

  err_init_oslrt200[i] <- ifelse((a200_init<0.05), 1, 0)
  err_init_moslrt200[i] <- ifelse((a2002_init<0.05), 1, 0)
  err_init_rc200[i] <- ifelse((b200_init<0.05), 1, 0)
  err_init_de200[i] <- ifelse((c200_init<0.05), 1, 0)              
  err_init_ee200[i] <- ifelse((d200_init<0.05), 1, 0)
  err_init_me200[i] <- ifelse((e200_init<0.05), 1, 0)
  err_init_max_hoch_200[i] <- ifelse(g200_hoch_init<0.05, 1, 0)
  err_init_max_exact_200[i] <- ifelse(g200_exact_init<0.05, 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_init <- sum(err_init_oslrt20)/nit
mos20_init <- sum(err_init_moslrt20)/nit
rc20_init <- sum(err_init_rc20)/nit
de20_init <- sum(err_init_de20)/nit
ee20_init <- sum(err_init_ee20)/nit
me20_init <- sum(err_init_me20)/nit
max_hoch20_init <- sum(err_init_max_hoch_20)/nit
max_exact20_init <- sum(err_init_max_exact_20)/nit

diff1_os20 <- ((os20_1-os20_init)/os20_init)*100
diff1_mos20 <- ((mos20_1-mos20_init)/mos20_init)*100
diff1_ee20 <- ((ee20_1-ee20_init)/ee20_init)*100
diff1_me20 <- ((me20_1-me20_init)/me20_init)*100
diff1_de20 <- ((de20_1-de20_init)/de20_init)*100
diff1_rc20 <- ((rc20_1-rc20_init)/rc20_init)*100
diff1_hoch20 <- ((max_hoch20_1-max_hoch20_init)/max_hoch20_init)*100
diff1_exact20 <- ((max_exact20_1-max_exact20_init)/max_exact20_init)*100

diff3_os20 <- ((os20_3-os20_init)/os20_init)*100
diff3_mos20 <- ((mos20_3-mos20_init)/mos20_init)*100
diff3_ee20 <- ((ee20_3-ee20_init)/ee20_init)*100
diff3_me20 <- ((me20_3-me20_init)/me20_init)*100
diff3_de20 <- ((de20_3-de20_init)/de20_init)*100
diff3_rc20 <- ((rc20_3-rc20_init)/rc20_init)*100
diff3_hoch20 <- ((max_hoch20_3-max_hoch20_init)/max_hoch20_init)*100
diff3_exact20 <- ((max_exact20_3-max_exact20_init)/max_exact20_init)*100

os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_init <- sum(err_init_oslrt30)/nit
mos30_init <- sum(err_init_moslrt30)/nit
rc30_init <- sum(err_init_rc30)/nit
de30_init <- sum(err_init_de30)/nit
ee30_init <- sum(err_init_ee30)/nit
me30_init <- sum(err_init_me30)/nit
max_hoch30_init <- sum(err_init_max_hoch_30)/nit
max_exact30_init <- sum(err_init_max_exact_30)/nit

diff1_os30 <- ((os30_1-os30_init)/os30_init)*100
diff1_mos30 <- ((mos30_1-mos30_init)/mos30_init)*100
diff1_ee30 <- ((ee30_1-ee30_init)/ee30_init)*100
diff1_me30 <- ((me30_1-me30_init)/me30_init)*100
diff1_de30 <- ((de30_1-de30_init)/de30_init)*100
diff1_rc30 <- ((rc30_1-rc30_init)/rc30_init)*100
diff1_hoch30 <- ((max_hoch30_1-max_hoch30_init)/max_hoch30_init)*100
diff1_exact30 <- ((max_exact30_1-max_exact30_init)/max_exact30_init)*100

diff3_os30 <- ((os30_3-os30_init)/os30_init)*100
diff3_mos30 <- ((mos30_3-mos30_init)/mos30_init)*100
diff3_ee30 <- ((ee30_3-ee30_init)/ee30_init)*100
diff3_me30 <- ((me30_3-me30_init)/me30_init)*100
diff3_de30 <- ((de30_3-de30_init)/de30_init)*100
diff3_rc30 <- ((rc30_3-rc30_init)/rc30_init)*100
diff3_hoch30 <- ((max_hoch30_3-max_hoch30_init)/max_hoch30_init)*100
diff3_exact30 <- ((max_exact30_3-max_exact30_init)/max_exact30_init)*100


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_init <- sum(err_init_oslrt50)/nit
mos50_init <- sum(err_init_moslrt50)/nit
rc50_init <- sum(err_init_rc50)/nit
de50_init <- sum(err_init_de50)/nit
ee50_init <- sum(err_init_ee50)/nit
me50_init <- sum(err_init_me50)/nit
max_hoch50_init <- sum(err_init_max_hoch_50)/nit
max_exact50_init <- sum(err_init_max_exact_50)/nit

diff1_os50 <- ((os50_1-os50_init)/os50_init)*100
diff1_mos50 <- ((mos50_1-mos50_init)/mos50_init)*100
diff1_ee50 <- ((ee50_1-ee50_init)/ee50_init)*100
diff1_me50 <- ((me50_1-me50_init)/me50_init)*100
diff1_de50 <- ((de50_1-de50_init)/de50_init)*100
diff1_rc50 <- ((rc50_1-rc50_init)/rc50_init)*100
diff1_hoch50 <- ((max_hoch50_1-max_hoch50_init)/max_hoch50_init)*100
diff1_exact50 <- ((max_exact50_1-max_exact50_init)/max_exact50_init)*100

diff3_os50 <- ((os50_3-os50_init)/os50_init)*100
diff3_mos50 <- ((mos50_3-mos50_init)/mos50_init)*100
diff3_ee50 <- ((ee50_3-ee50_init)/ee50_init)*100
diff3_me50 <- ((me50_3-me50_init)/me50_init)*100
diff3_de50 <- ((de50_3-de50_init)/de50_init)*100
diff3_rc50 <- ((rc50_3-rc50_init)/rc50_init)*100
diff3_hoch50 <- ((max_hoch50_3-max_hoch50_init)/max_hoch50_init)*100
diff3_exact50 <- ((max_exact50_3-max_exact50_init)/max_exact50_init)*100


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_init <- sum(err_init_oslrt60)/nit
mos60_init <- sum(err_init_moslrt60)/nit
rc60_init <- sum(err_init_rc60)/nit
de60_init <- sum(err_init_de60)/nit
ee60_init <- sum(err_init_ee60)/nit
me60_init <- sum(err_init_me60)/nit
max_hoch60_init <- sum(err_init_max_hoch_60)/nit
max_exact60_init <- sum(err_init_max_exact_60)/nit

diff1_os60 <- ((os60_1-os60_init)/os60_init)*100
diff1_mos60 <- ((mos60_1-mos60_init)/mos60_init)*100
diff1_ee60 <- ((ee60_1-ee60_init)/ee60_init)*100
diff1_me60 <- ((me60_1-me60_init)/me60_init)*100
diff1_de60 <- ((de60_1-de60_init)/de60_init)*100
diff1_rc60 <- ((rc60_1-rc60_init)/rc60_init)*100
diff1_hoch60 <- ((max_hoch60_1-max_hoch60_init)/max_hoch60_init)*100
diff1_exact60 <- ((max_exact60_1-max_exact60_init)/max_exact60_init)*100

diff3_os60 <- ((os60_3-os60_init)/os60_init)*100
diff3_mos60 <- ((mos60_3-mos60_init)/mos60_init)*100
diff3_ee60 <- ((ee60_3-ee60_init)/ee60_init)*100
diff3_me60 <- ((me60_3-me60_init)/me60_init)*100
diff3_de60 <- ((de60_3-de60_init)/de60_init)*100
diff3_rc60 <- ((rc60_3-rc60_init)/rc60_init)*100
diff3_hoch60 <- ((max_hoch60_3-max_hoch60_init)/max_hoch60_init)*100
diff3_exact60 <- ((max_exact60_3-max_exact60_init)/max_exact60_init)*100


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_init <- sum(err_init_oslrt80)/nit
mos80_init <- sum(err_init_moslrt80)/nit
rc80_init <- sum(err_init_rc80)/nit
de80_init <- sum(err_init_de80)/nit
ee80_init <- sum(err_init_ee80)/nit
me80_init <- sum(err_init_me80)/nit
max_hoch80_init <- sum(err_init_max_hoch_80)/nit
max_exact80_init <- sum(err_init_max_exact_80)/nit

diff1_os80 <- ((os80_1-os80_init)/os80_init)*100
diff1_mos80 <- ((mos80_1-mos80_init)/mos80_init)*100
diff1_ee80 <- ((ee80_1-ee80_init)/ee80_init)*100
diff1_me80 <- ((me80_1-me80_init)/me80_init)*100
diff1_de80 <- ((de80_1-de80_init)/de80_init)*100
diff1_rc80 <- ((rc80_1-rc80_init)/rc80_init)*100
diff1_hoch80 <- ((max_hoch80_1-max_hoch80_init)/max_hoch80_init)*100
diff1_exact80 <- ((max_exact80_1-max_exact80_init)/max_exact80_init)*100

diff3_os80 <- ((os80_3-os80_init)/os80_init)*100
diff3_mos80 <- ((mos80_3-mos80_init)/mos80_init)*100
diff3_ee80 <- ((ee80_3-ee80_init)/ee80_init)*100
diff3_me80 <- ((me80_3-me80_init)/me80_init)*100
diff3_de80 <- ((de80_3-de80_init)/de80_init)*100
diff3_rc80 <- ((rc80_3-rc80_init)/rc80_init)*100
diff3_hoch80 <- ((max_hoch80_3-max_hoch80_init)/max_hoch80_init)*100
diff3_exact80 <- ((max_exact80_3-max_exact80_init)/max_exact80_init)*100


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_init <- sum(err_init_oslrt100)/nit
mos100_init <- sum(err_init_moslrt100)/nit
rc100_init <- sum(err_init_rc100)/nit
de100_init <- sum(err_init_de100)/nit
ee100_init <- sum(err_init_ee100)/nit
me100_init <- sum(err_init_me100)/nit
max_hoch100_init <- sum(err_init_max_hoch_100)/nit
max_exact100_init <- sum(err_init_max_exact_100)/nit

diff1_os100 <- ((os100_1-os100_init)/os100_init)*100
diff1_mos100 <- ((mos100_1-mos100_init)/mos100_init)*100
diff1_ee100 <- ((ee100_1-ee100_init)/ee100_init)*100
diff1_me100 <- ((me100_1-me100_init)/me100_init)*100
diff1_de100 <- ((de100_1-de100_init)/de100_init)*100
diff1_rc100 <- ((rc100_1-rc100_init)/rc100_init)*100
diff1_hoch100 <- ((max_hoch100_1-max_hoch100_init)/max_hoch100_init)*100
diff1_exact100 <- ((max_exact100_1-max_exact100_init)/max_exact100_init)*100

diff3_os100 <- ((os100_3-os100_init)/os100_init)*100
diff3_mos100 <- ((mos100_3-mos100_init)/mos100_init)*100
diff3_ee100 <- ((ee100_3-ee100_init)/ee100_init)*100
diff3_me100 <- ((me100_3-me100_init)/me100_init)*100
diff3_de100 <- ((de100_3-de100_init)/de100_init)*100
diff3_rc100 <- ((rc100_3-rc100_init)/rc100_init)*100
diff3_hoch100 <- ((max_hoch100_3-max_hoch100_init)/max_hoch100_init)*100
diff3_exact100 <- ((max_exact100_3-max_exact100_init)/max_exact100_init)*100


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_init <- sum(err_init_oslrt150)/nit
mos150_init <- sum(err_init_moslrt150)/nit
rc150_init <- sum(err_init_rc150)/nit
de150_init <- sum(err_init_de150)/nit
ee150_init <- sum(err_init_ee150)/nit
me150_init <- sum(err_init_me150)/nit
max_hoch150_init <- sum(err_init_max_hoch_150)/nit
max_exact150_init <- sum(err_init_max_exact_150)/nit

diff1_os150 <- ((os150_1-os150_init)/os150_init)*100
diff1_mos150 <- ((mos150_1-mos150_init)/mos150_init)*100
diff1_ee150 <- ((ee150_1-ee150_init)/ee150_init)*100
diff1_me150 <- ((me150_1-me150_init)/me150_init)*100
diff1_de150 <- ((de150_1-de150_init)/de150_init)*100
diff1_rc150 <- ((rc150_1-rc150_init)/rc150_init)*100
diff1_hoch150 <- ((max_hoch150_1-max_hoch150_init)/max_hoch150_init)*100
diff1_exact150 <- ((max_exact150_1-max_exact150_init)/max_exact150_init)*100

diff3_os150 <- ((os150_3-os150_init)/os150_init)*100
diff3_mos150 <- ((mos150_3-mos150_init)/mos150_init)*100
diff3_ee150 <- ((ee150_3-ee150_init)/ee150_init)*100
diff3_me150 <- ((me150_3-me150_init)/me150_init)*100
diff3_de150 <- ((de150_3-de150_init)/de150_init)*100
diff3_rc150 <- ((rc150_3-rc150_init)/rc150_init)*100
diff3_hoch150 <- ((max_hoch150_3-max_hoch150_init)/max_hoch150_init)*100
diff3_exact150 <- ((max_exact150_3-max_exact150_init)/max_exact150_init)*100


os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_init <- sum(err_init_oslrt200)/nit
mos200_init <- sum(err_init_moslrt200)/nit
rc200_init <- sum(err_init_rc200)/nit
de200_init <- sum(err_init_de200)/nit
ee200_init <- sum(err_init_ee200)/nit
me200_init <- sum(err_init_me200)/nit
r_200_init <- sum(err_init_rmst_200)/nit
max_hoch200_init <- sum(err_init_max_hoch_200)/nit
max_exact200_init <- sum(err_init_max_exact_200)/nit

diff1_os200 <- ((os200_1-os200_init)/os200_init)*100
diff1_mos200 <- ((mos200_1-mos200_init)/mos200_init)*100
diff1_ee200 <- ((ee200_1-ee200_init)/ee200_init)*100
diff1_me200 <- ((me200_1-me200_init)/me200_init)*100
diff1_de200 <- ((de200_1-de200_init)/de200_init)*100
diff1_rc200 <- ((rc200_1-rc200_init)/rc200_init)*100
diff1_hoch200 <- ((max_hoch200_1-max_hoch200_init)/max_hoch200_init)*100
diff1_exact200 <- ((max_exact200_1-max_exact200_init)/max_exact200_init)*100

diff3_os200 <- ((os200_3-os200_init)/os200_init)*100
diff3_mos200 <- ((mos200_3-mos200_init)/mos200_init)*100
diff3_ee200 <- ((ee200_3-ee200_init)/ee200_init)*100
diff3_me200 <- ((me200_3-me200_init)/me200_init)*100
diff3_de200 <- ((de200_3-de200_init)/de200_init)*100
diff3_rc200 <- ((rc200_3-rc200_init)/rc200_init)*100
diff3_hoch200 <- ((max_hoch200_3-max_hoch200_init)/max_hoch200_init)*100
diff3_exact200 <- ((max_exact200_3-max_exact200_init)/max_exact200_init)*100


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
diff1_osl <- c(diff1_os20, diff1_os30, diff1_os50, diff1_os60, diff1_os80, diff1_os100, diff1_os150, diff1_os200)
diff1_mosl <- c(diff1_mos20, diff1_mos30, diff1_mos50, diff1_mos60, diff1_mos80, diff1_mos100, diff1_mos150, diff1_mos200)
diff1_rc <- c(diff1_rc20, diff1_rc30, diff1_rc50, diff1_rc60, diff1_rc80, diff1_rc100, diff1_rc150, diff1_rc200)
diff1_de <- c(diff1_de20, diff1_de30, diff1_de50, diff1_de60, diff1_de80, diff1_de100, diff1_de150, diff1_de200)
diff1_ee <- c(diff1_ee20, diff1_ee30, diff1_ee50, diff1_ee60, diff1_ee80, diff1_ee100, diff1_ee150, diff1_ee200)
diff1_me <- c(diff1_me20, diff1_me30, diff1_me50, diff1_me60, diff1_me80, diff1_me100, diff1_me150, diff1_me200)
diff1_hoch <- c(diff1_hoch20, diff1_hoch30, diff1_hoch50, diff1_hoch60, diff1_hoch80, diff1_hoch100, diff1_hoch150, diff1_hoch200)
diff1_exact <- c(diff1_exact20, diff1_exact30, diff1_exact50, diff1_exact60, diff1_exact80, diff1_exact100, diff1_exact150, diff1_exact200)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
diff3_osl <- c(diff3_os20, diff3_os30, diff3_os50, diff3_os60, diff3_os80, diff3_os100, diff3_os150, diff3_os200)
diff3_mosl <- c(diff3_mos20, diff3_mos30, diff3_mos50, diff3_mos60, diff3_mos80, diff3_mos100, diff3_mos150, diff3_mos200)
diff3_rc <- c(diff3_rc20, diff3_rc30, diff3_rc50, diff3_rc60, diff3_rc80, diff3_rc100, diff3_rc150, diff3_rc200)
diff3_de <- c(diff3_de20, diff3_de30, diff3_de50, diff3_de60, diff3_de80, diff3_de100, diff3_de150, diff3_de200)
diff3_ee <- c(diff3_ee20, diff3_ee30, diff3_ee50, diff3_ee60, diff3_ee80, diff3_ee100, diff3_ee150, diff3_ee200)
diff3_me <- c(diff3_me20, diff3_me30, diff3_me50, diff3_me60, diff3_me80, diff3_me100, diff3_me150, diff3_me200)
diff3_hoch <- c(diff3_hoch20, diff3_hoch30, diff3_hoch50, diff3_hoch60, diff3_hoch80, diff3_hoch100, diff3_hoch150, diff3_hoch200)
diff3_exact <- c(diff3_exact20, diff3_exact30, diff3_exact50, diff3_exact60, diff3_exact80, diff3_exact100, diff3_exact150, diff3_exact200)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

CH_1 <- ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 6: crossing hazards',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
CH_1

d2_1 <- data.frame(Sample.size = n, Error = c(diff1_osl, diff1_mosl, diff1_ee, diff1_me, diff1_de, diff1_rc, diff1_hoch, diff1_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_1$Test <- as.factor(d2_1$Test)
d2_1$Test <- factor(d2_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_CH_1 <- ggplot(d2_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_1$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 6: crossing hazards',
       subtitle = 'pi = 1')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_CH_1


d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

CH_3 <- ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size',
       y = 'Power',
       title = 'Scenario 6: crossing hazards',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))
CH_3

d2_3 <- data.frame(Sample.size = n, Error = c(diff3_osl, diff3_mosl, diff3_ee, diff3_me, diff3_de, diff3_rc, diff3_hoch, diff3_exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), 
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d2_3$Test <- as.factor(d2_3$Test)
d2_3$Test <- factor(d2_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (multivariate normal integration)'))

Diff_CH_3 <- ggplot(d2_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(breaks = n, labels = d2_3$Sample.size[1:8], name = 'Sample size')+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size', 
       y = 'Relative difference (%)',
       title = 'Scenario 6: crossing hazards',
       subtitle = 'pi = 0.6')+
  geom_hline(yintercept = 0, size = 0.2)+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x.top = element_text(size = 12, angle = 90),
        axis.text.x.bottom = element_text(size = 12, angle = 90),
        axis.title.x.top = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
Diff_CH_3




####Figure 5####
fig5 <-  Diff_Eff_3 + Diff_PH_3 + Diff_EE_3 + Diff_ME_3 + Diff_DE_3 + Diff_CH_3 + plot_layout(guides = 'collect', ncol = 3) & theme(legend.position = 'bottom')
fig5
