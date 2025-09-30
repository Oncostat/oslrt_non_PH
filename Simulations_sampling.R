source(file = "Tests_functions.r", echo = FALSE)

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
#m1 <- 2   #Null effect HR = 1
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
#HR = 1 (0 0.02 0.07 0.12 0.2)
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
pi2 <- 0.8
pi3 <- 0.6
pi4 <- 0.5

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_2 <- rep(0, nit)
err_moslrt20_2 <- rep(0, nit)
err_rc20_2 <- rep(0, nit)
err_de20_2 <- rep(0, nit)
err_ee20_2 <- rep(0, nit)
err_me20_2 <- rep(0, nit)
err_max1_hoch20_2 <- rep(0, nit)
err_max1_exact20_2 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)

err_oslrt20_4 <- rep(0, nit)
err_moslrt20_4 <- rep(0, nit)
err_rc20_4 <- rep(0, nit)
err_de20_4 <- rep(0, nit)
err_ee20_4 <- rep(0, nit)
err_me20_4 <- rep(0, nit)
err_max1_hoch20_4 <- rep(0, nit)
err_max1_exact20_4 <- rep(0, nit)

tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_2 <- rep(0, nit)
err_moslrt30_2 <- rep(0, nit)
err_rc30_2 <- rep(0, nit)
err_de30_2 <- rep(0, nit)
err_ee30_2 <- rep(0, nit)
err_me30_2 <- rep(0, nit)
err_max1_hoch30_2 <- rep(0, nit)
err_max1_exact30_2 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)

err_oslrt30_4 <- rep(0, nit)
err_moslrt30_4 <- rep(0, nit)
err_rc30_4 <- rep(0, nit)
err_de30_4 <- rep(0, nit)
err_ee30_4 <- rep(0, nit)
err_me30_4 <- rep(0, nit)
err_max1_hoch30_4 <- rep(0, nit)
err_max1_exact30_4 <- rep(0, nit)

tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_2 <- rep(0, nit)
err_moslrt50_2 <- rep(0, nit)
err_rc50_2 <- rep(0, nit)
err_de50_2 <- rep(0, nit)
err_ee50_2 <- rep(0, nit)
err_me50_2 <- rep(0, nit)
err_max1_hoch50_2 <- rep(0, nit)
err_max1_exact50_2 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)

err_oslrt50_4 <- rep(0, nit)
err_moslrt50_4 <- rep(0, nit)
err_rc50_4 <- rep(0, nit)
err_de50_4 <- rep(0, nit)
err_ee50_4 <- rep(0, nit)
err_me50_4 <- rep(0, nit)
err_max1_hoch50_4 <- rep(0, nit)
err_max1_exact50_4 <- rep(0, nit)

tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_2 <- rep(0, nit)
err_moslrt60_2 <- rep(0, nit)
err_rc60_2 <- rep(0, nit)
err_de60_2 <- rep(0, nit)
err_ee60_2 <- rep(0, nit)
err_me60_2 <- rep(0, nit)
err_max1_hoch60_2 <- rep(0, nit)
err_max1_exact60_2 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)

err_oslrt60_4 <- rep(0, nit)
err_moslrt60_4 <- rep(0, nit)
err_rc60_4 <- rep(0, nit)
err_de60_4 <- rep(0, nit)
err_ee60_4 <- rep(0, nit)
err_me60_4 <- rep(0, nit)
err_max1_hoch60_4 <- rep(0, nit)
err_max1_exact60_4 <- rep(0, nit)

tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_2 <- rep(0, nit)
err_moslrt80_2 <- rep(0, nit)
err_rc80_2 <- rep(0, nit)
err_de80_2 <- rep(0, nit)
err_ee80_2 <- rep(0, nit)
err_me80_2 <- rep(0, nit)
err_max1_hoch80_2 <- rep(0, nit)
err_max1_exact80_2 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)

err_oslrt80_4 <- rep(0, nit)
err_moslrt80_4 <- rep(0, nit)
err_rc80_4 <- rep(0, nit)
err_de80_4 <- rep(0, nit)
err_ee80_4 <- rep(0, nit)
err_me80_4 <- rep(0, nit)
err_max1_hoch80_4 <- rep(0, nit)
err_max1_exact80_4 <- rep(0, nit)

tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_2 <- rep(0, nit)
err_moslrt100_2 <- rep(0, nit)
err_rc100_2 <- rep(0, nit)
err_de100_2 <- rep(0, nit)
err_ee100_2 <- rep(0, nit)
err_me100_2 <- rep(0, nit)
err_max1_hoch100_2 <- rep(0, nit)
err_max1_exact100_2 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)

err_oslrt100_4 <- rep(0, nit)
err_moslrt100_4 <- rep(0, nit)
err_rc100_4 <- rep(0, nit)
err_de100_4 <- rep(0, nit)
err_ee100_4 <- rep(0, nit)
err_me100_4 <- rep(0, nit)
err_max1_hoch100_4 <- rep(0, nit)
err_max1_exact100_4 <- rep(0, nit)

tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_2 <- rep(0, nit)
err_moslrt150_2 <- rep(0, nit)
err_rc150_2 <- rep(0, nit)
err_de150_2 <- rep(0, nit)
err_ee150_2 <- rep(0, nit)
err_me150_2 <- rep(0, nit)
err_max1_hoch150_2 <- rep(0, nit)
err_max1_exact150_2 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)

err_oslrt150_4 <- rep(0, nit)
err_moslrt150_4 <- rep(0, nit)
err_rc150_4 <- rep(0, nit)
err_de150_4 <- rep(0, nit)
err_ee150_4 <- rep(0, nit)
err_me150_4 <- rep(0, nit)
err_max1_hoch150_4 <- rep(0, nit)
err_max1_exact150_4 <- rep(0, nit)

tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_2 <- rep(0, nit)
err_moslrt200_2 <- rep(0, nit)
err_rc200_2 <- rep(0, nit)
err_de200_2 <- rep(0, nit)
err_ee200_2 <- rep(0, nit)
err_me200_2 <- rep(0, nit)
err_max1_hoch200_2 <- rep(0, nit)
err_max1_exact200_2 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_oslrt200_4 <- rep(0, nit)
err_moslrt200_4 <- rep(0, nit)
err_rc200_4 <- rep(0, nit)
err_de200_4 <- rep(0, nit)
err_ee200_4 <- rep(0, nit)
err_me200_4 <- rep(0, nit)
err_max1_hoch200_4 <- rep(0, nit)
err_max1_exact200_4 <- rep(0, nit)

tx_cens200 <- rep(0, nit)
tx_censadm200 <- rep(0, nit)

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
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  a202_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)[2]
  b20_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  c20_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi2)[2]
  d20_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi2)[2]
  e20_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f20_2 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a20_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  a202_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)[2]
  b20_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  c20_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi4)[2]
  d20_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi4)[2]
  e20_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f20_4 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  a302_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)[2]
  b30_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  c30_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi2)[2]
  d30_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi2)[2]
  e30_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f30_2 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a30_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  a302_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)[2]
  b30_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  c30_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi4)[2]
  d30_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi4)[2]
  e30_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f30_4 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  a502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)[2]
  b50_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  c50_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi2)[2]
  d50_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi2)[2]
  e50_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f50_2 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a50_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  a502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)[2]
  b50_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  c50_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi4)[2]
  d50_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi4)[2]
  e50_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f50_4 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  a602_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)[2]
  b60_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  c60_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi2)[2]
  d60_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi2)[2]
  e60_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f60_2 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a60_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  a602_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)[2]
  b60_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  c60_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi4)[2]
  d60_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi4)[2]
  e60_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f60_4 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  a802_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)[2]
  b80_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  c80_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi2)[2]
  d80_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi2)[2]
  e80_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f80_2 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a80_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  a802_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)[2]
  b80_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  c80_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi4)[2]
  d80_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi4)[2]
  e80_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f80_4 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  a1002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)[2]
  b100_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  c100_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi2)[2]
  d100_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi2)[2]
  e100_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f100_2 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a100_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  a1002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)[2]
  b100_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  c100_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi4)[2]
  d100_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi4)[2]
  e100_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f100_4 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  a1502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)[2]
  b150_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  c150_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi2)[2]
  d150_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi2)[2]
  e150_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f150_2 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a150_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  a1502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)[2]
  b150_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  c150_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi4)[2]
  d150_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi4)[2]
  e150_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f150_4 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  a2002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)[2]
  b200_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  c200_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi2)[2]
  d200_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi2)[2]
  e200_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f200_2 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a200_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  a2002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)[2]
  b200_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  c200_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi4)[2]
  d200_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi4)[2]
  e200_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f200_4 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  #censoring rate with adm censoring
  tx_censadm20[i] <- 1-sum(delta20)/20
  tx_censadm30[i] <- 1-sum(delta30)/30
  tx_censadm50[i] <- 1-sum(delta50)/50
  tx_censadm60[i] <- 1-sum(delta60)/60
  tx_censadm80[i] <- 1-sum(delta80)/80
  tx_censadm100[i] <- 1-sum(delta100)/100
  tx_censadm150[i] <- 1-sum(delta150)/150
  tx_censadm200[i] <- 1-sum(delta200)/200
  
  #censoring rate without adm censoring
  tx_cens20[i] <- 1-sum(del20)/20
  tx_cens30[i] <- 1-sum(del30)/30
  tx_cens50[i] <- 1-sum(del50)/50
  tx_cens60[i] <- 1-sum(del60)/60
  tx_cens80[i] <- 1-sum(del80)/80
  tx_cens100[i] <- 1-sum(del100)/100
  tx_cens150[i] <- 1-sum(del150)/150
  tx_cens200[i] <- 1-sum(del200)/200
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_2[i] <- ifelse((a20_2<0.05), 1, 0)
  err_moslrt20_2[i] <- ifelse((a202_2<0.05), 1, 0)
  err_rc20_2[i] <- ifelse((b20_2<0.05), 1, 0)
  err_de20_2[i] <- ifelse((c20_2<0.05), 1, 0)
  err_ee20_2[i] <- ifelse((d20_2<0.05), 1, 0)
  err_me20_2[i] <- ifelse((e20_2<0.05), 1, 0)
  err_max1_hoch20_2[i] <- ifelse((f20_2[2]<0.05), 1, 0)
  err_max1_exact20_2[i] <- ifelse((f20_2[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  err_oslrt20_4[i] <- ifelse((a20_4<0.05), 1, 0)
  err_moslrt20_4[i] <- ifelse((a202_4<0.05), 1, 0)
  err_rc20_4[i] <- ifelse((b20_4<0.05), 1, 0)
  err_de20_4[i] <- ifelse((c20_4<0.05), 1, 0)
  err_ee20_4[i] <- ifelse((d20_4<0.05), 1, 0)
  err_me20_4[i] <- ifelse((e20_4<0.05), 1, 0)
  err_max1_hoch20_4[i] <- ifelse((f20_4[2]<0.05), 1, 0)
  err_max1_exact20_4[i] <- ifelse((f20_4[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_2[i] <- ifelse((a30_2<0.05), 1, 0)
  err_moslrt30_2[i] <- ifelse((a302_2<0.05), 1, 0)
  err_rc30_2[i] <- ifelse((b30_2<0.05), 1, 0)
  err_de30_2[i] <- ifelse((c30_2<0.05), 1, 0)
  err_ee30_2[i] <- ifelse((d30_2<0.05), 1, 0)
  err_me30_2[i] <- ifelse((e30_2<0.05), 1, 0)
  err_max1_hoch30_2[i] <- ifelse((f30_2[2]<0.05), 1, 0)
  err_max1_exact30_2[i] <- ifelse((f30_2[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  err_oslrt30_4[i] <- ifelse((a30_4<0.05), 1, 0)
  err_moslrt30_4[i] <- ifelse((a302_4<0.05), 1, 0)
  err_rc30_4[i] <- ifelse((b30_4<0.05), 1, 0)
  err_de30_4[i] <- ifelse((c30_4<0.05), 1, 0)
  err_ee30_4[i] <- ifelse((d30_4<0.05), 1, 0)
  err_me30_4[i] <- ifelse((e30_4<0.05), 1, 0)
  err_max1_hoch30_4[i] <- ifelse((f30_4[2]<0.05), 1, 0)
  err_max1_exact30_4[i] <- ifelse((f30_4[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_2[i] <- ifelse((a50_2<0.05), 1, 0)
  err_moslrt50_2[i] <- ifelse((a502_2<0.05), 1, 0)
  err_rc50_2[i] <- ifelse((b50_2<0.05), 1, 0)
  err_de50_2[i] <- ifelse((c50_2<0.05), 1, 0)
  err_ee50_2[i] <- ifelse((d50_2<0.05), 1, 0)
  err_me50_2[i] <- ifelse((e50_2<0.05), 1, 0)
  err_max1_hoch50_2[i] <- ifelse((f50_2[2]<0.05), 1, 0)
  err_max1_exact50_2[i] <- ifelse((f50_2[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  err_oslrt50_4[i] <- ifelse((a50_4<0.05), 1, 0)
  err_moslrt50_4[i] <- ifelse((a502_4<0.05), 1, 0)
  err_rc50_4[i] <- ifelse((b50_4<0.05), 1, 0)
  err_de50_4[i] <- ifelse((c50_4<0.05), 1, 0)
  err_ee50_4[i] <- ifelse((d50_4<0.05), 1, 0)
  err_me50_4[i] <- ifelse((e50_4<0.05), 1, 0)
  err_max1_hoch50_4[i] <- ifelse((f50_4[2]<0.05), 1, 0)
  err_max1_exact50_4[i] <- ifelse((f50_4[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_2[i] <- ifelse((a60_2<0.05), 1, 0)
  err_moslrt60_2[i] <- ifelse((a602_2<0.05), 1, 0)
  err_rc60_2[i] <- ifelse((b60_2<0.05), 1, 0)
  err_de60_2[i] <- ifelse((c60_2<0.05), 1, 0)
  err_ee60_2[i] <- ifelse((d60_2<0.05), 1, 0)
  err_me60_2[i] <- ifelse((e60_2<0.05), 1, 0)
  err_max1_hoch60_2[i] <- ifelse((f60_2[2]<0.05), 1, 0)
  err_max1_exact60_2[i] <- ifelse((f60_2[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  err_oslrt60_4[i] <- ifelse((a60_4<0.05), 1, 0)
  err_moslrt60_4[i] <- ifelse((a602_4<0.05), 1, 0)
  err_rc60_4[i] <- ifelse((b60_4<0.05), 1, 0)
  err_de60_4[i] <- ifelse((c60_4<0.05), 1, 0)
  err_ee60_4[i] <- ifelse((d60_4<0.05), 1, 0)
  err_me60_4[i] <- ifelse((e60_4<0.05), 1, 0)
  err_max1_hoch60_4[i] <- ifelse((f60_4[2]<0.05), 1, 0)
  err_max1_exact60_4[i] <- ifelse((f60_4[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_2[i] <- ifelse((a80_2<0.05), 1, 0)
  err_moslrt80_2[i] <- ifelse((a802_2<0.05), 1, 0)
  err_rc80_2[i] <- ifelse((b80_2<0.05), 1, 0)
  err_de80_2[i] <- ifelse((c80_2<0.05), 1, 0)
  err_ee80_2[i] <- ifelse((d80_2<0.05), 1, 0)
  err_me80_2[i] <- ifelse((e80_2<0.05), 1, 0)
  err_max1_hoch80_2[i] <- ifelse((f80_2[2]<0.05), 1, 0)
  err_max1_exact80_2[i] <- ifelse((f80_2[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  err_oslrt80_4[i] <- ifelse((a80_4<0.05), 1, 0)
  err_moslrt80_4[i] <- ifelse((a802_4<0.05), 1, 0)
  err_rc80_4[i] <- ifelse((b80_4<0.05), 1, 0)
  err_de80_4[i] <- ifelse((c80_4<0.05), 1, 0)
  err_ee80_4[i] <- ifelse((d80_4<0.05), 1, 0)
  err_me80_4[i] <- ifelse((e80_4<0.05), 1, 0)
  err_max1_hoch80_4[i] <- ifelse((f80_4[2]<0.05), 1, 0)
  err_max1_exact80_4[i] <- ifelse((f80_4[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_2[i] <- ifelse((a100_2<0.05), 1, 0)
  err_moslrt100_2[i] <- ifelse((a1002_2<0.05), 1, 0)
  err_rc100_2[i] <- ifelse((b100_2<0.05), 1, 0)
  err_de100_2[i] <- ifelse((c100_2<0.05), 1, 0)
  err_ee100_2[i] <- ifelse((d100_2<0.05), 1, 0)
  err_me100_2[i] <- ifelse((e100_2<0.05), 1, 0)
  err_max1_hoch100_2[i] <- ifelse((f100_2[2]<0.05), 1, 0)
  err_max1_exact100_2[i] <- ifelse((f100_2[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  err_oslrt100_4[i] <- ifelse((a100_4<0.05), 1, 0)
  err_moslrt100_4[i] <- ifelse((a1002_4<0.05), 1, 0)
  err_rc100_4[i] <- ifelse((b100_4<0.05), 1, 0)
  err_de100_4[i] <- ifelse((c100_4<0.05), 1, 0)
  err_ee100_4[i] <- ifelse((d100_4<0.05), 1, 0)
  err_me100_4[i] <- ifelse((e100_4<0.05), 1, 0)
  err_max1_hoch100_4[i] <- ifelse((f100_4[2]<0.05), 1, 0)
  err_max1_exact100_4[i] <- ifelse((f100_4[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_2[i] <- ifelse((a150_2<0.05), 1, 0)
  err_moslrt150_2[i] <- ifelse((a1502_2<0.05), 1, 0)
  err_rc150_2[i] <- ifelse((b150_2<0.05), 1, 0)
  err_de150_2[i] <- ifelse((c150_2<0.05), 1, 0)
  err_ee150_2[i] <- ifelse((d150_2<0.05), 1, 0)
  err_me150_2[i] <- ifelse((e150_2<0.05), 1, 0)
  err_max1_hoch150_2[i] <- ifelse((f150_2[2]<0.05), 1, 0)
  err_max1_exact150_2[i] <- ifelse((f150_2[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  err_oslrt150_4[i] <- ifelse((a150_4<0.05), 1, 0)
  err_moslrt150_4[i] <- ifelse((a1502_4<0.05), 1, 0)
  err_rc150_4[i] <- ifelse((b150_4<0.05), 1, 0)
  err_de150_4[i] <- ifelse((c150_4<0.05), 1, 0)
  err_ee150_4[i] <- ifelse((d150_4<0.05), 1, 0)
  err_me150_4[i] <- ifelse((e150_4<0.05), 1, 0)
  err_max1_hoch150_4[i] <- ifelse((f150_4[2]<0.05), 1, 0)
  err_max1_exact150_4[i] <- ifelse((f150_4[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_2[i] <- ifelse((a200_2<0.05), 1, 0)
  err_moslrt200_2[i] <- ifelse((a2002_2<0.05), 1, 0)
  err_rc200_2[i] <- ifelse((b200_2<0.05), 1, 0)
  err_de200_2[i] <- ifelse((c200_2<0.05), 1, 0)
  err_ee200_2[i] <- ifelse((d200_2<0.05), 1, 0)
  err_me200_2[i] <- ifelse((e200_2<0.05), 1, 0)
  err_max1_hoch200_2[i] <- ifelse((f200_2[2]<0.05), 1, 0)
  err_max1_exact200_2[i] <- ifelse((f200_2[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)
  
  err_oslrt200_4[i] <- ifelse((a200_4<0.05), 1, 0)
  err_moslrt200_4[i] <- ifelse((a2002_4<0.05), 1, 0)
  err_rc200_4[i] <- ifelse((b200_4<0.05), 1, 0)
  err_de200_4[i] <- ifelse((c200_4<0.05), 1, 0)
  err_ee200_4[i] <- ifelse((d200_4<0.05), 1, 0)
  err_me200_4[i] <- ifelse((e200_4<0.05), 1, 0)
  err_max1_hoch200_4[i] <- ifelse((f200_4[2]<0.05), 1, 0)
  err_max1_exact200_4[i] <- ifelse((f200_4[3]<0.05), 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_2 <- sum(err_oslrt20_2)/nit
mos20_2 <- sum(err_moslrt20_2)/nit
rc20_2 <- sum(err_rc20_2)/nit
de20_2 <- sum(na.omit(err_de20_2))/length(na.omit(err_de20_2))
ee20_2 <- sum(err_ee20_2)/nit
me20_2 <- sum(err_me20_2)/nit
max1_hoch20_2 <- sum(err_max1_hoch20_2)/nit
max1_exact20_2 <- sum(na.omit(err_max1_exact20_2))/length(na.omit(err_max1_exact20_2))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_4 <- sum(err_oslrt20_4)/nit
mos20_4 <- sum(err_moslrt20_4)/nit
rc20_4 <- sum(err_rc20_4)/nit
de20_4 <- sum(na.omit(err_de20_4))/length(na.omit(err_de20_4))
ee20_4 <- sum(err_ee20_4)/nit
me20_4 <- sum(err_me20_4)/nit
max1_hoch20_4 <- sum(err_max1_hoch20_4)/nit
max1_exact20_4 <- sum(na.omit(err_max1_exact20_4))/length(na.omit(err_max1_exact20_4))

mean(tx_cens20)
mean(tx_censadm20)


os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_2 <- sum(err_oslrt30_2)/nit
mos30_2 <- sum(err_moslrt30_2)/nit
rc30_2 <- sum(err_rc30_2)/nit
de30_2 <- sum(na.omit(err_de30_2))/length(na.omit(err_de30_2))
ee30_2 <- sum(err_ee30_2)/nit
me30_2 <- sum(err_me30_2)/nit
max1_hoch30_2 <- sum(err_max1_hoch30_2)/nit
max1_exact30_2 <- sum(na.omit(err_max1_exact30_2))/length(na.omit(err_max1_exact30_2))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_4 <- sum(err_oslrt30_4)/nit
mos30_4 <- sum(err_moslrt30_4)/nit
rc30_4 <- sum(err_rc30_4)/nit
de30_4 <- sum(na.omit(err_de30_4))/length(na.omit(err_de30_4))
ee30_4 <- sum(err_ee30_4)/nit
me30_4 <- sum(err_me30_4)/nit
max1_hoch30_4 <- sum(err_max1_hoch30_4)/nit
max1_exact30_4 <- sum(na.omit(err_max1_exact30_4))/length(na.omit(err_max1_exact30_4))

mean(tx_cens30)
mean(tx_censadm30)


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_2 <- sum(err_oslrt50_2)/nit
mos50_2 <- sum(err_moslrt50_2)/nit
rc50_2 <- sum(err_rc50_2)/nit
de50_2 <- sum(err_de50_2)/nit
ee50_2 <- sum(err_ee50_2)/nit
me50_2 <- sum(err_me50_2)/nit
max1_hoch50_2 <- sum(err_max1_hoch50_2)/nit
max1_exact50_2 <- sum(na.omit(err_max1_exact50_2))/length(na.omit(err_max1_exact50_2))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_4 <- sum(err_oslrt50_4)/nit
mos50_4 <- sum(err_moslrt50_4)/nit
rc50_4 <- sum(err_rc50_4)/nit
de50_4 <- sum(err_de50_4)/nit
ee50_4 <- sum(err_ee50_4)/nit
me50_4 <- sum(err_me50_4)/nit
max1_hoch50_4 <- sum(err_max1_hoch50_4)/nit
max1_exact50_4 <- sum(na.omit(err_max1_exact50_4))/length(na.omit(err_max1_exact50_4))

mean(tx_cens50)
mean(tx_censadm50)


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_2 <- sum(err_oslrt60_2)/nit
mos60_2 <- sum(err_moslrt60_2)/nit
rc60_2 <- sum(err_rc60_2)/nit
de60_2 <- sum(err_de60_2)/nit
ee60_2 <- sum(err_ee60_2)/nit
me60_2 <- sum(err_me60_2)/nit
max1_hoch60_2 <- sum(err_max1_hoch60_2)/nit
max1_exact60_2 <- sum(na.omit(err_max1_exact60_2))/length(na.omit(err_max1_exact60_2))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_4 <- sum(err_oslrt60_4)/nit
mos60_4 <- sum(err_moslrt60_4)/nit
rc60_4 <- sum(err_rc60_4)/nit
de60_4 <- sum(err_de60_4)/nit
ee60_4 <- sum(err_ee60_4)/nit
me60_4 <- sum(err_me60_4)/nit
max1_hoch60_4 <- sum(err_max1_hoch60_4)/nit
max1_exact60_4 <- sum(na.omit(err_max1_exact60_4))/length(na.omit(err_max1_exact60_4))

mean(tx_cens60)
mean(tx_censadm60)


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_2 <- sum(err_oslrt80_2)/nit
mos80_2 <- sum(err_moslrt80_2)/nit
rc80_2 <- sum(err_rc80_2)/nit
de80_2 <- sum(err_de80_2)/nit
ee80_2 <- sum(err_ee80_2)/nit
me80_2 <- sum(err_me80_2)/nit
max1_hoch80_2 <- sum(err_max1_hoch80_2)/nit
max1_exact80_2 <- sum(na.omit(err_max1_exact80_2))/length(na.omit(err_max1_exact80_2))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_4 <- sum(err_oslrt80_4)/nit
mos80_4 <- sum(err_moslrt80_4)/nit
rc80_4 <- sum(err_rc80_4)/nit
de80_4 <- sum(err_de80_4)/nit
ee80_4 <- sum(err_ee80_4)/nit
me80_4 <- sum(err_me80_4)/nit
max1_hoch80_4 <- sum(err_max1_hoch80_4)/nit
max1_exact80_4 <- sum(na.omit(err_max1_exact80_4))/length(na.omit(err_max1_exact80_4))

mean(tx_cens80)
mean(tx_censadm80)


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_2 <- sum(err_oslrt100_2)/nit
mos100_2 <- sum(err_moslrt100_2)/nit
rc100_2 <- sum(err_rc100_2)/nit
de100_2 <- sum(err_de100_2)/nit
ee100_2 <- sum(err_ee100_2)/nit
me100_2 <- sum(err_me100_2)/nit
max1_hoch100_2 <- sum(err_max1_hoch100_2)/nit
max1_exact100_2 <- sum(na.omit(err_max1_exact100_2))/length(na.omit(err_max1_exact100_2))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_4 <- sum(err_oslrt100_4)/nit
mos100_4 <- sum(err_moslrt100_4)/nit
rc100_4 <- sum(err_rc100_4)/nit
de100_4 <- sum(err_de100_4)/nit
ee100_4 <- sum(err_ee100_4)/nit
me100_4 <- sum(err_me100_4)/nit
max1_hoch100_4 <- sum(err_max1_hoch100_4)/nit
max1_exact100_4 <- sum(na.omit(err_max1_exact100_4))/length(na.omit(err_max1_exact100_4))

mean(tx_cens100)
mean(tx_censadm100)


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_2 <- sum(err_oslrt150_2)/nit
mos150_2 <- sum(err_moslrt150_2)/nit
rc150_2 <- sum(err_rc150_2)/nit
de150_2 <- sum(err_de150_2)/nit
ee150_2 <- sum(err_ee150_2)/nit
me150_2 <- sum(na.omit(err_me150_2))/length(na.omit(err_me150_2))
max1_hoch150_2 <- sum(err_max1_hoch150_2)/nit
max1_exact150_2 <- sum(na.omit(err_max1_exact150_2))/length(na.omit(err_max1_exact150_2))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_4 <- sum(err_oslrt150_4)/nit
mos150_4 <- sum(err_moslrt150_4)/nit
rc150_4 <- sum(err_rc150_4)/nit
de150_4 <- sum(err_de150_4)/nit
ee150_4 <- sum(err_ee150_4)/nit
me150_4 <- sum(na.omit(err_me150_4))/length(na.omit(err_me150_4))
max1_hoch150_4 <- sum(err_max1_hoch150_4)/nit
max1_exact150_4 <- sum(na.omit(err_max1_exact150_4))/length(na.omit(err_max1_exact150_4))

mean(tx_cens150)
mean(tx_censadm150)

os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_2 <- sum(err_oslrt200_2)/nit
mos200_2 <- sum(err_moslrt200_2)/nit
rc200_2 <- sum(err_rc200_2)/nit
de200_2 <- sum(err_de200_2)/nit
ee200_2 <- sum(err_ee200_2)/nit
me200_2 <- sum(err_me200_2)/nit
max1_hoch200_2 <- sum(err_max1_hoch200_2)/nit
max1_exact200_2 <- sum(na.omit(err_max1_exact200_2))/length(na.omit(err_max1_exact200_2))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_4 <- sum(err_oslrt200_4)/nit
mos200_4 <- sum(err_moslrt200_4)/nit
rc200_4 <- sum(err_rc200_4)/nit
de200_4 <- sum(err_de200_4)/nit
ee200_4 <- sum(err_ee200_4)/nit
me200_4 <- sum(err_me200_4)/nit
max1_hoch200_4 <- sum(err_max1_hoch200_4)/nit
max1_exact200_4 <- sum(na.omit(err_max1_exact200_4))/length(na.omit(err_max1_exact200_4))

mean(tx_cens200)
mean(tx_censadm200)


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_2 <- c(os20_2, os30_2, os50_2, os60_2, os80_2, os100_2, os150_2, os200_2)
mosl_2 <- c(mos20_2, mos30_2, mos50_2, mos60_2, mos80_2, mos100_2, mos150_2, mos200_2)
rc_2 <- c(rc20_2, rc30_2, rc50_2, rc60_2, rc80_2, rc100_2, rc150_2, rc200_2)
de_2 <- c(de20_2, de30_2, de50_2, de60_2, de80_2, de100_2, de150_2, de200_2)
ee_2 <- c(ee20_2, ee30_2, ee50_2, ee60_2, ee80_2, ee100_2, ee150_2, ee200_2)
me_2 <- c(me20_2, me30_2, me50_2, me60_2, me80_2, me100_2, me150_2, me200_2)
max1_hochberg_2 <- c(max1_hoch20_2, max1_hoch30_2, max1_hoch50_2, max1_hoch60_2, max1_hoch80_2, max1_hoch100_2, max1_hoch150_2, max1_hoch200_2)
max1_pmult_2 <- c(max1_exact20_2, max1_exact30_2, max1_exact50_2, max1_exact60_2, max1_exact80_2, max1_exact100_2, max1_exact150_2, max1_exact200_2)
osl_2
mosl_2
ee_2
me_2
de_2
rc_2
max1_hochberg_2
max1_pmult_2

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

osl_4 <- c(os20_4, os30_4, os50_4, os60_4, os80_4, os100_4, os150_4, os200_4)
mosl_4 <- c(mos20_4, mos30_4, mos50_4, mos60_4, mos80_4, mos100_4, mos150_4, mos200_4)
rc_4 <- c(rc20_4, rc30_4, rc50_4, rc60_4, rc80_4, rc100_4, rc150_4, rc200_4)
de_4 <- c(de20_4, de30_4, de50_4, de60_4, de80_4, de100_4, de150_4, de200_4)
ee_4 <- c(ee20_4, ee30_4, ee50_4, ee60_4, ee80_4, ee100_4, ee150_4, ee200_4)
me_4 <- c(me20_4, me30_4, me50_4, me60_4, me80_4, me100_4, me150_4, me200_4)
max1_hochberg_4 <- c(max1_hoch20_4, max1_hoch30_4, max1_hoch50_4, max1_hoch60_4, max1_hoch80_4, max1_hoch100_4, max1_hoch150_4, max1_hoch200_4)
max1_pmult_4 <- c(max1_exact20_4, max1_exact30_4, max1_exact50_4, max1_exact60_4, max1_exact80_4, max1_exact100_4, max1_exact150_4, max1_exact200_4)
osl_4
mosl_4
ee_4
me_4
de_4
rc_4
max1_hochberg_4
max1_pmult_4

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 1',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  #geom_hline(yintercept = 0.05, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_2 <- data.frame(Sample.size = n, Error = c(osl_2, mosl_2, ee_2, me_2, de_2, rc_2, max1_hochberg_2, max1_pmult_2),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_2$Test <- as.factor(d1_2$Test)
d1_2$Test <- factor(d1_2$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_2, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.8',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  #geom_hline(yintercept = 0.05, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))

d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.6',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  #geom_hline(yintercept = 0.05, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_4 <- data.frame(Sample.size = n, Error = c(osl_4, mosl_4, ee_4, me_4, de_4, rc_4, max1_hochberg_4, max1_pmult_4),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_4$Test <- as.factor(d1_4$Test)
d1_4$Test <- factor(d1_4$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_4, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.5',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  #geom_hline(yintercept = 0.05, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))



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
pi2 <- 0.8
pi3 <- 0.6
pi4 <- 0.5

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_2 <- rep(0, nit)
err_moslrt20_2 <- rep(0, nit)
err_rc20_2 <- rep(0, nit)
err_de20_2 <- rep(0, nit)
err_ee20_2 <- rep(0, nit)
err_me20_2 <- rep(0, nit)
err_max1_hoch20_2 <- rep(0, nit)
err_max1_exact20_2 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)

err_oslrt20_4 <- rep(0, nit)
err_moslrt20_4 <- rep(0, nit)
err_rc20_4 <- rep(0, nit)
err_de20_4 <- rep(0, nit)
err_ee20_4 <- rep(0, nit)
err_me20_4 <- rep(0, nit)
err_max1_hoch20_4 <- rep(0, nit)
err_max1_exact20_4 <- rep(0, nit)

tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_2 <- rep(0, nit)
err_moslrt30_2 <- rep(0, nit)
err_rc30_2 <- rep(0, nit)
err_de30_2 <- rep(0, nit)
err_ee30_2 <- rep(0, nit)
err_me30_2 <- rep(0, nit)
err_max1_hoch30_2 <- rep(0, nit)
err_max1_exact30_2 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)

err_oslrt30_4 <- rep(0, nit)
err_moslrt30_4 <- rep(0, nit)
err_rc30_4 <- rep(0, nit)
err_de30_4 <- rep(0, nit)
err_ee30_4 <- rep(0, nit)
err_me30_4 <- rep(0, nit)
err_max1_hoch30_4 <- rep(0, nit)
err_max1_exact30_4 <- rep(0, nit)

tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_2 <- rep(0, nit)
err_moslrt50_2 <- rep(0, nit)
err_rc50_2 <- rep(0, nit)
err_de50_2 <- rep(0, nit)
err_ee50_2 <- rep(0, nit)
err_me50_2 <- rep(0, nit)
err_max1_hoch50_2 <- rep(0, nit)
err_max1_exact50_2 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)

err_oslrt50_4 <- rep(0, nit)
err_moslrt50_4 <- rep(0, nit)
err_rc50_4 <- rep(0, nit)
err_de50_4 <- rep(0, nit)
err_ee50_4 <- rep(0, nit)
err_me50_4 <- rep(0, nit)
err_max1_hoch50_4 <- rep(0, nit)
err_max1_exact50_4 <- rep(0, nit)

tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_2 <- rep(0, nit)
err_moslrt60_2 <- rep(0, nit)
err_rc60_2 <- rep(0, nit)
err_de60_2 <- rep(0, nit)
err_ee60_2 <- rep(0, nit)
err_me60_2 <- rep(0, nit)
err_max1_hoch60_2 <- rep(0, nit)
err_max1_exact60_2 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)

err_oslrt60_4 <- rep(0, nit)
err_moslrt60_4 <- rep(0, nit)
err_rc60_4 <- rep(0, nit)
err_de60_4 <- rep(0, nit)
err_ee60_4 <- rep(0, nit)
err_me60_4 <- rep(0, nit)
err_max1_hoch60_4 <- rep(0, nit)
err_max1_exact60_4 <- rep(0, nit)

tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_2 <- rep(0, nit)
err_moslrt80_2 <- rep(0, nit)
err_rc80_2 <- rep(0, nit)
err_de80_2 <- rep(0, nit)
err_ee80_2 <- rep(0, nit)
err_me80_2 <- rep(0, nit)
err_max1_hoch80_2 <- rep(0, nit)
err_max1_exact80_2 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)

err_oslrt80_4 <- rep(0, nit)
err_moslrt80_4 <- rep(0, nit)
err_rc80_4 <- rep(0, nit)
err_de80_4 <- rep(0, nit)
err_ee80_4 <- rep(0, nit)
err_me80_4 <- rep(0, nit)
err_max1_hoch80_4 <- rep(0, nit)
err_max1_exact80_4 <- rep(0, nit)

tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_2 <- rep(0, nit)
err_moslrt100_2 <- rep(0, nit)
err_rc100_2 <- rep(0, nit)
err_de100_2 <- rep(0, nit)
err_ee100_2 <- rep(0, nit)
err_me100_2 <- rep(0, nit)
err_max1_hoch100_2 <- rep(0, nit)
err_max1_exact100_2 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)

err_oslrt100_4 <- rep(0, nit)
err_moslrt100_4 <- rep(0, nit)
err_rc100_4 <- rep(0, nit)
err_de100_4 <- rep(0, nit)
err_ee100_4 <- rep(0, nit)
err_me100_4 <- rep(0, nit)
err_max1_hoch100_4 <- rep(0, nit)
err_max1_exact100_4 <- rep(0, nit)

tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_2 <- rep(0, nit)
err_moslrt150_2 <- rep(0, nit)
err_rc150_2 <- rep(0, nit)
err_de150_2 <- rep(0, nit)
err_ee150_2 <- rep(0, nit)
err_me150_2 <- rep(0, nit)
err_max1_hoch150_2 <- rep(0, nit)
err_max1_exact150_2 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)

err_oslrt150_4 <- rep(0, nit)
err_moslrt150_4 <- rep(0, nit)
err_rc150_4 <- rep(0, nit)
err_de150_4 <- rep(0, nit)
err_ee150_4 <- rep(0, nit)
err_me150_4 <- rep(0, nit)
err_max1_hoch150_4 <- rep(0, nit)
err_max1_exact150_4 <- rep(0, nit)

tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_2 <- rep(0, nit)
err_moslrt200_2 <- rep(0, nit)
err_rc200_2 <- rep(0, nit)
err_de200_2 <- rep(0, nit)
err_ee200_2 <- rep(0, nit)
err_me200_2 <- rep(0, nit)
err_max1_hoch200_2 <- rep(0, nit)
err_max1_exact200_2 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_oslrt200_4 <- rep(0, nit)
err_moslrt200_4 <- rep(0, nit)
err_rc200_4 <- rep(0, nit)
err_de200_4 <- rep(0, nit)
err_ee200_4 <- rep(0, nit)
err_me200_4 <- rep(0, nit)
err_max1_hoch200_4 <- rep(0, nit)
err_max1_exact200_4 <- rep(0, nit)

tx_cens200 <- rep(0, nit)
tx_censadm200 <- rep(0, nit)

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
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  a202_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)[2]
  b20_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  c20_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi2)[2]
  d20_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi2)[2]
  e20_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f20_2 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a20_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  a202_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)[2]
  b20_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  c20_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi4)[2]
  d20_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi4)[2]
  e20_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f20_4 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  a302_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)[2]
  b30_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  c30_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi2)[2]
  d30_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi2)[2]
  e30_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f30_2 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a30_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  a302_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)[2]
  b30_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  c30_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi4)[2]
  d30_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi4)[2]
  e30_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f30_4 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  a502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)[2]
  b50_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  c50_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi2)[2]
  d50_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi2)[2]
  e50_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f50_2 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a50_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  a502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)[2]
  b50_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  c50_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi4)[2]
  d50_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi4)[2]
  e50_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f50_4 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  a602_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)[2]
  b60_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  c60_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi2)[2]
  d60_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi2)[2]
  e60_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f60_2 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a60_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  a602_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)[2]
  b60_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  c60_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi4)[2]
  d60_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi4)[2]
  e60_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f60_4 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  a802_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)[2]
  b80_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  c80_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi2)[2]
  d80_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi2)[2]
  e80_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f80_2 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a80_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  a802_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)[2]
  b80_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  c80_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi4)[2]
  d80_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi4)[2]
  e80_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f80_4 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  a1002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)[2]
  b100_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  c100_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi2)[2]
  d100_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi2)[2]
  e100_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f100_2 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a100_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  a1002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)[2]
  b100_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  c100_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi4)[2]
  d100_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi4)[2]
  e100_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f100_4 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  a1502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)[2]
  b150_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  c150_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi2)[2]
  d150_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi2)[2]
  e150_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f150_2 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a150_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  a1502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)[2]
  b150_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  c150_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi4)[2]
  d150_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi4)[2]
  e150_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f150_4 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  a2002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)[2]
  b200_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  c200_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi2)[2]
  d200_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi2)[2]
  e200_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f200_2 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a200_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  a2002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)[2]
  b200_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  c200_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi4)[2]
  d200_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi4)[2]
  e200_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f200_4 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  #censoring rate with adm censoring
  tx_censadm20[i] <- 1-sum(delta20)/20
  tx_censadm30[i] <- 1-sum(delta30)/30
  tx_censadm50[i] <- 1-sum(delta50)/50
  tx_censadm60[i] <- 1-sum(delta60)/60
  tx_censadm80[i] <- 1-sum(delta80)/80
  tx_censadm100[i] <- 1-sum(delta100)/100
  tx_censadm150[i] <- 1-sum(delta150)/150
  tx_censadm200[i] <- 1-sum(delta200)/200
  
  #censoring rate without adm censoring
  tx_cens20[i] <- 1-sum(del20)/20
  tx_cens30[i] <- 1-sum(del30)/30
  tx_cens50[i] <- 1-sum(del50)/50
  tx_cens60[i] <- 1-sum(del60)/60
  tx_cens80[i] <- 1-sum(del80)/80
  tx_cens100[i] <- 1-sum(del100)/100
  tx_cens150[i] <- 1-sum(del150)/150
  tx_cens200[i] <- 1-sum(del200)/200
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_2[i] <- ifelse((a20_2<0.05), 1, 0)
  err_moslrt20_2[i] <- ifelse((a202_2<0.05), 1, 0)
  err_rc20_2[i] <- ifelse((b20_2<0.05), 1, 0)
  err_de20_2[i] <- ifelse((c20_2<0.05), 1, 0)
  err_ee20_2[i] <- ifelse((d20_2<0.05), 1, 0)
  err_me20_2[i] <- ifelse((e20_2<0.05), 1, 0)
  err_max1_hoch20_2[i] <- ifelse((f20_2[2]<0.05), 1, 0)
  err_max1_exact20_2[i] <- ifelse((f20_2[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  err_oslrt20_4[i] <- ifelse((a20_4<0.05), 1, 0)
  err_moslrt20_4[i] <- ifelse((a202_4<0.05), 1, 0)
  err_rc20_4[i] <- ifelse((b20_4<0.05), 1, 0)
  err_de20_4[i] <- ifelse((c20_4<0.05), 1, 0)
  err_ee20_4[i] <- ifelse((d20_4<0.05), 1, 0)
  err_me20_4[i] <- ifelse((e20_4<0.05), 1, 0)
  err_max1_hoch20_4[i] <- ifelse((f20_4[2]<0.05), 1, 0)
  err_max1_exact20_4[i] <- ifelse((f20_4[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_2[i] <- ifelse((a30_2<0.05), 1, 0)
  err_moslrt30_2[i] <- ifelse((a302_2<0.05), 1, 0)
  err_rc30_2[i] <- ifelse((b30_2<0.05), 1, 0)
  err_de30_2[i] <- ifelse((c30_2<0.05), 1, 0)
  err_ee30_2[i] <- ifelse((d30_2<0.05), 1, 0)
  err_me30_2[i] <- ifelse((e30_2<0.05), 1, 0)
  err_max1_hoch30_2[i] <- ifelse((f30_2[2]<0.05), 1, 0)
  err_max1_exact30_2[i] <- ifelse((f30_2[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  err_oslrt30_4[i] <- ifelse((a30_4<0.05), 1, 0)
  err_moslrt30_4[i] <- ifelse((a302_4<0.05), 1, 0)
  err_rc30_4[i] <- ifelse((b30_4<0.05), 1, 0)
  err_de30_4[i] <- ifelse((c30_4<0.05), 1, 0)
  err_ee30_4[i] <- ifelse((d30_4<0.05), 1, 0)
  err_me30_4[i] <- ifelse((e30_4<0.05), 1, 0)
  err_max1_hoch30_4[i] <- ifelse((f30_4[2]<0.05), 1, 0)
  err_max1_exact30_4[i] <- ifelse((f30_4[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_2[i] <- ifelse((a50_2<0.05), 1, 0)
  err_moslrt50_2[i] <- ifelse((a502_2<0.05), 1, 0)
  err_rc50_2[i] <- ifelse((b50_2<0.05), 1, 0)
  err_de50_2[i] <- ifelse((c50_2<0.05), 1, 0)
  err_ee50_2[i] <- ifelse((d50_2<0.05), 1, 0)
  err_me50_2[i] <- ifelse((e50_2<0.05), 1, 0)
  err_max1_hoch50_2[i] <- ifelse((f50_2[2]<0.05), 1, 0)
  err_max1_exact50_2[i] <- ifelse((f50_2[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  err_oslrt50_4[i] <- ifelse((a50_4<0.05), 1, 0)
  err_moslrt50_4[i] <- ifelse((a502_4<0.05), 1, 0)
  err_rc50_4[i] <- ifelse((b50_4<0.05), 1, 0)
  err_de50_4[i] <- ifelse((c50_4<0.05), 1, 0)
  err_ee50_4[i] <- ifelse((d50_4<0.05), 1, 0)
  err_me50_4[i] <- ifelse((e50_4<0.05), 1, 0)
  err_max1_hoch50_4[i] <- ifelse((f50_4[2]<0.05), 1, 0)
  err_max1_exact50_4[i] <- ifelse((f50_4[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_2[i] <- ifelse((a60_2<0.05), 1, 0)
  err_moslrt60_2[i] <- ifelse((a602_2<0.05), 1, 0)
  err_rc60_2[i] <- ifelse((b60_2<0.05), 1, 0)
  err_de60_2[i] <- ifelse((c60_2<0.05), 1, 0)
  err_ee60_2[i] <- ifelse((d60_2<0.05), 1, 0)
  err_me60_2[i] <- ifelse((e60_2<0.05), 1, 0)
  err_max1_hoch60_2[i] <- ifelse((f60_2[2]<0.05), 1, 0)
  err_max1_exact60_2[i] <- ifelse((f60_2[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  err_oslrt60_4[i] <- ifelse((a60_4<0.05), 1, 0)
  err_moslrt60_4[i] <- ifelse((a602_4<0.05), 1, 0)
  err_rc60_4[i] <- ifelse((b60_4<0.05), 1, 0)
  err_de60_4[i] <- ifelse((c60_4<0.05), 1, 0)
  err_ee60_4[i] <- ifelse((d60_4<0.05), 1, 0)
  err_me60_4[i] <- ifelse((e60_4<0.05), 1, 0)
  err_max1_hoch60_4[i] <- ifelse((f60_4[2]<0.05), 1, 0)
  err_max1_exact60_4[i] <- ifelse((f60_4[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_2[i] <- ifelse((a80_2<0.05), 1, 0)
  err_moslrt80_2[i] <- ifelse((a802_2<0.05), 1, 0)
  err_rc80_2[i] <- ifelse((b80_2<0.05), 1, 0)
  err_de80_2[i] <- ifelse((c80_2<0.05), 1, 0)
  err_ee80_2[i] <- ifelse((d80_2<0.05), 1, 0)
  err_me80_2[i] <- ifelse((e80_2<0.05), 1, 0)
  err_max1_hoch80_2[i] <- ifelse((f80_2[2]<0.05), 1, 0)
  err_max1_exact80_2[i] <- ifelse((f80_2[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  err_oslrt80_4[i] <- ifelse((a80_4<0.05), 1, 0)
  err_moslrt80_4[i] <- ifelse((a802_4<0.05), 1, 0)
  err_rc80_4[i] <- ifelse((b80_4<0.05), 1, 0)
  err_de80_4[i] <- ifelse((c80_4<0.05), 1, 0)
  err_ee80_4[i] <- ifelse((d80_4<0.05), 1, 0)
  err_me80_4[i] <- ifelse((e80_4<0.05), 1, 0)
  err_max1_hoch80_4[i] <- ifelse((f80_4[2]<0.05), 1, 0)
  err_max1_exact80_4[i] <- ifelse((f80_4[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_2[i] <- ifelse((a100_2<0.05), 1, 0)
  err_moslrt100_2[i] <- ifelse((a1002_2<0.05), 1, 0)
  err_rc100_2[i] <- ifelse((b100_2<0.05), 1, 0)
  err_de100_2[i] <- ifelse((c100_2<0.05), 1, 0)
  err_ee100_2[i] <- ifelse((d100_2<0.05), 1, 0)
  err_me100_2[i] <- ifelse((e100_2<0.05), 1, 0)
  err_max1_hoch100_2[i] <- ifelse((f100_2[2]<0.05), 1, 0)
  err_max1_exact100_2[i] <- ifelse((f100_2[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  err_oslrt100_4[i] <- ifelse((a100_4<0.05), 1, 0)
  err_moslrt100_4[i] <- ifelse((a1002_4<0.05), 1, 0)
  err_rc100_4[i] <- ifelse((b100_4<0.05), 1, 0)
  err_de100_4[i] <- ifelse((c100_4<0.05), 1, 0)
  err_ee100_4[i] <- ifelse((d100_4<0.05), 1, 0)
  err_me100_4[i] <- ifelse((e100_4<0.05), 1, 0)
  err_max1_hoch100_4[i] <- ifelse((f100_4[2]<0.05), 1, 0)
  err_max1_exact100_4[i] <- ifelse((f100_4[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_2[i] <- ifelse((a150_2<0.05), 1, 0)
  err_moslrt150_2[i] <- ifelse((a1502_2<0.05), 1, 0)
  err_rc150_2[i] <- ifelse((b150_2<0.05), 1, 0)
  err_de150_2[i] <- ifelse((c150_2<0.05), 1, 0)
  err_ee150_2[i] <- ifelse((d150_2<0.05), 1, 0)
  err_me150_2[i] <- ifelse((e150_2<0.05), 1, 0)
  err_max1_hoch150_2[i] <- ifelse((f150_2[2]<0.05), 1, 0)
  err_max1_exact150_2[i] <- ifelse((f150_2[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  err_oslrt150_4[i] <- ifelse((a150_4<0.05), 1, 0)
  err_moslrt150_4[i] <- ifelse((a1502_4<0.05), 1, 0)
  err_rc150_4[i] <- ifelse((b150_4<0.05), 1, 0)
  err_de150_4[i] <- ifelse((c150_4<0.05), 1, 0)
  err_ee150_4[i] <- ifelse((d150_4<0.05), 1, 0)
  err_me150_4[i] <- ifelse((e150_4<0.05), 1, 0)
  err_max1_hoch150_4[i] <- ifelse((f150_4[2]<0.05), 1, 0)
  err_max1_exact150_4[i] <- ifelse((f150_4[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_2[i] <- ifelse((a200_2<0.05), 1, 0)
  err_moslrt200_2[i] <- ifelse((a2002_2<0.05), 1, 0)
  err_rc200_2[i] <- ifelse((b200_2<0.05), 1, 0)
  err_de200_2[i] <- ifelse((c200_2<0.05), 1, 0)
  err_ee200_2[i] <- ifelse((d200_2<0.05), 1, 0)
  err_me200_2[i] <- ifelse((e200_2<0.05), 1, 0)
  err_max1_hoch200_2[i] <- ifelse((f200_2[2]<0.05), 1, 0)
  err_max1_exact200_2[i] <- ifelse((f200_2[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)
  
  err_oslrt200_4[i] <- ifelse((a200_4<0.05), 1, 0)
  err_moslrt200_4[i] <- ifelse((a2002_4<0.05), 1, 0)
  err_rc200_4[i] <- ifelse((b200_4<0.05), 1, 0)
  err_de200_4[i] <- ifelse((c200_4<0.05), 1, 0)
  err_ee200_4[i] <- ifelse((d200_4<0.05), 1, 0)
  err_me200_4[i] <- ifelse((e200_4<0.05), 1, 0)
  err_max1_hoch200_4[i] <- ifelse((f200_4[2]<0.05), 1, 0)
  err_max1_exact200_4[i] <- ifelse((f200_4[3]<0.05), 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_2 <- sum(err_oslrt20_2)/nit
mos20_2 <- sum(err_moslrt20_2)/nit
rc20_2 <- sum(err_rc20_2)/nit
de20_2 <- sum(na.omit(err_de20_2))/length(na.omit(err_de20_2))
ee20_2 <- sum(err_ee20_2)/nit
me20_2 <- sum(err_me20_2)/nit
max1_hoch20_2 <- sum(err_max1_hoch20_2)/nit
max1_exact20_2 <- sum(na.omit(err_max1_exact20_2))/length(na.omit(err_max1_exact20_2))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_4 <- sum(err_oslrt20_4)/nit
mos20_4 <- sum(err_moslrt20_4)/nit
rc20_4 <- sum(err_rc20_4)/nit
de20_4 <- sum(na.omit(err_de20_4))/length(na.omit(err_de20_4))
ee20_4 <- sum(err_ee20_4)/nit
me20_4 <- sum(err_me20_4)/nit
max1_hoch20_4 <- sum(err_max1_hoch20_4)/nit
max1_exact20_4 <- sum(na.omit(err_max1_exact20_4))/length(na.omit(err_max1_exact20_4))

mean(tx_cens20)
mean(tx_censadm20)


os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_2 <- sum(err_oslrt30_2)/nit
mos30_2 <- sum(err_moslrt30_2)/nit
rc30_2 <- sum(err_rc30_2)/nit
de30_2 <- sum(na.omit(err_de30_2))/length(na.omit(err_de30_2))
ee30_2 <- sum(err_ee30_2)/nit
me30_2 <- sum(err_me30_2)/nit
max1_hoch30_2 <- sum(err_max1_hoch30_2)/nit
max1_exact30_2 <- sum(na.omit(err_max1_exact30_2))/length(na.omit(err_max1_exact30_2))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_4 <- sum(err_oslrt30_4)/nit
mos30_4 <- sum(err_moslrt30_4)/nit
rc30_4 <- sum(err_rc30_4)/nit
de30_4 <- sum(na.omit(err_de30_4))/length(na.omit(err_de30_4))
ee30_4 <- sum(err_ee30_4)/nit
me30_4 <- sum(err_me30_4)/nit
max1_hoch30_4 <- sum(err_max1_hoch30_4)/nit
max1_exact30_4 <- sum(na.omit(err_max1_exact30_4))/length(na.omit(err_max1_exact30_4))

mean(tx_cens30)
mean(tx_censadm30)


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_2 <- sum(err_oslrt50_2)/nit
mos50_2 <- sum(err_moslrt50_2)/nit
rc50_2 <- sum(err_rc50_2)/nit
de50_2 <- sum(err_de50_2)/nit
ee50_2 <- sum(err_ee50_2)/nit
me50_2 <- sum(err_me50_2)/nit
max1_hoch50_2 <- sum(err_max1_hoch50_2)/nit
max1_exact50_2 <- sum(na.omit(err_max1_exact50_2))/length(na.omit(err_max1_exact50_2))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_4 <- sum(err_oslrt50_4)/nit
mos50_4 <- sum(err_moslrt50_4)/nit
rc50_4 <- sum(err_rc50_4)/nit
de50_4 <- sum(err_de50_4)/nit
ee50_4 <- sum(err_ee50_4)/nit
me50_4 <- sum(err_me50_4)/nit
max1_hoch50_4 <- sum(err_max1_hoch50_4)/nit
max1_exact50_4 <- sum(na.omit(err_max1_exact50_4))/length(na.omit(err_max1_exact50_4))

mean(tx_cens50)
mean(tx_censadm50)


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_2 <- sum(err_oslrt60_2)/nit
mos60_2 <- sum(err_moslrt60_2)/nit
rc60_2 <- sum(err_rc60_2)/nit
de60_2 <- sum(err_de60_2)/nit
ee60_2 <- sum(err_ee60_2)/nit
me60_2 <- sum(err_me60_2)/nit
max1_hoch60_2 <- sum(err_max1_hoch60_2)/nit
max1_exact60_2 <- sum(na.omit(err_max1_exact60_2))/length(na.omit(err_max1_exact60_2))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_4 <- sum(err_oslrt60_4)/nit
mos60_4 <- sum(err_moslrt60_4)/nit
rc60_4 <- sum(err_rc60_4)/nit
de60_4 <- sum(err_de60_4)/nit
ee60_4 <- sum(err_ee60_4)/nit
me60_4 <- sum(err_me60_4)/nit
max1_hoch60_4 <- sum(err_max1_hoch60_4)/nit
max1_exact60_4 <- sum(na.omit(err_max1_exact60_4))/length(na.omit(err_max1_exact60_4))

mean(tx_cens60)
mean(tx_censadm60)


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_2 <- sum(err_oslrt80_2)/nit
mos80_2 <- sum(err_moslrt80_2)/nit
rc80_2 <- sum(err_rc80_2)/nit
de80_2 <- sum(err_de80_2)/nit
ee80_2 <- sum(err_ee80_2)/nit
me80_2 <- sum(err_me80_2)/nit
max1_hoch80_2 <- sum(err_max1_hoch80_2)/nit
max1_exact80_2 <- sum(na.omit(err_max1_exact80_2))/length(na.omit(err_max1_exact80_2))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_4 <- sum(err_oslrt80_4)/nit
mos80_4 <- sum(err_moslrt80_4)/nit
rc80_4 <- sum(err_rc80_4)/nit
de80_4 <- sum(err_de80_4)/nit
ee80_4 <- sum(err_ee80_4)/nit
me80_4 <- sum(err_me80_4)/nit
max1_hoch80_4 <- sum(err_max1_hoch80_4)/nit
max1_exact80_4 <- sum(na.omit(err_max1_exact80_4))/length(na.omit(err_max1_exact80_4))

mean(tx_cens80)
mean(tx_censadm80)


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_2 <- sum(err_oslrt100_2)/nit
mos100_2 <- sum(err_moslrt100_2)/nit
rc100_2 <- sum(err_rc100_2)/nit
de100_2 <- sum(err_de100_2)/nit
ee100_2 <- sum(err_ee100_2)/nit
me100_2 <- sum(err_me100_2)/nit
max1_hoch100_2 <- sum(err_max1_hoch100_2)/nit
max1_exact100_2 <- sum(na.omit(err_max1_exact100_2))/length(na.omit(err_max1_exact100_2))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_4 <- sum(err_oslrt100_4)/nit
mos100_4 <- sum(err_moslrt100_4)/nit
rc100_4 <- sum(err_rc100_4)/nit
de100_4 <- sum(err_de100_4)/nit
ee100_4 <- sum(err_ee100_4)/nit
me100_4 <- sum(err_me100_4)/nit
max1_hoch100_4 <- sum(err_max1_hoch100_4)/nit
max1_exact100_4 <- sum(na.omit(err_max1_exact100_4))/length(na.omit(err_max1_exact100_4))

mean(tx_cens100)
mean(tx_censadm100)


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_2 <- sum(err_oslrt150_2)/nit
mos150_2 <- sum(err_moslrt150_2)/nit
rc150_2 <- sum(err_rc150_2)/nit
de150_2 <- sum(err_de150_2)/nit
ee150_2 <- sum(err_ee150_2)/nit
me150_2 <- sum(na.omit(err_me150_2))/length(na.omit(err_me150_2))
max1_hoch150_2 <- sum(err_max1_hoch150_2)/nit
max1_exact150_2 <- sum(na.omit(err_max1_exact150_2))/length(na.omit(err_max1_exact150_2))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_4 <- sum(err_oslrt150_4)/nit
mos150_4 <- sum(err_moslrt150_4)/nit
rc150_4 <- sum(err_rc150_4)/nit
de150_4 <- sum(err_de150_4)/nit
ee150_4 <- sum(err_ee150_4)/nit
me150_4 <- sum(na.omit(err_me150_4))/length(na.omit(err_me150_4))
max1_hoch150_4 <- sum(err_max1_hoch150_4)/nit
max1_exact150_4 <- sum(na.omit(err_max1_exact150_4))/length(na.omit(err_max1_exact150_4))

mean(tx_cens150)
mean(tx_censadm150)

os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_2 <- sum(err_oslrt200_2)/nit
mos200_2 <- sum(err_moslrt200_2)/nit
rc200_2 <- sum(err_rc200_2)/nit
de200_2 <- sum(err_de200_2)/nit
ee200_2 <- sum(err_ee200_2)/nit
me200_2 <- sum(err_me200_2)/nit
max1_hoch200_2 <- sum(err_max1_hoch200_2)/nit
max1_exact200_2 <- sum(na.omit(err_max1_exact200_2))/length(na.omit(err_max1_exact200_2))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_4 <- sum(err_oslrt200_4)/nit
mos200_4 <- sum(err_moslrt200_4)/nit
rc200_4 <- sum(err_rc200_4)/nit
de200_4 <- sum(err_de200_4)/nit
ee200_4 <- sum(err_ee200_4)/nit
me200_4 <- sum(err_me200_4)/nit
max1_hoch200_4 <- sum(err_max1_hoch200_4)/nit
max1_exact200_4 <- sum(na.omit(err_max1_exact200_4))/length(na.omit(err_max1_exact200_4))

mean(tx_cens200)
mean(tx_censadm200)


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_2 <- c(os20_2, os30_2, os50_2, os60_2, os80_2, os100_2, os150_2, os200_2)
mosl_2 <- c(mos20_2, mos30_2, mos50_2, mos60_2, mos80_2, mos100_2, mos150_2, mos200_2)
rc_2 <- c(rc20_2, rc30_2, rc50_2, rc60_2, rc80_2, rc100_2, rc150_2, rc200_2)
de_2 <- c(de20_2, de30_2, de50_2, de60_2, de80_2, de100_2, de150_2, de200_2)
ee_2 <- c(ee20_2, ee30_2, ee50_2, ee60_2, ee80_2, ee100_2, ee150_2, ee200_2)
me_2 <- c(me20_2, me30_2, me50_2, me60_2, me80_2, me100_2, me150_2, me200_2)
max1_hochberg_2 <- c(max1_hoch20_2, max1_hoch30_2, max1_hoch50_2, max1_hoch60_2, max1_hoch80_2, max1_hoch100_2, max1_hoch150_2, max1_hoch200_2)
max1_pmult_2 <- c(max1_exact20_2, max1_exact30_2, max1_exact50_2, max1_exact60_2, max1_exact80_2, max1_exact100_2, max1_exact150_2, max1_exact200_2)
osl_2
mosl_2
ee_2
me_2
de_2
rc_2
max1_hochberg_2
max1_pmult_2

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

osl_4 <- c(os20_4, os30_4, os50_4, os60_4, os80_4, os100_4, os150_4, os200_4)
mosl_4 <- c(mos20_4, mos30_4, mos50_4, mos60_4, mos80_4, mos100_4, mos150_4, mos200_4)
rc_4 <- c(rc20_4, rc30_4, rc50_4, rc60_4, rc80_4, rc100_4, rc150_4, rc200_4)
de_4 <- c(de20_4, de30_4, de50_4, de60_4, de80_4, de100_4, de150_4, de200_4)
ee_4 <- c(ee20_4, ee30_4, ee50_4, ee60_4, ee80_4, ee100_4, ee150_4, ee200_4)
me_4 <- c(me20_4, me30_4, me50_4, me60_4, me80_4, me100_4, me150_4, me200_4)
max1_hochberg_4 <- c(max1_hoch20_4, max1_hoch30_4, max1_hoch50_4, max1_hoch60_4, max1_hoch80_4, max1_hoch100_4, max1_hoch150_4, max1_hoch200_4)
max1_pmult_4 <- c(max1_exact20_4, max1_exact30_4, max1_exact50_4, max1_exact60_4, max1_exact80_4, max1_exact100_4, max1_exact150_4, max1_exact200_4)
osl_4
mosl_4
ee_4
me_4
de_4
rc_4
max1_hochberg_4
max1_pmult_4

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 1',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_2 <- data.frame(Sample.size = n, Error = c(osl_2, mosl_2, ee_2, me_2, de_2, rc_2, max1_hochberg_2, max1_pmult_2),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_2$Test <- as.factor(d1_2$Test)
d1_2$Test <- factor(d1_2$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_2, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.8',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))

d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.6',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_4 <- data.frame(Sample.size = n, Error = c(osl_4, mosl_4, ee_4, me_4, de_4, rc_4, max1_hochberg_4, max1_pmult_4),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_4$Test <- as.factor(d1_4$Test)
d1_4$Test <- factor(d1_4$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_4, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.5',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


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

CP_EE <- 1
CP_DE <- 4
CP_ME1 <- 1
CP_ME2 <- 4

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

pi1 <- 1
pi2 <- 0.8
pi3 <- 0.6
pi4 <- 0.5

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_2 <- rep(0, nit)
err_moslrt20_2 <- rep(0, nit)
err_rc20_2 <- rep(0, nit)
err_de20_2 <- rep(0, nit)
err_ee20_2 <- rep(0, nit)
err_me20_2 <- rep(0, nit)
err_max1_hoch20_2 <- rep(0, nit)
err_max1_exact20_2 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)

err_oslrt20_4 <- rep(0, nit)
err_moslrt20_4 <- rep(0, nit)
err_rc20_4 <- rep(0, nit)
err_de20_4 <- rep(0, nit)
err_ee20_4 <- rep(0, nit)
err_me20_4 <- rep(0, nit)
err_max1_hoch20_4 <- rep(0, nit)
err_max1_exact20_4 <- rep(0, nit)

tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_2 <- rep(0, nit)
err_moslrt30_2 <- rep(0, nit)
err_rc30_2 <- rep(0, nit)
err_de30_2 <- rep(0, nit)
err_ee30_2 <- rep(0, nit)
err_me30_2 <- rep(0, nit)
err_max1_hoch30_2 <- rep(0, nit)
err_max1_exact30_2 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)

err_oslrt30_4 <- rep(0, nit)
err_moslrt30_4 <- rep(0, nit)
err_rc30_4 <- rep(0, nit)
err_de30_4 <- rep(0, nit)
err_ee30_4 <- rep(0, nit)
err_me30_4 <- rep(0, nit)
err_max1_hoch30_4 <- rep(0, nit)
err_max1_exact30_4 <- rep(0, nit)

tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_2 <- rep(0, nit)
err_moslrt50_2 <- rep(0, nit)
err_rc50_2 <- rep(0, nit)
err_de50_2 <- rep(0, nit)
err_ee50_2 <- rep(0, nit)
err_me50_2 <- rep(0, nit)
err_max1_hoch50_2 <- rep(0, nit)
err_max1_exact50_2 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)

err_oslrt50_4 <- rep(0, nit)
err_moslrt50_4 <- rep(0, nit)
err_rc50_4 <- rep(0, nit)
err_de50_4 <- rep(0, nit)
err_ee50_4 <- rep(0, nit)
err_me50_4 <- rep(0, nit)
err_max1_hoch50_4 <- rep(0, nit)
err_max1_exact50_4 <- rep(0, nit)

tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_2 <- rep(0, nit)
err_moslrt60_2 <- rep(0, nit)
err_rc60_2 <- rep(0, nit)
err_de60_2 <- rep(0, nit)
err_ee60_2 <- rep(0, nit)
err_me60_2 <- rep(0, nit)
err_max1_hoch60_2 <- rep(0, nit)
err_max1_exact60_2 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)

err_oslrt60_4 <- rep(0, nit)
err_moslrt60_4 <- rep(0, nit)
err_rc60_4 <- rep(0, nit)
err_de60_4 <- rep(0, nit)
err_ee60_4 <- rep(0, nit)
err_me60_4 <- rep(0, nit)
err_max1_hoch60_4 <- rep(0, nit)
err_max1_exact60_4 <- rep(0, nit)

tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_2 <- rep(0, nit)
err_moslrt80_2 <- rep(0, nit)
err_rc80_2 <- rep(0, nit)
err_de80_2 <- rep(0, nit)
err_ee80_2 <- rep(0, nit)
err_me80_2 <- rep(0, nit)
err_max1_hoch80_2 <- rep(0, nit)
err_max1_exact80_2 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)

err_oslrt80_4 <- rep(0, nit)
err_moslrt80_4 <- rep(0, nit)
err_rc80_4 <- rep(0, nit)
err_de80_4 <- rep(0, nit)
err_ee80_4 <- rep(0, nit)
err_me80_4 <- rep(0, nit)
err_max1_hoch80_4 <- rep(0, nit)
err_max1_exact80_4 <- rep(0, nit)

tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_2 <- rep(0, nit)
err_moslrt100_2 <- rep(0, nit)
err_rc100_2 <- rep(0, nit)
err_de100_2 <- rep(0, nit)
err_ee100_2 <- rep(0, nit)
err_me100_2 <- rep(0, nit)
err_max1_hoch100_2 <- rep(0, nit)
err_max1_exact100_2 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)

err_oslrt100_4 <- rep(0, nit)
err_moslrt100_4 <- rep(0, nit)
err_rc100_4 <- rep(0, nit)
err_de100_4 <- rep(0, nit)
err_ee100_4 <- rep(0, nit)
err_me100_4 <- rep(0, nit)
err_max1_hoch100_4 <- rep(0, nit)
err_max1_exact100_4 <- rep(0, nit)

tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_2 <- rep(0, nit)
err_moslrt150_2 <- rep(0, nit)
err_rc150_2 <- rep(0, nit)
err_de150_2 <- rep(0, nit)
err_ee150_2 <- rep(0, nit)
err_me150_2 <- rep(0, nit)
err_max1_hoch150_2 <- rep(0, nit)
err_max1_exact150_2 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)

err_oslrt150_4 <- rep(0, nit)
err_moslrt150_4 <- rep(0, nit)
err_rc150_4 <- rep(0, nit)
err_de150_4 <- rep(0, nit)
err_ee150_4 <- rep(0, nit)
err_me150_4 <- rep(0, nit)
err_max1_hoch150_4 <- rep(0, nit)
err_max1_exact150_4 <- rep(0, nit)

tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_2 <- rep(0, nit)
err_moslrt200_2 <- rep(0, nit)
err_rc200_2 <- rep(0, nit)
err_de200_2 <- rep(0, nit)
err_ee200_2 <- rep(0, nit)
err_me200_2 <- rep(0, nit)
err_max1_hoch200_2 <- rep(0, nit)
err_max1_exact200_2 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_oslrt200_4 <- rep(0, nit)
err_moslrt200_4 <- rep(0, nit)
err_rc200_4 <- rep(0, nit)
err_de200_4 <- rep(0, nit)
err_ee200_4 <- rep(0, nit)
err_me200_4 <- rep(0, nit)
err_max1_hoch200_4 <- rep(0, nit)
err_max1_exact200_4 <- rep(0, nit)

tx_cens200 <- rep(0, nit)
tx_censadm200 <- rep(0, nit)

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
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  a202_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)[2]
  b20_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  c20_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi2)[2]
  d20_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi2)[2]
  e20_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f20_2 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a20_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  a202_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)[2]
  b20_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  c20_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi4)[2]
  d20_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi4)[2]
  e20_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f20_4 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  a302_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)[2]
  b30_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  c30_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi2)[2]
  d30_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi2)[2]
  e30_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f30_2 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a30_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  a302_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)[2]
  b30_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  c30_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi4)[2]
  d30_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi4)[2]
  e30_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f30_4 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  a502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)[2]
  b50_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  c50_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi2)[2]
  d50_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi2)[2]
  e50_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f50_2 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a50_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  a502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)[2]
  b50_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  c50_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi4)[2]
  d50_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi4)[2]
  e50_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f50_4 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  a602_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)[2]
  b60_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  c60_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi2)[2]
  d60_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi2)[2]
  e60_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f60_2 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a60_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  a602_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)[2]
  b60_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  c60_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi4)[2]
  d60_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi4)[2]
  e60_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f60_4 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  a802_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)[2]
  b80_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  c80_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi2)[2]
  d80_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi2)[2]
  e80_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f80_2 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a80_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  a802_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)[2]
  b80_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  c80_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi4)[2]
  d80_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi4)[2]
  e80_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f80_4 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  a1002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)[2]
  b100_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  c100_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi2)[2]
  d100_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi2)[2]
  e100_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f100_2 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a100_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  a1002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)[2]
  b100_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  c100_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi4)[2]
  d100_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi4)[2]
  e100_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f100_4 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  a1502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)[2]
  b150_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  c150_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi2)[2]
  d150_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi2)[2]
  e150_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f150_2 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a150_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  a1502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)[2]
  b150_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  c150_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi4)[2]
  d150_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi4)[2]
  e150_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f150_4 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  a2002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)[2]
  b200_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  c200_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi2)[2]
  d200_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi2)[2]
  e200_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f200_2 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a200_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  a2002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)[2]
  b200_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  c200_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi4)[2]
  d200_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi4)[2]
  e200_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f200_4 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  #censoring rate with adm censoring
  tx_censadm20[i] <- 1-sum(delta20)/20
  tx_censadm30[i] <- 1-sum(delta30)/30
  tx_censadm50[i] <- 1-sum(delta50)/50
  tx_censadm60[i] <- 1-sum(delta60)/60
  tx_censadm80[i] <- 1-sum(delta80)/80
  tx_censadm100[i] <- 1-sum(delta100)/100
  tx_censadm150[i] <- 1-sum(delta150)/150
  tx_censadm200[i] <- 1-sum(delta200)/200
  
  #censoring rate without adm censoring
  tx_cens20[i] <- 1-sum(del20)/20
  tx_cens30[i] <- 1-sum(del30)/30
  tx_cens50[i] <- 1-sum(del50)/50
  tx_cens60[i] <- 1-sum(del60)/60
  tx_cens80[i] <- 1-sum(del80)/80
  tx_cens100[i] <- 1-sum(del100)/100
  tx_cens150[i] <- 1-sum(del150)/150
  tx_cens200[i] <- 1-sum(del200)/200
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_2[i] <- ifelse((a20_2<0.05), 1, 0)
  err_moslrt20_2[i] <- ifelse((a202_2<0.05), 1, 0)
  err_rc20_2[i] <- ifelse((b20_2<0.05), 1, 0)
  err_de20_2[i] <- ifelse((c20_2<0.05), 1, 0)
  err_ee20_2[i] <- ifelse((d20_2<0.05), 1, 0)
  err_me20_2[i] <- ifelse((e20_2<0.05), 1, 0)
  err_max1_hoch20_2[i] <- ifelse((f20_2[2]<0.05), 1, 0)
  err_max1_exact20_2[i] <- ifelse((f20_2[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  err_oslrt20_4[i] <- ifelse((a20_4<0.05), 1, 0)
  err_moslrt20_4[i] <- ifelse((a202_4<0.05), 1, 0)
  err_rc20_4[i] <- ifelse((b20_4<0.05), 1, 0)
  err_de20_4[i] <- ifelse((c20_4<0.05), 1, 0)
  err_ee20_4[i] <- ifelse((d20_4<0.05), 1, 0)
  err_me20_4[i] <- ifelse((e20_4<0.05), 1, 0)
  err_max1_hoch20_4[i] <- ifelse((f20_4[2]<0.05), 1, 0)
  err_max1_exact20_4[i] <- ifelse((f20_4[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_2[i] <- ifelse((a30_2<0.05), 1, 0)
  err_moslrt30_2[i] <- ifelse((a302_2<0.05), 1, 0)
  err_rc30_2[i] <- ifelse((b30_2<0.05), 1, 0)
  err_de30_2[i] <- ifelse((c30_2<0.05), 1, 0)
  err_ee30_2[i] <- ifelse((d30_2<0.05), 1, 0)
  err_me30_2[i] <- ifelse((e30_2<0.05), 1, 0)
  err_max1_hoch30_2[i] <- ifelse((f30_2[2]<0.05), 1, 0)
  err_max1_exact30_2[i] <- ifelse((f30_2[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  err_oslrt30_4[i] <- ifelse((a30_4<0.05), 1, 0)
  err_moslrt30_4[i] <- ifelse((a302_4<0.05), 1, 0)
  err_rc30_4[i] <- ifelse((b30_4<0.05), 1, 0)
  err_de30_4[i] <- ifelse((c30_4<0.05), 1, 0)
  err_ee30_4[i] <- ifelse((d30_4<0.05), 1, 0)
  err_me30_4[i] <- ifelse((e30_4<0.05), 1, 0)
  err_max1_hoch30_4[i] <- ifelse((f30_4[2]<0.05), 1, 0)
  err_max1_exact30_4[i] <- ifelse((f30_4[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_2[i] <- ifelse((a50_2<0.05), 1, 0)
  err_moslrt50_2[i] <- ifelse((a502_2<0.05), 1, 0)
  err_rc50_2[i] <- ifelse((b50_2<0.05), 1, 0)
  err_de50_2[i] <- ifelse((c50_2<0.05), 1, 0)
  err_ee50_2[i] <- ifelse((d50_2<0.05), 1, 0)
  err_me50_2[i] <- ifelse((e50_2<0.05), 1, 0)
  err_max1_hoch50_2[i] <- ifelse((f50_2[2]<0.05), 1, 0)
  err_max1_exact50_2[i] <- ifelse((f50_2[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  err_oslrt50_4[i] <- ifelse((a50_4<0.05), 1, 0)
  err_moslrt50_4[i] <- ifelse((a502_4<0.05), 1, 0)
  err_rc50_4[i] <- ifelse((b50_4<0.05), 1, 0)
  err_de50_4[i] <- ifelse((c50_4<0.05), 1, 0)
  err_ee50_4[i] <- ifelse((d50_4<0.05), 1, 0)
  err_me50_4[i] <- ifelse((e50_4<0.05), 1, 0)
  err_max1_hoch50_4[i] <- ifelse((f50_4[2]<0.05), 1, 0)
  err_max1_exact50_4[i] <- ifelse((f50_4[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_2[i] <- ifelse((a60_2<0.05), 1, 0)
  err_moslrt60_2[i] <- ifelse((a602_2<0.05), 1, 0)
  err_rc60_2[i] <- ifelse((b60_2<0.05), 1, 0)
  err_de60_2[i] <- ifelse((c60_2<0.05), 1, 0)
  err_ee60_2[i] <- ifelse((d60_2<0.05), 1, 0)
  err_me60_2[i] <- ifelse((e60_2<0.05), 1, 0)
  err_max1_hoch60_2[i] <- ifelse((f60_2[2]<0.05), 1, 0)
  err_max1_exact60_2[i] <- ifelse((f60_2[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  err_oslrt60_4[i] <- ifelse((a60_4<0.05), 1, 0)
  err_moslrt60_4[i] <- ifelse((a602_4<0.05), 1, 0)
  err_rc60_4[i] <- ifelse((b60_4<0.05), 1, 0)
  err_de60_4[i] <- ifelse((c60_4<0.05), 1, 0)
  err_ee60_4[i] <- ifelse((d60_4<0.05), 1, 0)
  err_me60_4[i] <- ifelse((e60_4<0.05), 1, 0)
  err_max1_hoch60_4[i] <- ifelse((f60_4[2]<0.05), 1, 0)
  err_max1_exact60_4[i] <- ifelse((f60_4[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_2[i] <- ifelse((a80_2<0.05), 1, 0)
  err_moslrt80_2[i] <- ifelse((a802_2<0.05), 1, 0)
  err_rc80_2[i] <- ifelse((b80_2<0.05), 1, 0)
  err_de80_2[i] <- ifelse((c80_2<0.05), 1, 0)
  err_ee80_2[i] <- ifelse((d80_2<0.05), 1, 0)
  err_me80_2[i] <- ifelse((e80_2<0.05), 1, 0)
  err_max1_hoch80_2[i] <- ifelse((f80_2[2]<0.05), 1, 0)
  err_max1_exact80_2[i] <- ifelse((f80_2[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  err_oslrt80_4[i] <- ifelse((a80_4<0.05), 1, 0)
  err_moslrt80_4[i] <- ifelse((a802_4<0.05), 1, 0)
  err_rc80_4[i] <- ifelse((b80_4<0.05), 1, 0)
  err_de80_4[i] <- ifelse((c80_4<0.05), 1, 0)
  err_ee80_4[i] <- ifelse((d80_4<0.05), 1, 0)
  err_me80_4[i] <- ifelse((e80_4<0.05), 1, 0)
  err_max1_hoch80_4[i] <- ifelse((f80_4[2]<0.05), 1, 0)
  err_max1_exact80_4[i] <- ifelse((f80_4[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_2[i] <- ifelse((a100_2<0.05), 1, 0)
  err_moslrt100_2[i] <- ifelse((a1002_2<0.05), 1, 0)
  err_rc100_2[i] <- ifelse((b100_2<0.05), 1, 0)
  err_de100_2[i] <- ifelse((c100_2<0.05), 1, 0)
  err_ee100_2[i] <- ifelse((d100_2<0.05), 1, 0)
  err_me100_2[i] <- ifelse((e100_2<0.05), 1, 0)
  err_max1_hoch100_2[i] <- ifelse((f100_2[2]<0.05), 1, 0)
  err_max1_exact100_2[i] <- ifelse((f100_2[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  err_oslrt100_4[i] <- ifelse((a100_4<0.05), 1, 0)
  err_moslrt100_4[i] <- ifelse((a1002_4<0.05), 1, 0)
  err_rc100_4[i] <- ifelse((b100_4<0.05), 1, 0)
  err_de100_4[i] <- ifelse((c100_4<0.05), 1, 0)
  err_ee100_4[i] <- ifelse((d100_4<0.05), 1, 0)
  err_me100_4[i] <- ifelse((e100_4<0.05), 1, 0)
  err_max1_hoch100_4[i] <- ifelse((f100_4[2]<0.05), 1, 0)
  err_max1_exact100_4[i] <- ifelse((f100_4[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_2[i] <- ifelse((a150_2<0.05), 1, 0)
  err_moslrt150_2[i] <- ifelse((a1502_2<0.05), 1, 0)
  err_rc150_2[i] <- ifelse((b150_2<0.05), 1, 0)
  err_de150_2[i] <- ifelse((c150_2<0.05), 1, 0)
  err_ee150_2[i] <- ifelse((d150_2<0.05), 1, 0)
  err_me150_2[i] <- ifelse((e150_2<0.05), 1, 0)
  err_max1_hoch150_2[i] <- ifelse((f150_2[2]<0.05), 1, 0)
  err_max1_exact150_2[i] <- ifelse((f150_2[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  err_oslrt150_4[i] <- ifelse((a150_4<0.05), 1, 0)
  err_moslrt150_4[i] <- ifelse((a1502_4<0.05), 1, 0)
  err_rc150_4[i] <- ifelse((b150_4<0.05), 1, 0)
  err_de150_4[i] <- ifelse((c150_4<0.05), 1, 0)
  err_ee150_4[i] <- ifelse((d150_4<0.05), 1, 0)
  err_me150_4[i] <- ifelse((e150_4<0.05), 1, 0)
  err_max1_hoch150_4[i] <- ifelse((f150_4[2]<0.05), 1, 0)
  err_max1_exact150_4[i] <- ifelse((f150_4[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_2[i] <- ifelse((a200_2<0.05), 1, 0)
  err_moslrt200_2[i] <- ifelse((a2002_2<0.05), 1, 0)
  err_rc200_2[i] <- ifelse((b200_2<0.05), 1, 0)
  err_de200_2[i] <- ifelse((c200_2<0.05), 1, 0)
  err_ee200_2[i] <- ifelse((d200_2<0.05), 1, 0)
  err_me200_2[i] <- ifelse((e200_2<0.05), 1, 0)
  err_max1_hoch200_2[i] <- ifelse((f200_2[2]<0.05), 1, 0)
  err_max1_exact200_2[i] <- ifelse((f200_2[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)
  
  err_oslrt200_4[i] <- ifelse((a200_4<0.05), 1, 0)
  err_moslrt200_4[i] <- ifelse((a2002_4<0.05), 1, 0)
  err_rc200_4[i] <- ifelse((b200_4<0.05), 1, 0)
  err_de200_4[i] <- ifelse((c200_4<0.05), 1, 0)
  err_ee200_4[i] <- ifelse((d200_4<0.05), 1, 0)
  err_me200_4[i] <- ifelse((e200_4<0.05), 1, 0)
  err_max1_hoch200_4[i] <- ifelse((f200_4[2]<0.05), 1, 0)
  err_max1_exact200_4[i] <- ifelse((f200_4[3]<0.05), 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_2 <- sum(err_oslrt20_2)/nit
mos20_2 <- sum(err_moslrt20_2)/nit
rc20_2 <- sum(err_rc20_2)/nit
de20_2 <- sum(na.omit(err_de20_2))/length(na.omit(err_de20_2))
ee20_2 <- sum(err_ee20_2)/nit
me20_2 <- sum(err_me20_2)/nit
max1_hoch20_2 <- sum(err_max1_hoch20_2)/nit
max1_exact20_2 <- sum(na.omit(err_max1_exact20_2))/length(na.omit(err_max1_exact20_2))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_4 <- sum(err_oslrt20_4)/nit
mos20_4 <- sum(err_moslrt20_4)/nit
rc20_4 <- sum(err_rc20_4)/nit
de20_4 <- sum(na.omit(err_de20_4))/length(na.omit(err_de20_4))
ee20_4 <- sum(err_ee20_4)/nit
me20_4 <- sum(err_me20_4)/nit
max1_hoch20_4 <- sum(err_max1_hoch20_4)/nit
max1_exact20_4 <- sum(na.omit(err_max1_exact20_4))/length(na.omit(err_max1_exact20_4))

mean(tx_cens20)
mean(tx_censadm20)


os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_2 <- sum(err_oslrt30_2)/nit
mos30_2 <- sum(err_moslrt30_2)/nit
rc30_2 <- sum(err_rc30_2)/nit
de30_2 <- sum(na.omit(err_de30_2))/length(na.omit(err_de30_2))
ee30_2 <- sum(err_ee30_2)/nit
me30_2 <- sum(err_me30_2)/nit
max1_hoch30_2 <- sum(err_max1_hoch30_2)/nit
max1_exact30_2 <- sum(na.omit(err_max1_exact30_2))/length(na.omit(err_max1_exact30_2))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_4 <- sum(err_oslrt30_4)/nit
mos30_4 <- sum(err_moslrt30_4)/nit
rc30_4 <- sum(err_rc30_4)/nit
de30_4 <- sum(na.omit(err_de30_4))/length(na.omit(err_de30_4))
ee30_4 <- sum(err_ee30_4)/nit
me30_4 <- sum(err_me30_4)/nit
max1_hoch30_4 <- sum(err_max1_hoch30_4)/nit
max1_exact30_4 <- sum(na.omit(err_max1_exact30_4))/length(na.omit(err_max1_exact30_4))

mean(tx_cens30)
mean(tx_censadm30)


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_2 <- sum(err_oslrt50_2)/nit
mos50_2 <- sum(err_moslrt50_2)/nit
rc50_2 <- sum(err_rc50_2)/nit
de50_2 <- sum(err_de50_2)/nit
ee50_2 <- sum(err_ee50_2)/nit
me50_2 <- sum(err_me50_2)/nit
max1_hoch50_2 <- sum(err_max1_hoch50_2)/nit
max1_exact50_2 <- sum(na.omit(err_max1_exact50_2))/length(na.omit(err_max1_exact50_2))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_4 <- sum(err_oslrt50_4)/nit
mos50_4 <- sum(err_moslrt50_4)/nit
rc50_4 <- sum(err_rc50_4)/nit
de50_4 <- sum(err_de50_4)/nit
ee50_4 <- sum(err_ee50_4)/nit
me50_4 <- sum(err_me50_4)/nit
max1_hoch50_4 <- sum(err_max1_hoch50_4)/nit
max1_exact50_4 <- sum(na.omit(err_max1_exact50_4))/length(na.omit(err_max1_exact50_4))

mean(tx_cens50)
mean(tx_censadm50)


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_2 <- sum(err_oslrt60_2)/nit
mos60_2 <- sum(err_moslrt60_2)/nit
rc60_2 <- sum(err_rc60_2)/nit
de60_2 <- sum(err_de60_2)/nit
ee60_2 <- sum(err_ee60_2)/nit
me60_2 <- sum(err_me60_2)/nit
max1_hoch60_2 <- sum(err_max1_hoch60_2)/nit
max1_exact60_2 <- sum(na.omit(err_max1_exact60_2))/length(na.omit(err_max1_exact60_2))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_4 <- sum(err_oslrt60_4)/nit
mos60_4 <- sum(err_moslrt60_4)/nit
rc60_4 <- sum(err_rc60_4)/nit
de60_4 <- sum(err_de60_4)/nit
ee60_4 <- sum(err_ee60_4)/nit
me60_4 <- sum(err_me60_4)/nit
max1_hoch60_4 <- sum(err_max1_hoch60_4)/nit
max1_exact60_4 <- sum(na.omit(err_max1_exact60_4))/length(na.omit(err_max1_exact60_4))

mean(tx_cens60)
mean(tx_censadm60)


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_2 <- sum(err_oslrt80_2)/nit
mos80_2 <- sum(err_moslrt80_2)/nit
rc80_2 <- sum(err_rc80_2)/nit
de80_2 <- sum(err_de80_2)/nit
ee80_2 <- sum(err_ee80_2)/nit
me80_2 <- sum(err_me80_2)/nit
max1_hoch80_2 <- sum(err_max1_hoch80_2)/nit
max1_exact80_2 <- sum(na.omit(err_max1_exact80_2))/length(na.omit(err_max1_exact80_2))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_4 <- sum(err_oslrt80_4)/nit
mos80_4 <- sum(err_moslrt80_4)/nit
rc80_4 <- sum(err_rc80_4)/nit
de80_4 <- sum(err_de80_4)/nit
ee80_4 <- sum(err_ee80_4)/nit
me80_4 <- sum(err_me80_4)/nit
max1_hoch80_4 <- sum(err_max1_hoch80_4)/nit
max1_exact80_4 <- sum(na.omit(err_max1_exact80_4))/length(na.omit(err_max1_exact80_4))

mean(tx_cens80)
mean(tx_censadm80)


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_2 <- sum(err_oslrt100_2)/nit
mos100_2 <- sum(err_moslrt100_2)/nit
rc100_2 <- sum(err_rc100_2)/nit
de100_2 <- sum(err_de100_2)/nit
ee100_2 <- sum(err_ee100_2)/nit
me100_2 <- sum(err_me100_2)/nit
max1_hoch100_2 <- sum(err_max1_hoch100_2)/nit
max1_exact100_2 <- sum(na.omit(err_max1_exact100_2))/length(na.omit(err_max1_exact100_2))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_4 <- sum(err_oslrt100_4)/nit
mos100_4 <- sum(err_moslrt100_4)/nit
rc100_4 <- sum(err_rc100_4)/nit
de100_4 <- sum(err_de100_4)/nit
ee100_4 <- sum(err_ee100_4)/nit
me100_4 <- sum(err_me100_4)/nit
max1_hoch100_4 <- sum(err_max1_hoch100_4)/nit
max1_exact100_4 <- sum(na.omit(err_max1_exact100_4))/length(na.omit(err_max1_exact100_4))

mean(tx_cens100)
mean(tx_censadm100)


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_2 <- sum(err_oslrt150_2)/nit
mos150_2 <- sum(err_moslrt150_2)/nit
rc150_2 <- sum(err_rc150_2)/nit
de150_2 <- sum(err_de150_2)/nit
ee150_2 <- sum(err_ee150_2)/nit
me150_2 <- sum(na.omit(err_me150_2))/length(na.omit(err_me150_2))
max1_hoch150_2 <- sum(err_max1_hoch150_2)/nit
max1_exact150_2 <- sum(na.omit(err_max1_exact150_2))/length(na.omit(err_max1_exact150_2))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_4 <- sum(err_oslrt150_4)/nit
mos150_4 <- sum(err_moslrt150_4)/nit
rc150_4 <- sum(err_rc150_4)/nit
de150_4 <- sum(err_de150_4)/nit
ee150_4 <- sum(err_ee150_4)/nit
me150_4 <- sum(na.omit(err_me150_4))/length(na.omit(err_me150_4))
max1_hoch150_4 <- sum(err_max1_hoch150_4)/nit
max1_exact150_4 <- sum(na.omit(err_max1_exact150_4))/length(na.omit(err_max1_exact150_4))

mean(tx_cens150)
mean(tx_censadm150)

os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_2 <- sum(err_oslrt200_2)/nit
mos200_2 <- sum(err_moslrt200_2)/nit
rc200_2 <- sum(err_rc200_2)/nit
de200_2 <- sum(err_de200_2)/nit
ee200_2 <- sum(err_ee200_2)/nit
me200_2 <- sum(err_me200_2)/nit
max1_hoch200_2 <- sum(err_max1_hoch200_2)/nit
max1_exact200_2 <- sum(na.omit(err_max1_exact200_2))/length(na.omit(err_max1_exact200_2))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_4 <- sum(err_oslrt200_4)/nit
mos200_4 <- sum(err_moslrt200_4)/nit
rc200_4 <- sum(err_rc200_4)/nit
de200_4 <- sum(err_de200_4)/nit
ee200_4 <- sum(err_ee200_4)/nit
me200_4 <- sum(err_me200_4)/nit
max1_hoch200_4 <- sum(err_max1_hoch200_4)/nit
max1_exact200_4 <- sum(na.omit(err_max1_exact200_4))/length(na.omit(err_max1_exact200_4))

mean(tx_cens200)
mean(tx_censadm200)


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_2 <- c(os20_2, os30_2, os50_2, os60_2, os80_2, os100_2, os150_2, os200_2)
mosl_2 <- c(mos20_2, mos30_2, mos50_2, mos60_2, mos80_2, mos100_2, mos150_2, mos200_2)
rc_2 <- c(rc20_2, rc30_2, rc50_2, rc60_2, rc80_2, rc100_2, rc150_2, rc200_2)
de_2 <- c(de20_2, de30_2, de50_2, de60_2, de80_2, de100_2, de150_2, de200_2)
ee_2 <- c(ee20_2, ee30_2, ee50_2, ee60_2, ee80_2, ee100_2, ee150_2, ee200_2)
me_2 <- c(me20_2, me30_2, me50_2, me60_2, me80_2, me100_2, me150_2, me200_2)
max1_hochberg_2 <- c(max1_hoch20_2, max1_hoch30_2, max1_hoch50_2, max1_hoch60_2, max1_hoch80_2, max1_hoch100_2, max1_hoch150_2, max1_hoch200_2)
max1_pmult_2 <- c(max1_exact20_2, max1_exact30_2, max1_exact50_2, max1_exact60_2, max1_exact80_2, max1_exact100_2, max1_exact150_2, max1_exact200_2)
osl_2
mosl_2
ee_2
me_2
de_2
rc_2
max1_hochberg_2
max1_pmult_2

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

osl_4 <- c(os20_4, os30_4, os50_4, os60_4, os80_4, os100_4, os150_4, os200_4)
mosl_4 <- c(mos20_4, mos30_4, mos50_4, mos60_4, mos80_4, mos100_4, mos150_4, mos200_4)
rc_4 <- c(rc20_4, rc30_4, rc50_4, rc60_4, rc80_4, rc100_4, rc150_4, rc200_4)
de_4 <- c(de20_4, de30_4, de50_4, de60_4, de80_4, de100_4, de150_4, de200_4)
ee_4 <- c(ee20_4, ee30_4, ee50_4, ee60_4, ee80_4, ee100_4, ee150_4, ee200_4)
me_4 <- c(me20_4, me30_4, me50_4, me60_4, me80_4, me100_4, me150_4, me200_4)
max1_hochberg_4 <- c(max1_hoch20_4, max1_hoch30_4, max1_hoch50_4, max1_hoch60_4, max1_hoch80_4, max1_hoch100_4, max1_hoch150_4, max1_hoch200_4)
max1_pmult_4 <- c(max1_exact20_4, max1_exact30_4, max1_exact50_4, max1_exact60_4, max1_exact80_4, max1_exact100_4, max1_exact150_4, max1_exact200_4)
osl_4
mosl_4
ee_4
me_4
de_4
rc_4
max1_hochberg_4
max1_pmult_4

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 1',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_2 <- data.frame(Sample.size = n, Error = c(osl_2, mosl_2, ee_2, me_2, de_2, rc_2, max1_hochberg_2, max1_pmult_2),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_2$Test <- as.factor(d1_2$Test)
d1_2$Test <- factor(d1_2$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_2, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.8',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))

d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.6',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_4 <- data.frame(Sample.size = n, Error = c(osl_4, mosl_4, ee_4, me_4, de_4, rc_4, max1_hochberg_4, max1_pmult_4),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_4$Test <- as.factor(d1_4$Test)
d1_4$Test <- factor(d1_4$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_4, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.5',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


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
pi2 <- 0.8
pi3 <- 0.6
pi4 <- 0.5

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_2 <- rep(0, nit)
err_moslrt20_2 <- rep(0, nit)
err_rc20_2 <- rep(0, nit)
err_de20_2 <- rep(0, nit)
err_ee20_2 <- rep(0, nit)
err_me20_2 <- rep(0, nit)
err_max1_hoch20_2 <- rep(0, nit)
err_max1_exact20_2 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)

err_oslrt20_4 <- rep(0, nit)
err_moslrt20_4 <- rep(0, nit)
err_rc20_4 <- rep(0, nit)
err_de20_4 <- rep(0, nit)
err_ee20_4 <- rep(0, nit)
err_me20_4 <- rep(0, nit)
err_max1_hoch20_4 <- rep(0, nit)
err_max1_exact20_4 <- rep(0, nit)

tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_2 <- rep(0, nit)
err_moslrt30_2 <- rep(0, nit)
err_rc30_2 <- rep(0, nit)
err_de30_2 <- rep(0, nit)
err_ee30_2 <- rep(0, nit)
err_me30_2 <- rep(0, nit)
err_max1_hoch30_2 <- rep(0, nit)
err_max1_exact30_2 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)

err_oslrt30_4 <- rep(0, nit)
err_moslrt30_4 <- rep(0, nit)
err_rc30_4 <- rep(0, nit)
err_de30_4 <- rep(0, nit)
err_ee30_4 <- rep(0, nit)
err_me30_4 <- rep(0, nit)
err_max1_hoch30_4 <- rep(0, nit)
err_max1_exact30_4 <- rep(0, nit)

tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_2 <- rep(0, nit)
err_moslrt50_2 <- rep(0, nit)
err_rc50_2 <- rep(0, nit)
err_de50_2 <- rep(0, nit)
err_ee50_2 <- rep(0, nit)
err_me50_2 <- rep(0, nit)
err_max1_hoch50_2 <- rep(0, nit)
err_max1_exact50_2 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)

err_oslrt50_4 <- rep(0, nit)
err_moslrt50_4 <- rep(0, nit)
err_rc50_4 <- rep(0, nit)
err_de50_4 <- rep(0, nit)
err_ee50_4 <- rep(0, nit)
err_me50_4 <- rep(0, nit)
err_max1_hoch50_4 <- rep(0, nit)
err_max1_exact50_4 <- rep(0, nit)

tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_2 <- rep(0, nit)
err_moslrt60_2 <- rep(0, nit)
err_rc60_2 <- rep(0, nit)
err_de60_2 <- rep(0, nit)
err_ee60_2 <- rep(0, nit)
err_me60_2 <- rep(0, nit)
err_max1_hoch60_2 <- rep(0, nit)
err_max1_exact60_2 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)

err_oslrt60_4 <- rep(0, nit)
err_moslrt60_4 <- rep(0, nit)
err_rc60_4 <- rep(0, nit)
err_de60_4 <- rep(0, nit)
err_ee60_4 <- rep(0, nit)
err_me60_4 <- rep(0, nit)
err_max1_hoch60_4 <- rep(0, nit)
err_max1_exact60_4 <- rep(0, nit)

tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_2 <- rep(0, nit)
err_moslrt80_2 <- rep(0, nit)
err_rc80_2 <- rep(0, nit)
err_de80_2 <- rep(0, nit)
err_ee80_2 <- rep(0, nit)
err_me80_2 <- rep(0, nit)
err_max1_hoch80_2 <- rep(0, nit)
err_max1_exact80_2 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)

err_oslrt80_4 <- rep(0, nit)
err_moslrt80_4 <- rep(0, nit)
err_rc80_4 <- rep(0, nit)
err_de80_4 <- rep(0, nit)
err_ee80_4 <- rep(0, nit)
err_me80_4 <- rep(0, nit)
err_max1_hoch80_4 <- rep(0, nit)
err_max1_exact80_4 <- rep(0, nit)

tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_2 <- rep(0, nit)
err_moslrt100_2 <- rep(0, nit)
err_rc100_2 <- rep(0, nit)
err_de100_2 <- rep(0, nit)
err_ee100_2 <- rep(0, nit)
err_me100_2 <- rep(0, nit)
err_max1_hoch100_2 <- rep(0, nit)
err_max1_exact100_2 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)

err_oslrt100_4 <- rep(0, nit)
err_moslrt100_4 <- rep(0, nit)
err_rc100_4 <- rep(0, nit)
err_de100_4 <- rep(0, nit)
err_ee100_4 <- rep(0, nit)
err_me100_4 <- rep(0, nit)
err_max1_hoch100_4 <- rep(0, nit)
err_max1_exact100_4 <- rep(0, nit)

tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_2 <- rep(0, nit)
err_moslrt150_2 <- rep(0, nit)
err_rc150_2 <- rep(0, nit)
err_de150_2 <- rep(0, nit)
err_ee150_2 <- rep(0, nit)
err_me150_2 <- rep(0, nit)
err_max1_hoch150_2 <- rep(0, nit)
err_max1_exact150_2 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)

err_oslrt150_4 <- rep(0, nit)
err_moslrt150_4 <- rep(0, nit)
err_rc150_4 <- rep(0, nit)
err_de150_4 <- rep(0, nit)
err_ee150_4 <- rep(0, nit)
err_me150_4 <- rep(0, nit)
err_max1_hoch150_4 <- rep(0, nit)
err_max1_exact150_4 <- rep(0, nit)

tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_2 <- rep(0, nit)
err_moslrt200_2 <- rep(0, nit)
err_rc200_2 <- rep(0, nit)
err_de200_2 <- rep(0, nit)
err_ee200_2 <- rep(0, nit)
err_me200_2 <- rep(0, nit)
err_max1_hoch200_2 <- rep(0, nit)
err_max1_exact200_2 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_oslrt200_4 <- rep(0, nit)
err_moslrt200_4 <- rep(0, nit)
err_rc200_4 <- rep(0, nit)
err_de200_4 <- rep(0, nit)
err_ee200_4 <- rep(0, nit)
err_me200_4 <- rep(0, nit)
err_max1_hoch200_4 <- rep(0, nit)
err_max1_exact200_4 <- rep(0, nit)

tx_cens200 <- rep(0, nit)
tx_censadm200 <- rep(0, nit)

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
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  a202_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)[2]
  b20_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  c20_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi2)[2]
  d20_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi2)[2]
  e20_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f20_2 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a20_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  a202_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)[2]
  b20_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  c20_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi4)[2]
  d20_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi4)[2]
  e20_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f20_4 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  a302_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)[2]
  b30_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  c30_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi2)[2]
  d30_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi2)[2]
  e30_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f30_2 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a30_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  a302_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)[2]
  b30_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  c30_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi4)[2]
  d30_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi4)[2]
  e30_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f30_4 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  a502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)[2]
  b50_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  c50_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi2)[2]
  d50_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi2)[2]
  e50_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f50_2 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a50_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  a502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)[2]
  b50_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  c50_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi4)[2]
  d50_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi4)[2]
  e50_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f50_4 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  a602_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)[2]
  b60_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  c60_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi2)[2]
  d60_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi2)[2]
  e60_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f60_2 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a60_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  a602_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)[2]
  b60_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  c60_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi4)[2]
  d60_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi4)[2]
  e60_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f60_4 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  a802_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)[2]
  b80_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  c80_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi2)[2]
  d80_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi2)[2]
  e80_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f80_2 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a80_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  a802_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)[2]
  b80_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  c80_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi4)[2]
  d80_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi4)[2]
  e80_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f80_4 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  a1002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)[2]
  b100_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  c100_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi2)[2]
  d100_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi2)[2]
  e100_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f100_2 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a100_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  a1002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)[2]
  b100_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  c100_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi4)[2]
  d100_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi4)[2]
  e100_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f100_4 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  a1502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)[2]
  b150_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  c150_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi2)[2]
  d150_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi2)[2]
  e150_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f150_2 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a150_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  a1502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)[2]
  b150_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  c150_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi4)[2]
  d150_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi4)[2]
  e150_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f150_4 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  a2002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)[2]
  b200_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  c200_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi2)[2]
  d200_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi2)[2]
  e200_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f200_2 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a200_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  a2002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)[2]
  b200_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  c200_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi4)[2]
  d200_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi4)[2]
  e200_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f200_4 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  #censoring rate with adm censoring
  tx_censadm20[i] <- 1-sum(delta20)/20
  tx_censadm30[i] <- 1-sum(delta30)/30
  tx_censadm50[i] <- 1-sum(delta50)/50
  tx_censadm60[i] <- 1-sum(delta60)/60
  tx_censadm80[i] <- 1-sum(delta80)/80
  tx_censadm100[i] <- 1-sum(delta100)/100
  tx_censadm150[i] <- 1-sum(delta150)/150
  tx_censadm200[i] <- 1-sum(delta200)/200
  
  #censoring rate without adm censoring
  tx_cens20[i] <- 1-sum(del20)/20
  tx_cens30[i] <- 1-sum(del30)/30
  tx_cens50[i] <- 1-sum(del50)/50
  tx_cens60[i] <- 1-sum(del60)/60
  tx_cens80[i] <- 1-sum(del80)/80
  tx_cens100[i] <- 1-sum(del100)/100
  tx_cens150[i] <- 1-sum(del150)/150
  tx_cens200[i] <- 1-sum(del200)/200
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_2[i] <- ifelse((a20_2<0.05), 1, 0)
  err_moslrt20_2[i] <- ifelse((a202_2<0.05), 1, 0)
  err_rc20_2[i] <- ifelse((b20_2<0.05), 1, 0)
  err_de20_2[i] <- ifelse((c20_2<0.05), 1, 0)
  err_ee20_2[i] <- ifelse((d20_2<0.05), 1, 0)
  err_me20_2[i] <- ifelse((e20_2<0.05), 1, 0)
  err_max1_hoch20_2[i] <- ifelse((f20_2[2]<0.05), 1, 0)
  err_max1_exact20_2[i] <- ifelse((f20_2[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  err_oslrt20_4[i] <- ifelse((a20_4<0.05), 1, 0)
  err_moslrt20_4[i] <- ifelse((a202_4<0.05), 1, 0)
  err_rc20_4[i] <- ifelse((b20_4<0.05), 1, 0)
  err_de20_4[i] <- ifelse((c20_4<0.05), 1, 0)
  err_ee20_4[i] <- ifelse((d20_4<0.05), 1, 0)
  err_me20_4[i] <- ifelse((e20_4<0.05), 1, 0)
  err_max1_hoch20_4[i] <- ifelse((f20_4[2]<0.05), 1, 0)
  err_max1_exact20_4[i] <- ifelse((f20_4[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_2[i] <- ifelse((a30_2<0.05), 1, 0)
  err_moslrt30_2[i] <- ifelse((a302_2<0.05), 1, 0)
  err_rc30_2[i] <- ifelse((b30_2<0.05), 1, 0)
  err_de30_2[i] <- ifelse((c30_2<0.05), 1, 0)
  err_ee30_2[i] <- ifelse((d30_2<0.05), 1, 0)
  err_me30_2[i] <- ifelse((e30_2<0.05), 1, 0)
  err_max1_hoch30_2[i] <- ifelse((f30_2[2]<0.05), 1, 0)
  err_max1_exact30_2[i] <- ifelse((f30_2[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  err_oslrt30_4[i] <- ifelse((a30_4<0.05), 1, 0)
  err_moslrt30_4[i] <- ifelse((a302_4<0.05), 1, 0)
  err_rc30_4[i] <- ifelse((b30_4<0.05), 1, 0)
  err_de30_4[i] <- ifelse((c30_4<0.05), 1, 0)
  err_ee30_4[i] <- ifelse((d30_4<0.05), 1, 0)
  err_me30_4[i] <- ifelse((e30_4<0.05), 1, 0)
  err_max1_hoch30_4[i] <- ifelse((f30_4[2]<0.05), 1, 0)
  err_max1_exact30_4[i] <- ifelse((f30_4[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_2[i] <- ifelse((a50_2<0.05), 1, 0)
  err_moslrt50_2[i] <- ifelse((a502_2<0.05), 1, 0)
  err_rc50_2[i] <- ifelse((b50_2<0.05), 1, 0)
  err_de50_2[i] <- ifelse((c50_2<0.05), 1, 0)
  err_ee50_2[i] <- ifelse((d50_2<0.05), 1, 0)
  err_me50_2[i] <- ifelse((e50_2<0.05), 1, 0)
  err_max1_hoch50_2[i] <- ifelse((f50_2[2]<0.05), 1, 0)
  err_max1_exact50_2[i] <- ifelse((f50_2[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  err_oslrt50_4[i] <- ifelse((a50_4<0.05), 1, 0)
  err_moslrt50_4[i] <- ifelse((a502_4<0.05), 1, 0)
  err_rc50_4[i] <- ifelse((b50_4<0.05), 1, 0)
  err_de50_4[i] <- ifelse((c50_4<0.05), 1, 0)
  err_ee50_4[i] <- ifelse((d50_4<0.05), 1, 0)
  err_me50_4[i] <- ifelse((e50_4<0.05), 1, 0)
  err_max1_hoch50_4[i] <- ifelse((f50_4[2]<0.05), 1, 0)
  err_max1_exact50_4[i] <- ifelse((f50_4[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_2[i] <- ifelse((a60_2<0.05), 1, 0)
  err_moslrt60_2[i] <- ifelse((a602_2<0.05), 1, 0)
  err_rc60_2[i] <- ifelse((b60_2<0.05), 1, 0)
  err_de60_2[i] <- ifelse((c60_2<0.05), 1, 0)
  err_ee60_2[i] <- ifelse((d60_2<0.05), 1, 0)
  err_me60_2[i] <- ifelse((e60_2<0.05), 1, 0)
  err_max1_hoch60_2[i] <- ifelse((f60_2[2]<0.05), 1, 0)
  err_max1_exact60_2[i] <- ifelse((f60_2[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  err_oslrt60_4[i] <- ifelse((a60_4<0.05), 1, 0)
  err_moslrt60_4[i] <- ifelse((a602_4<0.05), 1, 0)
  err_rc60_4[i] <- ifelse((b60_4<0.05), 1, 0)
  err_de60_4[i] <- ifelse((c60_4<0.05), 1, 0)
  err_ee60_4[i] <- ifelse((d60_4<0.05), 1, 0)
  err_me60_4[i] <- ifelse((e60_4<0.05), 1, 0)
  err_max1_hoch60_4[i] <- ifelse((f60_4[2]<0.05), 1, 0)
  err_max1_exact60_4[i] <- ifelse((f60_4[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_2[i] <- ifelse((a80_2<0.05), 1, 0)
  err_moslrt80_2[i] <- ifelse((a802_2<0.05), 1, 0)
  err_rc80_2[i] <- ifelse((b80_2<0.05), 1, 0)
  err_de80_2[i] <- ifelse((c80_2<0.05), 1, 0)
  err_ee80_2[i] <- ifelse((d80_2<0.05), 1, 0)
  err_me80_2[i] <- ifelse((e80_2<0.05), 1, 0)
  err_max1_hoch80_2[i] <- ifelse((f80_2[2]<0.05), 1, 0)
  err_max1_exact80_2[i] <- ifelse((f80_2[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  err_oslrt80_4[i] <- ifelse((a80_4<0.05), 1, 0)
  err_moslrt80_4[i] <- ifelse((a802_4<0.05), 1, 0)
  err_rc80_4[i] <- ifelse((b80_4<0.05), 1, 0)
  err_de80_4[i] <- ifelse((c80_4<0.05), 1, 0)
  err_ee80_4[i] <- ifelse((d80_4<0.05), 1, 0)
  err_me80_4[i] <- ifelse((e80_4<0.05), 1, 0)
  err_max1_hoch80_4[i] <- ifelse((f80_4[2]<0.05), 1, 0)
  err_max1_exact80_4[i] <- ifelse((f80_4[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_2[i] <- ifelse((a100_2<0.05), 1, 0)
  err_moslrt100_2[i] <- ifelse((a1002_2<0.05), 1, 0)
  err_rc100_2[i] <- ifelse((b100_2<0.05), 1, 0)
  err_de100_2[i] <- ifelse((c100_2<0.05), 1, 0)
  err_ee100_2[i] <- ifelse((d100_2<0.05), 1, 0)
  err_me100_2[i] <- ifelse((e100_2<0.05), 1, 0)
  err_max1_hoch100_2[i] <- ifelse((f100_2[2]<0.05), 1, 0)
  err_max1_exact100_2[i] <- ifelse((f100_2[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  err_oslrt100_4[i] <- ifelse((a100_4<0.05), 1, 0)
  err_moslrt100_4[i] <- ifelse((a1002_4<0.05), 1, 0)
  err_rc100_4[i] <- ifelse((b100_4<0.05), 1, 0)
  err_de100_4[i] <- ifelse((c100_4<0.05), 1, 0)
  err_ee100_4[i] <- ifelse((d100_4<0.05), 1, 0)
  err_me100_4[i] <- ifelse((e100_4<0.05), 1, 0)
  err_max1_hoch100_4[i] <- ifelse((f100_4[2]<0.05), 1, 0)
  err_max1_exact100_4[i] <- ifelse((f100_4[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_2[i] <- ifelse((a150_2<0.05), 1, 0)
  err_moslrt150_2[i] <- ifelse((a1502_2<0.05), 1, 0)
  err_rc150_2[i] <- ifelse((b150_2<0.05), 1, 0)
  err_de150_2[i] <- ifelse((c150_2<0.05), 1, 0)
  err_ee150_2[i] <- ifelse((d150_2<0.05), 1, 0)
  err_me150_2[i] <- ifelse((e150_2<0.05), 1, 0)
  err_max1_hoch150_2[i] <- ifelse((f150_2[2]<0.05), 1, 0)
  err_max1_exact150_2[i] <- ifelse((f150_2[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  err_oslrt150_4[i] <- ifelse((a150_4<0.05), 1, 0)
  err_moslrt150_4[i] <- ifelse((a1502_4<0.05), 1, 0)
  err_rc150_4[i] <- ifelse((b150_4<0.05), 1, 0)
  err_de150_4[i] <- ifelse((c150_4<0.05), 1, 0)
  err_ee150_4[i] <- ifelse((d150_4<0.05), 1, 0)
  err_me150_4[i] <- ifelse((e150_4<0.05), 1, 0)
  err_max1_hoch150_4[i] <- ifelse((f150_4[2]<0.05), 1, 0)
  err_max1_exact150_4[i] <- ifelse((f150_4[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_2[i] <- ifelse((a200_2<0.05), 1, 0)
  err_moslrt200_2[i] <- ifelse((a2002_2<0.05), 1, 0)
  err_rc200_2[i] <- ifelse((b200_2<0.05), 1, 0)
  err_de200_2[i] <- ifelse((c200_2<0.05), 1, 0)
  err_ee200_2[i] <- ifelse((d200_2<0.05), 1, 0)
  err_me200_2[i] <- ifelse((e200_2<0.05), 1, 0)
  err_max1_hoch200_2[i] <- ifelse((f200_2[2]<0.05), 1, 0)
  err_max1_exact200_2[i] <- ifelse((f200_2[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)
  
  err_oslrt200_4[i] <- ifelse((a200_4<0.05), 1, 0)
  err_moslrt200_4[i] <- ifelse((a2002_4<0.05), 1, 0)
  err_rc200_4[i] <- ifelse((b200_4<0.05), 1, 0)
  err_de200_4[i] <- ifelse((c200_4<0.05), 1, 0)
  err_ee200_4[i] <- ifelse((d200_4<0.05), 1, 0)
  err_me200_4[i] <- ifelse((e200_4<0.05), 1, 0)
  err_max1_hoch200_4[i] <- ifelse((f200_4[2]<0.05), 1, 0)
  err_max1_exact200_4[i] <- ifelse((f200_4[3]<0.05), 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_2 <- sum(err_oslrt20_2)/nit
mos20_2 <- sum(err_moslrt20_2)/nit
rc20_2 <- sum(err_rc20_2)/nit
de20_2 <- sum(na.omit(err_de20_2))/length(na.omit(err_de20_2))
ee20_2 <- sum(err_ee20_2)/nit
me20_2 <- sum(err_me20_2)/nit
max1_hoch20_2 <- sum(err_max1_hoch20_2)/nit
max1_exact20_2 <- sum(na.omit(err_max1_exact20_2))/length(na.omit(err_max1_exact20_2))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_4 <- sum(err_oslrt20_4)/nit
mos20_4 <- sum(err_moslrt20_4)/nit
rc20_4 <- sum(err_rc20_4)/nit
de20_4 <- sum(na.omit(err_de20_4))/length(na.omit(err_de20_4))
ee20_4 <- sum(err_ee20_4)/nit
me20_4 <- sum(err_me20_4)/nit
max1_hoch20_4 <- sum(err_max1_hoch20_4)/nit
max1_exact20_4 <- sum(na.omit(err_max1_exact20_4))/length(na.omit(err_max1_exact20_4))

mean(tx_cens20)
mean(tx_censadm20)


os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_2 <- sum(err_oslrt30_2)/nit
mos30_2 <- sum(err_moslrt30_2)/nit
rc30_2 <- sum(err_rc30_2)/nit
de30_2 <- sum(na.omit(err_de30_2))/length(na.omit(err_de30_2))
ee30_2 <- sum(err_ee30_2)/nit
me30_2 <- sum(err_me30_2)/nit
max1_hoch30_2 <- sum(err_max1_hoch30_2)/nit
max1_exact30_2 <- sum(na.omit(err_max1_exact30_2))/length(na.omit(err_max1_exact30_2))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_4 <- sum(err_oslrt30_4)/nit
mos30_4 <- sum(err_moslrt30_4)/nit
rc30_4 <- sum(err_rc30_4)/nit
de30_4 <- sum(na.omit(err_de30_4))/length(na.omit(err_de30_4))
ee30_4 <- sum(err_ee30_4)/nit
me30_4 <- sum(err_me30_4)/nit
max1_hoch30_4 <- sum(err_max1_hoch30_4)/nit
max1_exact30_4 <- sum(na.omit(err_max1_exact30_4))/length(na.omit(err_max1_exact30_4))

mean(tx_cens30)
mean(tx_censadm30)


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_2 <- sum(err_oslrt50_2)/nit
mos50_2 <- sum(err_moslrt50_2)/nit
rc50_2 <- sum(err_rc50_2)/nit
de50_2 <- sum(err_de50_2)/nit
ee50_2 <- sum(err_ee50_2)/nit
me50_2 <- sum(err_me50_2)/nit
max1_hoch50_2 <- sum(err_max1_hoch50_2)/nit
max1_exact50_2 <- sum(na.omit(err_max1_exact50_2))/length(na.omit(err_max1_exact50_2))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_4 <- sum(err_oslrt50_4)/nit
mos50_4 <- sum(err_moslrt50_4)/nit
rc50_4 <- sum(err_rc50_4)/nit
de50_4 <- sum(err_de50_4)/nit
ee50_4 <- sum(err_ee50_4)/nit
me50_4 <- sum(err_me50_4)/nit
max1_hoch50_4 <- sum(err_max1_hoch50_4)/nit
max1_exact50_4 <- sum(na.omit(err_max1_exact50_4))/length(na.omit(err_max1_exact50_4))

mean(tx_cens50)
mean(tx_censadm50)


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_2 <- sum(err_oslrt60_2)/nit
mos60_2 <- sum(err_moslrt60_2)/nit
rc60_2 <- sum(err_rc60_2)/nit
de60_2 <- sum(err_de60_2)/nit
ee60_2 <- sum(err_ee60_2)/nit
me60_2 <- sum(err_me60_2)/nit
max1_hoch60_2 <- sum(err_max1_hoch60_2)/nit
max1_exact60_2 <- sum(na.omit(err_max1_exact60_2))/length(na.omit(err_max1_exact60_2))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_4 <- sum(err_oslrt60_4)/nit
mos60_4 <- sum(err_moslrt60_4)/nit
rc60_4 <- sum(err_rc60_4)/nit
de60_4 <- sum(err_de60_4)/nit
ee60_4 <- sum(err_ee60_4)/nit
me60_4 <- sum(err_me60_4)/nit
max1_hoch60_4 <- sum(err_max1_hoch60_4)/nit
max1_exact60_4 <- sum(na.omit(err_max1_exact60_4))/length(na.omit(err_max1_exact60_4))

mean(tx_cens60)
mean(tx_censadm60)


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_2 <- sum(err_oslrt80_2)/nit
mos80_2 <- sum(err_moslrt80_2)/nit
rc80_2 <- sum(err_rc80_2)/nit
de80_2 <- sum(err_de80_2)/nit
ee80_2 <- sum(err_ee80_2)/nit
me80_2 <- sum(err_me80_2)/nit
max1_hoch80_2 <- sum(err_max1_hoch80_2)/nit
max1_exact80_2 <- sum(na.omit(err_max1_exact80_2))/length(na.omit(err_max1_exact80_2))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_4 <- sum(err_oslrt80_4)/nit
mos80_4 <- sum(err_moslrt80_4)/nit
rc80_4 <- sum(err_rc80_4)/nit
de80_4 <- sum(err_de80_4)/nit
ee80_4 <- sum(err_ee80_4)/nit
me80_4 <- sum(err_me80_4)/nit
max1_hoch80_4 <- sum(err_max1_hoch80_4)/nit
max1_exact80_4 <- sum(na.omit(err_max1_exact80_4))/length(na.omit(err_max1_exact80_4))

mean(tx_cens80)
mean(tx_censadm80)


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_2 <- sum(err_oslrt100_2)/nit
mos100_2 <- sum(err_moslrt100_2)/nit
rc100_2 <- sum(err_rc100_2)/nit
de100_2 <- sum(err_de100_2)/nit
ee100_2 <- sum(err_ee100_2)/nit
me100_2 <- sum(err_me100_2)/nit
max1_hoch100_2 <- sum(err_max1_hoch100_2)/nit
max1_exact100_2 <- sum(na.omit(err_max1_exact100_2))/length(na.omit(err_max1_exact100_2))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_4 <- sum(err_oslrt100_4)/nit
mos100_4 <- sum(err_moslrt100_4)/nit
rc100_4 <- sum(err_rc100_4)/nit
de100_4 <- sum(err_de100_4)/nit
ee100_4 <- sum(err_ee100_4)/nit
me100_4 <- sum(err_me100_4)/nit
max1_hoch100_4 <- sum(err_max1_hoch100_4)/nit
max1_exact100_4 <- sum(na.omit(err_max1_exact100_4))/length(na.omit(err_max1_exact100_4))

mean(tx_cens100)
mean(tx_censadm100)


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_2 <- sum(err_oslrt150_2)/nit
mos150_2 <- sum(err_moslrt150_2)/nit
rc150_2 <- sum(err_rc150_2)/nit
de150_2 <- sum(err_de150_2)/nit
ee150_2 <- sum(err_ee150_2)/nit
me150_2 <- sum(na.omit(err_me150_2))/length(na.omit(err_me150_2))
max1_hoch150_2 <- sum(err_max1_hoch150_2)/nit
max1_exact150_2 <- sum(na.omit(err_max1_exact150_2))/length(na.omit(err_max1_exact150_2))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_4 <- sum(err_oslrt150_4)/nit
mos150_4 <- sum(err_moslrt150_4)/nit
rc150_4 <- sum(err_rc150_4)/nit
de150_4 <- sum(err_de150_4)/nit
ee150_4 <- sum(err_ee150_4)/nit
me150_4 <- sum(na.omit(err_me150_4))/length(na.omit(err_me150_4))
max1_hoch150_4 <- sum(err_max1_hoch150_4)/nit
max1_exact150_4 <- sum(na.omit(err_max1_exact150_4))/length(na.omit(err_max1_exact150_4))

mean(tx_cens150)
mean(tx_censadm150)

os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_2 <- sum(err_oslrt200_2)/nit
mos200_2 <- sum(err_moslrt200_2)/nit
rc200_2 <- sum(err_rc200_2)/nit
de200_2 <- sum(err_de200_2)/nit
ee200_2 <- sum(err_ee200_2)/nit
me200_2 <- sum(err_me200_2)/nit
max1_hoch200_2 <- sum(err_max1_hoch200_2)/nit
max1_exact200_2 <- sum(na.omit(err_max1_exact200_2))/length(na.omit(err_max1_exact200_2))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_4 <- sum(err_oslrt200_4)/nit
mos200_4 <- sum(err_moslrt200_4)/nit
rc200_4 <- sum(err_rc200_4)/nit
de200_4 <- sum(err_de200_4)/nit
ee200_4 <- sum(err_ee200_4)/nit
me200_4 <- sum(err_me200_4)/nit
max1_hoch200_4 <- sum(err_max1_hoch200_4)/nit
max1_exact200_4 <- sum(na.omit(err_max1_exact200_4))/length(na.omit(err_max1_exact200_4))

mean(tx_cens200)
mean(tx_censadm200)


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_2 <- c(os20_2, os30_2, os50_2, os60_2, os80_2, os100_2, os150_2, os200_2)
mosl_2 <- c(mos20_2, mos30_2, mos50_2, mos60_2, mos80_2, mos100_2, mos150_2, mos200_2)
rc_2 <- c(rc20_2, rc30_2, rc50_2, rc60_2, rc80_2, rc100_2, rc150_2, rc200_2)
de_2 <- c(de20_2, de30_2, de50_2, de60_2, de80_2, de100_2, de150_2, de200_2)
ee_2 <- c(ee20_2, ee30_2, ee50_2, ee60_2, ee80_2, ee100_2, ee150_2, ee200_2)
me_2 <- c(me20_2, me30_2, me50_2, me60_2, me80_2, me100_2, me150_2, me200_2)
max1_hochberg_2 <- c(max1_hoch20_2, max1_hoch30_2, max1_hoch50_2, max1_hoch60_2, max1_hoch80_2, max1_hoch100_2, max1_hoch150_2, max1_hoch200_2)
max1_pmult_2 <- c(max1_exact20_2, max1_exact30_2, max1_exact50_2, max1_exact60_2, max1_exact80_2, max1_exact100_2, max1_exact150_2, max1_exact200_2)
osl_2
mosl_2
ee_2
me_2
de_2
rc_2
max1_hochberg_2
max1_pmult_2

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

osl_4 <- c(os20_4, os30_4, os50_4, os60_4, os80_4, os100_4, os150_4, os200_4)
mosl_4 <- c(mos20_4, mos30_4, mos50_4, mos60_4, mos80_4, mos100_4, mos150_4, mos200_4)
rc_4 <- c(rc20_4, rc30_4, rc50_4, rc60_4, rc80_4, rc100_4, rc150_4, rc200_4)
de_4 <- c(de20_4, de30_4, de50_4, de60_4, de80_4, de100_4, de150_4, de200_4)
ee_4 <- c(ee20_4, ee30_4, ee50_4, ee60_4, ee80_4, ee100_4, ee150_4, ee200_4)
me_4 <- c(me20_4, me30_4, me50_4, me60_4, me80_4, me100_4, me150_4, me200_4)
max1_hochberg_4 <- c(max1_hoch20_4, max1_hoch30_4, max1_hoch50_4, max1_hoch60_4, max1_hoch80_4, max1_hoch100_4, max1_hoch150_4, max1_hoch200_4)
max1_pmult_4 <- c(max1_exact20_4, max1_exact30_4, max1_exact50_4, max1_exact60_4, max1_exact80_4, max1_exact100_4, max1_exact150_4, max1_exact200_4)
osl_4
mosl_4
ee_4
me_4
de_4
rc_4
max1_hochberg_4
max1_pmult_4

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 1',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_2 <- data.frame(Sample.size = n, Error = c(osl_2, mosl_2, ee_2, me_2, de_2, rc_2, max1_hochberg_2, max1_pmult_2),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_2$Test <- as.factor(d1_2$Test)
d1_2$Test <- factor(d1_2$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_2, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.8',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))

d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.6',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_4 <- data.frame(Sample.size = n, Error = c(osl_4, mosl_4, ee_4, me_4, de_4, rc_4, max1_hochberg_4, max1_pmult_4),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_4$Test <- as.factor(d1_4$Test)
d1_4$Test <- factor(d1_4$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_4, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.5',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


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
#t <- rpwexp(n = 3000, rate = c(1/scale0*1.3, 1/scale0*0.7), intervals = CP, cumulative = FALSE)
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
#HR1 = 1.3 & HR2 = 0.7 (0 0.015 0.06 0.11 0.18)
lambda_cens <- 0.06   #15% of censoring for HR1 = 2 & HR2 = 0.5

CP_EE <- 1
CP_DE <- 1
CP_ME1 <- 1
CP_ME2 <- 4

ta <- 3   #accrual time - 3 years
tf <- 4   #follow-up time - 4 years

pi1 <- 1
pi2 <- 0.8
pi3 <- 0.6
pi4 <- 0.5

err_oslrt20_1 <- rep(0, nit)
err_moslrt20_1 <- rep(0, nit)
err_rc20_1 <- rep(0, nit)
err_de20_1 <- rep(0, nit)
err_ee20_1 <- rep(0, nit)
err_me20_1 <- rep(0, nit)
err_max1_hoch20_1 <- rep(0, nit)
err_max1_exact20_1 <- rep(0, nit)

err_oslrt20_2 <- rep(0, nit)
err_moslrt20_2 <- rep(0, nit)
err_rc20_2 <- rep(0, nit)
err_de20_2 <- rep(0, nit)
err_ee20_2 <- rep(0, nit)
err_me20_2 <- rep(0, nit)
err_max1_hoch20_2 <- rep(0, nit)
err_max1_exact20_2 <- rep(0, nit)

err_oslrt20_3 <- rep(0, nit)
err_moslrt20_3 <- rep(0, nit)
err_rc20_3 <- rep(0, nit)
err_de20_3 <- rep(0, nit)
err_ee20_3 <- rep(0, nit)
err_me20_3 <- rep(0, nit)
err_max1_hoch20_3 <- rep(0, nit)
err_max1_exact20_3 <- rep(0, nit)

err_oslrt20_4 <- rep(0, nit)
err_moslrt20_4 <- rep(0, nit)
err_rc20_4 <- rep(0, nit)
err_de20_4 <- rep(0, nit)
err_ee20_4 <- rep(0, nit)
err_me20_4 <- rep(0, nit)
err_max1_hoch20_4 <- rep(0, nit)
err_max1_exact20_4 <- rep(0, nit)

tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)


err_oslrt30_1 <- rep(0, nit)
err_moslrt30_1 <- rep(0, nit)
err_rc30_1 <- rep(0, nit)
err_de30_1 <- rep(0, nit)
err_ee30_1 <- rep(0, nit)
err_me30_1 <- rep(0, nit)
err_max1_hoch30_1 <- rep(0, nit)
err_max1_exact30_1 <- rep(0, nit)

err_oslrt30_2 <- rep(0, nit)
err_moslrt30_2 <- rep(0, nit)
err_rc30_2 <- rep(0, nit)
err_de30_2 <- rep(0, nit)
err_ee30_2 <- rep(0, nit)
err_me30_2 <- rep(0, nit)
err_max1_hoch30_2 <- rep(0, nit)
err_max1_exact30_2 <- rep(0, nit)

err_oslrt30_3 <- rep(0, nit)
err_moslrt30_3 <- rep(0, nit)
err_rc30_3 <- rep(0, nit)
err_de30_3 <- rep(0, nit)
err_ee30_3 <- rep(0, nit)
err_me30_3 <- rep(0, nit)
err_max1_hoch30_3 <- rep(0, nit)
err_max1_exact30_3 <- rep(0, nit)

err_oslrt30_4 <- rep(0, nit)
err_moslrt30_4 <- rep(0, nit)
err_rc30_4 <- rep(0, nit)
err_de30_4 <- rep(0, nit)
err_ee30_4 <- rep(0, nit)
err_me30_4 <- rep(0, nit)
err_max1_hoch30_4 <- rep(0, nit)
err_max1_exact30_4 <- rep(0, nit)

tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)


err_oslrt50_1 <- rep(0, nit)
err_moslrt50_1 <- rep(0, nit)
err_rc50_1 <- rep(0, nit)
err_de50_1 <- rep(0, nit)
err_ee50_1 <- rep(0, nit)
err_me50_1 <- rep(0, nit)
err_max1_hoch50_1 <- rep(0, nit)
err_max1_exact50_1 <- rep(0, nit)

err_oslrt50_2 <- rep(0, nit)
err_moslrt50_2 <- rep(0, nit)
err_rc50_2 <- rep(0, nit)
err_de50_2 <- rep(0, nit)
err_ee50_2 <- rep(0, nit)
err_me50_2 <- rep(0, nit)
err_max1_hoch50_2 <- rep(0, nit)
err_max1_exact50_2 <- rep(0, nit)

err_oslrt50_3 <- rep(0, nit)
err_moslrt50_3 <- rep(0, nit)
err_rc50_3 <- rep(0, nit)
err_de50_3 <- rep(0, nit)
err_ee50_3 <- rep(0, nit)
err_me50_3 <- rep(0, nit)
err_max1_hoch50_3 <- rep(0, nit)
err_max1_exact50_3 <- rep(0, nit)

err_oslrt50_4 <- rep(0, nit)
err_moslrt50_4 <- rep(0, nit)
err_rc50_4 <- rep(0, nit)
err_de50_4 <- rep(0, nit)
err_ee50_4 <- rep(0, nit)
err_me50_4 <- rep(0, nit)
err_max1_hoch50_4 <- rep(0, nit)
err_max1_exact50_4 <- rep(0, nit)

tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)


err_oslrt60_1 <- rep(0, nit)
err_moslrt60_1 <- rep(0, nit)
err_rc60_1 <- rep(0, nit)
err_de60_1 <- rep(0, nit)
err_ee60_1 <- rep(0, nit)
err_me60_1 <- rep(0, nit)
err_max1_hoch60_1 <- rep(0, nit)
err_max1_exact60_1 <- rep(0, nit)

err_oslrt60_2 <- rep(0, nit)
err_moslrt60_2 <- rep(0, nit)
err_rc60_2 <- rep(0, nit)
err_de60_2 <- rep(0, nit)
err_ee60_2 <- rep(0, nit)
err_me60_2 <- rep(0, nit)
err_max1_hoch60_2 <- rep(0, nit)
err_max1_exact60_2 <- rep(0, nit)

err_oslrt60_3 <- rep(0, nit)
err_moslrt60_3 <- rep(0, nit)
err_rc60_3 <- rep(0, nit)
err_de60_3 <- rep(0, nit)
err_ee60_3 <- rep(0, nit)
err_me60_3 <- rep(0, nit)
err_max1_hoch60_3 <- rep(0, nit)
err_max1_exact60_3 <- rep(0, nit)

err_oslrt60_4 <- rep(0, nit)
err_moslrt60_4 <- rep(0, nit)
err_rc60_4 <- rep(0, nit)
err_de60_4 <- rep(0, nit)
err_ee60_4 <- rep(0, nit)
err_me60_4 <- rep(0, nit)
err_max1_hoch60_4 <- rep(0, nit)
err_max1_exact60_4 <- rep(0, nit)

tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)


err_oslrt80_1 <- rep(0, nit)
err_moslrt80_1 <- rep(0, nit)
err_rc80_1 <- rep(0, nit)
err_de80_1 <- rep(0, nit)
err_ee80_1 <- rep(0, nit)
err_me80_1 <- rep(0, nit)
err_max1_hoch80_1 <- rep(0, nit)
err_max1_exact80_1 <- rep(0, nit)

err_oslrt80_2 <- rep(0, nit)
err_moslrt80_2 <- rep(0, nit)
err_rc80_2 <- rep(0, nit)
err_de80_2 <- rep(0, nit)
err_ee80_2 <- rep(0, nit)
err_me80_2 <- rep(0, nit)
err_max1_hoch80_2 <- rep(0, nit)
err_max1_exact80_2 <- rep(0, nit)

err_oslrt80_3 <- rep(0, nit)
err_moslrt80_3 <- rep(0, nit)
err_rc80_3 <- rep(0, nit)
err_de80_3 <- rep(0, nit)
err_ee80_3 <- rep(0, nit)
err_me80_3 <- rep(0, nit)
err_max1_hoch80_3 <- rep(0, nit)
err_max1_exact80_3 <- rep(0, nit)

err_oslrt80_4 <- rep(0, nit)
err_moslrt80_4 <- rep(0, nit)
err_rc80_4 <- rep(0, nit)
err_de80_4 <- rep(0, nit)
err_ee80_4 <- rep(0, nit)
err_me80_4 <- rep(0, nit)
err_max1_hoch80_4 <- rep(0, nit)
err_max1_exact80_4 <- rep(0, nit)

tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)


err_oslrt100_1 <- rep(0, nit)
err_moslrt100_1 <- rep(0, nit)
err_rc100_1 <- rep(0, nit)
err_de100_1 <- rep(0, nit)
err_ee100_1 <- rep(0, nit)
err_me100_1 <- rep(0, nit)
err_max1_hoch100_1 <- rep(0, nit)
err_max1_exact100_1 <- rep(0, nit)

err_oslrt100_2 <- rep(0, nit)
err_moslrt100_2 <- rep(0, nit)
err_rc100_2 <- rep(0, nit)
err_de100_2 <- rep(0, nit)
err_ee100_2 <- rep(0, nit)
err_me100_2 <- rep(0, nit)
err_max1_hoch100_2 <- rep(0, nit)
err_max1_exact100_2 <- rep(0, nit)

err_oslrt100_3 <- rep(0, nit)
err_moslrt100_3 <- rep(0, nit)
err_rc100_3 <- rep(0, nit)
err_de100_3 <- rep(0, nit)
err_ee100_3 <- rep(0, nit)
err_me100_3 <- rep(0, nit)
err_max1_hoch100_3 <- rep(0, nit)
err_max1_exact100_3 <- rep(0, nit)

err_oslrt100_4 <- rep(0, nit)
err_moslrt100_4 <- rep(0, nit)
err_rc100_4 <- rep(0, nit)
err_de100_4 <- rep(0, nit)
err_ee100_4 <- rep(0, nit)
err_me100_4 <- rep(0, nit)
err_max1_hoch100_4 <- rep(0, nit)
err_max1_exact100_4 <- rep(0, nit)

tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)


err_oslrt150_1 <- rep(0, nit)
err_moslrt150_1 <- rep(0, nit)
err_rc150_1 <- rep(0, nit)
err_de150_1 <- rep(0, nit)
err_ee150_1 <- rep(0, nit)
err_me150_1 <- rep(0, nit)
err_max1_hoch150_1 <- rep(0, nit)
err_max1_exact150_1 <- rep(0, nit)

err_oslrt150_2 <- rep(0, nit)
err_moslrt150_2 <- rep(0, nit)
err_rc150_2 <- rep(0, nit)
err_de150_2 <- rep(0, nit)
err_ee150_2 <- rep(0, nit)
err_me150_2 <- rep(0, nit)
err_max1_hoch150_2 <- rep(0, nit)
err_max1_exact150_2 <- rep(0, nit)

err_oslrt150_3 <- rep(0, nit)
err_moslrt150_3 <- rep(0, nit)
err_rc150_3 <- rep(0, nit)
err_de150_3 <- rep(0, nit)
err_ee150_3 <- rep(0, nit)
err_me150_3 <- rep(0, nit)
err_max1_hoch150_3 <- rep(0, nit)
err_max1_exact150_3 <- rep(0, nit)

err_oslrt150_4 <- rep(0, nit)
err_moslrt150_4 <- rep(0, nit)
err_rc150_4 <- rep(0, nit)
err_de150_4 <- rep(0, nit)
err_ee150_4 <- rep(0, nit)
err_me150_4 <- rep(0, nit)
err_max1_hoch150_4 <- rep(0, nit)
err_max1_exact150_4 <- rep(0, nit)

tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)


err_oslrt200_1 <- rep(0, nit)
err_moslrt200_1 <- rep(0, nit)
err_rc200_1 <- rep(0, nit)
err_de200_1 <- rep(0, nit)
err_ee200_1 <- rep(0, nit)
err_me200_1 <- rep(0, nit)
err_max1_hoch200_1 <- rep(0, nit)
err_max1_exact200_1 <- rep(0, nit)

err_oslrt200_2 <- rep(0, nit)
err_moslrt200_2 <- rep(0, nit)
err_rc200_2 <- rep(0, nit)
err_de200_2 <- rep(0, nit)
err_ee200_2 <- rep(0, nit)
err_me200_2 <- rep(0, nit)
err_max1_hoch200_2 <- rep(0, nit)
err_max1_exact200_2 <- rep(0, nit)

err_oslrt200_3 <- rep(0, nit)
err_moslrt200_3 <- rep(0, nit)
err_rc200_3 <- rep(0, nit)
err_de200_3 <- rep(0, nit)
err_ee200_3 <- rep(0, nit)
err_me200_3 <- rep(0, nit)
err_max1_hoch200_3 <- rep(0, nit)
err_max1_exact200_3 <- rep(0, nit)

err_oslrt200_4 <- rep(0, nit)
err_moslrt200_4 <- rep(0, nit)
err_rc200_4 <- rep(0, nit)
err_de200_4 <- rep(0, nit)
err_ee200_4 <- rep(0, nit)
err_me200_4 <- rep(0, nit)
err_max1_hoch200_4 <- rep(0, nit)
err_max1_exact200_4 <- rep(0, nit)

tx_cens200 <- rep(0, nit)
tx_censadm200 <- rep(0, nit)

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
  a20_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  a202_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)[2]
  b20_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi1)
  c20_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi1)[2]
  d20_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi1)[2]
  e20_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f20_1 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a20_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  a202_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)[2]
  b20_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi2)
  c20_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi2)[2]
  d20_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi2)[2]
  e20_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f20_2 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a20_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  a202_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)[2]
  b20_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi3)
  c20_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi3)[2]
  d20_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi3)[2]
  e20_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f20_3 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a20_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  a202_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)[2]
  b20_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data20, pi = pi4)
  c20_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_DE, pi = pi4)[2]
  d20_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data20, CP = CP_EE, pi = pi4)[2]
  e20_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data20, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f20_4 <- maxcombo1(data_exp = data20, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data30 <- ten(S30)
  a30_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  a302_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)[2]
  b30_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi1)
  c30_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi1)[2]
  d30_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi1)[2]
  e30_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f30_1 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a30_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  a302_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)[2]
  b30_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi2)
  c30_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi2)[2]
  d30_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi2)[2]
  e30_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f30_2 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a30_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  a302_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)[2]
  b30_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi3)
  c30_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi3)[2]
  d30_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi3)[2]
  e30_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f30_3 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a30_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  a302_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)[2]
  b30_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data30, pi = pi4)
  c30_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_DE, pi = pi4)[2]
  d30_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data30, CP = CP_EE, pi = pi4)[2]
  e30_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data30, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f30_4 <- maxcombo1(data_exp = data30, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data50 <- ten(S50)
  a50_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  a502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)[2]
  b50_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi1)
  c50_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi1)[2]
  d50_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi1)[2]
  e50_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f50_1 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a50_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  a502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)[2]
  b50_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi2)
  c50_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi2)[2]
  d50_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi2)[2]
  e50_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f50_2 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a50_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  a502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)[2]
  b50_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi3)
  c50_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi3)[2]
  d50_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi3)[2]
  e50_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f50_3 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a50_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  a502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)[2]
  b50_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data50, pi = pi4)
  c50_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_DE, pi = pi4)[2]
  d50_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data50, CP = CP_EE, pi = pi4)[2]
  e50_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data50, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f50_4 <- maxcombo1(data_exp = data50, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data60 <- ten(S60)
  a60_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  a602_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)[2]
  b60_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi1)
  c60_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi1)[2]
  d60_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi1)[2]
  e60_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f60_1 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a60_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  a602_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)[2]
  b60_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi2)
  c60_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi2)[2]
  d60_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi2)[2]
  e60_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f60_2 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a60_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  a602_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)[2]
  b60_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi3)
  c60_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi3)[2]
  d60_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi3)[2]
  e60_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f60_3 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a60_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  a602_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)[2]
  b60_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data60, pi = pi4)
  c60_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_DE, pi = pi4)[2]
  d60_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data60, CP = CP_EE, pi = pi4)[2]
  e60_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data60, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f60_4 <- maxcombo1(data_exp = data60, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data80 <- ten(S80)
  a80_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  a802_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)[2]
  b80_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi1)
  c80_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi1)[2]
  d80_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi1)[2]
  e80_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f80_1 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a80_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  a802_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)[2]
  b80_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi2)
  c80_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi2)[2]
  d80_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi2)[2]
  e80_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f80_2 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a80_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  a802_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)[2]
  b80_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi3)
  c80_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi3)[2]
  d80_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi3)[2]
  e80_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f80_3 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a80_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  a802_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)[2]
  b80_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data80, pi = pi4)
  c80_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_DE, pi = pi4)[2]
  d80_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data80, CP = CP_EE, pi = pi4)[2]
  e80_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data80, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f80_4 <- maxcombo1(data_exp = data80, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data100 <- ten(S100)
  a100_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  a1002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)[2]
  b100_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi1)
  c100_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi1)[2]
  d100_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi1)[2]
  e100_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f100_1 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a100_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  a1002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)[2]
  b100_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi2)
  c100_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi2)[2]
  d100_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi2)[2]
  e100_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f100_2 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a100_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  a1002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)[2]
  b100_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi3)
  c100_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi3)[2]
  d100_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi3)[2]
  e100_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f100_3 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a100_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  a1002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)[2]
  b100_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data100, pi = pi4)
  c100_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_DE, pi = pi4)[2]
  d100_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data100, CP = CP_EE, pi = pi4)[2]
  e100_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data100, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f100_4 <- maxcombo1(data_exp = data100, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data150 <- ten(S150)
  a150_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  a1502_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)[2]
  b150_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi1)
  c150_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi1)[2]
  d150_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi1)[2]
  e150_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f150_1 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a150_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  a1502_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)[2]
  b150_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi2)
  c150_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi2)[2]
  d150_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi2)[2]
  e150_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f150_2 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a150_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  a1502_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)[2]
  b150_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi3)
  c150_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi3)[2]
  d150_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi3)[2]
  e150_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f150_3 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a150_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  a1502_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)[2]
  b150_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data150, pi = pi4)
  c150_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_DE, pi = pi4)[2]
  d150_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data150, CP = CP_EE, pi = pi4)[2]
  e150_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data150, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f150_4 <- maxcombo1(data_exp = data150, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  data200 <- ten(S200)
  a200_1 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  a2002_1 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)[2]
  b200_1 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi1)
  c200_1 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi1)[2]
  d200_1 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi1)[2]
  e200_1 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi1)
  f200_1 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi1)
  
  a200_2 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  a2002_2 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)[2]
  b200_2 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi2)
  c200_2 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi2)[2]
  d200_2 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi2)[2]
  e200_2 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi2)
  f200_2 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi2)
  
  a200_3 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  a2002_3 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)[2]
  b200_3 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi3)
  c200_3 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi3)[2]
  d200_3 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi3)[2]
  e200_3 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi3)
  f200_3 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi3)
  
  a200_4 <- OSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  a2002_4 <- mOSLRT(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)[2]
  b200_4 <- Score_RC(shape_control = shape0, scale_control = scale0, data = data200, pi = pi4)
  c200_4 <- Score_DE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_DE, pi = pi4)[2]
  d200_4 <- Score_EE(shape_control = shape0, scale_control = scale0, data = data200, CP = CP_EE, pi = pi4)[2]
  e200_4 <- Score_ME(shape_control = shape0, scale_control = scale0, data = data200, CP1 = CP_ME1, CP2 = CP_ME2, pi = pi4)
  f200_4 <- maxcombo1(data_exp = data200, shape_control = shape0, scale_control = scale0, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, pi = pi4)
  
  
  #censoring rate with adm censoring
  tx_censadm20[i] <- 1-sum(delta20)/20
  tx_censadm30[i] <- 1-sum(delta30)/30
  tx_censadm50[i] <- 1-sum(delta50)/50
  tx_censadm60[i] <- 1-sum(delta60)/60
  tx_censadm80[i] <- 1-sum(delta80)/80
  tx_censadm100[i] <- 1-sum(delta100)/100
  tx_censadm150[i] <- 1-sum(delta150)/150
  tx_censadm200[i] <- 1-sum(delta200)/200
  
  #censoring rate without adm censoring
  tx_cens20[i] <- 1-sum(del20)/20
  tx_cens30[i] <- 1-sum(del30)/30
  tx_cens50[i] <- 1-sum(del50)/50
  tx_cens60[i] <- 1-sum(del60)/60
  tx_cens80[i] <- 1-sum(del80)/80
  tx_cens100[i] <- 1-sum(del100)/100
  tx_cens150[i] <- 1-sum(del150)/150
  tx_cens200[i] <- 1-sum(del200)/200
  
  err_oslrt20_1[i] <- ifelse((a20_1<0.05), 1, 0)
  err_moslrt20_1[i] <- ifelse((a202_1<0.05), 1, 0)
  err_rc20_1[i] <- ifelse((b20_1<0.05), 1, 0)
  err_de20_1[i] <- ifelse((c20_1<0.05), 1, 0)
  err_ee20_1[i] <- ifelse((d20_1<0.05), 1, 0)
  err_me20_1[i] <- ifelse((e20_1<0.05), 1, 0)
  err_max1_hoch20_1[i] <- ifelse((f20_1[2]<0.05), 1, 0)
  err_max1_exact20_1[i] <- ifelse((f20_1[3]<0.05), 1, 0)
  
  err_oslrt20_2[i] <- ifelse((a20_2<0.05), 1, 0)
  err_moslrt20_2[i] <- ifelse((a202_2<0.05), 1, 0)
  err_rc20_2[i] <- ifelse((b20_2<0.05), 1, 0)
  err_de20_2[i] <- ifelse((c20_2<0.05), 1, 0)
  err_ee20_2[i] <- ifelse((d20_2<0.05), 1, 0)
  err_me20_2[i] <- ifelse((e20_2<0.05), 1, 0)
  err_max1_hoch20_2[i] <- ifelse((f20_2[2]<0.05), 1, 0)
  err_max1_exact20_2[i] <- ifelse((f20_2[3]<0.05), 1, 0)
  
  err_oslrt20_3[i] <- ifelse((a20_3<0.05), 1, 0)
  err_moslrt20_3[i] <- ifelse((a202_3<0.05), 1, 0)
  err_rc20_3[i] <- ifelse((b20_3<0.05), 1, 0)
  err_de20_3[i] <- ifelse((c20_3<0.05), 1, 0)
  err_ee20_3[i] <- ifelse((d20_3<0.05), 1, 0)
  err_me20_3[i] <- ifelse((e20_3<0.05), 1, 0)
  err_max1_hoch20_3[i] <- ifelse((f20_3[2]<0.05), 1, 0)
  err_max1_exact20_3[i] <- ifelse((f20_3[3]<0.05), 1, 0)
  
  err_oslrt20_4[i] <- ifelse((a20_4<0.05), 1, 0)
  err_moslrt20_4[i] <- ifelse((a202_4<0.05), 1, 0)
  err_rc20_4[i] <- ifelse((b20_4<0.05), 1, 0)
  err_de20_4[i] <- ifelse((c20_4<0.05), 1, 0)
  err_ee20_4[i] <- ifelse((d20_4<0.05), 1, 0)
  err_me20_4[i] <- ifelse((e20_4<0.05), 1, 0)
  err_max1_hoch20_4[i] <- ifelse((f20_4[2]<0.05), 1, 0)
  err_max1_exact20_4[i] <- ifelse((f20_4[3]<0.05), 1, 0)
  
  
  err_oslrt30_1[i] <- ifelse((a30_1<0.05), 1, 0)
  err_moslrt30_1[i] <- ifelse((a302_1<0.05), 1, 0)
  err_rc30_1[i] <- ifelse((b30_1<0.05), 1, 0)
  err_de30_1[i] <- ifelse((c30_1<0.05), 1, 0)
  err_ee30_1[i] <- ifelse((d30_1<0.05), 1, 0)
  err_me30_1[i] <- ifelse((e30_1<0.05), 1, 0)
  err_max1_hoch30_1[i] <- ifelse((f30_1[2]<0.05), 1, 0)
  err_max1_exact30_1[i] <- ifelse((f30_1[3]<0.05), 1, 0)
  
  err_oslrt30_2[i] <- ifelse((a30_2<0.05), 1, 0)
  err_moslrt30_2[i] <- ifelse((a302_2<0.05), 1, 0)
  err_rc30_2[i] <- ifelse((b30_2<0.05), 1, 0)
  err_de30_2[i] <- ifelse((c30_2<0.05), 1, 0)
  err_ee30_2[i] <- ifelse((d30_2<0.05), 1, 0)
  err_me30_2[i] <- ifelse((e30_2<0.05), 1, 0)
  err_max1_hoch30_2[i] <- ifelse((f30_2[2]<0.05), 1, 0)
  err_max1_exact30_2[i] <- ifelse((f30_2[3]<0.05), 1, 0)
  
  err_oslrt30_3[i] <- ifelse((a30_3<0.05), 1, 0)
  err_moslrt30_3[i] <- ifelse((a302_3<0.05), 1, 0)
  err_rc30_3[i] <- ifelse((b30_3<0.05), 1, 0)
  err_de30_3[i] <- ifelse((c30_3<0.05), 1, 0)
  err_ee30_3[i] <- ifelse((d30_3<0.05), 1, 0)
  err_me30_3[i] <- ifelse((e30_3<0.05), 1, 0)
  err_max1_hoch30_3[i] <- ifelse((f30_3[2]<0.05), 1, 0)
  err_max1_exact30_3[i] <- ifelse((f30_3[3]<0.05), 1, 0)
  
  err_oslrt30_4[i] <- ifelse((a30_4<0.05), 1, 0)
  err_moslrt30_4[i] <- ifelse((a302_4<0.05), 1, 0)
  err_rc30_4[i] <- ifelse((b30_4<0.05), 1, 0)
  err_de30_4[i] <- ifelse((c30_4<0.05), 1, 0)
  err_ee30_4[i] <- ifelse((d30_4<0.05), 1, 0)
  err_me30_4[i] <- ifelse((e30_4<0.05), 1, 0)
  err_max1_hoch30_4[i] <- ifelse((f30_4[2]<0.05), 1, 0)
  err_max1_exact30_4[i] <- ifelse((f30_4[3]<0.05), 1, 0)
  
  
  err_oslrt50_1[i] <- ifelse((a50_1<0.05), 1, 0)
  err_moslrt50_1[i] <- ifelse((a502_1<0.05), 1, 0)
  err_rc50_1[i] <- ifelse((b50_1<0.05), 1, 0)
  err_de50_1[i] <- ifelse((c50_1<0.05), 1, 0)
  err_ee50_1[i] <- ifelse((d50_1<0.05), 1, 0)
  err_me50_1[i] <- ifelse((e50_1<0.05), 1, 0)
  err_max1_hoch50_1[i] <- ifelse((f50_1[2]<0.05), 1, 0)
  err_max1_exact50_1[i] <- ifelse((f50_1[3]<0.05), 1, 0)
  
  err_oslrt50_2[i] <- ifelse((a50_2<0.05), 1, 0)
  err_moslrt50_2[i] <- ifelse((a502_2<0.05), 1, 0)
  err_rc50_2[i] <- ifelse((b50_2<0.05), 1, 0)
  err_de50_2[i] <- ifelse((c50_2<0.05), 1, 0)
  err_ee50_2[i] <- ifelse((d50_2<0.05), 1, 0)
  err_me50_2[i] <- ifelse((e50_2<0.05), 1, 0)
  err_max1_hoch50_2[i] <- ifelse((f50_2[2]<0.05), 1, 0)
  err_max1_exact50_2[i] <- ifelse((f50_2[3]<0.05), 1, 0)
  
  err_oslrt50_3[i] <- ifelse((a50_3<0.05), 1, 0)
  err_moslrt50_3[i] <- ifelse((a502_3<0.05), 1, 0)
  err_rc50_3[i] <- ifelse((b50_3<0.05), 1, 0)
  err_de50_3[i] <- ifelse((c50_3<0.05), 1, 0)
  err_ee50_3[i] <- ifelse((d50_3<0.05), 1, 0)
  err_me50_3[i] <- ifelse((e50_3<0.05), 1, 0)
  err_max1_hoch50_3[i] <- ifelse((f50_3[2]<0.05), 1, 0)
  err_max1_exact50_3[i] <- ifelse((f50_3[3]<0.05), 1, 0)
  
  err_oslrt50_4[i] <- ifelse((a50_4<0.05), 1, 0)
  err_moslrt50_4[i] <- ifelse((a502_4<0.05), 1, 0)
  err_rc50_4[i] <- ifelse((b50_4<0.05), 1, 0)
  err_de50_4[i] <- ifelse((c50_4<0.05), 1, 0)
  err_ee50_4[i] <- ifelse((d50_4<0.05), 1, 0)
  err_me50_4[i] <- ifelse((e50_4<0.05), 1, 0)
  err_max1_hoch50_4[i] <- ifelse((f50_4[2]<0.05), 1, 0)
  err_max1_exact50_4[i] <- ifelse((f50_4[3]<0.05), 1, 0)
  
  
  err_oslrt60_1[i] <- ifelse((a60_1<0.05), 1, 0)
  err_moslrt60_1[i] <- ifelse((a602_1<0.05), 1, 0)
  err_rc60_1[i] <- ifelse((b60_1<0.05), 1, 0)
  err_de60_1[i] <- ifelse((c60_1<0.05), 1, 0)
  err_ee60_1[i] <- ifelse((d60_1<0.05), 1, 0)
  err_me60_1[i] <- ifelse((e60_1<0.05), 1, 0)
  err_max1_hoch60_1[i] <- ifelse((f60_1[2]<0.05), 1, 0)
  err_max1_exact60_1[i] <- ifelse((f60_1[3]<0.05), 1, 0)
  
  err_oslrt60_2[i] <- ifelse((a60_2<0.05), 1, 0)
  err_moslrt60_2[i] <- ifelse((a602_2<0.05), 1, 0)
  err_rc60_2[i] <- ifelse((b60_2<0.05), 1, 0)
  err_de60_2[i] <- ifelse((c60_2<0.05), 1, 0)
  err_ee60_2[i] <- ifelse((d60_2<0.05), 1, 0)
  err_me60_2[i] <- ifelse((e60_2<0.05), 1, 0)
  err_max1_hoch60_2[i] <- ifelse((f60_2[2]<0.05), 1, 0)
  err_max1_exact60_2[i] <- ifelse((f60_2[3]<0.05), 1, 0)
  
  err_oslrt60_3[i] <- ifelse((a60_3<0.05), 1, 0)
  err_moslrt60_3[i] <- ifelse((a602_3<0.05), 1, 0)
  err_rc60_3[i] <- ifelse((b60_3<0.05), 1, 0)
  err_de60_3[i] <- ifelse((c60_3<0.05), 1, 0)
  err_ee60_3[i] <- ifelse((d60_3<0.05), 1, 0)
  err_me60_3[i] <- ifelse((e60_3<0.05), 1, 0)
  err_max1_hoch60_3[i] <- ifelse((f60_3[2]<0.05), 1, 0)
  err_max1_exact60_3[i] <- ifelse((f60_3[3]<0.05), 1, 0)
  
  err_oslrt60_4[i] <- ifelse((a60_4<0.05), 1, 0)
  err_moslrt60_4[i] <- ifelse((a602_4<0.05), 1, 0)
  err_rc60_4[i] <- ifelse((b60_4<0.05), 1, 0)
  err_de60_4[i] <- ifelse((c60_4<0.05), 1, 0)
  err_ee60_4[i] <- ifelse((d60_4<0.05), 1, 0)
  err_me60_4[i] <- ifelse((e60_4<0.05), 1, 0)
  err_max1_hoch60_4[i] <- ifelse((f60_4[2]<0.05), 1, 0)
  err_max1_exact60_4[i] <- ifelse((f60_4[3]<0.05), 1, 0)
  
  
  err_oslrt80_1[i] <- ifelse((a80_1<0.05), 1, 0)
  err_moslrt80_1[i] <- ifelse((a802_1<0.05), 1, 0)
  err_rc80_1[i] <- ifelse((b80_1<0.05), 1, 0)
  err_de80_1[i] <- ifelse((c80_1<0.05), 1, 0)
  err_ee80_1[i] <- ifelse((d80_1<0.05), 1, 0)
  err_me80_1[i] <- ifelse((e80_1<0.05), 1, 0)
  err_max1_hoch80_1[i] <- ifelse((f80_1[2]<0.05), 1, 0)
  err_max1_exact80_1[i] <- ifelse((f80_1[3]<0.05), 1, 0)
  
  err_oslrt80_2[i] <- ifelse((a80_2<0.05), 1, 0)
  err_moslrt80_2[i] <- ifelse((a802_2<0.05), 1, 0)
  err_rc80_2[i] <- ifelse((b80_2<0.05), 1, 0)
  err_de80_2[i] <- ifelse((c80_2<0.05), 1, 0)
  err_ee80_2[i] <- ifelse((d80_2<0.05), 1, 0)
  err_me80_2[i] <- ifelse((e80_2<0.05), 1, 0)
  err_max1_hoch80_2[i] <- ifelse((f80_2[2]<0.05), 1, 0)
  err_max1_exact80_2[i] <- ifelse((f80_2[3]<0.05), 1, 0)
  
  err_oslrt80_3[i] <- ifelse((a80_3<0.05), 1, 0)
  err_moslrt80_3[i] <- ifelse((a802_3<0.05), 1, 0)
  err_rc80_3[i] <- ifelse((b80_3<0.05), 1, 0)
  err_de80_3[i] <- ifelse((c80_3<0.05), 1, 0)
  err_ee80_3[i] <- ifelse((d80_3<0.05), 1, 0)
  err_me80_3[i] <- ifelse((e80_3<0.05), 1, 0)
  err_max1_hoch80_3[i] <- ifelse((f80_3[2]<0.05), 1, 0)
  err_max1_exact80_3[i] <- ifelse((f80_3[3]<0.05), 1, 0)
  
  err_oslrt80_4[i] <- ifelse((a80_4<0.05), 1, 0)
  err_moslrt80_4[i] <- ifelse((a802_4<0.05), 1, 0)
  err_rc80_4[i] <- ifelse((b80_4<0.05), 1, 0)
  err_de80_4[i] <- ifelse((c80_4<0.05), 1, 0)
  err_ee80_4[i] <- ifelse((d80_4<0.05), 1, 0)
  err_me80_4[i] <- ifelse((e80_4<0.05), 1, 0)
  err_max1_hoch80_4[i] <- ifelse((f80_4[2]<0.05), 1, 0)
  err_max1_exact80_4[i] <- ifelse((f80_4[3]<0.05), 1, 0)
  
  
  err_oslrt100_1[i] <- ifelse((a100_1<0.05), 1, 0)
  err_moslrt100_1[i] <- ifelse((a1002_1<0.05), 1, 0)
  err_rc100_1[i] <- ifelse((b100_1<0.05), 1, 0)
  err_de100_1[i] <- ifelse((c100_1<0.05), 1, 0)
  err_ee100_1[i] <- ifelse((d100_1<0.05), 1, 0)
  err_me100_1[i] <- ifelse((e100_1<0.05), 1, 0)
  err_max1_hoch100_1[i] <- ifelse((f100_1[2]<0.05), 1, 0)
  err_max1_exact100_1[i] <- ifelse((f100_1[3]<0.05), 1, 0)
  
  err_oslrt100_2[i] <- ifelse((a100_2<0.05), 1, 0)
  err_moslrt100_2[i] <- ifelse((a1002_2<0.05), 1, 0)
  err_rc100_2[i] <- ifelse((b100_2<0.05), 1, 0)
  err_de100_2[i] <- ifelse((c100_2<0.05), 1, 0)
  err_ee100_2[i] <- ifelse((d100_2<0.05), 1, 0)
  err_me100_2[i] <- ifelse((e100_2<0.05), 1, 0)
  err_max1_hoch100_2[i] <- ifelse((f100_2[2]<0.05), 1, 0)
  err_max1_exact100_2[i] <- ifelse((f100_2[3]<0.05), 1, 0)
  
  err_oslrt100_3[i] <- ifelse((a100_3<0.05), 1, 0)
  err_moslrt100_3[i] <- ifelse((a1002_3<0.05), 1, 0)
  err_rc100_3[i] <- ifelse((b100_3<0.05), 1, 0)
  err_de100_3[i] <- ifelse((c100_3<0.05), 1, 0)
  err_ee100_3[i] <- ifelse((d100_3<0.05), 1, 0)
  err_me100_3[i] <- ifelse((e100_3<0.05), 1, 0)
  err_max1_hoch100_3[i] <- ifelse((f100_3[2]<0.05), 1, 0)
  err_max1_exact100_3[i] <- ifelse((f100_3[3]<0.05), 1, 0)
  
  err_oslrt100_4[i] <- ifelse((a100_4<0.05), 1, 0)
  err_moslrt100_4[i] <- ifelse((a1002_4<0.05), 1, 0)
  err_rc100_4[i] <- ifelse((b100_4<0.05), 1, 0)
  err_de100_4[i] <- ifelse((c100_4<0.05), 1, 0)
  err_ee100_4[i] <- ifelse((d100_4<0.05), 1, 0)
  err_me100_4[i] <- ifelse((e100_4<0.05), 1, 0)
  err_max1_hoch100_4[i] <- ifelse((f100_4[2]<0.05), 1, 0)
  err_max1_exact100_4[i] <- ifelse((f100_4[3]<0.05), 1, 0)
  
  
  err_oslrt150_1[i] <- ifelse((a150_1<0.05), 1, 0)
  err_moslrt150_1[i] <- ifelse((a1502_1<0.05), 1, 0)
  err_rc150_1[i] <- ifelse((b150_1<0.05), 1, 0)
  err_de150_1[i] <- ifelse((c150_1<0.05), 1, 0)
  err_ee150_1[i] <- ifelse((d150_1<0.05), 1, 0)
  err_me150_1[i] <- ifelse((e150_1<0.05), 1, 0)
  err_max1_hoch150_1[i] <- ifelse((f150_1[2]<0.05), 1, 0)
  err_max1_exact150_1[i] <- ifelse((f150_1[3]<0.05), 1, 0)
  
  err_oslrt150_2[i] <- ifelse((a150_2<0.05), 1, 0)
  err_moslrt150_2[i] <- ifelse((a1502_2<0.05), 1, 0)
  err_rc150_2[i] <- ifelse((b150_2<0.05), 1, 0)
  err_de150_2[i] <- ifelse((c150_2<0.05), 1, 0)
  err_ee150_2[i] <- ifelse((d150_2<0.05), 1, 0)
  err_me150_2[i] <- ifelse((e150_2<0.05), 1, 0)
  err_max1_hoch150_2[i] <- ifelse((f150_2[2]<0.05), 1, 0)
  err_max1_exact150_2[i] <- ifelse((f150_2[3]<0.05), 1, 0)
  
  err_oslrt150_3[i] <- ifelse((a150_3<0.05), 1, 0)
  err_moslrt150_3[i] <- ifelse((a1502_3<0.05), 1, 0)
  err_rc150_3[i] <- ifelse((b150_3<0.05), 1, 0)
  err_de150_3[i] <- ifelse((c150_3<0.05), 1, 0)
  err_ee150_3[i] <- ifelse((d150_3<0.05), 1, 0)
  err_me150_3[i] <- ifelse((e150_3<0.05), 1, 0)
  err_max1_hoch150_3[i] <- ifelse((f150_3[2]<0.05), 1, 0)
  err_max1_exact150_3[i] <- ifelse((f150_3[3]<0.05), 1, 0)
  
  err_oslrt150_4[i] <- ifelse((a150_4<0.05), 1, 0)
  err_moslrt150_4[i] <- ifelse((a1502_4<0.05), 1, 0)
  err_rc150_4[i] <- ifelse((b150_4<0.05), 1, 0)
  err_de150_4[i] <- ifelse((c150_4<0.05), 1, 0)
  err_ee150_4[i] <- ifelse((d150_4<0.05), 1, 0)
  err_me150_4[i] <- ifelse((e150_4<0.05), 1, 0)
  err_max1_hoch150_4[i] <- ifelse((f150_4[2]<0.05), 1, 0)
  err_max1_exact150_4[i] <- ifelse((f150_4[3]<0.05), 1, 0)
  
  
  err_oslrt200_1[i] <- ifelse((a200_1<0.05), 1, 0)
  err_moslrt200_1[i] <- ifelse((a2002_1<0.05), 1, 0)
  err_rc200_1[i] <- ifelse((b200_1<0.05), 1, 0)
  err_de200_1[i] <- ifelse((c200_1<0.05), 1, 0)
  err_ee200_1[i] <- ifelse((d200_1<0.05), 1, 0)
  err_me200_1[i] <- ifelse((e200_1<0.05), 1, 0)
  err_max1_hoch200_1[i] <- ifelse((f200_1[2]<0.05), 1, 0)
  err_max1_exact200_1[i] <- ifelse((f200_1[3]<0.05), 1, 0)
  
  err_oslrt200_2[i] <- ifelse((a200_2<0.05), 1, 0)
  err_moslrt200_2[i] <- ifelse((a2002_2<0.05), 1, 0)
  err_rc200_2[i] <- ifelse((b200_2<0.05), 1, 0)
  err_de200_2[i] <- ifelse((c200_2<0.05), 1, 0)
  err_ee200_2[i] <- ifelse((d200_2<0.05), 1, 0)
  err_me200_2[i] <- ifelse((e200_2<0.05), 1, 0)
  err_max1_hoch200_2[i] <- ifelse((f200_2[2]<0.05), 1, 0)
  err_max1_exact200_2[i] <- ifelse((f200_2[3]<0.05), 1, 0)
  
  err_oslrt200_3[i] <- ifelse((a200_3<0.05), 1, 0)
  err_moslrt200_3[i] <- ifelse((a2002_3<0.05), 1, 0)
  err_rc200_3[i] <- ifelse((b200_3<0.05), 1, 0)
  err_de200_3[i] <- ifelse((c200_3<0.05), 1, 0)
  err_ee200_3[i] <- ifelse((d200_3<0.05), 1, 0)
  err_me200_3[i] <- ifelse((e200_3<0.05), 1, 0)
  err_max1_hoch200_3[i] <- ifelse((f200_3[2]<0.05), 1, 0)
  err_max1_exact200_3[i] <- ifelse((f200_3[3]<0.05), 1, 0)
  
  err_oslrt200_4[i] <- ifelse((a200_4<0.05), 1, 0)
  err_moslrt200_4[i] <- ifelse((a2002_4<0.05), 1, 0)
  err_rc200_4[i] <- ifelse((b200_4<0.05), 1, 0)
  err_de200_4[i] <- ifelse((c200_4<0.05), 1, 0)
  err_ee200_4[i] <- ifelse((d200_4<0.05), 1, 0)
  err_me200_4[i] <- ifelse((e200_4<0.05), 1, 0)
  err_max1_hoch200_4[i] <- ifelse((f200_4[2]<0.05), 1, 0)
  err_max1_exact200_4[i] <- ifelse((f200_4[3]<0.05), 1, 0)
}

os20_1 <- sum(err_oslrt20_1)/nit
mos20_1 <- sum(err_moslrt20_1)/nit
rc20_1 <- sum(err_rc20_1)/nit
de20_1 <- sum(na.omit(err_de20_1))/length(na.omit(err_de20_1))
ee20_1 <- sum(err_ee20_1)/nit
me20_1 <- sum(err_me20_1)/nit
max1_hoch20_1 <- sum(err_max1_hoch20_1)/nit
max1_exact20_1 <- sum(na.omit(err_max1_exact20_1))/length(na.omit(err_max1_exact20_1))

os20_2 <- sum(err_oslrt20_2)/nit
mos20_2 <- sum(err_moslrt20_2)/nit
rc20_2 <- sum(err_rc20_2)/nit
de20_2 <- sum(na.omit(err_de20_2))/length(na.omit(err_de20_2))
ee20_2 <- sum(err_ee20_2)/nit
me20_2 <- sum(err_me20_2)/nit
max1_hoch20_2 <- sum(err_max1_hoch20_2)/nit
max1_exact20_2 <- sum(na.omit(err_max1_exact20_2))/length(na.omit(err_max1_exact20_2))

os20_3 <- sum(err_oslrt20_3)/nit
mos20_3 <- sum(err_moslrt20_3)/nit
rc20_3 <- sum(err_rc20_3)/nit
de20_3 <- sum(na.omit(err_de20_3))/length(na.omit(err_de20_3))
ee20_3 <- sum(err_ee20_3)/nit
me20_3 <- sum(err_me20_3)/nit
max1_hoch20_3 <- sum(err_max1_hoch20_3)/nit
max1_exact20_3 <- sum(na.omit(err_max1_exact20_3))/length(na.omit(err_max1_exact20_3))

os20_4 <- sum(err_oslrt20_4)/nit
mos20_4 <- sum(err_moslrt20_4)/nit
rc20_4 <- sum(err_rc20_4)/nit
de20_4 <- sum(na.omit(err_de20_4))/length(na.omit(err_de20_4))
ee20_4 <- sum(err_ee20_4)/nit
me20_4 <- sum(err_me20_4)/nit
max1_hoch20_4 <- sum(err_max1_hoch20_4)/nit
max1_exact20_4 <- sum(na.omit(err_max1_exact20_4))/length(na.omit(err_max1_exact20_4))

mean(tx_cens20)
mean(tx_censadm20)


os30_1 <- sum(err_oslrt30_1)/nit
mos30_1 <- sum(err_moslrt30_1)/nit
rc30_1 <- sum(err_rc30_1)/nit
de30_1 <- sum(na.omit(err_de30_1))/length(na.omit(err_de30_1))
ee30_1 <- sum(err_ee30_1)/nit
me30_1 <- sum(err_me30_1)/nit
max1_hoch30_1 <- sum(err_max1_hoch30_1)/nit
max1_exact30_1 <- sum(na.omit(err_max1_exact30_1))/length(na.omit(err_max1_exact30_1))

os30_2 <- sum(err_oslrt30_2)/nit
mos30_2 <- sum(err_moslrt30_2)/nit
rc30_2 <- sum(err_rc30_2)/nit
de30_2 <- sum(na.omit(err_de30_2))/length(na.omit(err_de30_2))
ee30_2 <- sum(err_ee30_2)/nit
me30_2 <- sum(err_me30_2)/nit
max1_hoch30_2 <- sum(err_max1_hoch30_2)/nit
max1_exact30_2 <- sum(na.omit(err_max1_exact30_2))/length(na.omit(err_max1_exact30_2))

os30_3 <- sum(err_oslrt30_3)/nit
mos30_3 <- sum(err_moslrt30_3)/nit
rc30_3 <- sum(err_rc30_3)/nit
de30_3 <- sum(na.omit(err_de30_3))/length(na.omit(err_de30_3))
ee30_3 <- sum(err_ee30_3)/nit
me30_3 <- sum(err_me30_3)/nit
max1_hoch30_3 <- sum(err_max1_hoch30_3)/nit
max1_exact30_3 <- sum(na.omit(err_max1_exact30_3))/length(na.omit(err_max1_exact30_3))

os30_4 <- sum(err_oslrt30_4)/nit
mos30_4 <- sum(err_moslrt30_4)/nit
rc30_4 <- sum(err_rc30_4)/nit
de30_4 <- sum(na.omit(err_de30_4))/length(na.omit(err_de30_4))
ee30_4 <- sum(err_ee30_4)/nit
me30_4 <- sum(err_me30_4)/nit
max1_hoch30_4 <- sum(err_max1_hoch30_4)/nit
max1_exact30_4 <- sum(na.omit(err_max1_exact30_4))/length(na.omit(err_max1_exact30_4))

mean(tx_cens30)
mean(tx_censadm30)


os50_1 <- sum(err_oslrt50_1)/nit
mos50_1 <- sum(err_moslrt50_1)/nit
rc50_1 <- sum(err_rc50_1)/nit
de50_1 <- sum(err_de50_1)/nit
ee50_1 <- sum(err_ee50_1)/nit
me50_1 <- sum(err_me50_1)/nit
max1_hoch50_1 <- sum(err_max1_hoch50_1)/nit
max1_exact50_1 <- sum(na.omit(err_max1_exact50_1))/length(na.omit(err_max1_exact50_1))

os50_2 <- sum(err_oslrt50_2)/nit
mos50_2 <- sum(err_moslrt50_2)/nit
rc50_2 <- sum(err_rc50_2)/nit
de50_2 <- sum(err_de50_2)/nit
ee50_2 <- sum(err_ee50_2)/nit
me50_2 <- sum(err_me50_2)/nit
max1_hoch50_2 <- sum(err_max1_hoch50_2)/nit
max1_exact50_2 <- sum(na.omit(err_max1_exact50_2))/length(na.omit(err_max1_exact50_2))

os50_3 <- sum(err_oslrt50_3)/nit
mos50_3 <- sum(err_moslrt50_3)/nit
rc50_3 <- sum(err_rc50_3)/nit
de50_3 <- sum(err_de50_3)/nit
ee50_3 <- sum(err_ee50_3)/nit
me50_3 <- sum(err_me50_3)/nit
max1_hoch50_3 <- sum(err_max1_hoch50_3)/nit
max1_exact50_3 <- sum(na.omit(err_max1_exact50_3))/length(na.omit(err_max1_exact50_3))

os50_4 <- sum(err_oslrt50_4)/nit
mos50_4 <- sum(err_moslrt50_4)/nit
rc50_4 <- sum(err_rc50_4)/nit
de50_4 <- sum(err_de50_4)/nit
ee50_4 <- sum(err_ee50_4)/nit
me50_4 <- sum(err_me50_4)/nit
max1_hoch50_4 <- sum(err_max1_hoch50_4)/nit
max1_exact50_4 <- sum(na.omit(err_max1_exact50_4))/length(na.omit(err_max1_exact50_4))

mean(tx_cens50)
mean(tx_censadm50)


os60_1 <- sum(err_oslrt60_1)/nit
mos60_1 <- sum(err_moslrt60_1)/nit
rc60_1 <- sum(err_rc60_1)/nit
de60_1 <- sum(err_de60_1)/nit
ee60_1 <- sum(err_ee60_1)/nit
me60_1 <- sum(err_me60_1)/nit
max1_hoch60_1 <- sum(err_max1_hoch60_1)/nit
max1_exact60_1 <- sum(na.omit(err_max1_exact60_1))/length(na.omit(err_max1_exact60_1))

os60_2 <- sum(err_oslrt60_2)/nit
mos60_2 <- sum(err_moslrt60_2)/nit
rc60_2 <- sum(err_rc60_2)/nit
de60_2 <- sum(err_de60_2)/nit
ee60_2 <- sum(err_ee60_2)/nit
me60_2 <- sum(err_me60_2)/nit
max1_hoch60_2 <- sum(err_max1_hoch60_2)/nit
max1_exact60_2 <- sum(na.omit(err_max1_exact60_2))/length(na.omit(err_max1_exact60_2))

os60_3 <- sum(err_oslrt60_3)/nit
mos60_3 <- sum(err_moslrt60_3)/nit
rc60_3 <- sum(err_rc60_3)/nit
de60_3 <- sum(err_de60_3)/nit
ee60_3 <- sum(err_ee60_3)/nit
me60_3 <- sum(err_me60_3)/nit
max1_hoch60_3 <- sum(err_max1_hoch60_3)/nit
max1_exact60_3 <- sum(na.omit(err_max1_exact60_3))/length(na.omit(err_max1_exact60_3))

os60_4 <- sum(err_oslrt60_4)/nit
mos60_4 <- sum(err_moslrt60_4)/nit
rc60_4 <- sum(err_rc60_4)/nit
de60_4 <- sum(err_de60_4)/nit
ee60_4 <- sum(err_ee60_4)/nit
me60_4 <- sum(err_me60_4)/nit
max1_hoch60_4 <- sum(err_max1_hoch60_4)/nit
max1_exact60_4 <- sum(na.omit(err_max1_exact60_4))/length(na.omit(err_max1_exact60_4))

mean(tx_cens60)
mean(tx_censadm60)


os80_1 <- sum(err_oslrt80_1)/nit
mos80_1 <- sum(err_moslrt80_1)/nit
rc80_1 <- sum(err_rc80_1)/nit
de80_1 <- sum(err_de80_1)/nit
ee80_1 <- sum(err_ee80_1)/nit
me80_1 <- sum(err_me80_1)/nit
max1_hoch80_1 <- sum(err_max1_hoch80_1)/nit
max1_exact80_1 <- sum(na.omit(err_max1_exact80_1))/length(na.omit(err_max1_exact80_1))

os80_2 <- sum(err_oslrt80_2)/nit
mos80_2 <- sum(err_moslrt80_2)/nit
rc80_2 <- sum(err_rc80_2)/nit
de80_2 <- sum(err_de80_2)/nit
ee80_2 <- sum(err_ee80_2)/nit
me80_2 <- sum(err_me80_2)/nit
max1_hoch80_2 <- sum(err_max1_hoch80_2)/nit
max1_exact80_2 <- sum(na.omit(err_max1_exact80_2))/length(na.omit(err_max1_exact80_2))

os80_3 <- sum(err_oslrt80_3)/nit
mos80_3 <- sum(err_moslrt80_3)/nit
rc80_3 <- sum(err_rc80_3)/nit
de80_3 <- sum(err_de80_3)/nit
ee80_3 <- sum(err_ee80_3)/nit
me80_3 <- sum(err_me80_3)/nit
max1_hoch80_3 <- sum(err_max1_hoch80_3)/nit
max1_exact80_3 <- sum(na.omit(err_max1_exact80_3))/length(na.omit(err_max1_exact80_3))

os80_4 <- sum(err_oslrt80_4)/nit
mos80_4 <- sum(err_moslrt80_4)/nit
rc80_4 <- sum(err_rc80_4)/nit
de80_4 <- sum(err_de80_4)/nit
ee80_4 <- sum(err_ee80_4)/nit
me80_4 <- sum(err_me80_4)/nit
max1_hoch80_4 <- sum(err_max1_hoch80_4)/nit
max1_exact80_4 <- sum(na.omit(err_max1_exact80_4))/length(na.omit(err_max1_exact80_4))

mean(tx_cens80)
mean(tx_censadm80)


os100_1 <- sum(err_oslrt100_1)/nit
mos100_1 <- sum(err_moslrt100_1)/nit
rc100_1 <- sum(err_rc100_1)/nit
de100_1 <- sum(err_de100_1)/nit
ee100_1 <- sum(err_ee100_1)/nit
me100_1 <- sum(err_me100_1)/nit
max1_hoch100_1 <- sum(err_max1_hoch100_1)/nit
max1_exact100_1 <- sum(na.omit(err_max1_exact100_1))/length(na.omit(err_max1_exact100_1))

os100_2 <- sum(err_oslrt100_2)/nit
mos100_2 <- sum(err_moslrt100_2)/nit
rc100_2 <- sum(err_rc100_2)/nit
de100_2 <- sum(err_de100_2)/nit
ee100_2 <- sum(err_ee100_2)/nit
me100_2 <- sum(err_me100_2)/nit
max1_hoch100_2 <- sum(err_max1_hoch100_2)/nit
max1_exact100_2 <- sum(na.omit(err_max1_exact100_2))/length(na.omit(err_max1_exact100_2))

os100_3 <- sum(err_oslrt100_3)/nit
mos100_3 <- sum(err_moslrt100_3)/nit
rc100_3 <- sum(err_rc100_3)/nit
de100_3 <- sum(err_de100_3)/nit
ee100_3 <- sum(err_ee100_3)/nit
me100_3 <- sum(err_me100_3)/nit
max1_hoch100_3 <- sum(err_max1_hoch100_3)/nit
max1_exact100_3 <- sum(na.omit(err_max1_exact100_3))/length(na.omit(err_max1_exact100_3))

os100_4 <- sum(err_oslrt100_4)/nit
mos100_4 <- sum(err_moslrt100_4)/nit
rc100_4 <- sum(err_rc100_4)/nit
de100_4 <- sum(err_de100_4)/nit
ee100_4 <- sum(err_ee100_4)/nit
me100_4 <- sum(err_me100_4)/nit
max1_hoch100_4 <- sum(err_max1_hoch100_4)/nit
max1_exact100_4 <- sum(na.omit(err_max1_exact100_4))/length(na.omit(err_max1_exact100_4))

mean(tx_cens100)
mean(tx_censadm100)


os150_1 <- sum(err_oslrt150_1)/nit
mos150_1 <- sum(err_moslrt150_1)/nit
rc150_1 <- sum(err_rc150_1)/nit
de150_1 <- sum(err_de150_1)/nit
ee150_1 <- sum(err_ee150_1)/nit
me150_1 <- sum(na.omit(err_me150_1))/length(na.omit(err_me150_1))
max1_hoch150_1 <- sum(err_max1_hoch150_1)/nit
max1_exact150_1 <- sum(na.omit(err_max1_exact150_1))/length(na.omit(err_max1_exact150_1))

os150_2 <- sum(err_oslrt150_2)/nit
mos150_2 <- sum(err_moslrt150_2)/nit
rc150_2 <- sum(err_rc150_2)/nit
de150_2 <- sum(err_de150_2)/nit
ee150_2 <- sum(err_ee150_2)/nit
me150_2 <- sum(na.omit(err_me150_2))/length(na.omit(err_me150_2))
max1_hoch150_2 <- sum(err_max1_hoch150_2)/nit
max1_exact150_2 <- sum(na.omit(err_max1_exact150_2))/length(na.omit(err_max1_exact150_2))

os150_3 <- sum(err_oslrt150_3)/nit
mos150_3 <- sum(err_moslrt150_3)/nit
rc150_3 <- sum(err_rc150_3)/nit
de150_3 <- sum(err_de150_3)/nit
ee150_3 <- sum(err_ee150_3)/nit
me150_3 <- sum(na.omit(err_me150_3))/length(na.omit(err_me150_3))
max1_hoch150_3 <- sum(err_max1_hoch150_3)/nit
max1_exact150_3 <- sum(na.omit(err_max1_exact150_3))/length(na.omit(err_max1_exact150_3))

os150_4 <- sum(err_oslrt150_4)/nit
mos150_4 <- sum(err_moslrt150_4)/nit
rc150_4 <- sum(err_rc150_4)/nit
de150_4 <- sum(err_de150_4)/nit
ee150_4 <- sum(err_ee150_4)/nit
me150_4 <- sum(na.omit(err_me150_4))/length(na.omit(err_me150_4))
max1_hoch150_4 <- sum(err_max1_hoch150_4)/nit
max1_exact150_4 <- sum(na.omit(err_max1_exact150_4))/length(na.omit(err_max1_exact150_4))

mean(tx_cens150)
mean(tx_censadm150)

os200_1 <- sum(err_oslrt200_1)/nit
mos200_1 <- sum(err_moslrt200_1)/nit
rc200_1 <- sum(err_rc200_1)/nit
de200_1 <- sum(err_de200_1)/nit
ee200_1 <- sum(err_ee200_1)/nit
me200_1 <- sum(err_me200_1)/nit
max1_hoch200_1 <- sum(err_max1_hoch200_1)/nit
max1_exact200_1 <- sum(na.omit(err_max1_exact200_1))/length(na.omit(err_max1_exact200_1))

os200_2 <- sum(err_oslrt200_2)/nit
mos200_2 <- sum(err_moslrt200_2)/nit
rc200_2 <- sum(err_rc200_2)/nit
de200_2 <- sum(err_de200_2)/nit
ee200_2 <- sum(err_ee200_2)/nit
me200_2 <- sum(err_me200_2)/nit
max1_hoch200_2 <- sum(err_max1_hoch200_2)/nit
max1_exact200_2 <- sum(na.omit(err_max1_exact200_2))/length(na.omit(err_max1_exact200_2))

os200_3 <- sum(err_oslrt200_3)/nit
mos200_3 <- sum(err_moslrt200_3)/nit
rc200_3 <- sum(err_rc200_3)/nit
de200_3 <- sum(err_de200_3)/nit
ee200_3 <- sum(err_ee200_3)/nit
me200_3 <- sum(err_me200_3)/nit
max1_hoch200_3 <- sum(err_max1_hoch200_3)/nit
max1_exact200_3 <- sum(na.omit(err_max1_exact200_3))/length(na.omit(err_max1_exact200_3))

os200_4 <- sum(err_oslrt200_4)/nit
mos200_4 <- sum(err_moslrt200_4)/nit
rc200_4 <- sum(err_rc200_4)/nit
de200_4 <- sum(err_de200_4)/nit
ee200_4 <- sum(err_ee200_4)/nit
me200_4 <- sum(err_me200_4)/nit
max1_hoch200_4 <- sum(err_max1_hoch200_4)/nit
max1_exact200_4 <- sum(na.omit(err_max1_exact200_4))/length(na.omit(err_max1_exact200_4))

mean(tx_cens200)
mean(tx_censadm200)


n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl_1 <- c(os20_1, os30_1, os50_1, os60_1, os80_1, os100_1, os150_1, os200_1)
mosl_1 <- c(mos20_1, mos30_1, mos50_1, mos60_1, mos80_1, mos100_1, mos150_1, mos200_1)
rc_1 <- c(rc20_1, rc30_1, rc50_1, rc60_1, rc80_1, rc100_1, rc150_1, rc200_1)
de_1 <- c(de20_1, de30_1, de50_1, de60_1, de80_1, de100_1, de150_1, de200_1)
ee_1 <- c(ee20_1, ee30_1, ee50_1, ee60_1, ee80_1, ee100_1, ee150_1, ee200_1)
me_1 <- c(me20_1, me30_1, me50_1, me60_1, me80_1, me100_1, me150_1, me200_1)
max1_hochberg_1 <- c(max1_hoch20_1, max1_hoch30_1, max1_hoch50_1, max1_hoch60_1, max1_hoch80_1, max1_hoch100_1, max1_hoch150_1, max1_hoch200_1)
max1_pmult_1 <- c(max1_exact20_1, max1_exact30_1, max1_exact50_1, max1_exact60_1, max1_exact80_1, max1_exact100_1, max1_exact150_1, max1_exact200_1)
osl_1
mosl_1
ee_1
me_1
de_1
rc_1
max1_hochberg_1
max1_pmult_1

osl_2 <- c(os20_2, os30_2, os50_2, os60_2, os80_2, os100_2, os150_2, os200_2)
mosl_2 <- c(mos20_2, mos30_2, mos50_2, mos60_2, mos80_2, mos100_2, mos150_2, mos200_2)
rc_2 <- c(rc20_2, rc30_2, rc50_2, rc60_2, rc80_2, rc100_2, rc150_2, rc200_2)
de_2 <- c(de20_2, de30_2, de50_2, de60_2, de80_2, de100_2, de150_2, de200_2)
ee_2 <- c(ee20_2, ee30_2, ee50_2, ee60_2, ee80_2, ee100_2, ee150_2, ee200_2)
me_2 <- c(me20_2, me30_2, me50_2, me60_2, me80_2, me100_2, me150_2, me200_2)
max1_hochberg_2 <- c(max1_hoch20_2, max1_hoch30_2, max1_hoch50_2, max1_hoch60_2, max1_hoch80_2, max1_hoch100_2, max1_hoch150_2, max1_hoch200_2)
max1_pmult_2 <- c(max1_exact20_2, max1_exact30_2, max1_exact50_2, max1_exact60_2, max1_exact80_2, max1_exact100_2, max1_exact150_2, max1_exact200_2)
osl_2
mosl_2
ee_2
me_2
de_2
rc_2
max1_hochberg_2
max1_pmult_2

osl_3 <- c(os20_3, os30_3, os50_3, os60_3, os80_3, os100_3, os150_3, os200_3)
mosl_3 <- c(mos20_3, mos30_3, mos50_3, mos60_3, mos80_3, mos100_3, mos150_3, mos200_3)
rc_3 <- c(rc20_3, rc30_3, rc50_3, rc60_3, rc80_3, rc100_3, rc150_3, rc200_3)
de_3 <- c(de20_3, de30_3, de50_3, de60_3, de80_3, de100_3, de150_3, de200_3)
ee_3 <- c(ee20_3, ee30_3, ee50_3, ee60_3, ee80_3, ee100_3, ee150_3, ee200_3)
me_3 <- c(me20_3, me30_3, me50_3, me60_3, me80_3, me100_3, me150_3, me200_3)
max1_hochberg_3 <- c(max1_hoch20_3, max1_hoch30_3, max1_hoch50_3, max1_hoch60_3, max1_hoch80_3, max1_hoch100_3, max1_hoch150_3, max1_hoch200_3)
max1_pmult_3 <- c(max1_exact20_3, max1_exact30_3, max1_exact50_3, max1_exact60_3, max1_exact80_3, max1_exact100_3, max1_exact150_3, max1_exact200_3)
osl_3
mosl_3
ee_3
me_3
de_3
rc_3
max1_hochberg_3
max1_pmult_3

osl_4 <- c(os20_4, os30_4, os50_4, os60_4, os80_4, os100_4, os150_4, os200_4)
mosl_4 <- c(mos20_4, mos30_4, mos50_4, mos60_4, mos80_4, mos100_4, mos150_4, mos200_4)
rc_4 <- c(rc20_4, rc30_4, rc50_4, rc60_4, rc80_4, rc100_4, rc150_4, rc200_4)
de_4 <- c(de20_4, de30_4, de50_4, de60_4, de80_4, de100_4, de150_4, de200_4)
ee_4 <- c(ee20_4, ee30_4, ee50_4, ee60_4, ee80_4, ee100_4, ee150_4, ee200_4)
me_4 <- c(me20_4, me30_4, me50_4, me60_4, me80_4, me100_4, me150_4, me200_4)
max1_hochberg_4 <- c(max1_hoch20_4, max1_hoch30_4, max1_hoch50_4, max1_hoch60_4, max1_hoch80_4, max1_hoch100_4, max1_hoch150_4, max1_hoch200_4)
max1_pmult_4 <- c(max1_exact20_4, max1_exact30_4, max1_exact50_4, max1_exact60_4, max1_exact80_4, max1_exact100_4, max1_exact150_4, max1_exact200_4)
osl_4
mosl_4
ee_4
me_4
de_4
rc_4
max1_hochberg_4
max1_pmult_4

d1_1 <- data.frame(Sample.size = n, Error = c(osl_1, mosl_1, ee_1, me_1, de_1, rc_1, max1_hochberg_1, max1_pmult_1),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_1$Test <- as.factor(d1_1$Test)
d1_1$Test <- factor(d1_1$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 1',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_2 <- data.frame(Sample.size = n, Error = c(osl_2, mosl_2, ee_2, me_2, de_2, rc_2, max1_hochberg_2, max1_pmult_2),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_2$Test <- as.factor(d1_2$Test)
d1_2$Test <- factor(d1_2$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_2, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.8',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))

d1_3 <- data.frame(Sample.size = n, Error = c(osl_3, mosl_3, ee_3, me_3, de_3, rc_3, max1_hochberg_3, max1_pmult_3),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_3$Test <- as.factor(d1_3$Test)
d1_3$Test <- factor(d1_3$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_3, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.6',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))


d1_4 <- data.frame(Sample.size = n, Error = c(osl_4, mosl_4, ee_4, me_4, de_4, rc_4, max1_hochberg_4, max1_pmult_4),
                   Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                            rep('Delayed effect', 8), rep('Crossing hazards', 8),
                            rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (multivariate normal integration)', 8)))
d1_4$Test <- as.factor(d1_4$Test)
d1_4$Test <- factor(d1_4$Test, levels = c('OSLRT', 'Modified OSLRT', 'Early effect', 'Middle effect',
                                          'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                          'max-Combo (multivariate normal integration)'))

ggplot(d1_4, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group',
       y = 'Power',
       title = 'pi = 0.5',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+
  ylim(0,1)+
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))

