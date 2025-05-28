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

err_oslrt20 <- rep(0, nit)
err_moslrt20 <- rep(0, nit)
err_rc20 <- rep(0, nit)
err_de20 <- rep(0, nit)
err_ee20 <- rep(0, nit)
err_me20 <- rep(0, nit)
err_rmst_20 <- rep(0, nit)
err_max_hoch_20 <- rep(0, nit)
err_max_exact_20 <- rep(0, nit)
obs20 <- rep(0, nit)
obs20_adm <- rep(0, nit)
tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)

err_oslrt30 <- rep(0, nit)
err_moslrt30 <- rep(0, nit)
err_rc30 <- rep(0, nit)
err_de30 <- rep(0, nit)
err_ee30 <- rep(0, nit)
err_me30 <- rep(0, nit)
err_rmst_30 <- rep(0, nit)
err_max_hoch_30 <- rep(0, nit)
err_max_exact_30 <- rep(0, nit)
obs30 <- rep(0, nit)
obs30_adm <- rep(0, nit)
tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)

err_oslrt50 <- rep(0, nit)
err_moslrt50 <- rep(0, nit)
err_rc50 <- rep(0, nit)
err_de50 <- rep(0, nit)
err_ee50 <- rep(0, nit)
err_me50 <- rep(0, nit)
err_rmst_50 <- rep(0, nit)
err_max_hoch_50 <- rep(0, nit)
err_max_exact_50 <- rep(0, nit)
obs50 <- rep(0, nit)
obs50_adm <- rep(0, nit)
tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)

err_oslrt60 <- rep(0, nit)
err_moslrt60 <- rep(0, nit)
err_rc60 <- rep(0, nit)
err_de60 <- rep(0, nit)
err_ee60 <- rep(0, nit)
err_me60 <- rep(0, nit)
err_rmst_60 <- rep(0, nit)
err_max_hoch_60 <- rep(0, nit)
err_max_exact_60 <- rep(0, nit)
obs60 <- rep(0, nit)
obs60_adm <- rep(0, nit)
tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)

err_oslrt80 <- rep(0, nit)
err_moslrt80 <- rep(0, nit)
err_rc80 <- rep(0, nit)
err_de80 <- rep(0, nit)
err_ee80 <- rep(0, nit)
err_me80 <- rep(0, nit)
err_rmst_80 <- rep(0, nit)
err_max_hoch_80 <- rep(0, nit)
err_max_exact_80 <- rep(0, nit)
obs80 <- rep(0, nit)
obs80_adm <- rep(0, nit)
tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)

err_oslrt100 <- rep(0, nit)
err_moslrt100 <- rep(0, nit)
err_rc100 <- rep(0, nit)
err_de100 <- rep(0, nit)
err_ee100 <- rep(0, nit)
err_me100 <- rep(0, nit)
err_rmst_100 <- rep(0, nit)
err_max_hoch_100 <- rep(0, nit)
err_max_exact_100 <- rep(0, nit)
obs100 <- rep(0, nit)
obs100_adm <- rep(0, nit)
tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)

err_oslrt150 <- rep(0, nit)
err_moslrt150 <- rep(0, nit)
err_rc150 <- rep(0, nit)
err_de150 <- rep(0, nit)
err_ee150 <- rep(0, nit)
err_me150 <- rep(0, nit)
err_rmst_150 <- rep(0, nit)
err_max_hoch_150 <- rep(0, nit)
err_max_exact_150 <- rep(0, nit)
obs150 <- rep(0, nit)
obs150_adm <- rep(0, nit)
tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)

err_oslrt200 <- rep(0, nit)
err_moslrt200 <- rep(0, nit)
err_rc200 <- rep(0, nit)
err_de200 <- rep(0, nit)
err_ee200 <- rep(0, nit)
err_me200 <- rep(0, nit)
err_rmst_200 <- rep(0, nit)
err_max_hoch_200 <- rep(0, nit)
err_max_exact_200 <- rep(0, nit)
obs200 <- rep(0, nit)
obs200_adm <- rep(0, nit)
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
  obs20[i] <- sum(del20)
  obs30[i] <- sum(del30)
  obs50[i] <- sum(del50)
  obs60[i] <- sum(del60)
  obs80[i] <- sum(del80)
  obs100[i] <- sum(del100)
  obs150[i] <- sum(del150)
  obs200[i] <- sum(del200)
  
  obs20_adm[i] <- sum(delta20)
  obs30_adm[i] <- sum(delta30)
  obs50_adm[i] <- sum(delta50)
  obs60_adm[i] <- sum(delta60)
  obs80_adm[i] <- sum(delta80)
  obs100_adm[i] <- sum(delta100)
  obs150_adm[i] <- sum(delta150)
  obs200_adm[i] <- sum(delta200)
  
  data20 <- ten(S20)
  a20 <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202 <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20 <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20 <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20 <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20 <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau20 <- max(t20)
  rmst_exp20 <- rmst_single(time = t20, status = delta20, tau = tau20)
  a <- test_SA(rmst_exp = rmst_exp20[1], se_rmst_exp = rmst_exp20[2], tau = tau20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data30 <- ten(S30)
  a30 <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302 <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30 <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30 <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30 <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30 <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau30 <- max(t30)
  rmst_exp30 <- rmst_single(time = t30, status = delta30, tau = tau30)
  b <- test_SA(rmst_exp = rmst_exp30[1], se_rmst_exp = rmst_exp30[2], tau = tau30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data50 <- ten(S50)
  a50 <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502 <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50 <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50 <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50 <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50 <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau50 <- max(t50)
  rmst_exp50 <- rmst_single(time = t50, status = delta50, tau = tau50)
  c <- test_SA(rmst_exp = rmst_exp50[1], se_rmst_exp = rmst_exp50[2], tau = tau50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data60 <- ten(S60)
  a60 <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602 <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60 <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60 <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60 <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60 <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau60 <- max(t60)
  rmst_exp60 <- rmst_single(time = t60, status = delta60, tau = tau60)
  d <- test_SA(rmst_exp = rmst_exp60[1], se_rmst_exp = rmst_exp60[2], tau = tau60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data80 <- ten(S80)
  a80 <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802 <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80 <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80 <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80 <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80 <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau80 <- max(t80)
  rmst_exp80 <- rmst_single(time = t80, status = delta80, tau = tau80)
  e <- test_SA(rmst_exp = rmst_exp80[1], se_rmst_exp = rmst_exp80[2], tau = tau80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data100 <- ten(S100)
  a100 <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002 <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100 <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100 <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100 <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100 <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau100 <- max(t100)
  rmst_exp100 <- rmst_single(time = t100, status = delta100, tau = tau100)
  f <- test_SA(rmst_exp = rmst_exp100[1], se_rmst_exp = rmst_exp100[2], tau = tau100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data150 <- ten(S150)
  a150 <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502 <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150 <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150 <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150 <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150 <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau150 <- max(t150)
  rmst_exp150 <- rmst_single(time = t150, status = delta150, tau = tau150)
  g <- test_SA(rmst_exp = rmst_exp150[1], se_rmst_exp = rmst_exp150[2], tau = tau150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data200 <- ten(S200)
  a200 <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002 <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200 <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200 <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200 <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200 <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau200 <- max(t200)
  rmst_exp200 <- rmst_single(time = t200, status = delta200, tau = tau200)
  h <- test_SA(rmst_exp = rmst_exp200[1], se_rmst_exp = rmst_exp200[2], tau = tau200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
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
  
  err_oslrt20[i] <- ifelse((a20<0.05), 1, 0)
  err_moslrt20[i] <- ifelse((a202<0.05), 1, 0)
  err_rc20[i] <- ifelse((b20<0.05), 1, 0)
  err_de20[i] <- ifelse((c20<0.05), 1, 0)
  err_ee20[i] <- ifelse((d20<0.05), 1, 0)
  err_me20[i] <- ifelse((e20<0.05), 1, 0)
  err_rmst_20[i] <- ifelse((a<0.05), 1, 0)
  err_max_hoch_20[i] <- ifelse(g20_hoch<0.05, 1, 0)
  err_max_exact_20[i] <- ifelse(g20_exact<0.05, 1, 0)
  
  err_oslrt30[i] <- ifelse((a30<0.05), 1, 0)
  err_moslrt30[i] <- ifelse((a302<0.05), 1, 0)
  err_rc30[i] <- ifelse((b30<0.05), 1, 0)
  err_de30[i] <- ifelse((c30<0.05), 1, 0)
  err_ee30[i] <- ifelse((d30<0.05), 1, 0)
  err_me30[i] <- ifelse((e30<0.05), 1, 0)
  err_rmst_30[i] <- ifelse((b<0.05), 1, 0)
  err_max_hoch_30[i] <- ifelse(g30_hoch<0.05, 1, 0)
  err_max_exact_30[i] <- ifelse(g30_exact<0.05, 1, 0)
  
  err_oslrt50[i] <- ifelse((a50<0.05), 1, 0)
  err_moslrt50[i] <- ifelse((a502<0.05), 1, 0)
  err_rc50[i] <- ifelse((b50<0.05), 1, 0)
  err_de50[i] <- ifelse((c50<0.05), 1, 0)
  err_ee50[i] <- ifelse((d50<0.05), 1, 0)
  err_me50[i] <- ifelse((e50<0.05), 1, 0)
  err_rmst_50[i] <- ifelse((c<0.05), 1, 0)
  err_max_hoch_50[i] <- ifelse(g50_hoch<0.05, 1, 0)
  err_max_exact_50[i] <- ifelse(g50_exact<0.05, 1, 0)
  
  err_oslrt60[i] <- ifelse((a60<0.05), 1, 0)
  err_moslrt60[i] <- ifelse((a602<0.05), 1, 0)
  err_rc60[i] <- ifelse((b60<0.05), 1, 0)
  err_de60[i] <- ifelse((c60<0.05), 1, 0)
  err_ee60[i] <- ifelse((d60<0.05), 1, 0)
  err_me60[i] <- ifelse((e60<0.05), 1, 0)
  err_rmst_60[i] <- ifelse((d<0.05), 1, 0)
  err_max_hoch_60[i] <- ifelse(g60_hoch<0.05, 1, 0)
  err_max_exact_60[i] <- ifelse(g60_exact<0.05, 1, 0)
  
  err_oslrt80[i] <- ifelse((a80<0.05), 1, 0)
  err_moslrt80[i] <- ifelse((a802<0.05), 1, 0)
  err_rc80[i] <- ifelse((b80<0.05), 1, 0)
  err_de80[i] <- ifelse((c80<0.05), 1, 0)
  err_ee80[i] <- ifelse((d80<0.05), 1, 0)
  err_me80[i] <- ifelse((e80<0.05), 1, 0)
  err_rmst_80[i] <- ifelse((e<0.05), 1, 0)
  err_max_hoch_80[i] <- ifelse(g80_hoch<0.05, 1, 0)
  err_max_exact_80[i] <- ifelse(g80_exact<0.05, 1, 0)
  
  err_oslrt100[i] <- ifelse((a100<0.05), 1, 0)
  err_moslrt100[i] <- ifelse((a1002<0.05), 1, 0)
  err_rc100[i] <- ifelse((b100<0.05), 1, 0)
  err_de100[i] <- ifelse((c100<0.05), 1, 0)
  err_ee100[i] <- ifelse((d100<0.05), 1, 0)
  err_me100[i] <- ifelse((e100<0.05), 1, 0)
  err_rmst_100[i] <- ifelse((f<0.05), 1, 0)
  err_max_hoch_100[i] <- ifelse(g100_hoch<0.05, 1, 0)
  err_max_exact_100[i] <- ifelse(g100_exact<0.05, 1, 0)
  
  err_oslrt150[i] <- ifelse((a150<0.05), 1, 0)
  err_moslrt150[i] <- ifelse((a1502<0.05), 1, 0)
  err_rc150[i] <- ifelse((b150<0.05), 1, 0)
  err_de150[i] <- ifelse((c150<0.05), 1, 0)
  err_ee150[i] <- ifelse((d150<0.05), 1, 0)
  err_me150[i] <- ifelse((e150<0.05), 1, 0)
  err_rmst_150[i] <- ifelse((g<0.05), 1, 0)
  err_max_hoch_150[i] <- ifelse(g150_hoch<0.05, 1, 0)
  err_max_exact_150[i] <- ifelse(g150_exact<0.05, 1, 0)
  
  err_oslrt200[i] <- ifelse((a200<0.05), 1, 0)
  err_moslrt200[i] <- ifelse((a2002<0.05), 1, 0)
  err_rc200[i] <- ifelse((b200<0.05), 1, 0)
  err_de200[i] <- ifelse((c200<0.05), 1, 0)              
  err_ee200[i] <- ifelse((d200<0.05), 1, 0)
  err_me200[i] <- ifelse((e200<0.05), 1, 0)
  err_rmst_200[i] <- ifelse((h<0.05), 1, 0)
  err_max_hoch_200[i] <- ifelse(g200_hoch<0.05, 1, 0)
  err_max_exact_200[i] <- ifelse(g200_exact<0.05, 1, 0)
}

os20 <- sum(err_oslrt20)/nit
mos20 <- sum(err_moslrt20)/nit
rc20 <- sum(err_rc20)/nit
de20 <- sum(err_de20)/nit
ee20 <- sum(err_ee20)/nit
me20 <- sum(err_me20)/nit
r_20 <- sum(err_rmst_20)/nit
max_hoch20 <- sum(err_max_hoch_20)/nit
max_exact20 <- sum(err_max_exact_20)/nit
O20 <- mean(obs20)
O20_adm <- mean(obs20_adm)
mean(tx_cens20)
mean(tx_censadm20)

os30 <- sum(err_oslrt30)/nit
mos30 <- sum(err_moslrt30)/nit
rc30 <- sum(err_rc30)/nit
de30 <- sum(err_de30)/nit
ee30 <- sum(err_ee30)/nit
me30 <- sum(err_me30)/nit
r_30 <- sum(err_rmst_30)/nit
max_hoch30 <- sum(err_max_hoch_30)/nit
max_exact30 <- sum(err_max_exact_30)/nit
O30 <- mean(obs30)
O30_adm <- mean(obs30_adm)
mean(tx_cens30)
mean(tx_censadm30)

os50 <- sum(err_oslrt50)/nit
mos50 <- sum(err_moslrt50)/nit
rc50 <- sum(err_rc50)/nit
de50 <- sum(err_de50)/nit
ee50 <- sum(err_ee50)/nit
me50 <- sum(err_me50)/nit
r_50 <- sum(err_rmst_50)/nit
max_hoch50 <- sum(err_max_hoch_50)/nit
max_exact50 <- sum(err_max_exact_50)/nit
O50 <- mean(obs50)
O50_adm <- mean(obs50_adm)
mean(tx_cens50)
mean(tx_censadm50)

os60 <- sum(err_oslrt60)/nit
mos60 <- sum(err_moslrt60)/nit
rc60 <- sum(err_rc60)/nit
de60 <- sum(err_de60)/nit
ee60 <- sum(err_ee60)/nit
me60 <- sum(err_me60)/nit
r_60 <- sum(err_rmst_60)/nit
max_hoch60 <- sum(err_max_hoch_60)/nit
max_exact60 <- sum(err_max_exact_60)/nit
O60 <- mean(obs60)
O60_adm <- mean(obs60_adm)
mean(tx_cens60)
mean(tx_censadm60)

os80 <- sum(err_oslrt80)/nit
mos80 <- sum(err_moslrt80)/nit
rc80 <- sum(err_rc80)/nit
de80 <- sum(err_de80)/nit
ee80 <- sum(err_ee80)/nit
me80 <- sum(err_me80)/nit
r_80 <- sum(err_rmst_80)/nit
max_hoch80 <- sum(err_max_hoch_80)/nit
max_exact80 <- sum(err_max_exact_80)/nit
O80 <- mean(obs80)
O80_adm <- mean(obs80_adm)
mean(tx_cens80)
mean(tx_censadm80)

os100 <- sum(err_oslrt100)/nit
mos100 <- sum(err_moslrt100)/nit
rc100 <- sum(err_rc100)/nit
de100 <- sum(err_de100)/nit
ee100 <- sum(err_ee100)/nit
me100 <- sum(err_me100)/nit
r_100 <- sum(err_rmst_100)/nit
max_hoch100 <- sum(err_max_hoch_100)/nit
max_exact100 <- sum(err_max_exact_100)/nit
O100 <- mean(obs100)
O100_adm <- mean(obs100_adm)
mean(tx_cens100)
mean(tx_censadm100)

os150 <- sum(err_oslrt150)/nit
mos150 <- sum(err_moslrt150)/nit
rc150 <- sum(err_rc150)/nit
de150 <- sum(err_de150)/nit
ee150 <- sum(err_ee150)/nit
me150 <- sum(err_me150)/nit
r_150 <- sum(err_rmst_150)/nit
max_hoch150 <- sum(err_max_hoch_150)/nit
max_exact150 <- sum(err_max_exact_150)/nit
O150 <- mean(obs150)
O150_adm <- mean(obs150_adm)
mean(tx_cens150)
mean(tx_censadm150)

os200 <- sum(err_oslrt200)/nit
mos200 <- sum(err_moslrt200)/nit
rc200 <- sum(err_rc200)/nit
de200 <- sum(err_de200)/nit
ee200 <- sum(err_ee200)/nit
me200 <- sum(err_me200)/nit
r_200 <- sum(err_rmst_200)/nit
max_hoch200 <- sum(err_max_hoch_200)/nit
max_exact200 <- sum(err_max_exact_200)/nit
O200 <- mean(obs200)
O200_adm <- mean(obs200_adm)
mean(tx_cens200)
mean(tx_censadm200)

n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl <- c(os20, os30, os50, os60, os80, os100, os150, os200)
mosl <- c(mos20, mos30, mos50, mos60, mos80, mos100, mos150, mos200)
rc <- c(rc20, rc30, rc50, rc60, rc80, rc100, rc150, rc200)
de <- c(de20, de30, de50, de60, de80, de100, de150, de200)
ee <- c(ee20, ee30, ee50, ee60, ee80, ee100, ee150, ee200)
me <- c(me20, me30, me50, me60, me80, me100, me150, me200)
rmst <- c(r_20, r_30, r_50, r_60, r_80, r_100, r_150, r_200)
hoch <- c(max_hoch20, max_hoch30, max_hoch50, max_hoch60, max_hoch80, max_hoch100, max_hoch150, max_hoch200)
exact <- c(max_exact20, max_exact30, max_exact50, max_exact60, max_exact80, max_exact100, max_exact150, max_exact200)
Obs <- c(round(O20), round(O30), round(O50), round(O60), round(O80), round(O100), round(O150), round(O200))
Obs_adm <- c(round(O20_adm), round(O30_adm), round(O50_adm), round(O60_adm), round(O80_adm), round(O100_adm), round(O150_adm), round(O200_adm))
osl
mosl
ee
me
de
rc
rmst
hoch
exact
Obs
Obs_adm

d1 <- data.frame(Sample.size = n, Obs_adm = Obs_adm, Error = c(osl, mosl, ee, me, de, rc, rmst, hoch, exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), rep('RMST', 8),
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (exact p-value)', 8)))
d1$Test <- as.factor(d1$Test)
d1$Test <- factor(d1$Test, levels = c('OSLRT', 'Modified OSLRT', 'RMST', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (exact p-value)'))

ggplot(d1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(name = 'Sample size', sec.axis = sec_axis(trans = ~., name = 'Number of events', breaks = n, labels = Obs_adm))+
  scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group', 
       y = 'Power',
       title = '',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+   ##nominal level for non null effect 80%
  #geom_hline(yintercept = 0.05, size = 0.2)+  ##nominal level for null effect 5%
  ylim(0,1)+
  theme(axis.text.x.top = element_text(size = 8),
        legend.position = 'right',
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

nit <- 10000

err_oslrt20 <- rep(0, nit)
err_moslrt20 <- rep(0, nit)
err_rc20 <- rep(0, nit)
err_de20 <- rep(0, nit)
err_ee20 <- rep(0, nit)
err_me20 <- rep(0, nit)
err_rmst_20 <- rep(0, nit)
err_max_hoch_20 <- rep(0, nit)
err_max_exact_20 <- rep(0, nit)
obs20 <- rep(0, nit)
obs20_adm <- rep(0, nit)
tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)

err_oslrt30 <- rep(0, nit)
err_moslrt30 <- rep(0, nit)
err_rc30 <- rep(0, nit)
err_de30 <- rep(0, nit)
err_ee30 <- rep(0, nit)
err_me30 <- rep(0, nit)
err_rmst_30 <- rep(0, nit)
err_max_hoch_30 <- rep(0, nit)
err_max_exact_30 <- rep(0, nit)
obs30 <- rep(0, nit)
obs30_adm <- rep(0, nit)
tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)

err_oslrt50 <- rep(0, nit)
err_moslrt50 <- rep(0, nit)
err_rc50 <- rep(0, nit)
err_de50 <- rep(0, nit)
err_ee50 <- rep(0, nit)
err_me50 <- rep(0, nit)
err_rmst_50 <- rep(0, nit)
err_max_hoch_50 <- rep(0, nit)
err_max_exact_50 <- rep(0, nit)
obs50 <- rep(0, nit)
obs50_adm <- rep(0, nit)
tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)

err_oslrt60 <- rep(0, nit)
err_moslrt60 <- rep(0, nit)
err_rc60 <- rep(0, nit)
err_de60 <- rep(0, nit)
err_ee60 <- rep(0, nit)
err_me60 <- rep(0, nit)
err_rmst_60 <- rep(0, nit)
err_max_hoch_60 <- rep(0, nit)
err_max_exact_60 <- rep(0, nit)
obs60 <- rep(0, nit)
obs60_adm <- rep(0, nit)
tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)

err_oslrt80 <- rep(0, nit)
err_moslrt80 <- rep(0, nit)
err_rc80 <- rep(0, nit)
err_de80 <- rep(0, nit)
err_ee80 <- rep(0, nit)
err_me80 <- rep(0, nit)
err_rmst_80 <- rep(0, nit)
err_max_hoch_80 <- rep(0, nit)
err_max_exact_80 <- rep(0, nit)
obs80 <- rep(0, nit)
obs80_adm <- rep(0, nit)
tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)

err_oslrt100 <- rep(0, nit)
err_moslrt100 <- rep(0, nit)
err_rc100 <- rep(0, nit)
err_de100 <- rep(0, nit)
err_ee100 <- rep(0, nit)
err_me100 <- rep(0, nit)
err_rmst_100 <- rep(0, nit)
err_max_hoch_100 <- rep(0, nit)
err_max_exact_100 <- rep(0, nit)
obs100 <- rep(0, nit)
obs100_adm <- rep(0, nit)
tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)

err_oslrt150 <- rep(0, nit)
err_moslrt150 <- rep(0, nit)
err_rc150 <- rep(0, nit)
err_de150 <- rep(0, nit)
err_ee150 <- rep(0, nit)
err_me150 <- rep(0, nit)
err_rmst_150 <- rep(0, nit)
err_max_hoch_150 <- rep(0, nit)
err_max_exact_150 <- rep(0, nit)
obs150 <- rep(0, nit)
obs150_adm <- rep(0, nit)
tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)

err_oslrt200 <- rep(0, nit)
err_moslrt200 <- rep(0, nit)
err_rc200 <- rep(0, nit)
err_de200 <- rep(0, nit)
err_ee200 <- rep(0, nit)
err_me200 <- rep(0, nit)
err_rmst_200 <- rep(0, nit)
err_max_hoch_200 <- rep(0, nit)
err_max_exact_200 <- rep(0, nit)
obs200 <- rep(0, nit)
obs200_adm <- rep(0, nit)
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
  obs20[i] <- sum(del20)
  obs30[i] <- sum(del30)
  obs50[i] <- sum(del50)
  obs60[i] <- sum(del60)
  obs80[i] <- sum(del80)
  obs100[i] <- sum(del100)
  obs150[i] <- sum(del150)
  obs200[i] <- sum(del200)
  
  obs20_adm[i] <- sum(delta20)
  obs30_adm[i] <- sum(delta30)
  obs50_adm[i] <- sum(delta50)
  obs60_adm[i] <- sum(delta60)
  obs80_adm[i] <- sum(delta80)
  obs100_adm[i] <- sum(delta100)
  obs150_adm[i] <- sum(delta150)
  obs200_adm[i] <- sum(delta200)
  
  data20 <- ten(S20)
  a20 <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202 <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20 <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20 <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20 <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20 <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau20 <- max(t20)
  rmst_exp20 <- rmst_single(time = t20, status = delta20, tau = tau20)
  a <- test_SA(rmst_exp = rmst_exp20[1], se_rmst_exp = rmst_exp20[2], tau = tau20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data30 <- ten(S30)
  a30 <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302 <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30 <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30 <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30 <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30 <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau30 <- max(t30)
  rmst_exp30 <- rmst_single(time = t30, status = delta30, tau = tau30)
  b <- test_SA(rmst_exp = rmst_exp30[1], se_rmst_exp = rmst_exp30[2], tau = tau30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data50 <- ten(S50)
  a50 <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502 <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50 <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50 <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50 <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50 <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau50 <- max(t50)
  rmst_exp50 <- rmst_single(time = t50, status = delta50, tau = tau50)
  c <- test_SA(rmst_exp = rmst_exp50[1], se_rmst_exp = rmst_exp50[2], tau = tau50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data60 <- ten(S60)
  a60 <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602 <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60 <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60 <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60 <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60 <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau60 <- max(t60)
  rmst_exp60 <- rmst_single(time = t60, status = delta60, tau = tau60)
  d <- test_SA(rmst_exp = rmst_exp60[1], se_rmst_exp = rmst_exp60[2], tau = tau60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data80 <- ten(S80)
  a80 <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802 <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80 <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80 <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80 <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80 <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau80 <- max(t80)
  rmst_exp80 <- rmst_single(time = t80, status = delta80, tau = tau80)
  e <- test_SA(rmst_exp = rmst_exp80[1], se_rmst_exp = rmst_exp80[2], tau = tau80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data100 <- ten(S100)
  a100 <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002 <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100 <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100 <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100 <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100 <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau100 <- max(t100)
  rmst_exp100 <- rmst_single(time = t100, status = delta100, tau = tau100)
  f <- test_SA(rmst_exp = rmst_exp100[1], se_rmst_exp = rmst_exp100[2], tau = tau100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data150 <- ten(S150)
  a150 <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502 <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150 <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150 <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150 <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150 <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau150 <- max(t150)
  rmst_exp150 <- rmst_single(time = t150, status = delta150, tau = tau150)
  g <- test_SA(rmst_exp = rmst_exp150[1], se_rmst_exp = rmst_exp150[2], tau = tau150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data200 <- ten(S200)
  a200 <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002 <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200 <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200 <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200 <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200 <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau200 <- max(t200)
  rmst_exp200 <- rmst_single(time = t200, status = delta200, tau = tau200)
  h <- test_SA(rmst_exp = rmst_exp200[1], se_rmst_exp = rmst_exp200[2], tau = tau200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
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
  
  err_oslrt20[i] <- ifelse((a20<0.05), 1, 0)
  err_moslrt20[i] <- ifelse((a202<0.05), 1, 0)
  err_rc20[i] <- ifelse((b20<0.05), 1, 0)
  err_de20[i] <- ifelse((c20<0.05), 1, 0)
  err_ee20[i] <- ifelse((d20<0.05), 1, 0)
  err_me20[i] <- ifelse((e20<0.05), 1, 0)
  err_rmst_20[i] <- ifelse((a<0.05), 1, 0)
  err_max_hoch_20[i] <- ifelse(g20_hoch<0.05, 1, 0)
  err_max_exact_20[i] <- ifelse(g20_exact<0.05, 1, 0)
  
  err_oslrt30[i] <- ifelse((a30<0.05), 1, 0)
  err_moslrt30[i] <- ifelse((a302<0.05), 1, 0)
  err_rc30[i] <- ifelse((b30<0.05), 1, 0)
  err_de30[i] <- ifelse((c30<0.05), 1, 0)
  err_ee30[i] <- ifelse((d30<0.05), 1, 0)
  err_me30[i] <- ifelse((e30<0.05), 1, 0)
  err_rmst_30[i] <- ifelse((b<0.05), 1, 0)
  err_max_hoch_30[i] <- ifelse(g30_hoch<0.05, 1, 0)
  err_max_exact_30[i] <- ifelse(g30_exact<0.05, 1, 0)
  
  err_oslrt50[i] <- ifelse((a50<0.05), 1, 0)
  err_moslrt50[i] <- ifelse((a502<0.05), 1, 0)
  err_rc50[i] <- ifelse((b50<0.05), 1, 0)
  err_de50[i] <- ifelse((c50<0.05), 1, 0)
  err_ee50[i] <- ifelse((d50<0.05), 1, 0)
  err_me50[i] <- ifelse((e50<0.05), 1, 0)
  err_rmst_50[i] <- ifelse((c<0.05), 1, 0)
  err_max_hoch_50[i] <- ifelse(g50_hoch<0.05, 1, 0)
  err_max_exact_50[i] <- ifelse(g50_exact<0.05, 1, 0)
  
  err_oslrt60[i] <- ifelse((a60<0.05), 1, 0)
  err_moslrt60[i] <- ifelse((a602<0.05), 1, 0)
  err_rc60[i] <- ifelse((b60<0.05), 1, 0)
  err_de60[i] <- ifelse((c60<0.05), 1, 0)
  err_ee60[i] <- ifelse((d60<0.05), 1, 0)
  err_me60[i] <- ifelse((e60<0.05), 1, 0)
  err_rmst_60[i] <- ifelse((d<0.05), 1, 0)
  err_max_hoch_60[i] <- ifelse(g60_hoch<0.05, 1, 0)
  err_max_exact_60[i] <- ifelse(g60_exact<0.05, 1, 0)
  
  err_oslrt80[i] <- ifelse((a80<0.05), 1, 0)
  err_moslrt80[i] <- ifelse((a802<0.05), 1, 0)
  err_rc80[i] <- ifelse((b80<0.05), 1, 0)
  err_de80[i] <- ifelse((c80<0.05), 1, 0)
  err_ee80[i] <- ifelse((d80<0.05), 1, 0)
  err_me80[i] <- ifelse((e80<0.05), 1, 0)
  err_rmst_80[i] <- ifelse((e<0.05), 1, 0)
  err_max_hoch_80[i] <- ifelse(g80_hoch<0.05, 1, 0)
  err_max_exact_80[i] <- ifelse(g80_exact<0.05, 1, 0)
  
  err_oslrt100[i] <- ifelse((a100<0.05), 1, 0)
  err_moslrt100[i] <- ifelse((a1002<0.05), 1, 0)
  err_rc100[i] <- ifelse((b100<0.05), 1, 0)
  err_de100[i] <- ifelse((c100<0.05), 1, 0)
  err_ee100[i] <- ifelse((d100<0.05), 1, 0)
  err_me100[i] <- ifelse((e100<0.05), 1, 0)
  err_rmst_100[i] <- ifelse((f<0.05), 1, 0)
  err_max_hoch_100[i] <- ifelse(g100_hoch<0.05, 1, 0)
  err_max_exact_100[i] <- ifelse(g100_exact<0.05, 1, 0)
  
  err_oslrt150[i] <- ifelse((a150<0.05), 1, 0)
  err_moslrt150[i] <- ifelse((a1502<0.05), 1, 0)
  err_rc150[i] <- ifelse((b150<0.05), 1, 0)
  err_de150[i] <- ifelse((c150<0.05), 1, 0)
  err_ee150[i] <- ifelse((d150<0.05), 1, 0)
  err_me150[i] <- ifelse((e150<0.05), 1, 0)
  err_rmst_150[i] <- ifelse((g<0.05), 1, 0)
  err_max_hoch_150[i] <- ifelse(g150_hoch<0.05, 1, 0)
  err_max_exact_150[i] <- ifelse(g150_exact<0.05, 1, 0)
  
  err_oslrt200[i] <- ifelse((a200<0.05), 1, 0)
  err_moslrt200[i] <- ifelse((a2002<0.05), 1, 0)
  err_rc200[i] <- ifelse((b200<0.05), 1, 0)
  err_de200[i] <- ifelse((c200<0.05), 1, 0)              
  err_ee200[i] <- ifelse((d200<0.05), 1, 0)
  err_me200[i] <- ifelse((e200<0.05), 1, 0)
  err_rmst_200[i] <- ifelse((h<0.05), 1, 0)
  err_max_hoch_200[i] <- ifelse(g200_hoch<0.05, 1, 0)
  err_max_exact_200[i] <- ifelse(g200_exact<0.05, 1, 0)
}

os20 <- sum(err_oslrt20)/nit
mos20 <- sum(err_moslrt20)/nit
rc20 <- sum(err_rc20)/nit
de20 <- sum(err_de20)/nit
ee20 <- sum(err_ee20)/nit
me20 <- sum(err_me20)/nit
r_20 <- sum(err_rmst_20)/nit
max_hoch20 <- sum(err_max_hoch_20)/nit
max_exact20 <- sum(err_max_exact_20)/nit
O20 <- mean(obs20)
O20_adm <- mean(obs20_adm)
mean(tx_cens20)
mean(tx_censadm20)

os30 <- sum(err_oslrt30)/nit
mos30 <- sum(err_moslrt30)/nit
rc30 <- sum(err_rc30)/nit
de30 <- sum(err_de30)/nit
ee30 <- sum(err_ee30)/nit
me30 <- sum(err_me30)/nit
r_30 <- sum(err_rmst_30)/nit
max_hoch30 <- sum(err_max_hoch_30)/nit
max_exact30 <- sum(err_max_exact_30)/nit
O30 <- mean(obs30)
O30_adm <- mean(obs30_adm)
mean(tx_cens30)
mean(tx_censadm30)

os50 <- sum(err_oslrt50)/nit
mos50 <- sum(err_moslrt50)/nit
rc50 <- sum(err_rc50)/nit
de50 <- sum(err_de50)/nit
ee50 <- sum(err_ee50)/nit
me50 <- sum(err_me50)/nit
r_50 <- sum(err_rmst_50)/nit
max_hoch50 <- sum(err_max_hoch_50)/nit
max_exact50 <- sum(err_max_exact_50)/nit
O50 <- mean(obs50)
O50_adm <- mean(obs50_adm)
mean(tx_cens50)
mean(tx_censadm50)

os60 <- sum(err_oslrt60)/nit
mos60 <- sum(err_moslrt60)/nit
rc60 <- sum(err_rc60)/nit
de60 <- sum(err_de60)/nit
ee60 <- sum(err_ee60)/nit
me60 <- sum(err_me60)/nit
r_60 <- sum(err_rmst_60)/nit
max_hoch60 <- sum(err_max_hoch_60)/nit
max_exact60 <- sum(err_max_exact_60)/nit
O60 <- mean(obs60)
O60_adm <- mean(obs60_adm)
mean(tx_cens60)
mean(tx_censadm60)

os80 <- sum(err_oslrt80)/nit
mos80 <- sum(err_moslrt80)/nit
rc80 <- sum(err_rc80)/nit
de80 <- sum(err_de80)/nit
ee80 <- sum(err_ee80)/nit
me80 <- sum(err_me80)/nit
r_80 <- sum(err_rmst_80)/nit
max_hoch80 <- sum(err_max_hoch_80)/nit
max_exact80 <- sum(err_max_exact_80)/nit
O80 <- mean(obs80)
O80_adm <- mean(obs80_adm)
mean(tx_cens80)
mean(tx_censadm80)

os100 <- sum(err_oslrt100)/nit
mos100 <- sum(err_moslrt100)/nit
rc100 <- sum(err_rc100)/nit
de100 <- sum(err_de100)/nit
ee100 <- sum(err_ee100)/nit
me100 <- sum(err_me100)/nit
r_100 <- sum(err_rmst_100)/nit
max_hoch100 <- sum(err_max_hoch_100)/nit
max_exact100 <- sum(err_max_exact_100)/nit
O100 <- mean(obs100)
O100_adm <- mean(obs100_adm)
mean(tx_cens100)
mean(tx_censadm100)

os150 <- sum(err_oslrt150)/nit
mos150 <- sum(err_moslrt150)/nit
rc150 <- sum(err_rc150)/nit
de150 <- sum(err_de150)/nit
ee150 <- sum(err_ee150)/nit
me150 <- sum(err_me150)/nit
r_150 <- sum(err_rmst_150)/nit
max_hoch150 <- sum(err_max_hoch_150)/nit
max_exact150 <- sum(err_max_exact_150)/nit
O150 <- mean(obs150)
O150_adm <- mean(obs150_adm)
mean(tx_cens150)
mean(tx_censadm150)

os200 <- sum(err_oslrt200)/nit
mos200 <- sum(err_moslrt200)/nit
rc200 <- sum(err_rc200)/nit
de200 <- sum(err_de200)/nit
ee200 <- sum(err_ee200)/nit
me200 <- sum(err_me200)/nit
r_200 <- sum(err_rmst_200)/nit
max_hoch200 <- sum(err_max_hoch_200)/nit
max_exact200 <- sum(err_max_exact_200)/nit
O200 <- mean(obs200)
O200_adm <- mean(obs200_adm)
mean(tx_cens200)
mean(tx_censadm200)

n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl <- c(os20, os30, os50, os60, os80, os100, os150, os200)
mosl <- c(mos20, mos30, mos50, mos60, mos80, mos100, mos150, mos200)
rc <- c(rc20, rc30, rc50, rc60, rc80, rc100, rc150, rc200)
de <- c(de20, de30, de50, de60, de80, de100, de150, de200)
ee <- c(ee20, ee30, ee50, ee60, ee80, ee100, ee150, ee200)
me <- c(me20, me30, me50, me60, me80, me100, me150, me200)
rmst <- c(r_20, r_30, r_50, r_60, r_80, r_100, r_150, r_200)
hoch <- c(max_hoch20, max_hoch30, max_hoch50, max_hoch60, max_hoch80, max_hoch100, max_hoch150, max_hoch200)
exact <- c(max_exact20, max_exact30, max_exact50, max_exact60, max_exact80, max_exact100, max_exact150, max_exact200)
Obs <- c(round(O20), round(O30), round(O50), round(O60), round(O80), round(O100), round(O150), round(O200))
Obs_adm <- c(round(O20_adm), round(O30_adm), round(O50_adm), round(O60_adm), round(O80_adm), round(O100_adm), round(O150_adm), round(O200_adm))
osl
mosl
ee
me
de
rc
rmst
hoch
exact
Obs
Obs_adm

d1 <- data.frame(Sample.size = n, Obs_adm = Obs_adm, Error = c(osl, mosl, ee, me, de, rc, rmst, hoch, exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), rep('RMST', 8),
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (exact p-value)', 8)))
d1$Test <- as.factor(d1$Test)
d1$Test <- factor(d1$Test, levels = c('OSLRT', 'Modified OSLRT', 'RMST', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (exact p-value)'))

ggplot(d1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(name = 'Sample size', sec.axis = sec_axis(trans = ~., name = 'Number of events', breaks = n, labels = Obs_adm))+
  scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group', 
       y = 'Power',
       title = '',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+   ##nominal level for non null effect 80%
  #geom_hline(yintercept = 0.05, size = 0.2)+  ##nominal level for null effect 5%
  ylim(0,1)+
  theme(axis.text.x.top = element_text(size = 8),
        legend.position = 'right',
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

nit <- 10000

err_oslrt20 <- rep(0, nit)
err_moslrt20 <- rep(0, nit)
err_rc20 <- rep(0, nit)
err_de20 <- rep(0, nit)
err_ee20 <- rep(0, nit)
err_me20 <- rep(0, nit)
err_rmst_20 <- rep(0, nit)
err_max_hoch_20 <- rep(0, nit)
err_max_exact_20 <- rep(0, nit)
obs20 <- rep(0, nit)
obs20_adm <- rep(0, nit)
tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)

err_oslrt30 <- rep(0, nit)
err_moslrt30 <- rep(0, nit)
err_rc30 <- rep(0, nit)
err_de30 <- rep(0, nit)
err_ee30 <- rep(0, nit)
err_me30 <- rep(0, nit)
err_rmst_30 <- rep(0, nit)
err_max_hoch_30 <- rep(0, nit)
err_max_exact_30 <- rep(0, nit)
obs30 <- rep(0, nit)
obs30_adm <- rep(0, nit)
tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)

err_oslrt50 <- rep(0, nit)
err_moslrt50 <- rep(0, nit)
err_rc50 <- rep(0, nit)
err_de50 <- rep(0, nit)
err_ee50 <- rep(0, nit)
err_me50 <- rep(0, nit)
err_rmst_50 <- rep(0, nit)
err_max_hoch_50 <- rep(0, nit)
err_max_exact_50 <- rep(0, nit)
obs50 <- rep(0, nit)
obs50_adm <- rep(0, nit)
tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)

err_oslrt60 <- rep(0, nit)
err_moslrt60 <- rep(0, nit)
err_rc60 <- rep(0, nit)
err_de60 <- rep(0, nit)
err_ee60 <- rep(0, nit)
err_me60 <- rep(0, nit)
err_rmst_60 <- rep(0, nit)
err_max_hoch_60 <- rep(0, nit)
err_max_exact_60 <- rep(0, nit)
obs60 <- rep(0, nit)
obs60_adm <- rep(0, nit)
tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)

err_oslrt80 <- rep(0, nit)
err_moslrt80 <- rep(0, nit)
err_rc80 <- rep(0, nit)
err_de80 <- rep(0, nit)
err_ee80 <- rep(0, nit)
err_me80 <- rep(0, nit)
err_rmst_80 <- rep(0, nit)
err_max_hoch_80 <- rep(0, nit)
err_max_exact_80 <- rep(0, nit)
obs80 <- rep(0, nit)
obs80_adm <- rep(0, nit)
tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)

err_oslrt100 <- rep(0, nit)
err_moslrt100 <- rep(0, nit)
err_rc100 <- rep(0, nit)
err_de100 <- rep(0, nit)
err_ee100 <- rep(0, nit)
err_me100 <- rep(0, nit)
err_rmst_100 <- rep(0, nit)
err_max_hoch_100 <- rep(0, nit)
err_max_exact_100 <- rep(0, nit)
obs100 <- rep(0, nit)
obs100_adm <- rep(0, nit)
tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)

err_oslrt150 <- rep(0, nit)
err_moslrt150 <- rep(0, nit)
err_rc150 <- rep(0, nit)
err_de150 <- rep(0, nit)
err_ee150 <- rep(0, nit)
err_me150 <- rep(0, nit)
err_rmst_150 <- rep(0, nit)
err_max_hoch_150 <- rep(0, nit)
err_max_exact_150 <- rep(0, nit)
obs150 <- rep(0, nit)
obs150_adm <- rep(0, nit)
tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)

err_oslrt200 <- rep(0, nit)
err_moslrt200 <- rep(0, nit)
err_rc200 <- rep(0, nit)
err_de200 <- rep(0, nit)
err_ee200 <- rep(0, nit)
err_me200 <- rep(0, nit)
err_rmst_200 <- rep(0, nit)
err_max_hoch_200 <- rep(0, nit)
err_max_exact_200 <- rep(0, nit)
obs200 <- rep(0, nit)
obs200_adm <- rep(0, nit)
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
  obs20[i] <- sum(del20)
  obs30[i] <- sum(del30)
  obs50[i] <- sum(del50)
  obs60[i] <- sum(del60)
  obs80[i] <- sum(del80)
  obs100[i] <- sum(del100)
  obs150[i] <- sum(del150)
  obs200[i] <- sum(del200)
  
  obs20_adm[i] <- sum(delta20)
  obs30_adm[i] <- sum(delta30)
  obs50_adm[i] <- sum(delta50)
  obs60_adm[i] <- sum(delta60)
  obs80_adm[i] <- sum(delta80)
  obs100_adm[i] <- sum(delta100)
  obs150_adm[i] <- sum(delta150)
  obs200_adm[i] <- sum(delta200)
  
  data20 <- ten(S20)
  a20 <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202 <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20 <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20 <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20 <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20 <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau20 <- max(t20)
  rmst_exp20 <- rmst_single(time = t20, status = delta20, tau = tau20)
  a <- test_SA(rmst_exp = rmst_exp20[1], se_rmst_exp = rmst_exp20[2], tau = tau20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data30 <- ten(S30)
  a30 <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302 <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30 <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30 <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30 <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30 <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau30 <- max(t30)
  rmst_exp30 <- rmst_single(time = t30, status = delta30, tau = tau30)
  b <- test_SA(rmst_exp = rmst_exp30[1], se_rmst_exp = rmst_exp30[2], tau = tau30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data50 <- ten(S50)
  a50 <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502 <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50 <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50 <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50 <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50 <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau50 <- max(t50)
  rmst_exp50 <- rmst_single(time = t50, status = delta50, tau = tau50)
  c <- test_SA(rmst_exp = rmst_exp50[1], se_rmst_exp = rmst_exp50[2], tau = tau50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data60 <- ten(S60)
  a60 <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602 <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60 <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60 <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60 <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60 <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau60 <- max(t60)
  rmst_exp60 <- rmst_single(time = t60, status = delta60, tau = tau60)
  d <- test_SA(rmst_exp = rmst_exp60[1], se_rmst_exp = rmst_exp60[2], tau = tau60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data80 <- ten(S80)
  a80 <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802 <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80 <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80 <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80 <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80 <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau80 <- max(t80)
  rmst_exp80 <- rmst_single(time = t80, status = delta80, tau = tau80)
  e <- test_SA(rmst_exp = rmst_exp80[1], se_rmst_exp = rmst_exp80[2], tau = tau80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data100 <- ten(S100)
  a100 <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002 <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100 <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100 <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100 <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100 <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau100 <- max(t100)
  rmst_exp100 <- rmst_single(time = t100, status = delta100, tau = tau100)
  f <- test_SA(rmst_exp = rmst_exp100[1], se_rmst_exp = rmst_exp100[2], tau = tau100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data150 <- ten(S150)
  a150 <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502 <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150 <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150 <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150 <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150 <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau150 <- max(t150)
  rmst_exp150 <- rmst_single(time = t150, status = delta150, tau = tau150)
  g <- test_SA(rmst_exp = rmst_exp150[1], se_rmst_exp = rmst_exp150[2], tau = tau150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data200 <- ten(S200)
  a200 <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002 <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200 <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200 <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200 <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200 <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau200 <- max(t200)
  rmst_exp200 <- rmst_single(time = t200, status = delta200, tau = tau200)
  h <- test_SA(rmst_exp = rmst_exp200[1], se_rmst_exp = rmst_exp200[2], tau = tau200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
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
  
  err_oslrt20[i] <- ifelse((a20<0.05), 1, 0)
  err_moslrt20[i] <- ifelse((a202<0.05), 1, 0)
  err_rc20[i] <- ifelse((b20<0.05), 1, 0)
  err_de20[i] <- ifelse((c20<0.05), 1, 0)
  err_ee20[i] <- ifelse((d20<0.05), 1, 0)
  err_me20[i] <- ifelse((e20<0.05), 1, 0)
  err_rmst_20[i] <- ifelse((a<0.05), 1, 0)
  err_max_hoch_20[i] <- ifelse(g20_hoch<0.05, 1, 0)
  err_max_exact_20[i] <- ifelse(g20_exact<0.05, 1, 0)
  
  err_oslrt30[i] <- ifelse((a30<0.05), 1, 0)
  err_moslrt30[i] <- ifelse((a302<0.05), 1, 0)
  err_rc30[i] <- ifelse((b30<0.05), 1, 0)
  err_de30[i] <- ifelse((c30<0.05), 1, 0)
  err_ee30[i] <- ifelse((d30<0.05), 1, 0)
  err_me30[i] <- ifelse((e30<0.05), 1, 0)
  err_rmst_30[i] <- ifelse((b<0.05), 1, 0)
  err_max_hoch_30[i] <- ifelse(g30_hoch<0.05, 1, 0)
  err_max_exact_30[i] <- ifelse(g30_exact<0.05, 1, 0)
  
  err_oslrt50[i] <- ifelse((a50<0.05), 1, 0)
  err_moslrt50[i] <- ifelse((a502<0.05), 1, 0)
  err_rc50[i] <- ifelse((b50<0.05), 1, 0)
  err_de50[i] <- ifelse((c50<0.05), 1, 0)
  err_ee50[i] <- ifelse((d50<0.05), 1, 0)
  err_me50[i] <- ifelse((e50<0.05), 1, 0)
  err_rmst_50[i] <- ifelse((c<0.05), 1, 0)
  err_max_hoch_50[i] <- ifelse(g50_hoch<0.05, 1, 0)
  err_max_exact_50[i] <- ifelse(g50_exact<0.05, 1, 0)
  
  err_oslrt60[i] <- ifelse((a60<0.05), 1, 0)
  err_moslrt60[i] <- ifelse((a602<0.05), 1, 0)
  err_rc60[i] <- ifelse((b60<0.05), 1, 0)
  err_de60[i] <- ifelse((c60<0.05), 1, 0)
  err_ee60[i] <- ifelse((d60<0.05), 1, 0)
  err_me60[i] <- ifelse((e60<0.05), 1, 0)
  err_rmst_60[i] <- ifelse((d<0.05), 1, 0)
  err_max_hoch_60[i] <- ifelse(g60_hoch<0.05, 1, 0)
  err_max_exact_60[i] <- ifelse(g60_exact<0.05, 1, 0)
  
  err_oslrt80[i] <- ifelse((a80<0.05), 1, 0)
  err_moslrt80[i] <- ifelse((a802<0.05), 1, 0)
  err_rc80[i] <- ifelse((b80<0.05), 1, 0)
  err_de80[i] <- ifelse((c80<0.05), 1, 0)
  err_ee80[i] <- ifelse((d80<0.05), 1, 0)
  err_me80[i] <- ifelse((e80<0.05), 1, 0)
  err_rmst_80[i] <- ifelse((e<0.05), 1, 0)
  err_max_hoch_80[i] <- ifelse(g80_hoch<0.05, 1, 0)
  err_max_exact_80[i] <- ifelse(g80_exact<0.05, 1, 0)
  
  err_oslrt100[i] <- ifelse((a100<0.05), 1, 0)
  err_moslrt100[i] <- ifelse((a1002<0.05), 1, 0)
  err_rc100[i] <- ifelse((b100<0.05), 1, 0)
  err_de100[i] <- ifelse((c100<0.05), 1, 0)
  err_ee100[i] <- ifelse((d100<0.05), 1, 0)
  err_me100[i] <- ifelse((e100<0.05), 1, 0)
  err_rmst_100[i] <- ifelse((f<0.05), 1, 0)
  err_max_hoch_100[i] <- ifelse(g100_hoch<0.05, 1, 0)
  err_max_exact_100[i] <- ifelse(g100_exact<0.05, 1, 0)
  
  err_oslrt150[i] <- ifelse((a150<0.05), 1, 0)
  err_moslrt150[i] <- ifelse((a1502<0.05), 1, 0)
  err_rc150[i] <- ifelse((b150<0.05), 1, 0)
  err_de150[i] <- ifelse((c150<0.05), 1, 0)
  err_ee150[i] <- ifelse((d150<0.05), 1, 0)
  err_me150[i] <- ifelse((e150<0.05), 1, 0)
  err_rmst_150[i] <- ifelse((g<0.05), 1, 0)
  err_max_hoch_150[i] <- ifelse(g150_hoch<0.05, 1, 0)
  err_max_exact_150[i] <- ifelse(g150_exact<0.05, 1, 0)
  
  err_oslrt200[i] <- ifelse((a200<0.05), 1, 0)
  err_moslrt200[i] <- ifelse((a2002<0.05), 1, 0)
  err_rc200[i] <- ifelse((b200<0.05), 1, 0)
  err_de200[i] <- ifelse((c200<0.05), 1, 0)              
  err_ee200[i] <- ifelse((d200<0.05), 1, 0)
  err_me200[i] <- ifelse((e200<0.05), 1, 0)
  err_rmst_200[i] <- ifelse((h<0.05), 1, 0)
  err_max_hoch_200[i] <- ifelse(g200_hoch<0.05, 1, 0)
  err_max_exact_200[i] <- ifelse(g200_exact<0.05, 1, 0)
}

os20 <- sum(err_oslrt20)/nit
mos20 <- sum(err_moslrt20)/nit
rc20 <- sum(err_rc20)/nit
de20 <- sum(err_de20)/nit
ee20 <- sum(err_ee20)/nit
me20 <- sum(err_me20)/nit
r_20 <- sum(err_rmst_20)/nit
max_hoch20 <- sum(err_max_hoch_20)/nit
max_exact20 <- sum(err_max_exact_20)/nit
O20 <- mean(obs20)
O20_adm <- mean(obs20_adm)
mean(tx_cens20)
mean(tx_censadm20)

os30 <- sum(err_oslrt30)/nit
mos30 <- sum(err_moslrt30)/nit
rc30 <- sum(err_rc30)/nit
de30 <- sum(err_de30)/nit
ee30 <- sum(err_ee30)/nit
me30 <- sum(err_me30)/nit
r_30 <- sum(err_rmst_30)/nit
max_hoch30 <- sum(err_max_hoch_30)/nit
max_exact30 <- sum(err_max_exact_30)/nit
O30 <- mean(obs30)
O30_adm <- mean(obs30_adm)
mean(tx_cens30)
mean(tx_censadm30)

os50 <- sum(err_oslrt50)/nit
mos50 <- sum(err_moslrt50)/nit
rc50 <- sum(err_rc50)/nit
de50 <- sum(err_de50)/nit
ee50 <- sum(err_ee50)/nit
me50 <- sum(err_me50)/nit
r_50 <- sum(err_rmst_50)/nit
max_hoch50 <- sum(err_max_hoch_50)/nit
max_exact50 <- sum(err_max_exact_50)/nit
O50 <- mean(obs50)
O50_adm <- mean(obs50_adm)
mean(tx_cens50)
mean(tx_censadm50)

os60 <- sum(err_oslrt60)/nit
mos60 <- sum(err_moslrt60)/nit
rc60 <- sum(err_rc60)/nit
de60 <- sum(err_de60)/nit
ee60 <- sum(err_ee60)/nit
me60 <- sum(err_me60)/nit
r_60 <- sum(err_rmst_60)/nit
max_hoch60 <- sum(err_max_hoch_60)/nit
max_exact60 <- sum(err_max_exact_60)/nit
O60 <- mean(obs60)
O60_adm <- mean(obs60_adm)
mean(tx_cens60)
mean(tx_censadm60)

os80 <- sum(err_oslrt80)/nit
mos80 <- sum(err_moslrt80)/nit
rc80 <- sum(err_rc80)/nit
de80 <- sum(err_de80)/nit
ee80 <- sum(err_ee80)/nit
me80 <- sum(err_me80)/nit
r_80 <- sum(err_rmst_80)/nit
max_hoch80 <- sum(err_max_hoch_80)/nit
max_exact80 <- sum(err_max_exact_80)/nit
O80 <- mean(obs80)
O80_adm <- mean(obs80_adm)
mean(tx_cens80)
mean(tx_censadm80)

os100 <- sum(err_oslrt100)/nit
mos100 <- sum(err_moslrt100)/nit
rc100 <- sum(err_rc100)/nit
de100 <- sum(err_de100)/nit
ee100 <- sum(err_ee100)/nit
me100 <- sum(err_me100)/nit
r_100 <- sum(err_rmst_100)/nit
max_hoch100 <- sum(err_max_hoch_100)/nit
max_exact100 <- sum(err_max_exact_100)/nit
O100 <- mean(obs100)
O100_adm <- mean(obs100_adm)
mean(tx_cens100)
mean(tx_censadm100)

os150 <- sum(err_oslrt150)/nit
mos150 <- sum(err_moslrt150)/nit
rc150 <- sum(err_rc150)/nit
de150 <- sum(err_de150)/nit
ee150 <- sum(err_ee150)/nit
me150 <- sum(err_me150)/nit
r_150 <- sum(err_rmst_150)/nit
max_hoch150 <- sum(err_max_hoch_150)/nit
max_exact150 <- sum(err_max_exact_150)/nit
O150 <- mean(obs150)
O150_adm <- mean(obs150_adm)
mean(tx_cens150)
mean(tx_censadm150)

os200 <- sum(err_oslrt200)/nit
mos200 <- sum(err_moslrt200)/nit
rc200 <- sum(err_rc200)/nit
de200 <- sum(err_de200)/nit
ee200 <- sum(err_ee200)/nit
me200 <- sum(err_me200)/nit
r_200 <- sum(err_rmst_200)/nit
max_hoch200 <- sum(err_max_hoch_200)/nit
max_exact200 <- sum(err_max_exact_200)/nit
O200 <- mean(obs200)
O200_adm <- mean(obs200_adm)
mean(tx_cens200)
mean(tx_censadm200)

n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl <- c(os20, os30, os50, os60, os80, os100, os150, os200)
mosl <- c(mos20, mos30, mos50, mos60, mos80, mos100, mos150, mos200)
rc <- c(rc20, rc30, rc50, rc60, rc80, rc100, rc150, rc200)
de <- c(de20, de30, de50, de60, de80, de100, de150, de200)
ee <- c(ee20, ee30, ee50, ee60, ee80, ee100, ee150, ee200)
me <- c(me20, me30, me50, me60, me80, me100, me150, me200)
rmst <- c(r_20, r_30, r_50, r_60, r_80, r_100, r_150, r_200)
hoch <- c(max_hoch20, max_hoch30, max_hoch50, max_hoch60, max_hoch80, max_hoch100, max_hoch150, max_hoch200)
exact <- c(max_exact20, max_exact30, max_exact50, max_exact60, max_exact80, max_exact100, max_exact150, max_exact200)
Obs <- c(round(O20), round(O30), round(O50), round(O60), round(O80), round(O100), round(O150), round(O200))
Obs_adm <- c(round(O20_adm), round(O30_adm), round(O50_adm), round(O60_adm), round(O80_adm), round(O100_adm), round(O150_adm), round(O200_adm))
osl
mosl
ee
me
de
rc
rmst
hoch
exact
Obs
Obs_adm

d1 <- data.frame(Sample.size = n, Obs_adm = Obs_adm, Error = c(osl, mosl, ee, me, de, rc, rmst, hoch, exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), rep('RMST', 8),
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (exact p-value)', 8)))
d1$Test <- as.factor(d1$Test)
d1$Test <- factor(d1$Test, levels = c('OSLRT', 'Modified OSLRT', 'RMST', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (exact p-value)'))

ggplot(d1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(name = 'Sample size', sec.axis = sec_axis(trans = ~., name = 'Number of events', breaks = n, labels = Obs_adm))+
  scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group', 
       y = 'Power',
       title = '',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+   ##nominal level for non null effect 80%
  #geom_hline(yintercept = 0.05, size = 0.2)+  ##nominal level for null effect 5%
  ylim(0,1)+
  theme(axis.text.x.top = element_text(size = 8),
        legend.position = 'right',
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

nit <- 10000

err_oslrt20 <- rep(0, nit)
err_moslrt20 <- rep(0, nit)
err_rc20 <- rep(0, nit)
err_de20 <- rep(0, nit)
err_ee20 <- rep(0, nit)
err_me20 <- rep(0, nit)
err_rmst_20 <- rep(0, nit)
err_max_hoch_20 <- rep(0, nit)
err_max_exact_20 <- rep(0, nit)
obs20 <- rep(0, nit)
obs20_adm <- rep(0, nit)
tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)

err_oslrt30 <- rep(0, nit)
err_moslrt30 <- rep(0, nit)
err_rc30 <- rep(0, nit)
err_de30 <- rep(0, nit)
err_ee30 <- rep(0, nit)
err_me30 <- rep(0, nit)
err_rmst_30 <- rep(0, nit)
err_max_hoch_30 <- rep(0, nit)
err_max_exact_30 <- rep(0, nit)
obs30 <- rep(0, nit)
obs30_adm <- rep(0, nit)
tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)

err_oslrt50 <- rep(0, nit)
err_moslrt50 <- rep(0, nit)
err_rc50 <- rep(0, nit)
err_de50 <- rep(0, nit)
err_ee50 <- rep(0, nit)
err_me50 <- rep(0, nit)
err_rmst_50 <- rep(0, nit)
err_max_hoch_50 <- rep(0, nit)
err_max_exact_50 <- rep(0, nit)
obs50 <- rep(0, nit)
obs50_adm <- rep(0, nit)
tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)

err_oslrt60 <- rep(0, nit)
err_moslrt60 <- rep(0, nit)
err_rc60 <- rep(0, nit)
err_de60 <- rep(0, nit)
err_ee60 <- rep(0, nit)
err_me60 <- rep(0, nit)
err_rmst_60 <- rep(0, nit)
err_max_hoch_60 <- rep(0, nit)
err_max_exact_60 <- rep(0, nit)
obs60 <- rep(0, nit)
obs60_adm <- rep(0, nit)
tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)

err_oslrt80 <- rep(0, nit)
err_moslrt80 <- rep(0, nit)
err_rc80 <- rep(0, nit)
err_de80 <- rep(0, nit)
err_ee80 <- rep(0, nit)
err_me80 <- rep(0, nit)
err_rmst_80 <- rep(0, nit)
err_max_hoch_80 <- rep(0, nit)
err_max_exact_80 <- rep(0, nit)
obs80 <- rep(0, nit)
obs80_adm <- rep(0, nit)
tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)

err_oslrt100 <- rep(0, nit)
err_moslrt100 <- rep(0, nit)
err_rc100 <- rep(0, nit)
err_de100 <- rep(0, nit)
err_ee100 <- rep(0, nit)
err_me100 <- rep(0, nit)
err_rmst_100 <- rep(0, nit)
err_max_hoch_100 <- rep(0, nit)
err_max_exact_100 <- rep(0, nit)
obs100 <- rep(0, nit)
obs100_adm <- rep(0, nit)
tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)

err_oslrt150 <- rep(0, nit)
err_moslrt150 <- rep(0, nit)
err_rc150 <- rep(0, nit)
err_de150 <- rep(0, nit)
err_ee150 <- rep(0, nit)
err_me150 <- rep(0, nit)
err_rmst_150 <- rep(0, nit)
err_max_hoch_150 <- rep(0, nit)
err_max_exact_150 <- rep(0, nit)
obs150 <- rep(0, nit)
obs150_adm <- rep(0, nit)
tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)

err_oslrt200 <- rep(0, nit)
err_moslrt200 <- rep(0, nit)
err_rc200 <- rep(0, nit)
err_de200 <- rep(0, nit)
err_ee200 <- rep(0, nit)
err_me200 <- rep(0, nit)
err_rmst_200 <- rep(0, nit)
err_max_hoch_200 <- rep(0, nit)
err_max_exact_200 <- rep(0, nit)
obs200 <- rep(0, nit)
obs200_adm <- rep(0, nit)
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
  obs20[i] <- sum(del20)
  obs30[i] <- sum(del30)
  obs50[i] <- sum(del50)
  obs60[i] <- sum(del60)
  obs80[i] <- sum(del80)
  obs100[i] <- sum(del100)
  obs150[i] <- sum(del150)
  obs200[i] <- sum(del200)
  
  obs20_adm[i] <- sum(delta20)
  obs30_adm[i] <- sum(delta30)
  obs50_adm[i] <- sum(delta50)
  obs60_adm[i] <- sum(delta60)
  obs80_adm[i] <- sum(delta80)
  obs100_adm[i] <- sum(delta100)
  obs150_adm[i] <- sum(delta150)
  obs200_adm[i] <- sum(delta200)
  
  data20 <- ten(S20)
  a20 <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202 <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20 <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20 <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20 <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20 <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau20 <- max(t20)
  rmst_exp20 <- rmst_single(time = t20, status = delta20, tau = tau20)
  a <- test_SA(rmst_exp = rmst_exp20[1], se_rmst_exp = rmst_exp20[2], tau = tau20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data30 <- ten(S30)
  a30 <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302 <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30 <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30 <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30 <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30 <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau30 <- max(t30)
  rmst_exp30 <- rmst_single(time = t30, status = delta30, tau = tau30)
  b <- test_SA(rmst_exp = rmst_exp30[1], se_rmst_exp = rmst_exp30[2], tau = tau30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data50 <- ten(S50)
  a50 <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502 <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50 <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50 <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50 <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50 <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau50 <- max(t50)
  rmst_exp50 <- rmst_single(time = t50, status = delta50, tau = tau50)
  c <- test_SA(rmst_exp = rmst_exp50[1], se_rmst_exp = rmst_exp50[2], tau = tau50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data60 <- ten(S60)
  a60 <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602 <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60 <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60 <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60 <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60 <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau60 <- max(t60)
  rmst_exp60 <- rmst_single(time = t60, status = delta60, tau = tau60)
  d <- test_SA(rmst_exp = rmst_exp60[1], se_rmst_exp = rmst_exp60[2], tau = tau60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data80 <- ten(S80)
  a80 <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802 <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80 <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80 <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80 <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80 <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau80 <- max(t80)
  rmst_exp80 <- rmst_single(time = t80, status = delta80, tau = tau80)
  e <- test_SA(rmst_exp = rmst_exp80[1], se_rmst_exp = rmst_exp80[2], tau = tau80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data100 <- ten(S100)
  a100 <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002 <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100 <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100 <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100 <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100 <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau100 <- max(t100)
  rmst_exp100 <- rmst_single(time = t100, status = delta100, tau = tau100)
  f <- test_SA(rmst_exp = rmst_exp100[1], se_rmst_exp = rmst_exp100[2], tau = tau100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data150 <- ten(S150)
  a150 <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502 <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150 <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150 <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150 <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150 <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau150 <- max(t150)
  rmst_exp150 <- rmst_single(time = t150, status = delta150, tau = tau150)
  g <- test_SA(rmst_exp = rmst_exp150[1], se_rmst_exp = rmst_exp150[2], tau = tau150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data200 <- ten(S200)
  a200 <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002 <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200 <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200 <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200 <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200 <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau200 <- max(t200)
  rmst_exp200 <- rmst_single(time = t200, status = delta200, tau = tau200)
  h <- test_SA(rmst_exp = rmst_exp200[1], se_rmst_exp = rmst_exp200[2], tau = tau200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
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
  
  err_oslrt20[i] <- ifelse((a20<0.05), 1, 0)
  err_moslrt20[i] <- ifelse((a202<0.05), 1, 0)
  err_rc20[i] <- ifelse((b20<0.05), 1, 0)
  err_de20[i] <- ifelse((c20<0.05), 1, 0)
  err_ee20[i] <- ifelse((d20<0.05), 1, 0)
  err_me20[i] <- ifelse((e20<0.05), 1, 0)
  err_rmst_20[i] <- ifelse((a<0.05), 1, 0)
  err_max_hoch_20[i] <- ifelse(g20_hoch<0.05, 1, 0)
  err_max_exact_20[i] <- ifelse(g20_exact<0.05, 1, 0)
  
  err_oslrt30[i] <- ifelse((a30<0.05), 1, 0)
  err_moslrt30[i] <- ifelse((a302<0.05), 1, 0)
  err_rc30[i] <- ifelse((b30<0.05), 1, 0)
  err_de30[i] <- ifelse((c30<0.05), 1, 0)
  err_ee30[i] <- ifelse((d30<0.05), 1, 0)
  err_me30[i] <- ifelse((e30<0.05), 1, 0)
  err_rmst_30[i] <- ifelse((b<0.05), 1, 0)
  err_max_hoch_30[i] <- ifelse(g30_hoch<0.05, 1, 0)
  err_max_exact_30[i] <- ifelse(g30_exact<0.05, 1, 0)
  
  err_oslrt50[i] <- ifelse((a50<0.05), 1, 0)
  err_moslrt50[i] <- ifelse((a502<0.05), 1, 0)
  err_rc50[i] <- ifelse((b50<0.05), 1, 0)
  err_de50[i] <- ifelse((c50<0.05), 1, 0)
  err_ee50[i] <- ifelse((d50<0.05), 1, 0)
  err_me50[i] <- ifelse((e50<0.05), 1, 0)
  err_rmst_50[i] <- ifelse((c<0.05), 1, 0)
  err_max_hoch_50[i] <- ifelse(g50_hoch<0.05, 1, 0)
  err_max_exact_50[i] <- ifelse(g50_exact<0.05, 1, 0)
  
  err_oslrt60[i] <- ifelse((a60<0.05), 1, 0)
  err_moslrt60[i] <- ifelse((a602<0.05), 1, 0)
  err_rc60[i] <- ifelse((b60<0.05), 1, 0)
  err_de60[i] <- ifelse((c60<0.05), 1, 0)
  err_ee60[i] <- ifelse((d60<0.05), 1, 0)
  err_me60[i] <- ifelse((e60<0.05), 1, 0)
  err_rmst_60[i] <- ifelse((d<0.05), 1, 0)
  err_max_hoch_60[i] <- ifelse(g60_hoch<0.05, 1, 0)
  err_max_exact_60[i] <- ifelse(g60_exact<0.05, 1, 0)
  
  err_oslrt80[i] <- ifelse((a80<0.05), 1, 0)
  err_moslrt80[i] <- ifelse((a802<0.05), 1, 0)
  err_rc80[i] <- ifelse((b80<0.05), 1, 0)
  err_de80[i] <- ifelse((c80<0.05), 1, 0)
  err_ee80[i] <- ifelse((d80<0.05), 1, 0)
  err_me80[i] <- ifelse((e80<0.05), 1, 0)
  err_rmst_80[i] <- ifelse((e<0.05), 1, 0)
  err_max_hoch_80[i] <- ifelse(g80_hoch<0.05, 1, 0)
  err_max_exact_80[i] <- ifelse(g80_exact<0.05, 1, 0)
  
  err_oslrt100[i] <- ifelse((a100<0.05), 1, 0)
  err_moslrt100[i] <- ifelse((a1002<0.05), 1, 0)
  err_rc100[i] <- ifelse((b100<0.05), 1, 0)
  err_de100[i] <- ifelse((c100<0.05), 1, 0)
  err_ee100[i] <- ifelse((d100<0.05), 1, 0)
  err_me100[i] <- ifelse((e100<0.05), 1, 0)
  err_rmst_100[i] <- ifelse((f<0.05), 1, 0)
  err_max_hoch_100[i] <- ifelse(g100_hoch<0.05, 1, 0)
  err_max_exact_100[i] <- ifelse(g100_exact<0.05, 1, 0)
  
  err_oslrt150[i] <- ifelse((a150<0.05), 1, 0)
  err_moslrt150[i] <- ifelse((a1502<0.05), 1, 0)
  err_rc150[i] <- ifelse((b150<0.05), 1, 0)
  err_de150[i] <- ifelse((c150<0.05), 1, 0)
  err_ee150[i] <- ifelse((d150<0.05), 1, 0)
  err_me150[i] <- ifelse((e150<0.05), 1, 0)
  err_rmst_150[i] <- ifelse((g<0.05), 1, 0)
  err_max_hoch_150[i] <- ifelse(g150_hoch<0.05, 1, 0)
  err_max_exact_150[i] <- ifelse(g150_exact<0.05, 1, 0)
  
  err_oslrt200[i] <- ifelse((a200<0.05), 1, 0)
  err_moslrt200[i] <- ifelse((a2002<0.05), 1, 0)
  err_rc200[i] <- ifelse((b200<0.05), 1, 0)
  err_de200[i] <- ifelse((c200<0.05), 1, 0)              
  err_ee200[i] <- ifelse((d200<0.05), 1, 0)
  err_me200[i] <- ifelse((e200<0.05), 1, 0)
  err_rmst_200[i] <- ifelse((h<0.05), 1, 0)
  err_max_hoch_200[i] <- ifelse(g200_hoch<0.05, 1, 0)
  err_max_exact_200[i] <- ifelse(g200_exact<0.05, 1, 0)
}

os20 <- sum(err_oslrt20)/nit
mos20 <- sum(err_moslrt20)/nit
rc20 <- sum(err_rc20)/nit
de20 <- sum(err_de20)/nit
ee20 <- sum(err_ee20)/nit
me20 <- sum(err_me20)/nit
r_20 <- sum(err_rmst_20)/nit
max_hoch20 <- sum(err_max_hoch_20)/nit
max_exact20 <- sum(err_max_exact_20)/nit
O20 <- mean(obs20)
O20_adm <- mean(obs20_adm)
mean(tx_cens20)
mean(tx_censadm20)

os30 <- sum(err_oslrt30)/nit
mos30 <- sum(err_moslrt30)/nit
rc30 <- sum(err_rc30)/nit
de30 <- sum(err_de30)/nit
ee30 <- sum(err_ee30)/nit
me30 <- sum(err_me30)/nit
r_30 <- sum(err_rmst_30)/nit
max_hoch30 <- sum(err_max_hoch_30)/nit
max_exact30 <- sum(err_max_exact_30)/nit
O30 <- mean(obs30)
O30_adm <- mean(obs30_adm)
mean(tx_cens30)
mean(tx_censadm30)

os50 <- sum(err_oslrt50)/nit
mos50 <- sum(err_moslrt50)/nit
rc50 <- sum(err_rc50)/nit
de50 <- sum(err_de50)/nit
ee50 <- sum(err_ee50)/nit
me50 <- sum(err_me50)/nit
r_50 <- sum(err_rmst_50)/nit
max_hoch50 <- sum(err_max_hoch_50)/nit
max_exact50 <- sum(err_max_exact_50)/nit
O50 <- mean(obs50)
O50_adm <- mean(obs50_adm)
mean(tx_cens50)
mean(tx_censadm50)

os60 <- sum(err_oslrt60)/nit
mos60 <- sum(err_moslrt60)/nit
rc60 <- sum(err_rc60)/nit
de60 <- sum(err_de60)/nit
ee60 <- sum(err_ee60)/nit
me60 <- sum(err_me60)/nit
r_60 <- sum(err_rmst_60)/nit
max_hoch60 <- sum(err_max_hoch_60)/nit
max_exact60 <- sum(err_max_exact_60)/nit
O60 <- mean(obs60)
O60_adm <- mean(obs60_adm)
mean(tx_cens60)
mean(tx_censadm60)

os80 <- sum(err_oslrt80)/nit
mos80 <- sum(err_moslrt80)/nit
rc80 <- sum(err_rc80)/nit
de80 <- sum(err_de80)/nit
ee80 <- sum(err_ee80)/nit
me80 <- sum(err_me80)/nit
r_80 <- sum(err_rmst_80)/nit
max_hoch80 <- sum(err_max_hoch_80)/nit
max_exact80 <- sum(err_max_exact_80)/nit
O80 <- mean(obs80)
O80_adm <- mean(obs80_adm)
mean(tx_cens80)
mean(tx_censadm80)

os100 <- sum(err_oslrt100)/nit
mos100 <- sum(err_moslrt100)/nit
rc100 <- sum(err_rc100)/nit
de100 <- sum(err_de100)/nit
ee100 <- sum(err_ee100)/nit
me100 <- sum(err_me100)/nit
r_100 <- sum(err_rmst_100)/nit
max_hoch100 <- sum(err_max_hoch_100)/nit
max_exact100 <- sum(err_max_exact_100)/nit
O100 <- mean(obs100)
O100_adm <- mean(obs100_adm)
mean(tx_cens100)
mean(tx_censadm100)

os150 <- sum(err_oslrt150)/nit
mos150 <- sum(err_moslrt150)/nit
rc150 <- sum(err_rc150)/nit
de150 <- sum(err_de150)/nit
ee150 <- sum(err_ee150)/nit
me150 <- sum(err_me150)/nit
r_150 <- sum(err_rmst_150)/nit
max_hoch150 <- sum(err_max_hoch_150)/nit
max_exact150 <- sum(err_max_exact_150)/nit
O150 <- mean(obs150)
O150_adm <- mean(obs150_adm)
mean(tx_cens150)
mean(tx_censadm150)

os200 <- sum(err_oslrt200)/nit
mos200 <- sum(err_moslrt200)/nit
rc200 <- sum(err_rc200)/nit
de200 <- sum(err_de200)/nit
ee200 <- sum(err_ee200)/nit
me200 <- sum(err_me200)/nit
r_200 <- sum(err_rmst_200)/nit
max_hoch200 <- sum(err_max_hoch_200)/nit
max_exact200 <- sum(err_max_exact_200)/nit
O200 <- mean(obs200)
O200_adm <- mean(obs200_adm)
mean(tx_cens200)
mean(tx_censadm200)

n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl <- c(os20, os30, os50, os60, os80, os100, os150, os200)
mosl <- c(mos20, mos30, mos50, mos60, mos80, mos100, mos150, mos200)
rc <- c(rc20, rc30, rc50, rc60, rc80, rc100, rc150, rc200)
de <- c(de20, de30, de50, de60, de80, de100, de150, de200)
ee <- c(ee20, ee30, ee50, ee60, ee80, ee100, ee150, ee200)
me <- c(me20, me30, me50, me60, me80, me100, me150, me200)
rmst <- c(r_20, r_30, r_50, r_60, r_80, r_100, r_150, r_200)
hoch <- c(max_hoch20, max_hoch30, max_hoch50, max_hoch60, max_hoch80, max_hoch100, max_hoch150, max_hoch200)
exact <- c(max_exact20, max_exact30, max_exact50, max_exact60, max_exact80, max_exact100, max_exact150, max_exact200)
Obs <- c(round(O20), round(O30), round(O50), round(O60), round(O80), round(O100), round(O150), round(O200))
Obs_adm <- c(round(O20_adm), round(O30_adm), round(O50_adm), round(O60_adm), round(O80_adm), round(O100_adm), round(O150_adm), round(O200_adm))
osl
mosl
ee
me
de
rc
rmst
hoch
exact
Obs
Obs_adm

d1 <- data.frame(Sample.size = n, Obs_adm = Obs_adm, Error = c(osl, mosl, ee, me, de, rc, rmst, hoch, exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), rep('RMST', 8),
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (exact p-value)', 8)))
d1$Test <- as.factor(d1$Test)
d1$Test <- factor(d1$Test, levels = c('OSLRT', 'Modified OSLRT', 'RMST', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (exact p-value)'))

ggplot(d1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(name = 'Sample size', sec.axis = sec_axis(trans = ~., name = 'Number of events', breaks = n, labels = Obs_adm))+
  scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group', 
       y = 'Power',
       title = '',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+   ##nominal level for non null effect 80%
  #geom_hline(yintercept = 0.05, size = 0.2)+  ##nominal level for null effect 5%
  ylim(0,1)+
  theme(axis.text.x.top = element_text(size = 8),
        legend.position = 'right',
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

nit <- 10000

err_oslrt20 <- rep(0, nit)
err_moslrt20 <- rep(0, nit)
err_rc20 <- rep(0, nit)
err_de20 <- rep(0, nit)
err_ee20 <- rep(0, nit)
err_me20 <- rep(0, nit)
err_rmst_20 <- rep(0, nit)
err_max_hoch_20 <- rep(0, nit)
err_max_exact_20 <- rep(0, nit)
obs20 <- rep(0, nit)
obs20_adm <- rep(0, nit)
tx_cens20 <- rep(0, nit)
tx_censadm20 <- rep(0, nit)

err_oslrt30 <- rep(0, nit)
err_moslrt30 <- rep(0, nit)
err_rc30 <- rep(0, nit)
err_de30 <- rep(0, nit)
err_ee30 <- rep(0, nit)
err_me30 <- rep(0, nit)
err_rmst_30 <- rep(0, nit)
err_max_hoch_30 <- rep(0, nit)
err_max_exact_30 <- rep(0, nit)
obs30 <- rep(0, nit)
obs30_adm <- rep(0, nit)
tx_cens30 <- rep(0, nit)
tx_censadm30 <- rep(0, nit)

err_oslrt50 <- rep(0, nit)
err_moslrt50 <- rep(0, nit)
err_rc50 <- rep(0, nit)
err_de50 <- rep(0, nit)
err_ee50 <- rep(0, nit)
err_me50 <- rep(0, nit)
err_rmst_50 <- rep(0, nit)
err_max_hoch_50 <- rep(0, nit)
err_max_exact_50 <- rep(0, nit)
obs50 <- rep(0, nit)
obs50_adm <- rep(0, nit)
tx_cens50 <- rep(0, nit)
tx_censadm50 <- rep(0, nit)

err_oslrt60 <- rep(0, nit)
err_moslrt60 <- rep(0, nit)
err_rc60 <- rep(0, nit)
err_de60 <- rep(0, nit)
err_ee60 <- rep(0, nit)
err_me60 <- rep(0, nit)
err_rmst_60 <- rep(0, nit)
err_max_hoch_60 <- rep(0, nit)
err_max_exact_60 <- rep(0, nit)
obs60 <- rep(0, nit)
obs60_adm <- rep(0, nit)
tx_cens60 <- rep(0, nit)
tx_censadm60 <- rep(0, nit)

err_oslrt80 <- rep(0, nit)
err_moslrt80 <- rep(0, nit)
err_rc80 <- rep(0, nit)
err_de80 <- rep(0, nit)
err_ee80 <- rep(0, nit)
err_me80 <- rep(0, nit)
err_rmst_80 <- rep(0, nit)
err_max_hoch_80 <- rep(0, nit)
err_max_exact_80 <- rep(0, nit)
obs80 <- rep(0, nit)
obs80_adm <- rep(0, nit)
tx_cens80 <- rep(0, nit)
tx_censadm80 <- rep(0, nit)

err_oslrt100 <- rep(0, nit)
err_moslrt100 <- rep(0, nit)
err_rc100 <- rep(0, nit)
err_de100 <- rep(0, nit)
err_ee100 <- rep(0, nit)
err_me100 <- rep(0, nit)
err_rmst_100 <- rep(0, nit)
err_max_hoch_100 <- rep(0, nit)
err_max_exact_100 <- rep(0, nit)
obs100 <- rep(0, nit)
obs100_adm <- rep(0, nit)
tx_cens100 <- rep(0, nit)
tx_censadm100 <- rep(0, nit)

err_oslrt150 <- rep(0, nit)
err_moslrt150 <- rep(0, nit)
err_rc150 <- rep(0, nit)
err_de150 <- rep(0, nit)
err_ee150 <- rep(0, nit)
err_me150 <- rep(0, nit)
err_rmst_150 <- rep(0, nit)
err_max_hoch_150 <- rep(0, nit)
err_max_exact_150 <- rep(0, nit)
obs150 <- rep(0, nit)
obs150_adm <- rep(0, nit)
tx_cens150 <- rep(0, nit)
tx_censadm150 <- rep(0, nit)

err_oslrt200 <- rep(0, nit)
err_moslrt200 <- rep(0, nit)
err_rc200 <- rep(0, nit)
err_de200 <- rep(0, nit)
err_ee200 <- rep(0, nit)
err_me200 <- rep(0, nit)
err_rmst_200 <- rep(0, nit)
err_max_hoch_200 <- rep(0, nit)
err_max_exact_200 <- rep(0, nit)
obs200 <- rep(0, nit)
obs200_adm <- rep(0, nit)
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
  obs20[i] <- sum(del20)
  obs30[i] <- sum(del30)
  obs50[i] <- sum(del50)
  obs60[i] <- sum(del60)
  obs80[i] <- sum(del80)
  obs100[i] <- sum(del100)
  obs150[i] <- sum(del150)
  obs200[i] <- sum(del200)
  
  obs20_adm[i] <- sum(delta20)
  obs30_adm[i] <- sum(delta30)
  obs50_adm[i] <- sum(delta50)
  obs60_adm[i] <- sum(delta60)
  obs80_adm[i] <- sum(delta80)
  obs100_adm[i] <- sum(delta100)
  obs150_adm[i] <- sum(delta150)
  obs200_adm[i] <- sum(delta200)
  
  data20 <- ten(S20)
  a20 <- OSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a202 <- mOSLRT(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b20 <- Score_RC(data = data20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c20 <- Score_DE(data = data20, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d20 <- Score_EE(data = data20, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e20 <- Score_ME(data = data20, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau20 <- max(t20)
  rmst_exp20 <- rmst_single(time = t20, status = delta20, tau = tau20)
  a <- test_SA(rmst_exp = rmst_exp20[1], se_rmst_exp = rmst_exp20[2], tau = tau20, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g20_hoch <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g20_exact <- maxcombo1(data = data20, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data30 <- ten(S30)
  a30 <- OSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a302 <- mOSLRT(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b30 <- Score_RC(data = data30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c30 <- Score_DE(data = data30, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d30 <- Score_EE(data = data30, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e30 <- Score_ME(data = data30, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau30 <- max(t30)
  rmst_exp30 <- rmst_single(time = t30, status = delta30, tau = tau30)
  b <- test_SA(rmst_exp = rmst_exp30[1], se_rmst_exp = rmst_exp30[2], tau = tau30, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g30_hoch <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g30_exact <- maxcombo1(data = data30, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data50 <- ten(S50)
  a50 <- OSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a502 <- mOSLRT(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b50 <- Score_RC(data = data50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c50 <- Score_DE(data = data50, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d50 <- Score_EE(data = data50, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e50 <- Score_ME(data = data50, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau50 <- max(t50)
  rmst_exp50 <- rmst_single(time = t50, status = delta50, tau = tau50)
  c <- test_SA(rmst_exp = rmst_exp50[1], se_rmst_exp = rmst_exp50[2], tau = tau50, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g50_hoch <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g50_exact <- maxcombo1(data = data50, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data60 <- ten(S60)
  a60 <- OSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a602 <- mOSLRT(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b60 <- Score_RC(data = data60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c60 <- Score_DE(data = data60, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d60 <- Score_EE(data = data60, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e60 <- Score_ME(data = data60, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau60 <- max(t60)
  rmst_exp60 <- rmst_single(time = t60, status = delta60, tau = tau60)
  d <- test_SA(rmst_exp = rmst_exp60[1], se_rmst_exp = rmst_exp60[2], tau = tau60, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g60_hoch <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g60_exact <- maxcombo1(data = data60, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data80 <- ten(S80)
  a80 <- OSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a802 <- mOSLRT(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b80 <- Score_RC(data = data80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c80 <- Score_DE(data = data80, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d80 <- Score_EE(data = data80, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e80 <- Score_ME(data = data80, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau80 <- max(t80)
  rmst_exp80 <- rmst_single(time = t80, status = delta80, tau = tau80)
  e <- test_SA(rmst_exp = rmst_exp80[1], se_rmst_exp = rmst_exp80[2], tau = tau80, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g80_hoch <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g80_exact <- maxcombo1(data = data80, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data100 <- ten(S100)
  a100 <- OSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1002 <- mOSLRT(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b100 <- Score_RC(data = data100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c100 <- Score_DE(data = data100, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d100 <- Score_EE(data = data100, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e100 <- Score_ME(data = data100, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau100 <- max(t100)
  rmst_exp100 <- rmst_single(time = t100, status = delta100, tau = tau100)
  f <- test_SA(rmst_exp = rmst_exp100[1], se_rmst_exp = rmst_exp100[2], tau = tau100, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g100_hoch <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g100_exact <- maxcombo1(data = data100, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data150 <- ten(S150)
  a150 <- OSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a1502 <- mOSLRT(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b150 <- Score_RC(data = data150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c150 <- Score_DE(data = data150, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d150 <- Score_EE(data = data150, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e150 <- Score_ME(data = data150, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau150 <- max(t150)
  rmst_exp150 <- rmst_single(time = t150, status = delta150, tau = tau150)
  g <- test_SA(rmst_exp = rmst_exp150[1], se_rmst_exp = rmst_exp150[2], tau = tau150, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g150_hoch <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g150_exact <- maxcombo1(data = data150, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
  data200 <- ten(S200)
  a200 <- OSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  a2002 <- mOSLRT(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  b200 <- Score_RC(data = data200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  c200 <- Score_DE(data = data200, CP = CP_DE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  d200 <- Score_EE(data = data200, CP = CP_EE, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  e200 <- Score_ME(data = data200, CP1 = CP_ME1, CP2 = CP_ME2, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  tau200 <- max(t200)
  rmst_exp200 <- rmst_single(time = t200, status = delta200, tau = tau200)
  h <- test_SA(rmst_exp = rmst_exp200[1], se_rmst_exp = rmst_exp200[2], tau = tau200, shape_weib = shape0, scale_weib = scale0, distr = distr0)
  g200_hoch <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[1]
  g200_exact <- maxcombo1(data = data200, CP1 = 1, CP2 = 3, CP3 = 3, CP4 = 5, shape_weib = shape0, scale_weib = scale0, distr = distr0)[2]
  
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
  
  err_oslrt20[i] <- ifelse((a20<0.05), 1, 0)
  err_moslrt20[i] <- ifelse((a202<0.05), 1, 0)
  err_rc20[i] <- ifelse((b20<0.05), 1, 0)
  err_de20[i] <- ifelse((c20<0.05), 1, 0)
  err_ee20[i] <- ifelse((d20<0.05), 1, 0)
  err_me20[i] <- ifelse((e20<0.05), 1, 0)
  err_rmst_20[i] <- ifelse((a<0.05), 1, 0)
  err_max_hoch_20[i] <- ifelse(g20_hoch<0.05, 1, 0)
  err_max_exact_20[i] <- ifelse(g20_exact<0.05, 1, 0)
  
  err_oslrt30[i] <- ifelse((a30<0.05), 1, 0)
  err_moslrt30[i] <- ifelse((a302<0.05), 1, 0)
  err_rc30[i] <- ifelse((b30<0.05), 1, 0)
  err_de30[i] <- ifelse((c30<0.05), 1, 0)
  err_ee30[i] <- ifelse((d30<0.05), 1, 0)
  err_me30[i] <- ifelse((e30<0.05), 1, 0)
  err_rmst_30[i] <- ifelse((b<0.05), 1, 0)
  err_max_hoch_30[i] <- ifelse(g30_hoch<0.05, 1, 0)
  err_max_exact_30[i] <- ifelse(g30_exact<0.05, 1, 0)
  
  err_oslrt50[i] <- ifelse((a50<0.05), 1, 0)
  err_moslrt50[i] <- ifelse((a502<0.05), 1, 0)
  err_rc50[i] <- ifelse((b50<0.05), 1, 0)
  err_de50[i] <- ifelse((c50<0.05), 1, 0)
  err_ee50[i] <- ifelse((d50<0.05), 1, 0)
  err_me50[i] <- ifelse((e50<0.05), 1, 0)
  err_rmst_50[i] <- ifelse((c<0.05), 1, 0)
  err_max_hoch_50[i] <- ifelse(g50_hoch<0.05, 1, 0)
  err_max_exact_50[i] <- ifelse(g50_exact<0.05, 1, 0)
  
  err_oslrt60[i] <- ifelse((a60<0.05), 1, 0)
  err_moslrt60[i] <- ifelse((a602<0.05), 1, 0)
  err_rc60[i] <- ifelse((b60<0.05), 1, 0)
  err_de60[i] <- ifelse((c60<0.05), 1, 0)
  err_ee60[i] <- ifelse((d60<0.05), 1, 0)
  err_me60[i] <- ifelse((e60<0.05), 1, 0)
  err_rmst_60[i] <- ifelse((d<0.05), 1, 0)
  err_max_hoch_60[i] <- ifelse(g60_hoch<0.05, 1, 0)
  err_max_exact_60[i] <- ifelse(g60_exact<0.05, 1, 0)
  
  err_oslrt80[i] <- ifelse((a80<0.05), 1, 0)
  err_moslrt80[i] <- ifelse((a802<0.05), 1, 0)
  err_rc80[i] <- ifelse((b80<0.05), 1, 0)
  err_de80[i] <- ifelse((c80<0.05), 1, 0)
  err_ee80[i] <- ifelse((d80<0.05), 1, 0)
  err_me80[i] <- ifelse((e80<0.05), 1, 0)
  err_rmst_80[i] <- ifelse((e<0.05), 1, 0)
  err_max_hoch_80[i] <- ifelse(g80_hoch<0.05, 1, 0)
  err_max_exact_80[i] <- ifelse(g80_exact<0.05, 1, 0)
  
  err_oslrt100[i] <- ifelse((a100<0.05), 1, 0)
  err_moslrt100[i] <- ifelse((a1002<0.05), 1, 0)
  err_rc100[i] <- ifelse((b100<0.05), 1, 0)
  err_de100[i] <- ifelse((c100<0.05), 1, 0)
  err_ee100[i] <- ifelse((d100<0.05), 1, 0)
  err_me100[i] <- ifelse((e100<0.05), 1, 0)
  err_rmst_100[i] <- ifelse((f<0.05), 1, 0)
  err_max_hoch_100[i] <- ifelse(g100_hoch<0.05, 1, 0)
  err_max_exact_100[i] <- ifelse(g100_exact<0.05, 1, 0)
  
  err_oslrt150[i] <- ifelse((a150<0.05), 1, 0)
  err_moslrt150[i] <- ifelse((a1502<0.05), 1, 0)
  err_rc150[i] <- ifelse((b150<0.05), 1, 0)
  err_de150[i] <- ifelse((c150<0.05), 1, 0)
  err_ee150[i] <- ifelse((d150<0.05), 1, 0)
  err_me150[i] <- ifelse((e150<0.05), 1, 0)
  err_rmst_150[i] <- ifelse((g<0.05), 1, 0)
  err_max_hoch_150[i] <- ifelse(g150_hoch<0.05, 1, 0)
  err_max_exact_150[i] <- ifelse(g150_exact<0.05, 1, 0)
  
  err_oslrt200[i] <- ifelse((a200<0.05), 1, 0)
  err_moslrt200[i] <- ifelse((a2002<0.05), 1, 0)
  err_rc200[i] <- ifelse((b200<0.05), 1, 0)
  err_de200[i] <- ifelse((c200<0.05), 1, 0)              
  err_ee200[i] <- ifelse((d200<0.05), 1, 0)
  err_me200[i] <- ifelse((e200<0.05), 1, 0)
  err_rmst_200[i] <- ifelse((h<0.05), 1, 0)
  err_max_hoch_200[i] <- ifelse(g200_hoch<0.05, 1, 0)
  err_max_exact_200[i] <- ifelse(g200_exact<0.05, 1, 0)
}

os20 <- sum(err_oslrt20)/nit
mos20 <- sum(err_moslrt20)/nit
rc20 <- sum(err_rc20)/nit
de20 <- sum(err_de20)/nit
ee20 <- sum(err_ee20)/nit
me20 <- sum(err_me20)/nit
r_20 <- sum(err_rmst_20)/nit
max_hoch20 <- sum(err_max_hoch_20)/nit
max_exact20 <- sum(err_max_exact_20)/nit
O20 <- mean(obs20)
O20_adm <- mean(obs20_adm)
mean(tx_cens20)
mean(tx_censadm20)

os30 <- sum(err_oslrt30)/nit
mos30 <- sum(err_moslrt30)/nit
rc30 <- sum(err_rc30)/nit
de30 <- sum(err_de30)/nit
ee30 <- sum(err_ee30)/nit
me30 <- sum(err_me30)/nit
r_30 <- sum(err_rmst_30)/nit
max_hoch30 <- sum(err_max_hoch_30)/nit
max_exact30 <- sum(err_max_exact_30)/nit
O30 <- mean(obs30)
O30_adm <- mean(obs30_adm)
mean(tx_cens30)
mean(tx_censadm30)

os50 <- sum(err_oslrt50)/nit
mos50 <- sum(err_moslrt50)/nit
rc50 <- sum(err_rc50)/nit
de50 <- sum(err_de50)/nit
ee50 <- sum(err_ee50)/nit
me50 <- sum(err_me50)/nit
r_50 <- sum(err_rmst_50)/nit
max_hoch50 <- sum(err_max_hoch_50)/nit
max_exact50 <- sum(err_max_exact_50)/nit
O50 <- mean(obs50)
O50_adm <- mean(obs50_adm)
mean(tx_cens50)
mean(tx_censadm50)

os60 <- sum(err_oslrt60)/nit
mos60 <- sum(err_moslrt60)/nit
rc60 <- sum(err_rc60)/nit
de60 <- sum(err_de60)/nit
ee60 <- sum(err_ee60)/nit
me60 <- sum(err_me60)/nit
r_60 <- sum(err_rmst_60)/nit
max_hoch60 <- sum(err_max_hoch_60)/nit
max_exact60 <- sum(err_max_exact_60)/nit
O60 <- mean(obs60)
O60_adm <- mean(obs60_adm)
mean(tx_cens60)
mean(tx_censadm60)

os80 <- sum(err_oslrt80)/nit
mos80 <- sum(err_moslrt80)/nit
rc80 <- sum(err_rc80)/nit
de80 <- sum(err_de80)/nit
ee80 <- sum(err_ee80)/nit
me80 <- sum(err_me80)/nit
r_80 <- sum(err_rmst_80)/nit
max_hoch80 <- sum(err_max_hoch_80)/nit
max_exact80 <- sum(err_max_exact_80)/nit
O80 <- mean(obs80)
O80_adm <- mean(obs80_adm)
mean(tx_cens80)
mean(tx_censadm80)

os100 <- sum(err_oslrt100)/nit
mos100 <- sum(err_moslrt100)/nit
rc100 <- sum(err_rc100)/nit
de100 <- sum(err_de100)/nit
ee100 <- sum(err_ee100)/nit
me100 <- sum(err_me100)/nit
r_100 <- sum(err_rmst_100)/nit
max_hoch100 <- sum(err_max_hoch_100)/nit
max_exact100 <- sum(err_max_exact_100)/nit
O100 <- mean(obs100)
O100_adm <- mean(obs100_adm)
mean(tx_cens100)
mean(tx_censadm100)

os150 <- sum(err_oslrt150)/nit
mos150 <- sum(err_moslrt150)/nit
rc150 <- sum(err_rc150)/nit
de150 <- sum(err_de150)/nit
ee150 <- sum(err_ee150)/nit
me150 <- sum(err_me150)/nit
r_150 <- sum(err_rmst_150)/nit
max_hoch150 <- sum(err_max_hoch_150)/nit
max_exact150 <- sum(err_max_exact_150)/nit
O150 <- mean(obs150)
O150_adm <- mean(obs150_adm)
mean(tx_cens150)
mean(tx_censadm150)

os200 <- sum(err_oslrt200)/nit
mos200 <- sum(err_moslrt200)/nit
rc200 <- sum(err_rc200)/nit
de200 <- sum(err_de200)/nit
ee200 <- sum(err_ee200)/nit
me200 <- sum(err_me200)/nit
r_200 <- sum(err_rmst_200)/nit
max_hoch200 <- sum(err_max_hoch_200)/nit
max_exact200 <- sum(err_max_exact_200)/nit
O200 <- mean(obs200)
O200_adm <- mean(obs200_adm)
mean(tx_cens200)
mean(tx_censadm200)

n <- c(20, 30, 50, 60, 80, 100, 150, 200)
osl <- c(os20, os30, os50, os60, os80, os100, os150, os200)
mosl <- c(mos20, mos30, mos50, mos60, mos80, mos100, mos150, mos200)
rc <- c(rc20, rc30, rc50, rc60, rc80, rc100, rc150, rc200)
de <- c(de20, de30, de50, de60, de80, de100, de150, de200)
ee <- c(ee20, ee30, ee50, ee60, ee80, ee100, ee150, ee200)
me <- c(me20, me30, me50, me60, me80, me100, me150, me200)
rmst <- c(r_20, r_30, r_50, r_60, r_80, r_100, r_150, r_200)
hoch <- c(max_hoch20, max_hoch30, max_hoch50, max_hoch60, max_hoch80, max_hoch100, max_hoch150, max_hoch200)
exact <- c(max_exact20, max_exact30, max_exact50, max_exact60, max_exact80, max_exact100, max_exact150, max_exact200)
Obs <- c(round(O20), round(O30), round(O50), round(O60), round(O80), round(O100), round(O150), round(O200))
Obs_adm <- c(round(O20_adm), round(O30_adm), round(O50_adm), round(O60_adm), round(O80_adm), round(O100_adm), round(O150_adm), round(O200_adm))
osl
mosl
ee
me
de
rc
rmst
hoch
exact
Obs
Obs_adm

d1 <- data.frame(Sample.size = n, Obs_adm = Obs_adm, Error = c(osl, mosl, ee, me, de, rc, rmst, hoch, exact), 
                 Test = c(rep('OSLRT', 8), rep('Modified OSLRT', 8), rep('Early effect', 8), rep('Middle effect', 8),
                          rep('Delayed effect', 8), rep('Crossing hazards', 8), rep('RMST', 8),
                          rep('max-Combo (Hochberg correction)', 8), rep('max-Combo (exact p-value)', 8)))
d1$Test <- as.factor(d1$Test)
d1$Test <- factor(d1$Test, levels = c('OSLRT', 'Modified OSLRT', 'RMST', 'Early effect', 'Middle effect', 
                                      'Delayed effect', 'Crossing hazards', 'max-Combo (Hochberg correction)',
                                      'max-Combo (exact p-value)'))

ggplot(d1, aes(x = Sample.size, y = Error, group = Test))+
  geom_line(aes(color = Test), size = 1)+
  geom_point(aes(color = Test, shape = Test), size = 2)+
  scale_x_continuous(name = 'Sample size', sec.axis = sec_axis(trans = ~., name = 'Number of events', breaks = n, labels = Obs_adm))+
  scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18))+
  scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue', 'orange', 'green3', 'magenta', 'brown', 'mediumturquoise'))+
  labs(x = 'Sample size of the experimental group', 
       y = 'Power',
       title = '',
       subtitle = '')+
  geom_hline(yintercept = 0.8, size = 0.2)+   ##nominal level for non null effect 80%
  #geom_hline(yintercept = 0.05, size = 0.2)+  ##nominal level for null effect 5%
  ylim(0,1)+
  theme(axis.text.x.top = element_text(size = 8),
        legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15))













