#######Appendix of Section 6#######

source("../../figures_functions.R")

##Figure 20 - distribution of median survival##

set.seed(5)
nit <- 10000
m0_miss <- rgamma(n = nit, shape = 80, rate = 40) 
hist(m0_miss, xlim = c(1, 3.5), ylim = c(0, 2000), breaks = 20, xlab = 'Value of the median', main = '')


##Figure 21 - distribution of exponential parameter##

set.seed(5)
nit <- 10000
shape0 <- 1
m0_miss <- rgamma(n = nit, shape = 80, rate = 40)
scale0_miss <- m0_miss/(-log(0.5))^(1/shape0)
hist(1/scale0_miss, xlim = c(0.15, 0.6), ylim = c(0, 2000), breaks = 20, xlab = 'Value of the exponential parameter', main = '')


##Figure 22 - scenarios with exponential parameter misspecification##

set.seed(5)
np <- 500
shape0 <- 1
m0_miss <- rgamma(n = np, shape = 80, rate = 40)
scale0_miss <- m0_miss/(-log(0.5))^(1/shape0)

shape_cont <- 1
scale_cont <- 2/(-log(0.5))^(1/shape_cont)
t_cont <- nphsim::rpwexp(n = 5000, rate = 1/scale_cont, cumulative = FALSE)
delta_cont <- ifelse(t_cont < (2555/365), 1, 0)
t_cont <- ifelse(t_cont > (2555/365), (2555/365), t_cont)
S_cont <- survfit(Surv(t_cont, delta_cont)~1)

tNE <- nphsim::rpwexp(n = 5000, rate = 1/scale_cont, cumulative = FALSE)
deltaNE <- ifelse(tNE < (2555/365), 1, 0)
tNE <- ifelse(tNE > (2555/365), (2555/365), tNE)
S_NE <- survfit(Surv(tNE, deltaNE)~1)

tPH <- nphsim::rpwexp(n = 5000, rate = 1/scale_cont*0.5, cumulative = FALSE)
deltaPH <- ifelse(tPH < (2555/365), 1, 0)
tPH <- ifelse(tPH > (2555/365), (2555/365), tPH)
S_PH <- survfit(Surv(tPH, deltaPH)~1)

tEE <- nphsim::rpwexp(n = 5000, c(1/scale_cont*0.5, 1/scale_cont),
                      intervals = 1, cumulative = FALSE)
deltaEE <- ifelse(tEE < (2555/365), 1, 0)
tEE <- ifelse(tEE > (2555/365), (2555/365), tEE)
S_EE <- survfit(Surv(tEE, deltaEE)~1)

tME <- nphsim::rpwexp(n = 5000, c(1/scale_cont, 1/scale_cont*0.5, 1/scale_cont),
                      intervals = c(1, 3), cumulative = FALSE)
deltaME <- ifelse(tME < (2555/365), 1, 0)
tME <- ifelse(tME > (2555/365), (2555/365), tME)
S_ME <- survfit(Surv(tME, deltaME)~1)

tDE <- nphsim::rpwexp(n = 5000, c(1/scale_cont, 1/scale_cont*0.5),
                      intervals = 3, cumulative = FALSE)
deltaDE <- ifelse(tDE < (2555/365), 1, 0)
tDE <- ifelse(tDE > (2555/365), (2555/365), tDE)
S_DE <- survfit(Surv(tDE, deltaDE)~1)

tCH <- nphsim::rpwexp(n = 5000, c(1/scale_cont*2, 1/scale_cont*0.5),
                      intervals = 1, cumulative = FALSE)
deltaCH <- ifelse(tCH < (2555/365), 1, 0)
tCH <- ifelse(tCH > (2555/365), (2555/365), tCH)
S_CH <- survfit(Surv(tCH, deltaCH)~1)

par(mfrow = c(2, 3))
plot(S_cont, conf.int = FALSE, col = 'black',
     xlab = 'Years', ylab = 'Survival',
     main = 'Scenario 1: null effect')
for(i in 1:np){
  t <- nphsim::rpwexp(n = 5000, rate = 1/scale0_miss[i], cumulative = FALSE)
  delta <- ifelse(t < (2555/365), 1, 0)
  t <- ifelse(t > (2555/365), (2555/365), t)
  S <- survfit(Surv(t, delta)~1)
  lines(S, conf.int = FALSE, col = 'grey')
}
lines(S_cont, conf.int = FALSE, col = 'black')
lines(S_NE, conf.int = FALSE, col = 'blue')
legend("topright", 0.9,
       legend = c("Control", "Experimental"),
       col = c("black", "blue"),
       lty = 1)

plot(S_cont, conf.int = FALSE, col = 'black',
     xlab = 'Years', ylab = 'Survival',
     main = 'Scenario 2: proportional hazards')
for(i in 1:np){
  t <- nphsim::rpwexp(n = 5000, rate = 1/scale0_miss[i], cumulative = FALSE)
  delta <- ifelse(t < (2555/365), 1, 0)
  t <- ifelse(t > (2555/365), (2555/365), t)
  S <- survfit(Surv(t, delta)~1)
  lines(S, conf.int = FALSE, col = 'grey')
}
lines(S_cont, conf.int = FALSE, col = 'black')
lines(S_PH, conf.int = FALSE, col = 'blue')
legend("topright", 0.9,
       legend = c("Control", "Experimental"),
       col = c("black", "blue"),
       lty = 1)

plot(S_cont, conf.int = FALSE, col = 'black',
     xlab = 'Years', ylab = 'Survival',
     main = 'Scenario 3: early effect')
for(i in 1:np){
  t <- nphsim::rpwexp(n = 5000, rate = 1/scale0_miss[i], cumulative = FALSE)
  delta <- ifelse(t < (2555/365), 1, 0)
  t <- ifelse(t > (2555/365), (2555/365), t)
  S <- survfit(Surv(t, delta)~1)
  lines(S, conf.int = FALSE, col = 'grey')
}
lines(S_cont, conf.int = FALSE, col = 'black')
lines(S_EE, conf.int = FALSE, col = 'blue')
legend("topright", 0.9,
       legend = c("Control", "Experimental"),
       col = c("black", "blue"),
       lty = 1)

plot(S_cont, conf.int = FALSE, col = 'black',
     xlab = 'Years', ylab = 'Survival',
     main = 'Scenario 4: middle effect')
for(i in 1:np){
  t <- nphsim::rpwexp(n = 5000, rate = 1/scale0_miss[i], cumulative = FALSE)
  delta <- ifelse(t < (2555/365), 1, 0)
  t <- ifelse(t > (2555/365), (2555/365), t)
  S <- survfit(Surv(t, delta)~1)
  lines(S, conf.int = FALSE, col = 'grey')
}
lines(S_cont, conf.int = FALSE, col = 'black')
lines(S_ME, conf.int = FALSE, col = 'blue')
legend("topright", 0.9,
       legend = c("Control", "Experimental"),
       col = c("black", "blue"),
       lty = 1)

plot(S_cont, conf.int = FALSE, col = 'black',
     xlab = 'Years', ylab = 'Survival',
     main = 'Scenario 5: delayed effect')
for(i in 1:np){
  t <- nphsim::rpwexp(n = 5000, rate = 1/scale0_miss[i], cumulative = FALSE)
  delta <- ifelse(t < (2555/365), 1, 0)
  t <- ifelse(t > (2555/365), (2555/365), t)
  S <- survfit(Surv(t, delta)~1)
  lines(S, conf.int = FALSE, col = 'grey')
}
lines(S_cont, conf.int = FALSE, col = 'black')
lines(S_DE, conf.int = FALSE, col = 'blue')
legend("topright", 0.9,
       legend = c("Control", "Experimental"),
       col = c("black", "blue"),
       lty = 1)

plot(S_cont, conf.int = FALSE, col = 'black',
     xlab = 'Years', ylab = 'Survival',
     main = 'Scenario 6: crossing hazards')
for(i in 1:np){
  t <- nphsim::rpwexp(n = 5000, rate = 1/scale0_miss[i], cumulative = FALSE)
  delta <- ifelse(t < (2555/365), 1, 0)
  t <- ifelse(t > (2555/365), (2555/365), t)
  S <- survfit(Surv(t, delta)~1)
  lines(S, conf.int = FALSE, col = 'grey')
}
lines(S_cont, conf.int = FALSE, col = 'black')
lines(S_CH, conf.int = FALSE, col = 'blue')
legend("topright", 0.9,
       legend = c("Control", "Experimental"),
       col = c("black", "blue"),
       lty = 1)

##Figure 23 - all results misspecification exponential parameter##

load("./misspe_param/NE_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
NE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/PH_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
PH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
PH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
PH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
PH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
PH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/EE_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
EE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
EE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
EE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
EE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
EE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/ME_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
ME0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
ME5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
ME15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
ME25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
ME35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/DE_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
DE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
DE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
DE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
DE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
DE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/CH_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
CH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
CH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
CH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
CH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
CH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 24 - all results relative difference for misspecification exponential parameter##

load("./misspe_param/NE_complete.RData")
data_misspe_NE <- res
load("../initial/intermediate_results/initial/NE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_NE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_NE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_NE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_NE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_NE[["35%"]])
NE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = 0,
                          ylim_max = 450)
NE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = 0,
                          ylim_max = 450)
NE15 <- plot_diff_appendix(data = data15,
                          subtitle = "15% of censoring",
                           ylim_min = 0,
                           ylim_max = 450)
NE25 <- plot_diff_appendix(data = data25,
                          subtitle = "25% of censoring",
                           ylim_min = 0,
                           ylim_max = 450)
NE35 <- plot_diff_appendix(data = data35,
                          subtitle = "35% of censoring",
                           ylim_min = 0,
                           ylim_max = 450)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/PH_complete.RData")
data_misspe_PH <- res
load("../initial/intermediate_results/initial/PH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_PH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_PH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_PH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_PH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_PH[["35%"]])
PH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -12,
                          ylim_max = 32)
PH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -12,
                          ylim_max = 32)
PH15 <- plot_diff_appendix(data = data15,
                          subtitle = "15% of censoring",
                          ylim_min = -12,
                          ylim_max = 32)
PH25 <- plot_diff_appendix(data = data25,
                          subtitle = "25% of censoring",
                          ylim_min = -12,
                          ylim_max = 32)
PH35 <- plot_diff_appendix(data = data35,
                          subtitle = "35% of censoring",
                          ylim_min = -12,
                          ylim_max = 32)
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/EE_complete.RData")
data_misspe_EE <- res
load("../initial/intermediate_results/initial/EE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_EE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_EE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_EE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_EE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_EE[["35%"]])
EE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -20,
                          ylim_max = 401)
EE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -20,
                          ylim_max = 401)
EE15 <- plot_diff_appendix(data = data15,
                          subtitle = "15% of censoring",
                          ylim_min = -20,
                          ylim_max = 401)
EE25 <- plot_diff_appendix(data = data25,
                          subtitle = "25% of censoring",
                          ylim_min = -20,
                          ylim_max = 401)
EE35 <- plot_diff_appendix(data = data35,
                          subtitle = "35% of censoring",
                          ylim_min = -20,
                          ylim_max = 401)
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/ME_complete.RData")
data_misspe_ME <- res
load("../initial/intermediate_results/initial/ME_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_ME[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_ME[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_ME[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_ME[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_ME[["35%"]])
ME0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -12,
                          ylim_max = 20)
ME5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -12,
                          ylim_max = 20)
ME15 <- plot_diff_appendix(data = data15,
                          subtitle = "15% of censoring",
                          ylim_min = -12,
                          ylim_max = 20)
ME25 <- plot_diff_appendix(data = data25,
                          subtitle = "25% of censoring",
                          ylim_min = -12,
                          ylim_max = 20)
ME35 <- plot_diff_appendix(data = data35,
                          subtitle = "35% of censoring",
                          ylim_min = -12,
                          ylim_max = 20)
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/DE_complete.RData")
data_misspe_DE <- res
load("../initial/intermediate_results/initial/DE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_DE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_DE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_DE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_DE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_DE[["35%"]])
DE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -15,
                          ylim_max = 280)
DE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -15,
                          ylim_max = 280)
DE15 <- plot_diff_appendix(data = data15,
                          subtitle = "15% of censoring",
                          ylim_min = -15,
                          ylim_max = 280)
DE25 <- plot_diff_appendix(data = data25,
                          subtitle = "25% of censoring",
                          ylim_min = -15,
                          ylim_max = 280)
DE35 <- plot_diff_appendix(data = data35,
                          subtitle = "35% of censoring",
                          ylim_min = -15,
                          ylim_max = 280)
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_param/CH_complete.RData")
data_misspe_CH <- res
load("../initial/intermediate_results/initial/CH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_CH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_CH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_CH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_CH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_CH[["35%"]])
CH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -70,
                          ylim_max = 560)
CH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -70,
                          ylim_max = 560)
CH15 <- plot_diff_appendix(data = data15,
                          subtitle = "15% of censoring",
                          ylim_min = -70,
                          ylim_max = 560)
CH25 <- plot_diff_appendix(data = data25,
                          subtitle = "25% of censoring",
                          ylim_min = -70,
                          ylim_max = 560)
CH35 <- plot_diff_appendix(data = data35,
                          subtitle = "35% of censoring",
                          ylim_min = -70,
                          ylim_max = 560)
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 25 - all results sampling variability pi = 0.6##

load("./sampling_variability/NE_complete_pi0.6.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
NE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/PH_complete_pi0.6.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
PH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
PH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
PH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
PH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
PH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/EE_complete_pi0.6.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
EE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
EE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
EE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
EE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
EE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/ME_complete_pi0.6.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
ME0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
ME5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
ME15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
ME25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
ME35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/DE_complete_pi0.6.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
DE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
DE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
DE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
DE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
DE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/CH_complete_pi0.6.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
CH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
CH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
CH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
CH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
CH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 26 - all results relative difference for sampling variability pi = 0.6##

load("./sampling_variability/NE_complete_pi0.6.RData")
data_misspe_NE <- res
load("../initial/intermediate_results/initial/NE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_NE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_NE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_NE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_NE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_NE[["35%"]])
NE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
NE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
NE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
NE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
NE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/PH_complete_pi0.6.RData")
data_misspe_PH <- res
load("../initial/intermediate_results/initial/PH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_PH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_PH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_PH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_PH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_PH[["35%"]])
PH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
PH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
PH15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
PH25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
PH35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/EE_complete_pi0.6.RData")
data_misspe_EE <- res
load("../initial/intermediate_results/initial/EE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_EE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_EE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_EE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_EE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_EE[["35%"]])
EE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
EE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
EE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
EE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
EE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/ME_complete_pi0.6.RData")
data_misspe_ME <- res
load("../initial/intermediate_results/initial/ME_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_ME[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_ME[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_ME[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_ME[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_ME[["35%"]])
ME0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
ME5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
ME15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
ME25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
ME35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/DE_complete_pi0.6.RData")
data_misspe_DE <- res
load("../initial/intermediate_results/initial/DE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_DE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_DE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_DE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_DE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_DE[["35%"]])
DE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
DE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
DE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
DE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
DE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/CH_complete_pi0.6.RData")
data_misspe_CH <- res
load("../initial/intermediate_results/initial/CH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_CH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_CH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_CH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_CH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_CH[["35%"]])
CH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
CH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
CH15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
CH25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
CH35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 27 - all results sampling variability pi = 1##

load("./sampling_variability/NE_complete_pi1.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
NE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/PH_complete_pi1.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
PH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
PH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
PH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
PH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
PH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/EE_complete_pi1.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
EE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
EE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
EE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
EE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
EE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/ME_complete_pi1.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
ME0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
ME5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
ME15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
ME25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
ME35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/DE_complete_pi1.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
DE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
DE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
DE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
DE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
DE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/CH_complete_pi1.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
CH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
CH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
CH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
CH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
CH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 28 - all results relative difference for sampling variability pi = 1##

load("./sampling_variability/NE_complete_pi1.RData")
data_misspe_NE <- res
load("../initial/intermediate_results/initial/NE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_NE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_NE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_NE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_NE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_NE[["35%"]])
NE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
NE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
NE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
NE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
NE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/PH_complete_pi1.RData")
data_misspe_PH <- res
load("../initial/intermediate_results/initial/PH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_PH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_PH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_PH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_PH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_PH[["35%"]])
PH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
PH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
PH15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
PH25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
PH35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/EE_complete_pi1.RData")
data_misspe_EE <- res
load("../initial/intermediate_results/initial/EE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_EE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_EE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_EE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_EE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_EE[["35%"]])
EE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
EE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
EE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
EE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
EE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/ME_complete_pi1.RData")
data_misspe_ME <- res
load("../initial/intermediate_results/initial/ME_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_ME[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_ME[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_ME[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_ME[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_ME[["35%"]])
ME0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
ME5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
ME15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
ME25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
ME35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/DE_complete_pi1.RData")
data_misspe_DE <- res
load("../initial/intermediate_results/initial/DE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_DE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_DE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_DE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_DE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_DE[["35%"]])
DE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
DE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
DE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
DE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
DE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./sampling_variability/CH_complete_pi1.RData")
data_misspe_CH <- res
load("../initial/intermediate_results/initial/CH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_CH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_CH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_CH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_CH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_CH[["35%"]])
CH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
CH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 0)
CH15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
CH25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
CH35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 0)
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 29 - all results misspecification distribution##

load("./misspe_distribution/NE_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
NE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring",
                           threshold = 0.05,
                           ylim_max = 0.1)
NE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring",
                            threshold = 0.05,
                            ylim_max = 0.1)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/PH_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
PH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
PH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
PH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
PH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
PH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/EE_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
EE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
EE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
EE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
EE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
EE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/ME_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
ME0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
ME5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
ME15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
ME25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
ME35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/DE_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
DE0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
DE5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
DE15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
DE25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
DE35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/CH_complete.RData")
data0 <- make_plot_data(mean_res = res[["0%"]])
data5 <- make_plot_data(mean_res = res[["5%"]])
data15 <- make_plot_data(mean_res = res[["15%"]])
data25 <- make_plot_data(mean_res = res[["25%"]])
data35 <- make_plot_data(mean_res = res[["35%"]])
CH0 <- plot_power_appendix(data = data0,
                           subtitle = "0% of censoring")
CH5 <- plot_power_appendix(data = data5,
                           subtitle = "5% of censoring")
CH15 <- plot_power_appendix(data = data15,
                            subtitle = "15% of censoring")
CH25 <- plot_power_appendix(data = data25,
                            subtitle = "25% of censoring")
CH35 <- plot_power_appendix(data = data35,
                            subtitle = "35% of censoring")
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")


##Figure 30 - all results relative difference for misspecification distribution##

load("./misspe_distribution/NE_complete.RData")
data_misspe_NE <- res
load("../initial/intermediate_results/initial/NE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_NE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_NE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_NE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_NE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_NE[["35%"]])
NE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 2500)
NE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -10,
                          ylim_max = 2500)
NE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -10,
                           ylim_max = 2500)
NE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 2500)
NE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -10,
                           ylim_max = 2500)
NE <- NE0 + NE5 + NE15 + NE25 + NE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/PH_complete.RData")
data_misspe_PH <- res
load("../initial/intermediate_results/initial/PH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_PH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_PH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_PH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_PH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_PH[["35%"]])
PH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -10,
                          ylim_max = 250)
PH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -10,
                          ylim_max = 250)
PH15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -10,
                           ylim_max = 250)
PH25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -10,
                           ylim_max = 250)
PH35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -10,
                           ylim_max = 250)
PH <- PH0 + PH5 + PH15 + PH25 + PH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/EE_complete.RData")
data_misspe_EE <- res
load("../initial/intermediate_results/initial/EE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_EE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_EE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_EE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_EE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_EE[["35%"]])
EE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 600)
EE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 600)
EE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 600)
EE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 600)
EE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 600)
EE <- EE0 + EE5 + EE15 + EE25 + EE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/ME_complete.RData")
data_misspe_ME <- res
load("../initial/intermediate_results/initial/ME_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_ME[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_ME[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_ME[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_ME[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_ME[["35%"]])
ME0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -70,
                          ylim_max = 370)
ME5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -70,
                          ylim_max = 370)
ME15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -70,
                           ylim_max = 370)
ME25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -70,
                           ylim_max = 370)
ME35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -70,
                           ylim_max = 370)
ME <- ME0 + ME5 + ME15 + ME25 + ME35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/DE_complete.RData")
data_misspe_DE <- res
load("../initial/intermediate_results/initial/DE_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_DE[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_DE[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_DE[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_DE[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_DE[["35%"]])
DE0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -60,
                          ylim_max = 610)
DE5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -60,
                          ylim_max = 610)
DE15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -60,
                           ylim_max = 610)
DE25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -60,
                           ylim_max = 610)
DE35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -60,
                           ylim_max = 610)
DE <- DE0 + DE5 + DE15 + DE25 + DE35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

load("./misspe_distribution/CH_complete.RData")
data_misspe_CH <- res
load("../initial/intermediate_results/initial/CH_0.5_complete")
data0 <- make_diff_data(mean_res_init = res[["0%"]], mean_res_misspe = data_misspe_CH[["0%"]])
data5 <- make_diff_data(mean_res_init = res[["5%"]], mean_res_misspe = data_misspe_CH[["5%"]])
data15 <- make_diff_data(mean_res_init = res[["15%"]], mean_res_misspe = data_misspe_CH[["15%"]])
data25 <- make_diff_data(mean_res_init = res[["25%"]], mean_res_misspe = data_misspe_CH[["25%"]])
data35 <- make_diff_data(mean_res_init = res[["35%"]], mean_res_misspe = data_misspe_CH[["35%"]])
CH0 <- plot_diff_appendix(data = data0,
                          subtitle = "0% of censoring",
                          ylim_min = -100,
                          ylim_max = 110)
CH5 <- plot_diff_appendix(data = data5,
                          subtitle = "5% of censoring",
                          ylim_min = -100,
                          ylim_max = 110)
CH15 <- plot_diff_appendix(data = data15,
                           subtitle = "15% of censoring",
                           ylim_min = -100,
                           ylim_max = 110)
CH25 <- plot_diff_appendix(data = data25,
                           subtitle = "25% of censoring",
                           ylim_min = -100,
                           ylim_max = 110)
CH35 <- plot_diff_appendix(data = data35,
                           subtitle = "35% of censoring",
                           ylim_min = -100,
                           ylim_max = 110)
CH <- CH0 + CH5 + CH15 + CH25 + CH35 + plot_layout(guides = 'collect', ncol = 5) & theme(legend.position='bottom')

plot_grid(NE, PH, EE, ME, DE, CH, labels = c('Scenario 1: null effect', 'Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                             'Scenario 4: middle effect', 'Scenario 5: delayed effect', 'Scenario 6: crossing hazards'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")
