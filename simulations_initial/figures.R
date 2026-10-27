#######Figures of Section 5#######

source("../../figures_functions.R")

##Figure 1 - Survival curves##

set.seed(5)

shape_cont <- 1
scale_cont <- 2/(-log(0.5))^(1/shape_cont)

t_cont <- nphsim::rpwexp(n = 5000, rate = 1/scale_cont, cumulative = FALSE)
delta_cont <- ifelse(t_cont < (2555/365), 1, 0)
t_cont <- ifelse(t_cont > (2555/365), (2555/365), t_cont)
t_logis <- rllogis(n = 5000, shape = 1.7, scale = 2)
delta_logis <- ifelse(t_logis < (2555/365), 1, 0)
t_logis <- ifelse(t_logis > (2555/365), (2555/365), t_logis)

NE <- plot_scenario(rate = 1/scale_cont,
                    scenario = 1,
                    effect = 'null effect',
                    t_cont = t_cont,
                    delta_cont = delta_cont, 
                    t_logis = t_logis,
                    delta_logis = delta_logis)
PH <- plot_scenario(rate = 1/scale_cont*0.5,
                    scenario = 2,
                    effect = 'proportional hazards',
                    t_cont = t_cont,
                    delta_cont = delta_cont,
                    t_logis = t_logis,
                    delta_logis = delta_logis)
EE <- plot_scenario(rate = c(1/scale_cont*0.5, 1/scale_cont),
                    intervals = 1,
                    vlines = 1,
                    scenario = 3,
                    effect = 'early effect',
                    t_cont = t_cont,
                    delta_cont = delta_cont,
                    t_logis = t_logis,
                    delta_logis = delta_logis)
ME <- plot_scenario(rate = c(1/scale_cont, 1/scale_cont*0.5, 1/scale_cont),
                    intervals = c(1, 3),
                    vlines = c(1, 4),
                    scenario = 4,
                    effect = 'middle effect',
                    t_cont = t_cont,
                    delta_cont = delta_cont,
                    t_logis = t_logis,
                    delta_logis = delta_logis)
DE <- plot_scenario(rate = c(1/scale_cont, 1/scale_cont*0.5),
                    intervals = 3,
                    vlines = 3,
                    scenario = 5,
                    effect = 'delayed effect',
                    t_cont = t_cont,
                    delta_cont = delta_cont,
                    t_logis = t_logis,
                    delta_logis = delta_logis)
CH <- plot_scenario(rate = c(1/scale_cont*2, 1/scale_cont*0.5),
                   intervals = 1,
                   vlines = 1,
                   scenario = 6,
                   effect = 'crossing hazards',
                   t_cont = t_cont,
                   delta_cont = delta_cont,
                   t_logis = t_logis,
                   delta_logis = delta_logis)
ggarrange(NE$plot, PH$plot, EE$plot, ME$plot, DE$plot, CH$plot, 
          ncol = 3, nrow = 2,
          common.legend = TRUE, legend = 'bottom')


##Figure 2 - type I error and power##

load("./intermediate_results/initial/NE.RData")
data_NE <- make_plot_data(mean_res = res_15)
NE <- plot_power(data = data_NE,
                 title = 'Scenario 1: null effect', 
                 threshold = 0.05,
                 ylim = 0.1)
load("./intermediate_results/initial/PH.RData")
data_PH <- make_plot_data(mean_res = res_15)
PH <- plot_power(data = data_PH,
                 title = 'Scenario 2: proportional hazards')
load("./intermediate_results/initial/EE.RData")
data_EE <- make_plot_data(mean_res = res_15)
EE <- plot_power(data = data_EE,
                 title = 'Scenario 3: early effect')
load("./intermediate_results/initial/ME.RData")
data_ME <- make_plot_data(mean_res = res_15)
ME <- plot_power(data = data_ME,
                 title = 'Scenario 4: middle effect')
load("./intermediate_results/initial/DE.RData")
data_DE <- make_plot_data(mean_res = res_15)
DE <- plot_power(data = data_DE,
                 title = 'Scenario 5: delayed effect')
load("./intermediate_results/initial/CH.RData")
data_CH <- make_plot_data(mean_res = res_15)
CH <- plot_power(data = data_CH,
                 title = 'Scenario 6: crossing hazards')
NE + PH + EE + ME + DE + CH + plot_layout(guides = 'collect', ncol = 3) &
  theme(legend.position='bottom')


##Figure 3 - misspecification of CPs##

load("./intermediate_results/misspe_cps/EE.RData")
data_EE <- make_plot_missp_data(mean_res = res_15)
EE <- plot_power_misspe_cps(data = data_EE, 
                            title = 'Scenario 3: early effect',
                            color_lines = 'blue')
load("./intermediate_results/misspe_cps/DE.RData")
data_DE <- make_plot_missp_data(mean_res = res_15)
DE <- plot_power_misspe_cps(data = data_DE,
                            title = 'Scenario 5: delayed effect',
                            color_lines = 'green3')

EE + DE + plot_layout(guides = 'collect', ncol = 2) &
  theme(legend.position='bottom')
