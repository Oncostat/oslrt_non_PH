#######Appendix of Section 5#######

source("../../figures_functions.R")

##Figure 10 - hazard functions##

set.seed(5)

shape_cont <- 1
scale_cont <- 2/(-log(0.5))^(1/shape_cont)

h_cont <- hexp(x = seq(0.01, 7, length.out = 5000), rate = 1/scale_cont)
h_logis <- hllogis(x = seq(0.002, 7, length.out = 5000), shape = 1.7, scale = 2)

NE <- plot_hazard(rate = 1/scale_cont,
                  scenario = 1,
                  effect = 'null effect',
                  h_cont = h_cont, 
                  h_logis = h_logis)
PH <- plot_hazard(rate = 1/scale_cont*0.5,
                  scenario = 2,
                  effect = 'proportional hazards',
                  h_cont = h_cont, 
                  h_logis = h_logis)
EE <- plot_hazard(rate = c(1/scale_cont*0.5, 1/scale_cont),
                  intervals = 1,
                  scenario = 3,
                  effect = 'early effect',
                  h_cont = h_cont, 
                  h_logis = h_logis)
ME <- plot_hazard(rate = c(1/scale_cont, 1/scale_cont*0.5, 1/scale_cont),
                  intervals = c(1, 3),
                  scenario = 4,
                  effect = 'middle effect',
                  h_cont = h_cont,
                  h_logis = h_logis)
DE <- plot_hazard(rate = c(1/scale_cont, 1/scale_cont*0.5),
                  intervals = 3,
                  scenario = 5,
                  effect = 'delayed effect',
                  h_cont = h_cont,
                  h_logis = h_logis)
CH <- plot.hazard(rate = c(1/scale_cont*2, 1/scale_cont*0.5),
                  intervals = 1,
                  scenario = 6,
                  effect = 'crossing hazards',
                  h_cont = h_cont, 
                  h_logis = h_logis)
ggarrange(NE, PH, EE, ME, DE, CH, ncol = 3, nrow = 2,
          common.legend = TRUE, legend = 'bottom')


##Figure 11 - cumulative hazard functions##

set.seed(5)

shape_cont <- 1
scale_cont <- 2/(-log(0.5))^(1/shape_cont)

t_cont <- nphsim::rpwexp(n = 5000, rate = 1/scale_cont, cumulative = FALSE)
delta_cont <- ifelse(t_cont < (2555/365), 1, 0)
t_cont <- ifelse(t_cont > (2555/365), (2555/365), t_cont)
t_logis <- rllogis(n = 5000, shape = 1.7, scale = 2)
delta_logis <- ifelse(t_logis < (2555/365), 1, 0)
t_logis <- ifelse(t_logis > (2555/365), (2555/365), t_logis)

d_cont <- data.frame(Time = c(t_cont, t_logis), Status = c(delta_cont, delta_logis),
                     Group = c(rep('Control group (exponential)', 5000), 
                               rep('Control group (log-logistic)', 5000)))

ggsurvplot(fit = survfit(Surv(Time, Status)~Group, data = d_cont), 
           fun = "cumhaz",
           data = d_cont, palette = c('black', 'black'), 
           linetype = c(1, 2), censor = FALSE,
           legend = "bottom", legend.title = '',
           xlab = "Time (years)",
           ggtheme = theme_bw() +
             theme(axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   legend.text = element_text(size = 18),
                   legend.title = element_text(size = 18)))


##Figure 12 - complete results HR = 0.5##

load("./intermediate_results/initial/NE_0.5_complete.RData")
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

load("./intermediate_results/initial/PH_0.5_complete.RData")
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

load("./intermediate_results/initial/EE_0.5_complete.RData")
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

load("./intermediate_results/initial/ME_0.5_complete.RData")
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

load("./intermediate_results/initial/DE_0.5_complete.RData")
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

load("./intermediate_results/initial/CH_0.5_complete.RData")
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


##Figure 13 - complete results HR = 0.7##

load("./intermediate_results/initial/PH_0.7_complete.RData")
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

load("./intermediate_results/initial/EE_0.7_complete.RData")
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

load("./intermediate_results/initial/ME_0.7_complete.RData")
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

load("./intermediate_results/initial/DE_0.7_complete.RData")
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

plot_grid(PH, EE, ME, DE, labels = c('Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                      'Scenario 4: middle effect', 'Scenario 5: delayed effect'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")



##Figure 14 - complete results HR = 0.8##

load("./intermediate_results/initial/PH_0.8_complete.RData")
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

load("./intermediate_results/initial/EE_0.8_complete.RData")
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

load("./intermediate_results/initial/ME_0.8_complete.RData")
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

load("./intermediate_results/initial/DE_0.8_complete.RData")
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

plot_grid(PH, EE, ME, DE, labels = c('Scenario 2: proportional hazards', 'Scenario 3: early effect', 
                                     'Scenario 4: middle effect', 'Scenario 5: delayed effect'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")



##Figure 15 - max-Combo removing Z_DE##

labels_graphs <- c("max-Combo without Z_DE(k1)", "max-Combo without Z_DE(k2)",
                   "max-Combo without Z_DE(k1) and Z_DE(k2)")
load("./intermediate_results/max_combo/NE_DE_remove.RData")
dataNE <- make_plot_data_combo(mean_res = res, labels_graph = labels_graphs)
NE <- plot_power(data = dataNE, 
                 title = 'Scenario 1: null effect',
                 threshold = 0.05,
                 ylim_max = 0.1)
load("./intermediate_results/max_combo/PH_DE_remove.RData")
dataPH <- make_plot_data_combo(mean_res = res, labels_graph = labels_graphs)
PH <- plot_power(data = dataPH, 
                 title = 'Scenario 2: proportional hazards')
load("./intermediate_results/max_combo/EE_DE_remove.RData")
dataEE <- make_plot_data_combo(mean_res = res, labels_graph = labels_graphs)
EE <- plot_power(data = dataEE, 
                 title = 'Scenario 3: early effect')

NE + PH + EE + plot_layout(guides = 'collect', ncol = 3) &
  theme(legend.position='bottom')


##Figure 16 - max-Combo removing Z_EE##

labels_graphs <- c("max-Combo without Z_EE(k1)", "max-Combo without Z_EE(k2)",
                   "max-Combo without Z_EE(k1) and Z_EE(k2)")
load("./intermediate_results/max_combo/NE_EE_remove.RData")
dataNE <- make_plot_data_combo(mean_res = res, labels_graph = labels_graphs)
NE <- plot_power(data = dataNE, 
                 title = 'Scenario 1: null effect',
                 threshold = 0.05,
                 ylim_max = 0.1)
load("./intermediate_results/max_combo/ME_EE_remove.RData")
dataME <- make_plot_data_combo(mean_res = res, labels_graph = labels_graphs)
ME <- plot_power(data = dataME, 
                 title = 'Scenario 4: middle effect')
load("./intermediate_results/max_combo/DE_EE_remove.RData")
dataDE <- make_plot_data_combo(mean_res = res, labels_graph = labels_graphs)
DE <- plot_power(data = dataDE, 
                 title = 'Scenario 5: delayed effect')

NE + ME + DE + plot_layout(guides = 'collect', ncol = 3) &
  theme(legend.position='bottom')


##Figure 17 - max-Combo adding Z_ME and Z_CH##

labels_graphs <- c("max-Combo with Z_ME(k1 = 1 and k2 = 4)", "max-Combo with Z_CH",
                   "max-Combo with Z_ME(k1 = 1 and k2 = 4) and Z_CH")
load("./intermediate_results/max_combo/NE_ME_CH_add.RData")
dataNE <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
NE <- plot_power(data = dataNE, 
                 title = 'Scenario 1: null effect',
                 threshold = 0.05,
                 ylim_max = 0.1)
load("./intermediate_results/max_combo/PH_ME_CH_add.RData")
dataPH <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
PH <- plot_power(data = dataPH, 
                 title = 'Scenario 2: proportional hazards')
load("./intermediate_results/max_combo/EE_ME_CH_add.RData")
dataEE <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
EE <- plot_power(data = dataEE, 
                 title = 'Scenario 3: early effect')
load("./intermediate_results/max_combo/ME_ME_CH_add.RData")
dataME <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
ME <- plot_power(data = dataME, 
                 title = 'Scenario 4: middle effect')
load("./intermediate_results/max_combo/DE_ME_CH_add.RData")
dataDE <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
DE <- plot_power(data = dataDE, 
                 title = 'Scenario 5: delayed effect')
load("./intermediate_results/max_combo/CH_ME_CH_add.RData")
dataCH <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
CH <- plot_power(data = dataCH, 
                 title = 'Scenario 6: crossing hazards')

NE + PH + EE + ME + DE + CH + plot_layout(guides = 'collect', ncol = 3) &
  theme(legend.position='bottom')


##Figure 18 - max-Combo adding Z_EE and Z_DE##

labels_graphs <- c("max-Combo with Z_EE(k3 = 2)", "max-Combo with Z_DE(k3 = 1)",
                   "max-Combo with  Z_EE(k3 = 2) and Z_DE(k3 = 1)")
load("./intermediate_results/max_combo/NE_EE_DE_add.RData")
dataNE <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
NE <- plot_power(data = dataNE, 
                 title = 'Scenario 1: null effect',
                 threshold = 0.05,
                 ylim_max = 0.1)
load("./intermediate_results/max_combo/PH_EE_DE_add.RData")
dataPH <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
PH <- plot_power(data = dataPH, 
                 title = 'Scenario 2: proportional hazards')
load("./intermediate_results/max_combo/EE_EE_DE_add.RData")
dataEE <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
EE <- plot_power(data = dataEE, 
                 title = 'Scenario 3: early effect')
load("./intermediate_results/max_combo/ME_EE_DE_add.RData")
dataME <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
ME <- plot_power(data = dataME, 
                 title = 'Scenario 4: middle effect')
load("./intermediate_results/max_combo/DE_EE_DE_add.RData")
dataDE <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
DE <- plot_power(data = dataDE, 
                 title = 'Scenario 5: delayed effect')
load("./intermediate_results/max_combo/CH_EE_DE_add.RData")
dataCH <- make_plot_data_combo2(mean_res = res, labels_graph = labels_graphs)
CH <- plot_power(data = dataCH, 
                 title = 'Scenario 6: crossing hazards')

NE + PH + EE + ME + DE + CH + plot_layout(guides = 'collect', ncol = 3) &
  theme(legend.position='bottom')


##Figure 19 - all results for CP misspecification##

load("./intermediate_results/misspe_cps/EE_complete.RData")
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

load("./intermediate_results/misspe_cps/DE_complete.RData")
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

plot_grid(EE, DE, labels = c('Scenario 3: early effect', 'Scenario 5: delayed effect'), 
          label_size = 25, label_x = 0, label_y = 1, hjust = 0, vjust = 1, ncol = 2, align = "hv")
