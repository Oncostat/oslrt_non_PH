#######Figures of Section 5#######

source("../../figures_functions.R")


##Figure 4 - relative difference for misspecification of the expo parameter##

load("./param_misspe/NE.RData")
data_misspe_NE <- res_15
load("../initial/intermediate_results/initial/NE.RData")
data_NE <- res_15
diff_NE <- make_diff_data(mean_res_init = data_NE, mean_res_misspe = data_misspe_NE)
NE <- plot_diff_unique(data = diff_NE,
                       title = 'Scenario 1: null effect')

load("./param_misspe/PH.RData")
data_misspe_PH <- res_15
load("../initial/intermediate_results/initial/PH.RData")
data_PH <- res_15
diff_PH <- make_diff_data(mean_res_init = data_PH, mean_res_misspe = data_misspe_PH)
PH <- plot_diff_unique(data = diff_PH,
                       title = 'Scenario 2: proportional hazards')

load("./param_misspe/EE.RData")
data_misspe_EE <- res_15
load("../initial/intermediate_results/initial/EE.RData")
data_EE <- res_15
diff_EE <- make_diff_data(mean_res_init = data_EE, mean_res_misspe = data_misspe_EE)
EE <- plot_diff_unique(data = diff_EE,
                       title = 'Scenario 3: early effect')

load("./param_misspe/ME.RData")
data_misspe_ME <- res_15
load("../initial/intermediate_results/initial/ME.RData")
data_ME <- res_15
diff_ME <- make_diff_data(mean_res_init = data_ME, mean_res_misspe = data_misspe_ME)
ME <- plot_diff_unique(data = diff_ME,
                       title = 'Scenario 4: middle effect')

load("./param_misspe/DE.RData")
data_misspe_DE <- res_15
load("../initial/intermediate_results/initial/DE.RData")
data_DE <- res_15
diff_DE <- make_diff_data(mean_res_init = data_DE, mean_res_misspe = data_misspe_DE)
DE <- plot_diff_unique(data = diff_DE,
                       title = 'Scenario 5: delayed effect')

load("./param_misspe/CH.RData")
data_misspe_CH <- res_15
load("../initial/intermediate_results/initial/CH.RData")
data_CH <- res_15
diff_CH <- make_diff_data(mean_res_init = data_CH, mean_res_misspe = data_misspe_CH)
CH <- plot_diff_unique(data = diff_CH,
                       title = 'Scenario 6: crossing hazards')


##Figure 5 - relative difference for sampling variability##

load("./sampling_variability/NE.RData")
data_misspe_NE <- res_15
load("../initial/intermediate_results/initial/NE.RData")
data_NE <- res_15
diff_NE <- make_diff_sv_data(mean_res_init = data_NE, mean_res_misspe = data_misspe_NE)
NE <- plot_diff_sv_unique(data = diff_NE,
                       title = 'Scenario 1: null effect')

load("./sampling_variability/PH.RData")
data_misspe_PH <- res_15
load("../initial/intermediate_results/initial/PH.RData")
data_PH <- res_15
diff_PH <- make_diff_sv_data(mean_res_init = data_PH, mean_res_misspe = data_misspe_PH)
PH <- plot_diff_sv_unique(data = diff_PH,
                       title = 'Scenario 2: proportional hazards')

load("./sampling_variability/EE.RData")
data_misspe_EE <- res_15
load("../initial/intermediate_results/initial/EE.RData")
data_EE <- res_15
diff_EE <- make_diff_sv_data(mean_res_init = data_EE, mean_res_misspe = data_misspe_EE)
EE <- plot_diff_sv_unique(data = diff_EE,
                       title = 'Scenario 3: early effect')

load("./sampling_variability/ME.RData")
data_misspe_ME <- res_15
load("../initial/intermediate_results/initial/ME.RData")
data_ME <- res_15
diff_ME <- make_diff_sv_data(mean_res_init = data_ME, mean_res_misspe = data_misspe_ME)
ME <- plot_diff_sv_unique(data = diff_ME,
                       title = 'Scenario 4: middle effect')

load("./sampling_variability/DE.RData")
data_misspe_DE <- res_15
load("../initial/intermediate_results/initial/DE.RData")
data_DE <- res_15
diff_DE <- make_diff_sv_data(mean_res_init = data_DE, mean_res_misspe = data_misspe_DE)
DE <- plot_diff_sv_unique(data = diff_DE,
                       title = 'Scenario 5: delayed effect')

load("./sampling_variability/CH.RData")
data_misspe_CH <- res_15
load("../initial/intermediate_results/initial/CH.RData")
data_CH <- res_15
diff_CH <- make_diff_sv_data(mean_res_init = data_CH, mean_res_misspe = data_misspe_CH)
CH <- plot_diff_sv_unique(data = diff_CH,
                       title = 'Scenario 6: crossing hazards')


##Figure 6 - relative difference for distribution misspecification##

load("./distribution_misspe/NE.RData")
data_misspe_NE <- res_15
load("../initial/intermediate_results/initial/NE.RData")
data_NE <- res_15
diff_NE <- make_diff_data(mean_res_init = data_NE, mean_res_misspe = data_misspe_NE)
NE <- plot_diff_unique(data = diff_NE,
                       title = 'Scenario 1: null effect')

load("./distribution_misspe/PH.RData")
data_misspe_PH <- res_15
load("../initial/intermediate_results/initial/PH.RData")
data_PH <- res_15
diff_PH <- make_diff_data(mean_res_init = data_PH, mean_res_misspe = data_misspe_PH)
PH <- plot_diff_unique(data = diff_PH,
                       title = 'Scenario 2: proportional hazards')

load("./distribution_misspe/EE.RData")
data_misspe_EE <- res_15
load("../initial/intermediate_results/initial/EE.RData")
data_EE <- res_15
diff_EE <- make_diff_sv_data(mean_res_init = data_EE, mean_res_misspe = data_misspe_EE)
EE <- plot_diff_unique(data = diff_EE,
                       title = 'Scenario 3: early effect')

load("./distribution_misspe/ME.RData")
data_misspe_ME <- res_15
load("../initial/intermediate_results/initial/ME.RData")
data_ME <- res_15
diff_ME <- make_diff_data(mean_res_init = data_ME, mean_res_misspe = data_misspe_ME)
ME <- plot_diff_unique(data = diff_ME,
                       title = 'Scenario 4: middle effect')

load("./distribution_misspe/DE.RData")
data_misspe_DE <- res_15
load("../initial/intermediate_results/initial/DE.RData")
data_DE <- res_15
diff_DE <- make_diff_data(mean_res_init = data_DE, mean_res_misspe = data_misspe_DE)
DE <- plot_diff_unique(data = diff_DE,
                       title = 'Scenario 5: delayed effect')

load("./distribution_misspe/CH.RData")
data_misspe_CH <- res_15
load("../initial/intermediate_results/initial/CH.RData")
data_CH <- res_15
diff_CH <- make_diff_data(mean_res_init = data_CH, mean_res_misspe = data_misspe_CH)
CH <- plot_diff_unique(data = diff_CH,
                       title = 'Scenario 6: crossing hazards')
