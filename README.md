# oslrt_non_ph

## Article: One-sample survival tests in the presence of non-proportional hazards in oncology clinical trials
Authors: Chloé Szurewsky, Guosheng Yin, Gwénaël Le Teuff  
Pre-print avaible [here] (https://doi.org/10.48550/arXiv.2506.18608)  

functions.R  

This file contains the functions required to run the simulations and should be sourced to run every simulation study in files *code.R*:  
- *OSLRT* and *mOSLRT* are the classical one-sample log-rank test (Finkelstein et al, 2003) and its modified version (Wu, 2014) ;  
- *Score_EE*, *Score_ME*, *Score_DE* and *Score_CH* are the developed score tests for, respectively, an early, a middle, a delayed effects and crossing hazards ;  
- *rmst_single* returns the estimated value of the RMST and *test_SA* the test based on the RMST for single-arm trials;  
- *maxcombo1* is the combination test presented in the article that combines the mOSLRT, early effect score test and delayed effect score tests.
- *maxcombo_remDE1*, *maxcombo_remDE2* and *maxcombo_remDE* are alternative combination tests that remove one or two delayed effect score tests from the original combination test *maxcombo1*
- *maxcombo_remEE1*, *maxcombo_remEE2* and *maxcombo_remEE* are alternative combination tests that remove one or two early effect score tests from the original combination test *maxcombo1*
- *maxcombo_addME*, *maxcombo_addCH* and *maxcombo_addMECH* are alternative combination tests that add the middle effect, the crossing hazards or both score tests, respectively, to the original combination test *maxcombo1*
- *maxcombo_addEE*, *maxcombo_addDE* and *maxcombo_addEEDE* are alternative combination tests that add a third early effect, a third delayed effect or both score tests, respectively, to the original combination test *maxcombo1*
- *generate_initial* generates the simulated data for the experimental group (time of event and event indicator) for different scenarios
- *evaluate_initial* computes the tests for a given scenario to evaluate the performance
- *evaluate_misspe_cps* computes the early and delayed effects score tests to evaluate the performance when their change-points are misspecified
- *evaluate_remDE* evaluates the performance of the combination tests when one or two delayed effect score tests are removed from the original combination test
- *evaluate_remEE* evaluates the performance of the combination tests when one or two early effect score tests are removed from the original combination test
- *evaluate_addMECH* evaluates the performance of the combination tests when the middle effect and/or crossing hazards score tests are added to the original combination test
- *evaluate_addEEDE* evaluates the performance of the combination tests when a third early effect and/or delayed effect score tests are added to the original combination test
- *run_simulations*, *run_remEE*, *run_addMECH*, *run_addEEDE*, *run_misspe_CP*, *run_misspe_param* run the simulation to evaluate the performance of the tests for different scenarios and sample sizes for the experimental group for a given censoring rate of the experimental group
- *calculate_means* and *calculate_means_simp* transform the performance of each iteration into type I error or power by calculating the mean for each sample size and each test
- *run_simulations_complete*, *run_misspe_cps_complete* and *run_misspe_complete* run the simulation to evaluate the performance of the tests for different scenarios, different sample sizes and different censoring rates of the experimental group

figures_functions.R

This file contains the functions required to plot the figures and should be sourced to plot every figure in files *figures.R*:
- *plot_scenario* plots the survival functions of an external control group and an experimental group for a given scenario
- *plot_hazard* plots the hazard functions of an external control group and an experimental group for a given scenario
- *make_plot_data*, *make_plot_missp_data*, *make_plot_data_combo* and *make_plot_data_combo2* reorganise the obtained data set to plot the figures
- *plot_power* and *plot_power_misspe_cps* plot the figure with the sample sizes of the experimental group on the x-axis and the type I error or power in y-axis for a given scenario
- *make_diff_data* and *make_diff_sv_data* calculate the relative difference between two datasets
- *plot_diff_unique* and *plot_diff_sv_unique* plot the figure with the samples sizes of the experimental group in x-axis and the relative difference in y-axis for a given scenario
- *plot_power_appendix* plots the figure with the sample sizes of the experimental group on the x-axis and the type I error or power on the y-axis
- *plot_diff_appendix* plots the figure with the sample sizes of the experimental group on the x-axis and the relative difference on the y-axis

simulations_appendix

This folder contains two subfolders:
- initial that contains two files: *code.R* that generates the data for Figures 12 to 19 (in the appendix), it should be run first and *figures.R* to plot Figures 10 to 19.
- variability that contains two files: *code.R* that generates the data for Figures 20 to 30 (in the appendix), it should be run first and *figures.R* to plot the figures.

simulations_initial

This folder contains two files:
- *code.R* that generates the data for Figures 2 and 3 (Section 5), it should be run first
- *figures.R* to plot Figures 1 to 3 (Section 5)

simulations_variability

This folder contains two files:
- *code.R* that generates the data for Figures 4 to 6 (Section 6), it should be run first
- *figures.R* to plot Figures 14 to 6(Section 6)

For questions or remarks about the code, please contact C.Szurewsky (chloe.szurewsky.pro@gmail.com).
