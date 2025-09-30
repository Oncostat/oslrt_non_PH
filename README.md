# oslrt_non_ph

## Article: One-sample survival tests for non-proportional hazards in oncology clinical trial
Authors: Chloé Szurewsky, Guosheng Yin, Gwénaël Le Teuff  
Pre-print avaible [here] (https://doi.org/10.48550/arXiv.2506.18608)  

Tests_SA.R  

This file contains the functions required to run the simulations :  
- *OSLRT* and *mOSLRT* are the classical one-sample log-rank test (Finkelstein et al, 2003) and its modified version (Wu, 2014) ;  
- *Score_EE*, *Score_ME*, *Score_DE* and *Score_CH* are the developed score tests for, respectively, an early, a middle, a delayed effect and crossing hazards ;  
- *rmst_single* returns the estimated value of the RMST and *test_SA* the test based on the RMST for single-arm trials;  
- *maxcombo1* is the combination test presented in the article that combines the mOSLRT, early effect score test and delayed effect score tests.

Simulations.R

This file runs the simulation study and gives the type I error or power as shown in Figure 2, and in Figure 12 in appendix by changing the censoring rate, in Figure 13 and 14 in appendix with other hazard ratio. 

Simulations_parameter_misspe.R

This file runs the simulations to study the impact of the variability on the exponential parameter of the historical control group (section 6.1) and returns the relative difference in terms of type I error and power as shown in Figure 4 and Figure 20 in appendix with other censoring rates.

Simulations_dsitribution_misspe.R

This file runs the simulations to study the impact of the model misspecification of the survival distribution of the external control curve (section 6.3) and returns the relative difference in terms of type I error and power as shown in Figure 6 and Figure 30 in appendix with other censoring rates.

Simulations_sampling.R

This file runs the simulations to study the impact of the inlcusion of the correction to take into account the sampling variability of the external control group (section 6.2) and returns the empirical type I error and power as shown in Figures 21, 23, 25 and 27 in appendix. 


For questions or remarks about the code, please contact C.Szurewsky (chloe.szurewsky@gustaveroussy.fr) or G. Le Teuff (gwenael.leteuff@gustaveroussy.fr).
