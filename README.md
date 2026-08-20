# oslrt_non_ph

## Article: One-sample survival tests in the presence of non-proportional hazards in oncology clinical trials
Authors: Chloé Szurewsky, Guosheng Yin, Gwénaël Le Teuff  
Pre-print avaible [here] (https://doi.org/10.48550/arXiv.2506.18608)  

Tests_SA.R  

This file contains the functions required to run the simulations :  
- *OSLRT* and *mOSLRT* are the classical one-sample log-rank test (Finkelstein et al, 2003) and its modified version (Wu, 2014) ;  
- *Score_EE*, *Score_ME*, *Score_DE* and *Score_CH* are the developed score tests for, respectively, an early, a middle, a delayed effect and crossing hazards ;  
- *rmst_single* returns the estimated value of the RMST and *test_SA* the test based on the RMST for single-arm trials;  
- *maxcombo1* is the combination test presented in the article that combines the mOSLRT, early effect score test and delayed effect score tests.

Simulations.R

This file runs the simulation study and gives the type I error or power as shown in Figure 2.  
Figure 12 in Appendix can be obtained by changing the censoring rates, and Figures 13 and 14 by changing the hazard ratio.  

Simulations_distribution_misspe.R

This file runs the simulations to study the impact of the model misspecification of the survival distribution of the external control curve (section 6.3) and returns the relative difference in terms of type I error and power as in Figure 6 and the crude performance (type I and power) as in Figure 29.  
Figure 30 in Appendix can be obtained by changing the censoring rates.  

Simulations_parameter_misspe.R

This file runs the simulations to study the impact of the variability on the exponential parameter of the historical control group (section 6.1) and returns the relative difference in terms of type I error and power as  in Figure 4, and the crude performance (type I error and power) as in Figure 23.  
Figure 24 in Appendix can be obtained by changing the censoring rates.  

Simulations_sampling.R

This file runs the simulations to study the impact of the inclusion of the correction to take into account the sampling variability of the external control group (section 6.2) and returns the relative difference in terms of type I error and power as Figure 5 and the crude performance (type I error and power) as in Figures 25 and 27.  
Figures 26 and 27 in Appendix can be obtained by changing the censoring rates.  


For questions or remarks about the code, please contact C.Szurewsky (chloe.szurewsky.pro@gmail.com).
