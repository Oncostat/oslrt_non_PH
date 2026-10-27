##Load the libraries##
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(flexsurv)
library(survival)
library(survminer)

rm(list = ls())

plot_scenario <- function(rate, intervals = NULL, vlines = NULL,
                          scenario, effect, 
                          t_cont, delta_cont,
                          t_logis, delta_logis){
  if (is.null(intervals)){
    t_exp <- nphsim::rpwexp(n = 5000, rate = rate, cumulative = FALSE)
    } 
  else{
    t_exp <- nphsim::rpwexp(n = 5000, rate = rate, intervals = intervals, cumulative = FALSE)
  }
  
  delta_exp <- ifelse(t_exp < (2555/365), 1, 0)
  t_exp <- ifelse(t_exp > (2555/365), (2555/365), t_exp)
  
  ## Dataset
  d <- data.frame(
    Time = c(t_cont, t_exp, t_logis),
    Status = c(delta_cont, delta_exp, delta_logis),
    Group = c(rep('Control group (exponential)', length(t_cont)),
              rep('Experimental group', length(t_exp)),
              rep('Control group (log-logistic)', length(t_logis))))
  
  d$Group <- factor(d$Group,
    levels = c('Control group (exponential)',
               'Experimental group',
               'Control group (log-logistic)'))
  
  p <- ggsurvplot(
    fit = survfit(Surv(Time, Status) ~ Group, data = d),
    data = d,
    palette = c('black', 'black', 'black'),
    linetype = c(2, 1, 3),
    censor = FALSE,
    legend.title = '',
    legend.labs = c(
      'Control group (exponential)',
      'Experimental group',
      'Control group (log-logistic)'),
    xlab = 'Time (years)',
    title = paste0('Scenario ', scenario, ': ', effect),
    ggtheme = theme_classic() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 20, hjust = 0.5)))
  
  ## Vertical lines
  if(!is.null(vlines)){
    for(x in vlines){
      p <- p %++%
        geom_vline(xintercept = x,
                   linetype = 'dotted')
    }
  }
  return(p)
}

plot_hazard <- function(rate, intervals = NULL,
                        scenario, effect, 
                        h_cont, h_logis){
  t <- seq(0.01, 7, length.out = 5000)
  if (is.null(intervals)){
    h_exp <- hexp(x = t, rate = rate)
  } 
  else{
    if(length(intervals) == 1){
      h_exp <- c(rep(rate[1], 365*intervals), rep(rate[2], (2556-365*intervals)))
    }
    else if(length(intervals) == 2){
      h_exp <- c(rep(rate[1], 365*intervals[1]), rep(rate[2], 365*intervals[2]), rep(rate[3], (2556-365*(intervals[1]+intervals[2]))))
    }
  }
  
  ## Dataset
  d <- data.frame(
    Time = c(rep(t, 3)),
    Hazard = c(h_cont, h_exp, h_logis),
    Group = c(rep('Control group (exponential)', length(t)),
              rep('Experimental group', length(t)),
              rep('Control group (log-logistic)', length(t))))
  
  d$Group <- factor(d$Group,
                    levels = c('Control group (exponential)',
                               'Experimental group',
                               'Control group (log-logistic)'))
  
  p <- ggplot(d, aes(x = Time, y = Hazard, group = Group)) +
    geom_line(aes(color = Group, linetype = Group), size = 1) +
    scale_linetype_manual(values = c('dashed', 'solid', 'dotted')) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    labs(x = 'Time (years)',
         y = 'Hazard',
         title = paste0('Scenario ', scenario, ': ', effect)) +
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
  return(p)
}

make_plot_data <- function(mean_res){
  
  sample_sizes <- as.numeric(names(mean_res))
  
  test_labels <- c(
    err_oslrt = "OSLRT",
    err_moslrt = "Modified OSLRT",
    err_rmst = "RMST",
    err_ee = "Early effect",
    err_me = "Middle effect",
    err_de = "Delayed effect",
    err_ch = "Crossing hazards",
    err_hoch = "max-Combo (Hochberg correction)",
    err_exact = "max-Combo (multivariate normal integration)"
  )
  
  data <- do.call(rbind, lapply(seq_along(sample_sizes), function(i){
    
    res_n <- mean_res[[i]]
    tests <- names(test_labels)
    
    data.frame(Sample.size = sample_sizes[i],
               Events = as.numeric(res_n["obs"]),
               Test = unname(test_labels[tests]),
               Error = as.numeric(res_n[tests]))
  }))
  
  rownames(data) <- NULL
  data$Test <- factor(data$Test, levels = unname(test_labels))
  
  return(data)
}

plot_power <- function(data, title, threshold = 0.8, ylim_max = 1){
  
  sample_sizes <- sort(unique(data$Sample.size))
  event_labels <- data$Events[match(sample_sizes, data$Sample.size)]
  
  ggplot(data, aes(x = Sample.size, y = Error, group = Test)) +
    geom_line(aes(color = Test), size = 1) +
    geom_point(aes(color = Test, shape = Test), size= 2) +
    scale_x_continuous(breaks = sample_sizes,
                       labels = sample_sizes,
                       name = 'Sample size',
                       sec.axis = sec_axis(trans = ~.,
                                           name = "Number of events",
                                           breaks = sample_sizes,
                                           labels = round(event_labels))) +
    scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18)) +
    scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue',
                                  'orange', 'green3', 'magenta', 'brown',
                                  'mediumturquoise')) +
    labs(x = 'Sample size', 
         y = 'Power',
         title = title) +
    geom_hline(yintercept = threshold,
               size = 0.2) +
    ylim(0, ylim_max) +
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
}

make_plot_missp_data <- function(mean_res, scenario){
  
  sample_sizes <- as.numeric(names(mean_res))

  test_labels <- c(
    err_oslrt = "OSLRT",
    err_moslrt = "Modified OSLRT",
    err_score = "CP exact",
    err_remove_3 = "CP - 3 months",
    err_remove_6 = "CP - 6 months",
    err_add_3 = "CP + 3 months",
    err_add_6 = "CP + 6 months"
  )
  
  data <- do.call(rbind, lapply(seq_along(sample_sizes), function(i){
    
    res_n <- mean_res[[i]]
    tests <- names(test_labels)
    
    data.frame(Sample.size = sample_sizes[i],
               Events = as.numeric(res_n["obs"]),
               Test = unname(test_labels[tests]),
               Error = as.numeric(res_n[tests]))
  }))
  
  rownames(data) <- NULL
  data$Test <- factor(data$Test, levels = unname(test_labels))
  
  return(data)
}

plot_power_misspe_cps <- function(data, title, threshold = 0.8, ylim_max = 1,
                                  color_lines){
  
  sample_sizes <- sort(unique(data$Sample.size))
  
  ggplot(data, aes(x = Sample.size, y = Error, group = Test)) +
    geom_line(aes(color = Test, linetype = Test), size = 1) +
    geom_point(aes(color = Test, shape = Test), size = 2) +
    scale_x_continuous(breaks = sample_sizes,
                       labels = sample_sizes,
                       name = 'Sample size') +
    scale_shape_manual(values = c(18, 18, 18, 16, 15, 4, 8)) +
    scale_linetype_manual(values = c('solid', 'solid', 'solid', 'dashed', 'dotted', 'dashed', 'dotted')) +
    scale_color_manual(values = c('red', 'gold3', rep(color_lines, 5))) +
    labs(x = 'Sample size',
         y = 'Power',
         title = title) +
    geom_hline(yintercept = threshold,
               size = 0.2) +
    ylim(0, ylim_max) +
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
}

make_diff_data <- function(mean_res_init, mean_res_misspe){
  
  sample_sizes <- as.numeric(names(mean_res_init))
  
  test_labels <- c(
    err_oslrt = "OSLRT",
    err_moslrt = "Modified OSLRT",
    err_rmst = "RMST",
    err_ee = "Early effect",
    err_me = "Middle effect",
    err_de = "Delayed effect",
    err_ch = "Crossing hazards",
    err_hoch = "max-Combo (Hochberg correction)",
    err_exact = "max-Combo (multivariate normal integration)"
  )
  
  tests <- names(test_labels)
  
  if(!setequal(names(mean_res_init), names(mean_res_misspe))){
    stop("Data set have not the same samples sizes")
  }
  
  data <- do.call(rbind, lapply(sample_sizes, function(n){
    
    res_init <- mean_res_init[[as.character(n)]]
    res_misspe <- mean_res_misspe[[as.character(n)]]
    
    data.frame(Sample.size = n,
               Test = unname(test_labels[tests]),
               Error = as.numeric(((res_misspe[tests]-res_init[tests])/res_init[tests])*100))
  }))
  
  rownames(data) <- NULL
  data$Test <- factor(data$Test, levels = unname(test_labels))
  
  return(data)
}

plot_diff_unique <- function(data, title){
  
  sample_sizes <- sort(unique(data$Sample.size))
  
  ggplot(data, aes(x = Sample.size, y = Error, group = Test)) +
    geom_line(aes(color = Test), size = 1) +
    geom_point(aes(color = Test, shape = Test), size= 2) +
    scale_x_continuous(breaks = sample_sizes,
                       labels = sample_sizes,
                       name = 'Sample size') +
    scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18)) +
    scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue',
                                  'orange', 'green3', 'magenta', 'brown',
                                  'mediumturquoise')) +
    labs(x = 'Sample size', 
         y = 'Relative difference (%)',
         title = title) +
    geom_hline(yintercept = 0, size = 0.2) +
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
}

make_diff_sv_data <- function(mean_res_init, mean_res_misspe){
  
  sample_sizes <- as.numeric(names(mean_res_init))
  
  test_labels <- c(
    err_oslrt = "OSLRT",
    err_moslrt = "Modified OSLRT",
    err_ee = "Early effect",
    err_me = "Middle effect",
    err_de = "Delayed effect",
    err_ch = "Crossing hazards",
    err_hoch = "max-Combo (Hochberg correction)",
    err_exact = "max-Combo (multivariate normal integration)"
  )
  
  tests <- names(test_labels)
  
  if(!setequal(names(mean_res_init), names(mean_res_misspe))){
    stop("Data set have not the same samples sizes")
  }
  
  data <- do.call(rbind, lapply(sample_sizes, function(n){
    
    res_init <- mean_res_init[[as.character(n)]]
    res_misspe <- mean_res_misspe[[as.character(n)]]
    
    data.frame(Sample.size = n,
               Test = unname(test_labels[tests]),
               Error = as.numeric(((res_misspe[tests]-res_init[tests])/res_init[tests])*100))
  }))
  
  rownames(data) <- NULL
  data$Test <- factor(data$Test, levels = unname(test_labels))
  
  return(data)
}

plot_diff_sv_unique <- function(data, title){
  
  sample_sizes <- sort(unique(data$Sample.size))
  
  ggplot(data, aes(x = Sample.size, y = Error, group = Test)) +
    geom_line(aes(color = Test), size = 1) +
    geom_point(aes(color = Test, shape = Test), size= 2) +
    scale_x_continuous(breaks = sample_sizes,
                       labels = sample_sizes,
                       name = 'Sample size') +
    scale_shape_manual(values = c(18, 18, 8, 16, 4, 18, 18, 18)) +
    scale_color_manual(values = c('red', 'gold3', 'blue', 'orange', 
                                  'green3', 'magenta', 'brown',
                                  'mediumturquoise')) +
    labs(x = 'Sample size', 
         y = 'Relative difference (%)',
         title = title) +
    geom_hline(yintercept = 0, size = 0.2) +
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
}

plot_power_appendix <- function(data, subtitle, threshold = 0.8, ylim_max = 1){
  
  sample_sizes <- sort(unique(data$Sample.size))
  event_labels <- data$Events[match(sample_sizes, data$Sample.size)]
  
  ggplot(data, aes(x = Sample.size, y = Error, group = Test)) +
    geom_line(aes(color = Test), size = 1) +
    geom_point(aes(color = Test, shape = Test), size= 2) +
    scale_x_continuous(breaks = sample_sizes,
                       labels = sample_sizes,
                       name = 'Sample size',
                       sec.axis = sec_axis(trans = ~.,
                                           name = "Number of events",
                                           breaks = sample_sizes,
                                           labels = round(event_labels))) +
    scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18)) +
    scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue',
                                  'orange', 'green3', 'magenta', 'brown',
                                  'mediumturquoise')) +
    labs(x = 'Sample size', 
         y = 'Power',
         title = ,
         subtitle = subtitle) +
    geom_hline(yintercept = threshold,
               size = 0.2) +
    ylim(0, ylim_max) +
    theme(axis.text.x.top = element_text(angle = 90),
          axis.text.x.bottom = element_text(angle = 90),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 15))
}

plot_diff_appendix <- function(data, subtitle, ylim_min, ylim_max){
  
  sample_sizes <- sort(unique(data$Sample.size))
  
  ggplot(data, aes(x = Sample.size, y = Error, group = Test)) +
    geom_line(aes(color = Test), size = 1) +
    geom_point(aes(color = Test, shape = Test), size= 2) +
    scale_x_continuous(breaks = sample_sizes,
                       labels = sample_sizes,
                       name = 'Sample size') +
    scale_shape_manual(values = c(18, 18, 18, 8, 16, 4, 18, 18, 18)) +
    scale_color_manual(values = c('red', 'gold3', 'mediumpurple1', 'blue',
                                  'orange', 'green3', 'magenta', 'brown',
                                  'mediumturquoise')) +
    labs(x = 'Sample size', 
         y = 'Relative difference (%)',
         title = ,
         subtitle = subtitle) +
    geom_hline(yintercept = 0, size = 0.2) +
    ylim(ylim_min, ylim_max) +
    theme(axis.text.x.top = element_text(angle = 90),
          axis.text.x.bottom = element_text(angle = 90),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 15))
}

make_plot_data_combo <- function(mean_res, labels_graph){
  
  sample_sizes <- as.numeric(names(mean_res))
  
  test_labels <- c(
    err_init_hoch = "max-Combo (Hochberg correction)",
    err_init_exact = "max-Combo (multivariate normal integration)",
    err_rem_one_hoch = paste0(labels_graph[1], " (Hochberg correction)"),
    err_rem_one_exact = paste0(labels_graph[1], " (multivariate normal integration)"),
    err_rem_two_hoch = paste0(labels_graph[2], " (Hochberg correction)"),
    err_rem_two_exact = paste0(labels_graph[2], " (multivariate normal integration)"),
    err_rem_both_hoch = paste0(labels_graph[3], " (Hochberg correction)"),
    err_rem_both_exact = paste0(labels_graph[3], " (multivariate normal integration)")
  )
  
  data <- do.call(rbind, lapply(seq_along(sample_sizes), function(i){
    
    res_n <- mean_res[[i]]
    tests <- names(test_labels)
    
    data.frame(Sample.size = sample_sizes[i],
               Test = unname(test_labels[tests]),
               Error = as.numeric(res_n[tests]))
  }))
  
  rownames(data) <- NULL
  data$Test <- factor(data$Test, levels = unname(test_labels))
  
  return(data)
}

make_plot_data_combo2 <- function(mean_res, labels_graph){
  
  sample_sizes <- as.numeric(names(mean_res))
  
  test_labels <- c(
    err_init_hoch = "max-Combo (Hochberg correction)",
    err_init_exact = "max-Combo (multivariate normal integration)",
    err_add_one_hoch = paste0(labels_graph[1], " (Hochberg correction)"),
    err_add_one_exact = paste0(labels_graph[1], " (multivariate normal integration)"),
    err_add_two_hoch = paste0(labels_graph[2], " (Hochberg correction)"),
    err_add_two_exact = paste0(labels_graph[2], " (multivariate normal integration)"),
    err_add_both_hoch = paste0(labels_graph[3], " (Hochberg correction)"),
    err_add_both_exact = paste0(labels_graph[3], " (multivariate normal integration)")
  )
  
  data <- do.call(rbind, lapply(seq_along(sample_sizes), function(i){
    
    res_n <- mean_res[[i]]
    tests <- names(test_labels)
    
    data.frame(Sample.size = sample_sizes[i],
               Test = unname(test_labels[tests]),
               Error = as.numeric(res_n[tests]))
  }))
  
  rownames(data) <- NULL
  data$Test <- factor(data$Test, levels = unname(test_labels))
  
  return(data)
}







