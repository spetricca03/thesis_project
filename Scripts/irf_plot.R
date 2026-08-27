library(dplyr)
library(ggplot2)

plot_lpirfs <- function(lpirfs_results, plot_var, line_name, line_color, 
                        irf_line_width = 1.2, baseline_results = NULL) {
  
  H <- length(lpirfs_results$reg_outputs)
  irf_mean <- numeric(H)
  irf_se   <- numeric(H)
  irf_mean_base <- numeric(H)
  
  for (h in 1:H) {
    # full model
    summ <- lpirfs_results$reg_summaries[[h]]
    if (plot_var %in% rownames(summ)) {
      irf_mean[h] <- summ[plot_var, "Estimate"]
      irf_se[h]   <- summ[plot_var, "Std. Error"]
    } else {
      irf_mean[h] <- 0
      irf_se[h]   <- 0
    }
    
    # baseline model overlay
    if (!is.null(baseline_results)) {
      summ_base <- baseline_results$reg_summaries[[h]]
      if (plot_var %in% rownames(summ_base)) {
        irf_mean_base[h] <- summ_base[plot_var, "Estimate"]
      } else {
        irf_mean_base[h] <- 0
      }
    }
  }
  
  plot_df <- data.frame(
    horizon = 0:(H-1), 
    mean = irf_mean, 
    se = irf_se, 
    mean_base = irf_mean_base,
    Variable = line_name
  ) %>%
    mutate(
      low_68 = mean - 1 * se, up_68 = mean + 1 * se,
      low_90 = mean - 1.645 * se, up_90 = mean + 1.645 * se
    )
  
  rgb_val <- col2rgb(line_color)
  dark_line_color <- rgb(rgb_val[1]*0.6, rgb_val[2]*0.6, rgb_val[3]*0.6, maxColorValue=255)
  
  p <- ggplot(plot_df, aes(x = horizon)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_ribbon(aes(ymin = low_90, ymax = up_90, fill = Variable), alpha = 0.25) +
    geom_ribbon(aes(ymin = low_68, ymax = up_68, fill = Variable), alpha = 0.40)
  
  # inject baseline dashed line
  if (!is.null(baseline_results)) {
    p <- p + geom_line(aes(y = mean_base), color = "black", linetype = "dashed", linewidth = 0.6)
  }
  
  # main line on top
  p <- p + geom_line(aes(y = mean, color = Variable), linewidth = irf_line_width) +
    scale_fill_manual(name = NULL, values = setNames(line_color, line_name)) +
    scale_color_manual(name = NULL, values = setNames(dark_line_color, line_name)) +
    scale_x_continuous(breaks = seq(0, H-1, by = 2), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = alpha("gray50", 0.15), linetype = "dashed", linewidth = 0.15),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}