# load dependencies and custom plotting functions
source("Scripts/irf_plot.R") 
library(purrr)
library(lpirfs)
library(tidyverse)
library(patchwork)

# define global parameters and target variables
Shock <- "MP_brw" 
h <- 13

df <- readRDS("Data/df_ready.rds")
outcomes <- c("l_Va", "l_piv", "l_nominal")
targets <- c("Asset_Tangibility", "Leverage_Assets_2", "St_ratio")

clean_labels <- list(
  Asset_Tangibility = "Asset Tangibility",
  Leverage_Assets_2 = "Leverage",
  St_ratio          = "Maturing Debt Share"
)

# define control variable sets for the full specifications
full_controls <- list(
  Asset_Tangibility = c("Leverage_Assets_2", "Young_Firm_Share", 
                        "Asset_Tangibility", "St_ratio", "KY_ratio", 
                        "Durable", "Nondurable", "FPA", "Quick_ratio"),
  Leverage_Assets_2 = c("Leverage_Assets_2", "Young_Firm_Share", 
                        "Asset_Tangibility", "St_ratio", "KY_ratio", 
                        "Durable", "Nondurable", "FPA", "Quick_ratio"),
  St_ratio          = c("Leverage_Assets_2", "Young_Firm_Share", 
                        "Asset_Tangibility", "St_ratio", "KY_ratio", 
                        "Durable", "Nondurable", "FPA", "Quick_ratio")
)

# define function to estimate full local projection models with controls
compute_full_models <- function(df, targets, outcomes, full_controls) {
  
  # extract and apply conditional mean scaling to all interacted variables
  all_vars_to_scale <- unique(c(targets, unlist(full_controls)))
  
  for(v in all_vars_to_scale) {
    if(v %in% colnames(df)) {
      p25 <- quantile(df[[v]], 0.25, na.rm = TRUE)
      p75 <- quantile(df[[v]], 0.75, na.rm = TRUE)
      
      mean_bot <- mean(df[[v]][df[[v]] <= p25], na.rm = TRUE)
      mean_top <- mean(df[[v]][df[[v]] >= p75], na.rm = TRUE)
      
      scale_factor <- mean_top - mean_bot
      df[[v]] <- df[[v]] / scale_factor
      
      interaction_col <- paste0(Shock, "_x_", v)
      df[[interaction_col]] <- df[[Shock]] * df[[v]]
    }
  }
  
  # initialize list to store estimated models
  cache <- list()
  
  for(y in outcomes) {
    cache[[y]] <- list()
    for(target in targets) {
      Main_Var <- paste0(Shock, "_x_", target)
      
      base_ctrls <- full_controls[[target]]
      # exclude the primary target from the interaction controls to prevent collinearity
      interact_vars <- setdiff(base_ctrls, target) 
      dynamic_exog <- c(base_ctrls, paste0(Shock, "_x_", interact_vars))
      
      cache[[y]][[target]] <- lp_lin_panel(
        data_set = df,
        endog_data = y,
        diff_shock = FALSE,
        shock = Main_Var,
        panel_model = "within",
        panel_effect = "twoways",
        robust_cov = "vcovSCC",
        robust_maxlag = 4,
        c_exog_data = c(dynamic_exog),
        l_exog_data = c("PI_growth", "VA_growth"),
        lags_exog_data = 4,
        hor = h,
        confint = 1.96
      )
    }
  }
  return(cache)
}

# define function to render grid plots overlaying full and simple model results
render_full_grid <- function(full_cache, simple_cache, targets, outcomes) {
  
  plot_list <- list()
  
  for(i in seq_along(outcomes)) {
    y <- outcomes[i]
    
    col_title <- switch(y, 
                        "l_Va"      = "Output", 
                        "l_piv"     = "Prices", 
                        "l_nominal" = "Nominal Output")
    
    plot_color <- switch(y, 
                         "l_Va"      = "steelblue", 
                         "l_piv"     = "firebrick", 
                         "l_nominal" = "darkslategray")
    
    for(j in seq_along(targets)) {
      target <- targets[j]
      clean_target <- clean_labels[[target]]
      Main_Var <- paste0(Shock, "_x_", target)
      
      # retrieve results from both model caches
      res_full <- full_cache[[y]][[target]]
      res_simple <- simple_cache[[y]][[target]]
      
      p <- plot_lpirfs(
        lpirfs_results = res_full,
        plot_var = Main_Var,
        line_name = col_title,
        line_color = plot_color,
        irf_line_width = 0.6,
        baseline_results = res_simple # pass the simple model for overlay
      )
      
      # implement linetype legend using invisible dummy data mapping
      p <- p + 
        ggplot2::geom_line(
          data = data.frame(x = NA, y = NA, Model = c("Full Model", "Baseline")), 
          ggplot2::aes(x = x, y = y, linetype = Model), 
          inherit.aes = FALSE,
          na.rm = TRUE
        ) +
        ggplot2::scale_linetype_manual(
          values = c("Full Model" = "solid", "Baseline" = "dashed")
        ) +
        ggplot2::guides(
          linetype = ggplot2::guide_legend(override.aes = list(color = "black", linewidth = 0.6))
        )
      
      p <- p + ggplot2::scale_y_continuous(
        limits = function(x) c(-max(abs(x)), max(abs(x)))
      )
      
      p <- p + ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = 8), 
          plot.margin = ggplot2::margin(t = 8, r = 5, b = 8, l = 5) 
        )
      
      # append column titles to the top row
      if (i == 1) {
        p <- p + ggplot2::labs(title = clean_target) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 10, margin = ggplot2::margin(b = 10))
          )
      }
      
      # append x-axis label to the bottom middle plot
      if (i == 3 && j == 2) {
        p <- p + ggplot2::labs(x = "Horizon") +
          ggplot2::theme(
            axis.title.x = ggplot2::element_text(size = 9, margin = ggplot2::margin(t = 10))
          )
      }
      
      plot_list <- append(plot_list, list(p))
    }
  }
  
  # assemble the internal grid using patchwork
  grid_inner <- wrap_plots(plot_list, ncol = length(targets), nrow = length(outcomes)) +
    plot_layout(guides = "collect") & 
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9),
      legend.key = ggplot2::element_blank()
    )
  
  # create a global y-axis label
  y_global <- ggplot2::ggplot() + 
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Percentage Points", 
                      angle = 90, size = 3.5) + 
    ggplot2::theme_void()
  
  final_grid <- y_global + grid_inner + patchwork::plot_layout(widths = c(0.04, 1))
  
  return(final_grid)
}

# estimate full models (requires my_model_cache from simple models to be present in the environment)
full_model_cache <- compute_full_models(df, targets, outcomes, full_controls)

# render the plot grid using both model caches
grid_full <- render_full_grid(full_model_cache, my_model_cache, targets, outcomes)

# export the final figure
ggplot2::ggsave(
  filename = "Figures/grid_full_models.pdf",
  plot = grid_full,
  width = 6.1,  
  height = 6.35, 
  device = "pdf"
)