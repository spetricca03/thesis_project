# load dependencies and custom plotting function
source("Scripts/irf_plot.R") 
library(purrr)
library(lpirfs)
library(tidyverse)
library(patchwork)

# define global parameters
Shock <- "MP_brw" 
h <- 13
outcomes <- c("l_Va", "l_piv", "l_nominal")

# load prepared dataset
df <- readRDS("Data/df_ready.rds") 

# define function to estimate baseline local projection models and cache results
compute_baseline_models <- function(df, outcomes, Shock, h) {
  cache <- list()
  for(y in outcomes) {
    message(sprintf("Estimating baseline model for: %s", y))
    cache[[y]] <- lp_lin_panel(
      data_set = df,
      endog_data = y,
      shock = Shock,
      diff_shock = FALSE,
      panel_model = "within",
      panel_effect = "individual",
      robust_cov = "vcovSCC",
      robust_maxlag = 4,
      c_exog_data = c(),
      l_exog_data = c("PI_growth", "VA_growth", "ebp", "GS1", "cpi_growth", "gdp_growth", "inflation", Shock),
      lags_exog_data = 4,
      hor = h,
      confint = 1.64
    )
  }
  return(cache)
}

# define function to generate and arrange impulse response plots
render_baseline_grid <- function(model_cache, outcomes, Shock) {
  plot_list <- list()
  
  for(y in outcomes) {
    res <- model_cache[[y]]
    
    col_title <- switch(y, 
                        "l_Va"      = "Output", 
                        "l_piv"     = "Prices", 
                        "l_nominal" = "Nominal Output")
    
    plot_color <- switch(y, 
                         "l_Va"      = "steelblue", 
                         "l_piv"     = "firebrick", 
                         "l_nominal" = "darkslategray")
    
    p <- plot_lpirfs(
      lpirfs_results = res, 
      plot_var = Shock,
      line_name = col_title,
      line_color = plot_color,
      irf_line_width = 0.6 
    )
    
    p <- p + ggplot2::scale_y_continuous(
      limits = function(x) c(-max(abs(x)), max(abs(x)))
    )
    
    x_label <- if (y == "l_nominal") "Horizon" else NULL
    y_label <- if (y == "l_Va") "Percentage Points" else NULL
    
    p <- p + ggplot2::labs(title = NULL, x = x_label, y = y_label) +
      ggplot2::theme(
        # activate the y-axis title only for the first plot
        axis.title.y = if (y == "l_Va") ggplot2::element_text(margin = ggplot2::margin(r = 10)) else ggplot2::element_blank(), 
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
        axis.text = ggplot2::element_text(size = 8)
      )
    
    plot_list[[y]] <- p
  }
  
  # define grid layout structure
  layout_design <- "
    AABB
    #CC#
  "
  
  # assemble plots using patchwork
  final_grid <- patchwork::wrap_plots(plot_list, design = layout_design) + 
    patchwork::plot_layout(guides = "collect") & 
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      # adjust legend spacing
      legend.text = ggplot2::element_text(size = 11, margin = ggplot2::margin(l = 6, r = 15)),
      legend.key = ggplot2::element_blank()
    )
  
  return(final_grid)
}


# estimate models
baseline_cache <- compute_baseline_models(df, outcomes, Shock, h)

# render the plot grid
grid_baseline <- render_baseline_grid(baseline_cache, outcomes, Shock)

# export the final figure
ggplot2::ggsave(
  filename = "Figures/baseline_irf.pdf", 
  plot = grid_baseline, 
  width = 6.1,  
  height = 5.6, 
  device = "pdf"
)