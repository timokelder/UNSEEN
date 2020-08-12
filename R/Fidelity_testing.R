#' Internal function to plot histograms for the boottest
#'
#' @param bootstrapped_ens THe bootstrapped ensemble
#' @param bootstrapped_ensratio The mean-bias corrected bootstrapped ensemble
#' @param obs THe observations
#' @param fun Function, such as mean, sd, skewness and kurtosis
#' @param main The title
#' @param units units label
#' @param fontsize the font size
#' @param biascor Boolean. Do you want to apply a mean-bias correction?
#' @noRd
plot_hist_combined <- function(bootstrapped_ens, bootstrapped_ensratio, obs, fun, main, units, fontsize, biascor = FALSE) {
  bootstrapped_fun <- apply(bootstrapped_ens, MARGIN = 2, FUN = fun)
  if (biascor == TRUE) {
    bootstrapped_fun_ratio <- apply(bootstrapped_ensratio, MARGIN = 2, FUN = fun)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = bootstrapped_fun),
      color = "black", fill = "black",
      alpha = 0.5,
      bins = 30
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = stats::quantile(bootstrapped_fun,
      probs = c(0.025, 0.975)
    )),
    color = "black", linetype = "dashed", size = 1
    )
  if (biascor == TRUE) {
    p <- p + ggplot2::geom_histogram(ggplot2::aes(x = bootstrapped_fun_ratio),
                                     color = "black", fill = "orange",
                                     alpha = 0.5, bins = 30
                                     ) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = stats::quantile(bootstrapped_fun_ratio,
                                                                    probs = c(0.025, 0.975)
                                                                    )),
                          color = "orange", linetype = "dashed", size = 1
                          )
  }
  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = fun(obs)),
                               color = "blue", size = 2
                               ) +
    ggplot2::labs(title = main, y = "Number of bootstrapped series", x = paste0(" (", units, ")")) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = fontsize),
      axis.text = ggplot2::element_text(size = fontsize)
    )
}


#' Fidelity testing
#'
#' Test the statistical consistency between large-ensemble simulations and shorter observed records
#'
#' Large-ensemble climate model simulations have a much greater sample size than observed records, complicating the
#' evaluation of such ensembles. The fidelity test bootstraps the large ensemble into sample sizes of
#' length equal to the observed record. It plots histograms of the distribution characteristics
#' (mean, standard deviation, skewness and kurtosis) of the bootstrapped timeseries, inlcuding 95% confidence
#' intervals. Distribution characteristic of the observed records are plotted on top.
#'
#'
#' @param obs     The observations. This function expects a vector, i.e. dataframe$variable.
#' @param ensemble The UNSEEN ensemble. This function expects a vector, i.e. dataframe$variable
#' @param fontsize The font size. Defaults to 11.
#' @param biascor Boolean. Do you want to apply a mean-bias correction?
#'
#' @return plots showing the bootstrapped tests of the mean, sd, skewness and kurtosis
#' @source Evaluation explaned in more detail in Kelder et al. 2020
#' @export
fidelity_test <- function(obs, ensemble, fontsize = 11, biascor = FALSE) {
  if (!is.numeric(obs)) {
    stop("Fidelity_test: obs should be numeric. This function expects a vector, i.e. dataframe$variable.")
  }
  if (!is.numeric(ensemble)) {
    stop("Fidelity_test: ensemble should be numeric. This function expects a vector, i.e. dataframe$variable.")
  }

  bootstrapped_ens <- base::sample(ensemble, size = length(obs) * 10000, replace = T) # The original raw data
  bootstrapped_ens <- array(bootstrapped_ens, dim = c(length(obs), 10000)) # Creates an array with 10.000 series of 35 values

  if (biascor == TRUE) {
    ensemble_ratio <- ensemble * mean(obs) / mean(ensemble)
    ## The simple ratio as mean bias corrected series
    bootstrapped_ensratio <- base::sample(ensemble_ratio, size = length(obs) * 10000, replace = T) # The original raw data
    bootstrapped_ensratio <- array(bootstrapped_ensratio, dim = c(length(obs), 10000)) # Creates an array with 10.000 series of 35 values
  }

  p1_comb <- plot_hist_combined(bootstrapped_ens, bootstrapped_ensratio, obs, fun = mean, main = "Mean", units = "mm/day", fontsize = fontsize, biascor = biascor) # Mean
  p2_comb <- plot_hist_combined(bootstrapped_ens, bootstrapped_ensratio, obs, fun = stats::sd, main = "Standard deviation", units = "mm/day", fontsize = fontsize, biascor = biascor) # Standard Deviation
  p3_comb <- plot_hist_combined(bootstrapped_ens, bootstrapped_ensratio, obs, fun = moments::skewness, main = "Skewness", units = "-", fontsize = fontsize, biascor = biascor) # Skewness
  p4_comb <- plot_hist_combined(bootstrapped_ens, bootstrapped_ensratio, obs, fun = moments::kurtosis, main = "Kurtosis", units = "-", fontsize = fontsize, biascor = biascor) # Kurtosis

  ggpubr::ggarrange(p1_comb, p2_comb, p3_comb, p4_comb,
    labels = c("a", "b", "c", "d"),
    font.label = list(size = fontsize, color = "black", face = "bold", family = NULL),
    ncol = 2, nrow = 2
  )
}
