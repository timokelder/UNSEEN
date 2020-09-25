#' Test the model stability: 1. density plot
#'
#' We plot the density distribution of the different leadtimes using ggplot.
#'
#'
#' @param ensemble The UNSEEN ensemble. This function expects an dataframe with the columns variable (precipitation) and leadtime.
#' @param var_name The column name containing the variable to be analyzed. Defaults to "tprate".
#' @param ld_name The column name containing the leadtimes. Defaults to "leadtime".
#' @param lab The x-label. Defaults to the variable name (var_name).
#'
#' @return a plot showing the empirical probability density distribution for each leadtime
#' @source Evaluation explained in more detail in Kelder et al. 2020
#' @source Colorblind friendly palette  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
#' @export
Model_stability_density <- function(ensemble, var_name = "tprate", ld_name = "leadtime", lab = var_name) {
  # I select five colors and put black at the end.
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#000000") # , "#0072B2", "#CC79A7")
  ### The Leadtime column has to be a factor for a grouped plot
  ensemble[[ld_name]] <- as.factor(ensemble[[ld_name]])

  p1 <-
    ggplot2::ggplot(
      data = ensemble,
      mapping = ggplot2::aes_(
        x = as.name(var_name), colour = as.name(ld_name))) +
    # ggplot2::ggtitle("UK") +
    ggplot2::labs(x = lab, y = "Density") +
    ggplot2::geom_line(stat = "density") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_colour_manual(values = cbPalette) #+
  # theme(
  #   text = element_text(size = 11),
  #   axis.text = element_text(size = 11),
  #   plot.title = element_text(hjust = 0.5)
  # )

  return(p1)
}


#' Test the model stability: 2. empirical extreme value plot
#'
#' We plot the density distribution of the different leadtimes using ggplot.
#' We want to show the confidence interval of the distribution of all lead times pooled together,
#' and test whether the individual lead times fall within these confidence intervals.
#' Therefore we bootstrap the pooled leadtimes into series with equal length of the individual leadtimes (875), with n=10000.
#'
#' @param ensemble The UNSEEN ensemble. This function expects an dataframe with variables leadtime, precipitation.
#' @param var_name The column name containing the variable to be analyzed. Defaults to "tprate".
#' @param ld_name The column name containing the leadtimes. Defaults to "leadtime".
#' @param lab The y-label. Defaults to the variable name (var_name).
#'
#' @return a plot with the empirical return values of the pooled ensemble including confidence intervals.
#' Individual lead times are plotted on top.
#' @source Evaluation explaned in more detail in Kelder et al. 2020
#' @source Colorblind friendly palette  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
#' @export
Model_stability_boot <- function(ensemble, var_name = "tprate", ld_name = "leadtime", lab = var_name) {
  #Define necessary global variables
  ci_2.5 <- ci_97.5 <- quantiles_all <- rps_all <- NULL

  ### The Leadtime column has to be a factor for a grouped plot
  ensemble[[ld_name]] <- as.factor(ensemble[[ld_name]])


  ensemble_length <- length(ensemble[[ld_name]])
  leadtime_length <- sum(ensemble[[ld_name]] == 2)
  bootstrapped_series <- sample(ensemble[[var_name]], size = leadtime_length * 10000, replace = T) # bootstraps the series of length equal to each lead time (875) with n= 10.000
  bootstrapped_array <- array(bootstrapped_series, dim = c(leadtime_length, 10000)) # Creates an array with 10.000 series of 875 values

  rps <- ensemble_length / 1:ensemble_length # The return periods for the entire ensemble of 4,375‬ years
  rps_ld <- leadtime_length / 1:leadtime_length # The return periods for the ensemble split up into 5 leadtimes, 875 years

  Rvs <- apply(bootstrapped_array,
               MARGIN = 2,
               FUN = stats::quantile,
               probs = 1 - 1 / (rps),
               na.rm = FALSE) # apply the function to each of the 10.000 series

  # calculate the lower and upper interval from the 10.000 values for each quantile.
  ci_rvs <- apply(Rvs,
                  MARGIN = 1,
                  FUN = stats::quantile,
                  probs = c(0.025, 0.975),
                  na.rm = FALSE)

  ## Create a dataframe including the return periods, empirical values and confidence intervals
  df_quantiles <- ensemble %>%
    dplyr::mutate(rps_all = rps, quantiles_all = stats::quantile(!!as.name(var_name), 1 - 1 / (rps), na.rm = FALSE))

  df_quantiles <- df_quantiles %>%
    dplyr::group_by(!!as.name(ld_name)) %>%
    dplyr::mutate(rps_ld = rps_ld, quantiles_ld = stats::quantile(!!as.name(var_name), 1 - 1 / (rps_ld), na.rm = FALSE))

  df_quantiles$ci_2.5 <- ci_rvs[1, ]
  df_quantiles$ci_97.5 <- ci_rvs[2, ]

  # And plot
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#000000") # , "#0072B2", "#CC79A7")
  cols <- c("95 % CI" = "black")
  p2 <-
    ggplot2::ggplot(df_quantiles) +
    ggplot2::geom_line(ggplot2::aes(x = rps_all, y = quantiles_all)) +
    ggplot2::geom_line(ggplot2::aes_(x = ~rps_ld, y = ~quantiles_ld, col = as.name(ld_name))) +
    ggplot2::geom_ribbon(ggplot2::aes(x = rps_all, ymin = ci_2.5, ymax = ci_97.5, fill = "95 % CI"), alpha = 0.1) +
    # xlim(NA,875)+
    ggplot2::scale_x_log10(limits = c(NA, leadtime_length)) +
    # scale_x_continuous(trans='log10') +
    ggplot2::scale_fill_manual(name = "Pooled data", values = cols) +
    ggplot2::scale_colour_manual(values = cbPalette) +
    ggplot2::xlab("Return period (years)") +
    ggplot2::ylab(lab) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(size = 11),
                   axis.text = ggplot2::element_text(size = 11)
    )
  return(p2)
}

#' Test the model stability: It plots the output of 1. Model_stability_density(): a density plot and 2. Model_stability_boot(): an empirical extreme value plot
#'
#' @param ensemble The UNSEEN ensemble. This function expects an dataframe with variables leadtime, precipitation.
#' @param var_name The column name containing the variable to be analyzed. Defaults to "tprate".
#' @param ld_name The column name containing the leadtimes. Defaults to "leadtime".
#' @param lab The label. Defaults to the variable name (var_name).
#'
#' @return a plot with the empirical return values of the pooled ensemble including confidence intervals.
#' Individual lead times are plotted on top.
#' @seealso [Model_stability_density()] [Model_stability_boot()]d
#' @source Evaluation explaned in more detail in Kelder et al. 2020
#' @source Colorblind friendly palette  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
#' @export
stability_test <- function(ensemble, var_name = "tprate", ld_name = "leadtime", lab = var_name) {
  #combine plots from function 1 and 2
  p1 <- Model_stability_density(ensemble = ensemble, var_name = var_name, ld_name = ld_name, lab = lab)
  p2 <- Model_stability_boot(ensemble = ensemble, var_name = var_name, ld_name = ld_name, lab = lab)
  p_combined <- ggpubr::ggarrange(p1, p2,
                          labels = c("a", "b"), # , "c", "d"),
                          hjust = c(-0.5, 1, -0.5, 1),
                          ncol = 1, nrow = 2,
                          font.label = list(size = 11, color = "black", face = "bold", family = NULL),
                          common.legend = TRUE
  )
  return(p_combined)
}
