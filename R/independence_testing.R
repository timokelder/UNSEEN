#' Calculate the correlation between each unique pair of ensemble members.
#'
#' @param ensemble The UNSEEN ensemble. This function expects a dataframe with the columns variable (precipitation), ensemble (number), leadtime.
#' It expects that the first member is 0 (as used in CDS) and that the first lead time is 2
#' (first month shows dependence and is therefore removed). See for example the SEAS5_UK dataset using  `?SEAS5_UK`
#' @param n_ensembles The number of ensemble members (not including pooled leadtimes) in the UNSEEN ensemble. Defaults to 25.
#' @param n_lds The number of leadtimes in the UNSEEN ensemble. Defaults to 5.
#' @param var_name The column name containing the variable to be analyzed. Defaults to "tprate".
#' @param ens_name The column name containing the ensemble members. Defaults to "number".
#' @param ld_name The column name containing the leadtimes. Defaults to "leadtime".
#' @param detrend Should the trend in the data be removed? By default, the data is detrended by differencing.
#' @return A plot showing the boxplot of the pairwise correlations for each leadtime. See Kelder et al. 2020.
#' @export
#' @section Warning:
#' The confidence intervals are based on a bootstrap test with n_ensembles = 25. These are now pre-computed and therefore
#' will not give the right CI bounds for different ensemble sizes. The bootstrapping test is included in the documentation in data_raw, but
#' might be included in the future as a function of n_ensembles and n_lds.
independence_test <- function(ensemble, n_ensembles = 25, n_lds = 5, detrend = TRUE, var_name = "tprate", ens_name = "number", ld_name = "leadtime") {
  `2.5%` <- `97.5%` <- Boxstat <- Freq <- leadtime <- x <- NULL


  ## Perform the pairwise test
  correlations_lds <- array( # create and array to fill the correlations in
    dim = c(n_ensembles, n_ensembles, n_lds), # Dimensions
    dimnames = list(
      memberx = as.character(0:(n_ensembles - 1)), # The names of the members start at 0
      membery = as.character(0:(n_ensembles - 1)), # Lead times start at 2 (first month shows dependence and is therefore removed)
      leadtime = as.character(2:(n_lds + 1))
    )
  )
  # lds, mbmrs
  for (ld in 2:(n_lds + 1)) {
    for (mbr1 in 0:(n_ensembles - 1)) {
      for (mbr2 in 0:(n_ensembles - 1)) {
        if (mbr1 > mbr2) { ## Only calculate this for the top half of the correlation matrix, as not to duplicate any values
          ## -> avoid correlating mbr 1 with mbr2 and then mbr2 with mbr 1.
          predictant <- ensemble[[var_name]][ensemble[[ens_name]] == mbr1 & ensemble[[ld_name]] == ld]
          predictor <- ensemble[[var_name]][ensemble[[ens_name]] == mbr2 & ensemble[[ld_name]] == ld]

          if (detrend == TRUE) {
            correlations_lds[as.character(mbr1), as.character(mbr2), as.character(ld)] <- stats::cor(
              x = diff(predictant, lag = 1, differences = 1),
              y = diff(predictor, lag = 1, differences = 1),
              method = "spearman"
            )
          } else{
            correlations_lds[as.character(mbr1), as.character(mbr2), as.character(ld)] <- stats::cor(
              x = predictant,
              y = predictor,
              method = "spearman"
            )
          }


        }
      }
    }
  }

  ##Convert to dataframe for ggplot
  correlations_lds_df = as.data.frame(as.table(correlations_lds))
  ## And plot
  CI_bounds_boxplots_df = as.data.frame(t(CI_bounds_boxplots))
  CI_bounds_boxplots_df$Boxstat = c('lower whisker', 'lower IQR', 'median','upper IQR', 'upper whisker')
  CI_bounds_boxplots_df_ld2 <- CI_bounds_boxplots_df
  CI_bounds_boxplots_df_ld2$x <- 0.5
  CI_bounds_boxplots_df_ld_end <- CI_bounds_boxplots_df
  CI_bounds_boxplots_df_ld_end$x <- n_lds + .5
  CI_bounds_boxplots_df_all = rbind(CI_bounds_boxplots_df_ld2,CI_bounds_boxplots_df_ld_end)

  ggplot2::ggplot(data = correlations_lds_df) +
    ggplot2::geom_violin(
      mapping = ggplot2::aes(
        x = leadtime,
        y = Freq,
        group = leadtime
      )
    ) +
    ggplot2::geom_boxplot(mapping = ggplot2::aes(x = leadtime, y = Freq,group = leadtime), width=0.2) + # same as violin
    ggplot2::geom_ribbon(data=CI_bounds_boxplots_df_all,
                         ggplot2::aes(x = x,
                                      ymin = `2.5%`,ymax=`97.5%`,
                                      group = Boxstat),
                         fill="grey",
                         alpha=0.4) +
    ggplot2::theme_classic() +
    ggplot2::ylab(bquote("Spearman" ~ rho))+
    ggplot2::ylim(c(-1,1))


  # The old plot with base R
  #   # par(mfrow=c(1,2),mar=c(4.5,5.1,2.1,2.1),cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
  #   boxplot(list(correlations_lds[, , "2"], correlations_lds[, , "3"], correlations_lds[, , "4"], correlations_lds[, , "5"], correlations_lds[, , "6"]),
  #     range = 0, # box whiskers to be the data extremes
  #     xaxt = "n",
  #     xlab = "Lead time",
  #     ylab = bquote("Spearman" ~ rho)
  #   )
  #   for (i in 1:length(CI_bounds_boxplots[1, ])) {  ## CI_bounds_boxplots is internal data, created by function b
  #     polygon(c(0, 6, 6, 0), c(rep(CI_bounds_boxplots[1, i], 2), rep(CI_bounds_boxplots[2, i], 2)), col = gray(0.8, alpha = 0.3))
  #   }
  #   Axis(side = 1, at = 1:6, labels = c(as.character(2:6), "all"))

  # return(correlations_lds)
}
