#' Illustrate the timeseries of the UNSEEN ensemble and the observed using ggplot
#'
#' @param ensemble The UNSEEN ensemble. This must be a dataframe containing at least the columns time and precipitation
#' @param obs The observations. This must be a dataframe with the variables year and precipitation
#' @param ensemble_yname The column name of the y variable. Defaults to "tprate"
#' @param ensemble_xname The column name of the x variable. Defaults to "time"
#' @param obs_yname The column name of the y variable. Defaults to "rr"
#' @param obs_xname The column name of the x variable. Defaults to "time"
#' @param ylab The y label (string)
#' @param title The title (string)
#' @return a ggplot timeseries with x = year and y = variable
#' @export
unseen_timeseries <- function(ensemble, obs, ensemble_yname = "tprate", ensemble_xname = "time", obs_yname = "rr", obs_xname = "time", ylab = "", title = "") {
  # year <- precipitation <- NULL
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = ensemble,
      mapping = ggplot2::aes_(
        x = as.name(ensemble_xname), y = as.name(ensemble_yname),
        group = as.name(ensemble_xname),
        fill = "UNSEEN"
      ),
      alpha = 0.3
    ) + ## Seas5 color is defined manually
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes_(
        x = as.name(obs_xname),
        y = as.name(obs_yname),
        col = "OBS"
      ),
      shape = 4,
      size = 2,
      stroke = 1.5
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(name = NULL, values = c("UNSEEN" = "black")) + ## Here SEAS5 color is defined
    ggplot2::scale_colour_manual(name = NULL, values = c("OBS" = "blue")) + ## And ERA5 color
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      legend.position = c(.95, .02),
      legend.justification = c("right", "bottom"),
      legend.spacing.y = ggplot2::unit(-0.2, "cm"),
      legend.title = ggplot2::element_blank()
    ) + # ,
    # text=element_text(size=7),
    #   axis.text = element_text(size=7))+
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(order = 2)
    )
}
