#' This is the SEAS5 UNSEEN ensemble of February Precipitation over the UK.
#'
#' A dataset containing 125 ensembles (25 members: column 'number'; 5 leadtimes: column 'leadtime') of February average precipitation (mm/d) for 1982-2016,
#' created from the SEAS5 hindcast dataset available through the Climate Datastore (CDS).
#' This dataset was created as part of the ECMWF Summer of Weather Code program: \url{https://github.com/esowc/UNSEEN-open}.
#'
#' @format A data frame with 4375 rows and 4 variables:
#' \describe{
#'   \item{leadtime}{the leadtime, in months, starting at 2.
#'   For example, leadtime 2 forecasts are initialized in January, leadtime 3 forecasts are intialized in December, etc.}
#'   \item{number}{The ensemble members. The hindcast is initialized with 25 ensemble members, named 0:24.}
#'   \item{time}{The year, stored as yyyy-02-01 (we only have February averages).}
#'   \item{tprate}{The total precipitation rate, i.e. the average February precipitation (mm/d).}
#'   ...
#' }
#' @source \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/seasonal-monthly-single-levels?tab=overview}
#' @source \url{https://github.com/esowc/UNSEEN-open}
#'
"SEAS5_UK"

#' EOBS February Precipitation over the UK.
#'
#' A dataset containing February average precipitation (mm/d) over 1950-2020, downloaded from \url{https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php}
#' and pre-processed as part of the ECMWF Summer of Weather Code program: \url{https://github.com/esowc/UNSEEN-open}.
#'
#' @format A data frame with 71 rows and 2 variables:
#' \describe{
#'   \item{time}{The year, stored as yyyy-02-28 (or 29,we only have February averages).}
#'   \item{rr}{The rainfall rate, i.e. the average February precipitation (mm/d).}
#'   ...
#' }
#' @source \url{https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php}
#' @source \url{https://github.com/esowc/UNSEEN-open}
"EOBS_UK"
