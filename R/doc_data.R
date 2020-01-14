#' Ten years of daily average air temperature
#'
#' A dataset containing ten years (2001-01-01 - 2010-12-31) of average air
#' temperature measured at the weather station Goettingen. The data is the result
#' of a reanalysis of observed data using the STARS model (cf. Orlowsky et al.
#' 2008). They were provided by Prof. Dr. Peter Werner (PIK).
#'
#' @format A data frame with 3652 rows and 6 variables:
#'   \tabular{ll}{
#'    \strong{Variable} \tab \strong{Description}\cr
#'    date              \tab calendar date as object of class Date\cr
#'    year              \tab years as integer\cr
#'    month             \tab month as integer \cr
#'    day               \tab day of month as integer\cr
#'    doy               \tab day of year as integer \cr
#'    t                 \tab average air temperature
#' }
#' @source Prof. Dr. Peter Werner (PIK)
#' @references
#'   Orlowsky, B., Gerstengarbe, F. W., Werner, P. C. (2008)
#'   A resampling scheme for regional climate simulations and its performance
#'   compared to a dynamical RCM.
#'   \emph{Theoretical and Applied Climatology}, \bold{92}(3–4), 209--223.
#'   \doi{10.1007/s00704-007-0352-y}
#' @name goe
#' @usage data(goe)
#' @examples
#' data(goe)
#' str(goe)
NULL
