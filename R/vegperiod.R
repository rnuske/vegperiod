#==============================================================================
# Vegetation Period
#   calculate start and end of vegetation periode
#   start according to Menzel (1997)
#   end von Wilpert, LWF-BROOK90, Albert-Nuske
#
# rewrite 2015-02-15 - 2015-02-26  by Robert Nuske
#  Written on my way to work.
#  Thanks to good working conditions (no internet) in the ICE.
#==============================================================================

#' Determine vegetation period.
#'
#' \code{vegperiod} calculates start and end day of vegetation period based on
#' daily average air temperature and the day of the year (DOY).
#'
#' Implemented are at moment the simple standard procedure used in climatology
#' (FORMAYER et al., 2007), vegetation start according to Menzel (1997) and
#' vegetation end accodring to von Wilpert (1990), a reimplementation of
#' vonWilpert found in LWF-BROOK90 and a very simple method by Nuske & Albert.
#'
#' The LWF-BROOK90 reimplementation was tested against the original code using
#' data from a range of different climate stations.
#'
#' @param dates vector calender dates as object of class \code{Date}.
#'        Same length as Tavg.
#' @param Tavg vector of daily average air temperatures. Same length as dates.
#' @param species name of tree species. Needed for start of vegetation
#'        calculation according to Menzel (1997). Must be one of "Larix decidua",
#'        "Picea abies (frueh)", "Picea abies (spaet)",
#'        "Picea abies (noerdl.)", "Picea omorika", "Pinus sylvestris",
#'        "Betula pubescens", "Quercus robur", "Quercus petraea",
#'        "Fagus sylvatica".
#' @param method.start name of method used to calculate the start of vegetation
#'        period. At the moment one of "StdClimatology", "Menzel".
#' @param method.end name method used to calculate the end of vegetaion period.
#'        At the moment one of "vonWilpert", "LWF-BROOK90", "NuskeAlbert" and
#'        "StdClimatology".
#' @param first.avg integer. Guesstimate number of previous years'
#'        \code{cold days} (needed by Menzel, 1997) from the \code{first.avg}
#'        first years. If \code{first.avg=0}, first year is only used for
#'        calculating \code{cold days} and discarded afterwards ->
#'        losing one year from the provided data.
#' @param temp.sum boolean. Return the sum of day degrees for the vegetation
#'   period.
#'
#' @return Returns a data.frame with year and DOY of start and end day of
#'   vegetation period. If \code{temp.sum=TRUE}, the data.frame contains an
#'   additional column with the sum of day degree within the vegetation period.
#'
#' @export
#'
#' @examples
#' data(goe)
#' vegperiod(dates=goe$date, Tavg=goe$t, species="Picea abies (frueh)",
#'   first.avg=5, method.start="Menzel", method.end="vonWilpert")
#'
vegperiod <- function(dates, Tavg, species, method.start, method.end,
                       first.avg=0, temp.sum=FALSE){

  # ToDo: accept a dataframe (dates, Tavg), (year, month, DOY, Tavg)
  #       or two vectors


  # Checks
  #----------------------------------------------------------------------------
  method.start <- match.arg(method.start,
                          choices=c('StdClimatology', 'Menzel'))
  method.end <- match.arg(method.end,
                          choices=c('StdClimatology', 'vonWilpert',
                                    'LWF-BROOK90', 'NuskeAlbert'))

  if(length(dates) != length(Tavg))
    stop("The arguments dates and Tavg must be of same length!")

  # put together the workhorse data.frame
  df <- data.frame(year  = as.integer(format(dates, "%Y")),
                   month = as.integer(format(dates, "%m")),
                   DOY   = as.integer(format(dates, "%j")),
                   Tavg)
  rm(dates, Tavg)

  # Last day according to vonWilpert (5th October in leap years)
  LastDOY <- 279

  # determine leap years
  years <- unique(df$year)
  leap <- ifelse((years%%4==0 & years%%100!=0) | years%%400==0, TRUE, FALSE)

  # Is first.avg in valid range?
  first.avg = as.integer(first.avg)
  if(first.avg < 0 || first.avg > length(years))
    stop("first.avg must be >= 0 and <= number of years.")

  # Do the provided years have the correct lengths (leap years)?
  if(first.avg == 0){
    # drop first year -> only Nov+Dec from first year required
    leap <- leap[-1]
    year.length <- c(61, 365 + leap[-length(leap)], LastDOY)
  } else {
    # guesstimate first year's NovDecCD -> full first year required
    year.length <- c(365 + leap[-length(leap)], LastDOY)
  }
  if(!all(tapply(df$DOY, df$year, FUN=length) >= year.length))
    stop(paste("Years in argument 'dates' do not have expected length."))

  if(first.avg == 0 &&
       length(df[df$year == years[1] & df$month >= 11, "DOY"]) != 61)
    stop("First year must at least contain full November & December")


  # calculating start and end
  #----------------------------------------------------------------------------
  start <- switch(method.start,
                  'StdClimatology' = .start_std_climatology(df),
                  'Menzel'         = .start_menzel(df=df, first.avg=first.avg,
                                                   species=species))

  # drop first year if only used for getting cold days
  if(first.avg == 0){
    df[df$years != years[1], ]
    years <- years[-1]
  }

  end <- switch(method.end,
                'vonWilpert'     = .end_vonWilpert(df),
                'LWF-BROOK90'    = .end_LWF_BROOK90(df),
                'NuskeAlbert'    = .end_NuskeAlbert(df, start),
                'StdClimatology' = .end_std_climatology(df))


  # collect results           when indicated calculate day degrees in vegperiod
  #----------------------------------------------------------------------------
  res <- data.frame(year=years, start=start, end=end)

  if(temp.sum){
    res$temp.sum <- numeric(nrow(res))
    for(i in 1:nrow(res)){
      res$temp.sums[i] <- sum(df$Tavg[df$year == years[i] &
                                        df$DOY >= start[i] &
                                        df$DOY <= end[i]])
    }
  }

  return(res)
}
