#==============================================================================
#
#  internal functions                                                [The Ends]
#
#==============================================================================


#==============================================================================
#
# End of vegetation period according to von Wilpert (1990)
#
#==============================================================================

#' Vegetation End Method "vonWilpert"
#'
#' The method `vonWilpert` is based on von Wilpert (1990). It was originally
#' developed for "Picea abies" in the Black Forest but is commonly used for all
#' tree species throughout Germany. As usual, the rules regarding the soilmatrix
#' are neglected in this implementation.
#'
#' @section Calculations:
#'
#' Orthodox are 3 criteria: **short day**, **temperature** and **drought criterion**
#' we consider -as usual- only short day and temperature citerion
#'
#' ## Temperature criterion:
#'  - 7 day moving average of daily mean temperatures
#'      at least 5 consecutive days under 10°C
#'  - if afterwards more than 5 consecutive days 7 day moving average over 10°C
#'     vegetation period gets restarted
#'
#' ## Short day criterion
#' - last day of the vegetation period is DOY 279 (5th of October in leap years)
#
#' @references
#' von Wilpert, K. (1990)
#'   Die Jahrringstruktur von Fichten in Abhängigkeit vom Bodenwasserhaushalt
#'   auf Pseudogley und Parabraunerde: Ein Methodenkonzept zur Erfassung
#'   standortsspezifischer Wasserstreßdispostion.
#'   \emph{Freiburger Bodenkundliche Abhandlungen}. Pages 106--108.
#'
#' @md
#' @name method_vonWilpert
#' @keywords internal
NULL

.end_vonWilpert <- function(df, Treshold=10, LastDOY=279){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - DOYs at least till LastDOY
  LastDOY <- as.integer(LastDOY)

  # Preparation
  #----------------------------------------------------------------------------
  # moving average with windows size 7 (symetric)
  df$TmovAvg <- as.numeric(stats::filter(df$Tavg, rep(1/7,7), sides=2))

  # mark periods ('cold', 'warm') before LastDOY and 'ignore' the rest
  df$period <- ifelse(df$DOY > LastDOY, 'ignore',
                      ifelse(df$TmovAvg < Treshold, 'cold', 'warm'))

  # determine continous strides of warm/cold by using run length encoding
  # cold period if stride at least 5
  # warm period if stride more than 5
  temp <- rle(df$period)
  temp$values[temp$lengths < 5] <- 'ignore'
  temp$values[temp$values == 'warm' & temp$lengths < 6] <- 'ignore'
  temp$values[is.na(temp$values)] <- 'ignore'
  df$period <- inverse.rle(temp)

  # Searching for the end
  #----------------------------------------------------------------------------
  # last warm period per year
  last.warm <- tapply(df$DOY[df$period == 'warm'],
                      df$year[df$period == 'warm'],
                      FUN=max)
  last.warm <- data.frame(year=as.integer(row.names(last.warm)), DOY=last.warm)

  # loop over all years
  years <- unique(df$year)
  end <- sapply(years,
                # cold period after last.warm? (yes: min(cold)+4; no: LastDOY)
                FUN=function(x) {
                  # cold period after warm period?
                  temp <- df[df$year == x & df$period == 'cold', 'DOY']
                  temp <- temp[temp > last.warm[last.warm$year == x, 'DOY']]
                  if(length(temp) > 0){
                    # 5th day of cold period is the end
                    min(temp) + 4L
                  } else {
                    # no colds after last.warm ->  default end
                    LastDOY
                  }
                }
  )
  return(end)
}


#==============================================================================
#
# End of vegetation period according to LWF-BROOK90
#
#==============================================================================

#' Vegetation End Method "LWF-BROOK90"
#'
#' The method `LWF-BROOK90` is -for the sake of convenience- a reimplementation
#' of the LWF-BROOK90 VBA (version 3.4) variant of "vonWilpert" (Hammel and
#' Kennel 2001). Their interpretation of von Wilpert (1990) and the somewhat
#' lower precision of VBA was mimicked.
#'
#' @section Calculations:
#'
#' ```
#' - starting search at June 1st
#' - propose end if 7-day moving average temperature is below 10°C
#'   on 5 consecutive days
#' - restart search for end if there is a warm periode (7-day moving average
#'   temperature above 10°C for 5 consecutive days
#' - nevertheless the vegetation periode stops latest at DOY 279
#' ```
#'
#' @references
#' Hammel, K. and Kennel, M. (2001)
#'   Charakterisierung und Analyse der Wasserverfügbarkeit und des
#'   Wasserhaushalts von Waldstandorten in Bayern mit dem Simulationsmodell
#'   BROOK90.
#'   \emph{Forstliche Forschungsberichte München}.
#'
#' von Wilpert, K. (1990)
#'   Die Jahrringstruktur von Fichten in Abhängigkeit vom Bodenwasserhaushalt
#'   auf Pseudogley und Parabraunerde: Ein Methodenkonzept zur Erfassung
#'   standortsspezifischer Wasserstreßdispostion.
#'   \emph{Freiburger Bodenkundliche Abhandlungen}.
#'
#' @md
#' @name method_LWF-BROOK90
#' @keywords internal
NULL

.end_LWF_BROOK90 <- function(df, Tmin=10, LastDOY=279){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - DOYs at least till 279
  LastDOY <- as.integer(LastDOY)

  # moving average with windows size 7 (only backward looking)
  #   round to mimic lower precision of VBA version
  #   (14 digits might be okay, 10 is on the save side)
  df$movAvgT <- round(as.numeric(stats::filter(df$Tavg, rep(1/7, 7), sides=1)), 10)

  # introduce 2 counters 'cold' and 'warm'  ('ignore' the rest)
  df$period <- ifelse(df$month > 5 & df$DOY <= LastDOY,
                      ifelse(df$movAvgT < Tmin, 'cold', 'warm'),
                      'ignore')

  # reset one counter if the other enters the stage by using run length encoding
  # cold/warm periode only valid if >4    => ignore all counts below 5
  temp <- rle(df$period)
  temp$values[temp$lengths < 5] <- 'ignore'
  temp$values[is.na(temp$values)] <- 'ignore'
  df$period <- inverse.rle(temp)


  # Searching the end
  #----------------------------------------------------------------------------
  # last warm period per year
  last.warm <- tapply(df$DOY[df$period == 'warm'],
                      df$year[df$period == 'warm'],
                      FUN=max)
  last.warm <- data.frame(year=as.integer(row.names(last.warm)), DOY=last.warm)

  # loop over all years
  years <- unique(df$year)
  end <- sapply(years,
                # cold period after last.warm? (yes: min(cold)+4; no: LastDOY)
                FUN=function(x) {
                  # cold period after warm period?
                  temp <- df[df$year == x & df$period == 'cold', 'DOY']
                  temp <- temp[temp > last.warm[last.warm$year == x, 'DOY']]
                  if(length(temp) > 0){
                    # 5th day of cold period is the end
                    min(temp) + 4L
                  } else {
                    # no colds after last.warm ->  default end
                    LastDOY
                  }
                }
  )
  return(end)
}



#==============================================================================
#
# End of vegetation period according to Nuske & Albert
#
#==============================================================================

#' Vegetation End Method "NuskeAlbert"
#'
#' The method `NuskeAlbert` provides a very simple method which is inspired by
#' standard climatological procedures but employs a 7 day moving average and
#' a 5 °C threshold (cf. Walther and Linderholm 2006).
#'
#' @section Calculations:
#' 2 criteria: temperature & short day criterion
#'
#' ## Temperature criterion
#' 7 day moving average of daily mean temperatures
#' end if 5 consecutive days under under 5° C
#' start the search at 1st of July / vegetation start
#'
#' ## Short day criterion
#' last day of the vegetation period is 5th of October
#'
#' @md
#' @references
#'   Walther, A. and Linderholm, H. W. (2006)
#'   A comparison of growing season indices for the Greater Baltic Area.
#'   \emph{International Journal of Biometeorology}, \bold{51}(2), 107--118.
#'   \doi{10.1007/s00484-006-0048-5}.
#'
#' @name method_NuskeAlbert
#' @keywords internal
NULL

.end_NuskeAlbert <- function(df, start, Tmin=5){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - time range covers at least veg.start/jul1 till oct5 (save side DOY 1-279)


  # Calculate correct DOYs
  # 1. July is DOY 182 and DOY 183 in leap years
  # 5. October is DOY 278 and 279 in leap years
  years <- unique(df$year)
  jul1 <- ifelse((years%%4==0 & years%%100!=0) | years%%400==0, 183L, 182L)

  searchstart <- ifelse(!is.na(start) & start > jul1, start, jul1)
  oct5 <- ifelse((years%%4==0 & years%%100!=0) | years%%400==0, 279L, 278L)

  # 7 day moving average under 5° and after 1 July / vegperiod start
  # moving average with windows size 7 (symetric window)
  df$TmovAvg <- as.numeric(stats::filter(df$Tavg, rep(1/7,7), sides=2))
  df$period <- ifelse(df$TmovAvg < Tmin, 1, 0)

  # ends on the 5th day  if no 5 day streak end on 5oct
  end <- oct5
  for(i in seq_along(years)){
    temp <- df[df$year == years[i] &
               df$DOY  >= searchstart[i] &
               df$DOY  <= oct5[i],
              ]

    temp$five <-  as.numeric(stats::filter(temp$period, rep(1, 5), sides=1))
    possible.end <- temp[!is.na(temp$five) & temp$five == 5, "DOY"]
    if(length(possible.end) > 0)
      end[i] <- min(possible.end)
  }
  return(end)
}


#==============================================================================
#
# End of vegetation period according to standard meteo procedure aka 'ETCCDI'
#
# first span after July 1st of 6 consecutive days with TG < 5°C.
#
#==============================================================================
.end_std_meteo <- function(df, Tmin=5){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - full years

  # 1. July is DOY 182 and DOY 183 in leap years
  years <- unique(df$year)
  leap_year <- ifelse((years%%4==0 & years%%100!=0) | years%%400==0, TRUE, FALSE)
  jul1 <- ifelse(leap_year, 183L, 182L)

  # mark days colder than 5°C
  df$period <- ifelse(df$Tavg < Tmin, 1, 0)

  # find first six day span per year
  end <- ifelse(leap_year, 366L, 365L)
  for(i in seq_along(years)){
    temp <- df[df$year == years[i] & df$DOY > jul1[i], ]
    temp$six <-  as.numeric(stats::filter(temp$period, rep(1, 6), sides=1))
    possible.end <- temp[!is.na(temp$six) & temp$six == 6, "DOY"]
    if(length(possible.end) > 0)
      end[i] <- min(possible.end)
  }

  return(end)
}
