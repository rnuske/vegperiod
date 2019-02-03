#==============================================================================
#
#  internal functions                                                [The Ends]
#
#==============================================================================


#==============================================================================
#
# End of vegetation priod according to von Wilpert (1990)
#
# orthodox are 3 criteria: short day, temperature & drought criterion
# we consider -as usual- only short day and temperature citerion
#
# temperature criterion
# - 7 day moving average of daily mean temperatures
#     at least 5 consecutive days under 10°C
# - if afterwards more than 5 consecutive days 7 day moving average over 10°C
#     vegetation period gets restarted
#
# short day criterion
# - last day of the vegetation period is DOY 279 (5th of October in leap years)
#
# Reference:
#   von Wilpert, K (1990) Die Jahrringstruktur von Fichten in Abhängigkeit vom
#   Bodenwasserhaushalt auf Pseudogley un Parabraunerde: Ein Methodenkonzept
#   zur Erfassung standortsspezifischer Wasserstreßdispostion. Freiburger
#   Bodenkundliche Abhandlungen: 24. ISSN: 0344-2691. Seiten: 106-108
#==============================================================================
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
# End of vegetation periode according to LWF-BROOK90
#
# - starting search at June 1st
# - propose end if 7-day moving average temperature is below 10°C
#   on 5 consecutive days
# - restart search for end if there is a warm periode (7-day moving average
#   temperature above 10°C for 5 consecutive days
# - nevertheless the vegetation periode stops latest at DOY 279
#
# Reference (commonly used):
#  Hammel, K. & Kennel, M. (2001) Charakterisierung und Analyse der
#  Wasserverfügbarkeit und des Wasserhaushalts von Waldstandorten in Bayern mit
#  dem Simulationsmodell BROOK90. Forstliche Forschungsberichte München, 185.
#==============================================================================
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
# End of vegetation priod according to Nuske & Albert
#
# 2 criteria: temperature & short day criterion
#
# temperature criterion
# 7 day moving average of daily mean temperatures
# end if 5 consecutive days under under 5° C
# start the search at 1st of July / vegetation start
#
# short day criterion
# last day of the vegetation period is 5th of October
#
# Reference for Tmin=5:
#  Walther, A. and Linderholm, H.W. (2006) A comparison of growing season
#  indices for the Greater Baltic Area. Int J Biometeorol 51: 107-118
#==============================================================================
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
# End of vegetation periode according to standard meteo procedure
#  aka 'ETCCDI' indix
#
# first span of at least 6 consecutive days with daily mean temperature TG > 5°C
# and first span after July 1st of 6 consecutive days with TG < 5°C.
#
# Reference:
#  often known as Growing season Length (GSL) e.g.
#   - Definition recommended by the CCl/CLIVAR/JCOMM Expert Team on Climate
#     Change Detection and Indices (ETCCDI)
#     http://etccdi.pacificclimate.org/list_27_indices.shtml
#     http://www.climdex.org/indices.html
#   - European Climate Assessment (ECA)
#     http://eca.knmi.nl/indicesextremes/indicesdictionary.php
#   - Frich, P. et al. (2002) Observed coherent changes in climatic extremes
#     during the second half of the twentieth century. Climate Research (19):
#     193-212.   http://www.climateknowledge.org/heat_waves/
#     Doc2004_Frich_Extremes_Index_ClimResearch_2002.pdf
#==============================================================================
.end_std_meteo <- function(df, Tmin=5){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - full years

  # 1. July is DOY 182 and DOY 183 in leap years
  years <- unique(df$year)
  jul1 <- ifelse((years%%4==0 & years%%100!=0) | years%%400==0, 183L, 182L)

  # mark days colder than 5°C
  df$period <- ifelse(df$Tavg < Tmin, 1, 0)

  # find first six day span per year
  end <- integer(length(years))
  for(i in seq_along(years)){
    temp <- df[df$year == years[i] & df$DOY > jul1[i], ]
    temp$six <-  as.numeric(stats::filter(temp$period, rep(1, 6), sides=1))
    possible.end <- temp[!is.na(temp$six) & temp$six == 6, "DOY"]
    if(length(possible.end) > 0)
      end[i] <- min(possible.end)
  }

  return(end)
}
