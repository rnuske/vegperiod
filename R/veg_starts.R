#==============================================================================
#
#  internal functions                                              [The Starts]
#
#==============================================================================
#
# Start of vegetation period according to Menzel (1997)
#
# - counting chill days in Nov & dec of previous year
# - count chill days in current year
# - determine critical temperature per days using regression equation fitted
#   by Menzel 1997
# - calculate HeatSum starting from Feb
# - vegetaion start if HeatSum >= criticalTemp
#
# References:
#   MENZEL,, A. (1997) Phänologie von Waldbäumen unter sich ändernden Klima-
#   bedingungen – Auswertung der Beobachtungen in den Internationalen
#   Phänologischen Gärten und Möglichkeiten der Modellierung von Phänodaten.
#   - Forstliche Forschungsberichte München, 164, 147 S.
#==============================================================================
.start_menzel <- function(df, est.prev, species){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - full years
  # - last year at least till DOY 279
  # - and previous Nov & Dec if est.prev==0
  # - est.prev >= 0 and <= number of years
  # - species contains a known name

  years <- unique(df$year)

  # Menzel's Parameter
  Mp <- switch(species,
               "Larix decidua"         = list(TbCD=7, TbH=3, a=1372, b=-246),
               "Picea abies (frueh)"   = list(TbCD=9, TbH=4, a=1848, b=-317),
               "Picea abies (spaet)"   = list(TbCD=9, TbH=5, a=1616, b=-274),
               "Picea abies (noerdl.)" = list(TbCD=9, TbH=4, a=2084, b=-350),
               "Picea omorika"         = list(TbCD=7, TbH=3, a=2833, b=-484),
               "Pinus sylvestris"      = list(TbCD=9, TbH=5, a=1395, b=-223),
               "Betula pubescens"      = list(TbCD=9, TbH=5, a=1438, b=-261),
               "Quercus robur"         = list(TbCD=9, TbH=4, a=1748, b=-298),
               "Quercus petraea"       = list(TbCD=9, TbH=3, a=1741, b=-282),
               "Fagus sylvatica"       = list(TbCD=9, TbH=6, a=1922, b=-348))

  # Chill Days
  #----------------------------------------------------------------------------
  # number of chill days in Nov and Dec trigger next years bud burst
  CDNovDec <- utils::stack(tapply(df[(df$month >= 11 & df$Tavg <= Mp$TbCD), "Tavg"],
                           df[(df$month >= 11 & df$Tavg <= Mp$TbCD), "year"],
                           FUN=length))

  # shove the chill days in to next year, where they trigger veg start
  CDNovDec$ind <- as.integer(levels(CDNovDec$ind)) + 1
  names(CDNovDec) <- c("CDprev", "year")

  # handle first year
  if(est.prev == 0){
    # drop first year
    df <- df[df$year != years[1], ]
  } else {
    # mean "est.prev number of years" as proxy for first year's previous chill days
    CDNovDec <- rbind(c(mean(CDNovDec$CDprev[est.prev]), years[1]),
                      CDNovDec)
  }

  # cumulative sums of chill days per year
  df$CD <- ifelse(df$Tavg <=  Mp$TbCD, 1, 0)
  ChillDays <- utils::stack(tapply(df$CD, df$year, FUN=cumsum))
  names(ChillDays) <- c("CDcumsum", "year")

  # merge and add to workhorse df
  ChillDays <- merge(ChillDays, CDNovDec, by="year", all.x=TRUE)
  df$CD <- ChillDays$CDprev +  ChillDays$CDcumsum
  rm(ChillDays, CDNovDec)

  # determine vegetation start with Menzel's regression
  #----------------------------------------------------------------------------
  # critical Temperature
  df$TCrit <- ifelse(df$CD > 0, Mp$a + Mp$b * log(df$CD), Mp$a)

  # cumsum of degrees above threshold
  #  (start in Feb and use only degree above thresh)
  df$HeatSum <- ifelse(df$month >= 2 & df$Tavg > Mp$TbH, df$Tavg - Mp$TbH, 0)
  df$HeatSum <- utils::stack(tapply(df$HeatSum, df$year, FUN=cumsum))$values

  # start of vegetation periode if HeatSum >= TCrit
  df$start <- df$HeatSum >= df$TCrit

  # determine first start and transfer to result data.frame
  start <- utils::stack(tapply(df$DOY[df$start],
                        df$year[df$start], FUN=min))$values
  return(start)
}


#==============================================================================
#
# Start of vegetation periode according to standard meteo procedure
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
.start_std_meteo <- function(df, Tmin=5){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - full years

  # mark days warmer than 5°C
  df$period <- ifelse(df$Tavg > Tmin, 1, 0)

  # find first six day span per year
  start <- tapply(df$period, df$year, FUN=function(x){
    sixer <- as.numeric(stats::filter(x, rep(1, 6), sides=1))
    doy <- which(!is.na(sixer) & sixer == 6)
    ifelse(length(doy) == 0, NA, min(doy))
  })

  return(as.vector(start))
}


#==============================================================================
#
# Start of vegetation periode according to DWD (Gooseberry, Ribes uva-crispa)
#  published in the klimaatlas under forest parameters
#
# starting from 18th February sum all day degrees above 0 °C (daily average air
#   temperature). Vegetetaion period starts at the day when 164 is crossed.
#
# Devloped by Wolfgang Janssen, Deutscher Wetterdienst, Abt. Agrarmeteorologie
#==============================================================================

.start_ribes <- function(df){
  # Assumptions:
  # - data.frame 'df' contains year, DOY, Tavg
  # - full years

  # start day: February 18th == 49 doy
  # only Tavg over 0°C are summed
  df[df$DOY < 49 | df$Tavg < 0, 'Tavg'] <- 0

  # find day where cum day degrees cross 164
  start <- tapply(df$Tavg, df$year, FUN=function(x){
    x <- cumsum(x)
    min(which(x > 164))
  })
  return(as.vector(start))
}
