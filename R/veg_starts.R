#==============================================================================
#
#  internal functions                                              [The Starts]
#
#==============================================================================


#==============================================================================
#
# Start of vegetation period according to Menzel (1997)
#
#==============================================================================

#' Vegetation Start Method "Menzel"
#'
#' The method "Menzel" implements the algorithm described in Menzel (1997).
#' The method is parameterized for 10 common tree species ( *Larix decidua*,
#' *Picea abies (frueh)*, *Picea abies (spaet)*, *Picea abies (noerdl.)*,
#' *Picea omorika*, *Pinus sylvestris*, *Betula pubescens*, *Quercus robur*,
#' *Quercus petraea*, *Fagus sylvatica*). It needs previous year's chill days.
#'
#' @section Calculations:
#'
#' ```
#'  - counting chill days in Nov & Dec of previous year if previous year available else estimate previous year's chill days
#'  - count chill days in current year
#'  - determine critical temperature per days using regression equation fitted by Menzel 1997
#'  - calculate HeatSum starting from Feb
#'  - vegetation start if HeatSum >= criticalTemp
#' ```
#'
#' @references
#' Menzel, A. (1997) Phänologie von Waldbäumen unter sich ändernden Klimabedingungen - Auswertung der Beobachtungen in den Internationalen Phänologischen Gärten und Möglichkeiten der Modellierung von Phänodaten. Forstliche Forschungsberichte München.
#'
#' @md
#' @name method_Menzel
#' @docType data
#' @keywords internal
NULL

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
    CDNovDec <- rbind(c(mean(CDNovDec$CDprev[1:est.prev]), years[1]),
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
  #  (start in Feb and consider only degrees above thresh)
  df$HeatSum <- ifelse(df$month >= 2 & df$Tavg > Mp$TbH, df$Tavg - Mp$TbH, 0)
  df$HeatSum <- utils::stack(tapply(df$HeatSum, df$year, FUN=cumsum))$values

  # vegetation period start if HeatSum >= TCrit
  df$start <- df$HeatSum >= df$TCrit

  # determine first of start candidates
  start <- utils::stack(tapply(df$DOY[df$start],
                        df$year[df$start], FUN=min))$values
  return(start)
}


#==============================================================================
#
# Start of vegetation period according to standard meteo procedure aka 'ETCCDI'
#
#==============================================================================

#' Vegetation Start and End Method "ETCCDI"
#'
#' The `ETCCDI` or `StdMeteo` method was defined by  Expert Team on Climate Change Detection and Indices and is often known as Growing season Length (GSL) and widely used in climate change studies.
#'
#' @section Calculation:
#'
#' ## Implementation of start
#' first span of at least 6 consecutive days with daily mean temperature TG > 5°C
#'
#' ```
#' # mark days warmer than 5°C
#' df$period <- ifelse(df$Tavg > Tmin, 1, 0)
#'
#' # find first six day span per year
#' start <- tapply(df$period, df$year, FUN=function(x){
#'   sixer <- as.numeric(stats::filter(x, rep(1, 6), sides=1))
#'   doy <- which(!is.na(sixer) & sixer == 6)
#'   ifelse(length(doy) == 0, NA, min(doy))
#' })
#' ```
#'
#' ## Implementation of end
#' first span after July 1st of 6 consecutive days with TG < 5°C.
#'
#' ```
#' # 1. July is DOY 182 and DOY 183 in leap years
#' years <- unique(df$year)
#' leap_year <- ifelse((years%%4==0 & years%%100!=0) | years%%400==0, TRUE, FALSE)
#' jul1 <- ifelse(leap_year, 183L, 182L)
#'
#' # mark days colder than 5°C
#' df$period <- ifelse(df$Tavg < Tmin, 1, 0)
#'
#' # find first six day span per year
#' end <- ifelse(leap_year, 366L, 365L)
#' for(i in seq_along(years)){
#'   temp <- df[df$year == years[i] & df$DOY > jul1[i], ]
#'   temp$six <-  as.numeric(stats::filter(temp$period, rep(1, 6), sides=1))
#'   possible.end <- temp[!is.na(temp$six) & temp$six == 6, "DOY"]
#'   if(length(possible.end) > 0)
#'     end[i] <- min(possible.end)
#' }
#' ```
#'
#' @references
#' Definition recommended by the CCl/CLIVAR/JCOMM Expert Team on Climate Change Detection and Indices (ETCCDI) http://etccdi.pacificclimate.org/list_27_indices.shtml http://www.climdex.org/indices.html
#'
#' European Climate Assessment (ECA) http://eca.knmi.nl/indicesextremes/indicesdictionary.php
#'
#' ETCCDI (2009) Climate Change Indices: Definitions of the 27 core indices. http://etccdi.pacificclimate.org/list_27_indices.shtml

#' Frich, P., Alexander, L., Della-Marta, P., Gleason, B., Haylock, M., Klein Tank, A. and Peterson, T. (2002) Observed coherent changes in climatic extremes during the second half of the twentieth century. Climate Research, 19, 193–212. https://doi.org/10.3354/cr019193.
#' http://www.climateknowledge.org/heat_waves/Doc2004_Frich_Extremes_Index_ClimResearch_2002.pdf
#'
#' Zhang, X., Alexander, L., Hegerl, G. C., Jones, P., Tank, A. K., Peterson, T. C., Trewin, B. and Zwiers, F. W. (2011) Indices for monitoring changes in extremes based on daily temperature and precipitation data. Wiley Interdisciplinary Reviews: Climate Change, 2(6), 851–870. https://doi.org/10.1002/wcc.147.
#'
#' @md
#' @keywords internal
#' @name method_ETCCDI
NULL

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
# Start of vegetation period according to DWD (Gooseberry, Ribes uva-crispa)
#  published in the klimaatlas under forest parameters
#
#==============================================================================

#' Vegetation Start Method "Ribes uva-crispa"
#'
#' The method `Ribes uva-crispa` is based on leaf-out of gooseberry (Janssen 2009).
#' It was developed by the Germany's National Meteorological Service
#' (Deutscher Wetterdienst, DWD) and is more robust against early starts than
#' common simple meteorological procedures.
#'
#' @section Calculation:
#'
#' starting from 18th February sum all day degrees above 0 °C (daily average air
#' temperature). Vegetetaion period starts at the day when 164 is crossed.
#'
#' ```
#' # start day: February 18th == 49 doy
#' # only Tavg over 0°C are summed
#' df[df$DOY < 49 | df$Tavg < 0, 'Tavg'] <- 0
#'
#' # find day where cum day degrees cross 164
#' start <- tapply(df$Tavg, df$year, FUN=function(x){
#'   x <- cumsum(x)
#'   min(which(x > 164))
#' })
#' ```
#'
#' @references
#' Janssen, W. (2009)
#'   Definition des Vegetationsanfanges.
#'   \emph{Internal Report, Deutscher Wetterdienst, Abteilung Agrarmeteorologie}.
#'
#' @md
#' @name method_Ribes
#' @keywords internal
NULL

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
