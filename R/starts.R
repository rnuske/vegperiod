#==============================================================================
#
#  internal functions                                              [The Starts]
#
#==============================================================================
#
# Start of vegetation periode according to Menzel (1997)
#
# - counting cold days in Nov & dec of previous year
# - count cold days in current year
# - determine critical temperature per days using regression equation fitted
#   by Menzel 1997
# - calculate HeatSum starting from Feb
# - vegetaion start if HeatSum >= criticalTemp
#
# References:
#   MENZEL,, A. (1997): Phänologie von Waldbäumen unter sich ändernden Klima-
#   bedingungen – Auswertung der Beobachtungen in den Internationalen
#   Phänologischen Gärten und Möglichkeiten der Modellierung von Phänodaten.
#   - Forstliche Forschungsberichte München, 164, 147 S.
#==============================================================================
.start_menzel <- function(df, first.avg=0,
                         species=c("Larix decidua", "Picea abies (frueh)",
                                   "Picea abies (spaet)",
                                   "Picea abies (noerdl.)", "Picea omorika",
                                   "Pinus sylvestris", "Betula pubescens",
                                   "Quercus robur", "Quercus petraea",
                                   "Fagus sylvatica")){
  # Assumptions:
  # - data.frame 'df' contains month, DOY, Tavg
  # - time range at least till DOY 279

  years <- unique(df$year)
  species <- match.arg(species)

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

  # Cold Days
  #----------------------------------------------------------------------------
  # number of cold days in Nov and Dec trigger next years bud burst
  CDNovDec <- stack(tapply(df[(df$month >= 11 & df$Tavg <= Mp$TbCD), "Tavg"],
                           df[(df$month >= 11 & df$Tavg <= Mp$TbCD), "year"],
                           FUN=length))

  # shove the cold days in to next year, where they trigger veg start
  CDNovDec$ind <- as.integer(levels(CDNovDec$ind)) + 1
  names(CDNovDec) <- c("CDprev", "year")

  # handle first year
  if(first.avg == 0){
    # drop first year
    df[df$years != years[1]-1, ]
  } else {
    # mean of "first.avg number of years" as proxy for first year's previous cold days
    CDNovDec <- rbind(c(mean(CDNovDec$CDprev[first.avg]), years[1]),
                      CDNovDec)
  }

  # cumulative sums of cold days per year
  df$CD <- ifelse(df$Tavg <=  Mp$TbCD, 1, 0)
  ColdDays <- stack(tapply(df$CD, df$year, FUN=cumsum))
  names(ColdDays) <- c("CDcumsum", "year")

  # merge and add to workhorse df
  ColdDays <- merge(ColdDays, CDNovDec, by="year", all.x=TRUE)
  df$CD <- ColdDays$CDprev +  ColdDays$CDcumsum
  rm(ColdDays, CDNovDec)

  # determine vegetation start with Menzel's regression
  #----------------------------------------------------------------------------
  # critical Temperature
  df$TCrit <- ifelse(df$CD > 0, Mp$a + Mp$b * log(df$CD), Mp$a)

  # cumsum of degrees above threshold
  #  (start in Feb and use only degree above thresh)
  df$HeatSum <- ifelse(df$month >= 2 & df$Tavg > Mp$TbH, df$Tavg - Mp$TbH, 0)
  df$HeatSum <- stack(tapply(df$HeatSum, df$year, FUN=cumsum))$values

  # start of vegetation periode if HeatSum >= TCrit
  df$start <- df$HeatSum >= df$TCrit

  # determine first start and transfer to result data.frame
  start <- stack(tapply(df$DOY[df$start],
                        df$year[df$start], FUN=min))$values
  return(start)
}

#==============================================================================
#
# Start of vegetation periode according to standard climatology procedure
#
# - temperature above 5°C on 5 consecutive days
#
# Reference:
#  e.g.  FORMAYER, H., HAAS, P., HOFSTÄTTER, M., RADANOVICS, S. & KROMP-KOLB,
#        H. (2007): Räumlich und zeitlich hochaufgelöste Temperaturszenarien
#        für Wien und ausgewählte Analysen bezüglich Adaptionsstrategien.
#        – BOKU-Met Bericht, 82 S.
#==============================================================================
.start_std_climatology <- function(df, Tmin=5){

  # mark days warmer than 5°C
  df$period <- ifelse(df$Tavg > Tmin, 1, 0)

  # find first five day streak per year
  years <- unique(df$year)
  start <- integer(length(years))
  for(i in 1:length(years)){
    temp <- df[df$year == years[i], ]
    temp$five <-  as.numeric(filter(temp$period, rep(1, 5), sides=1))
    possible.start <- temp[!is.na(temp$five) & temp$five == 5, "DOY"]
    if(length(possible.start) > 0)
      start[i] <- min(possible.start)
  }

  return(start)
}
