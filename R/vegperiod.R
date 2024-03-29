#==============================================================================
# Vegetation Period
#   calculate start and end of vegetation periode
#
# rewrite 2015-02-15 - 2015-02-26  by Robert Nuske
#  Written on my way to work.
#  Thanks to good working conditions (no internet) in the ICE.
#==============================================================================

#' Determine vegetation period
#'
#' Calculate start and end date of vegetation periods based on daily average
#' air temperature and the day of the year (DOY).
#' The sum of day degrees within the vegetation period is included for convenience.
#'
#'
#' @details
#' Common methods for determining the onset and end of thermal vegetation
#' periods are provided, for details see next sections. Popular choices with
#' regard to forest trees in Germany are `Menzel` and `vonWilpert`. Climate
#' change impact studies at NW-FVA are frequently conducted using `Menzel` with
#' "Picea abies (frueh)" and `NuskeAlbert` for all tree species; with tree
#' species specifics accounted for in subsequent statistical models.
#'
#' ## Start methods:
#' The method **[`Menzel`][method_Menzel]** implements the algorithm described in
#' Menzel (1997). The method is parameterized for 10 common tree species. It
#' needs previous year's chill days. **[`ETCCDI`][method_ETCCDI]** resp.
#' `StdMeteo` is a simple threshold based procedure as defined by the
#' Expert Team on Climate Change Detection and Indices (cf. ETCCDI 2009, Frich
#' et al. 2002, Zhang et al. 2011) leading to quite early vegetation starts.
#' This method is widely used in climate change studies. The method
#' **[`Ribes uva-crispa`][method_Ribes]** is based on leaf-out of gooseberry (Janssen
#' 2009). It was developed by the Germany's National Meteorological Service
#' (Deutscher Wetterdienst, DWD) and is more robust against early starts than
#' common simple meteorological procedures.
#'
#' ## End methods:
#' The end method **[`vonWilpert`][method_vonWilpert]** is based on von Wilpert (1990). It
#' was originally developed for *Picea abies* in the Black Forest but is
#' commonly used for all tree species throughout Germany. As usual, the rules
#' regarding the soilmatrix are neglected in this implementation.
#' **[`LWF-BROOK90`][method_LWF-BROOK90]** is -for the sake of convenience- a
#' reimplementation of the LWF-BROOK90 VBA (version 3.4) variant of "vonWilpert"
#' (Hammel and Kennel 2001). Their interpretation of von Wilpert (1990) and the
#' somewhat lower precision of VBA was mimicked. **[`NuskeAlbert`][method_NuskeAlbert]**
#' provide a very simple method which is inspired by standard climatological
#' procedures but employs a 7 day moving average and a 5 °C threshold (cf.
#' Walther and Linderholm 2006). **[`ETCCDI`][method_ETCCDI]** resp. `StdMeteo`
#' is a simple threshold based procedure as defined by the Expert Team on
#' Climate Change Detection and Indices (cf. ETCCDI 2009, Frich et al. 2002,
#' Zhang et al. 2011) leading to quite late vegetation ends.
#'
#' @param dates vector of calendar dates (objects of class `Date` or something
#'   understood by [as.Date()]). Must contain entire years if `est.prev > 0`
#'   else the first year may comprise only November and December.
#' @param Tavg vector of daily average air temperatures in degree Celsius.
#'   Same length as `dates`.
#' @param start.method name of method to use for vegetation start. One of
#'   `"Menzel"` (needs additional argument `species`, see below), `"StdMeteo"`
#'   resp. `"ETCCDI"`, `"Ribes uva-crispa"`. Can be abbreviated (partial
#'   matching). For further discussion see Details.
#' @param end.method name of method to use for vegetation end. One of
#'   `"vonWilpert"`, `"LWF-BROOK90"`, `"NuskeAlbert"` and `"StdMeteo"` resp.
#'   `"ETCCDI"`. Can be abbreviated (partial matching). For further discussion
#'   see Details.
#' @param Tsum.out boolean. Return the sum of daily mean temperatures above
#'   `Tsum.crit` within vegetation period, also known as growing day degrees.
#' @param Tsum.crit threshold for sum of day degrees. Only daily mean temperatures
#'   `> Tsum.crit` will be tallied. The default of `0` prevents negative
#'   daily temperatures from reducing the sum. Climate change studies often use
#'   a threshold of `5`.
#' @param species name of tree species (required if `start.method="Menzel"`
#'   ignored otherwise).
#'
#'   Must be one of `"Larix decidua"`, `"Picea abies (frueh)"`,
#'   `"Picea abies (spaet)"`, `"Picea abies (noerdl.)"`, `"Picea omorika"`,
#'   `"Pinus sylvestris"`, `"Betula pubescens"`, `"Quercus robur"`,
#'   `"Quercus petraea"`, `"Fagus sylvatica"`.
#' @param est.prev number of years to **est**imate **prev**ious year's chill
#'   days for the first year (required if `start.method="Menzel"` ignored
#'   otherwise).
#'
#'   `Menzel` requires the number of chill days of previous November and
#'   December. If `est.prev = 0` the first year is used to get the previous
#'   year's chill days and dropped afterwards. Thus, a year from the time
#'   series is lost. To avoid losing a year, `est.prev = n` estimates the
#'   previous year's chill days for the first year from the average of `n`
#'   first years of the time series.
#' @param check.data Performs plausibility checks on the temperature data to
#'   ensure that the temperatures have not been multiplied by ten.
#'   Plausible range is -35 to +40°C.
#'
#' @return A data.frame with year and DOY of start and end day of
#'   vegetation period. If `Tsum.out=TRUE`, the data.frame contains an
#'   additional column with the sum of day degrees within vegetation periods.
#'
#' @references
#'   ETCCDI (2009)
#'   Climate Change Indices: Definitions of the 27 core indices.
#'   \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#'
#'   Frich, P., Alexander, L., Della-Marta, P., Gleason, B., Haylock, M.,
#'   Klein Tank, A. and Peterson, T. (2002)
#'   Observed coherent changes in climatic extremes during the second half of
#'   the twentieth century.
#'   \emph{Climate Research}, \bold{19}, 193--212.
#'   \doi{10.3354/cr019193}.
#'
#'   Hammel, K. and Kennel, M. (2001)
#'   Charakterisierung und Analyse der Wasserverfügbarkeit und des
#'   Wasserhaushalts von Waldstandorten in Bayern mit dem Simulationsmodell
#'   BROOK90.
#'   \emph{Forstliche Forschungsberichte München}.
#'
#'   Janssen, W. (2009)
#'   Definition des Vegetationsanfanges.
#'   \emph{Internal Report, Deutscher Wetterdienst, Abteilung Agrarmeteorologie}.
#'
#'   Menzel, A. (1997)
#'   Phänologie von Waldbäumen unter sich ändernden Klimabedingungen -
#'   Auswertung der Beobachtungen in den Internationalen Phänologischen Gärten
#'   und Möglichkeiten der Modellierung von Phänodaten.
#'   \emph{Forstliche Forschungsberichte München}.
#'
#'   von Wilpert, K. (1990)
#'   Die Jahrringstruktur von Fichten in Abhängigkeit vom Bodenwasserhaushalt
#'   auf Pseudogley und Parabraunerde: Ein Methodenkonzept zur Erfassung
#'   standortsspezifischer Wasserstreßdispostion.
#'   \emph{Freiburger Bodenkundliche Abhandlungen}.
#'
#'   Walther, A. and Linderholm, H. W. (2006)
#'   A comparison of growing season indices for the Greater Baltic Area.
#'   \emph{International Journal of Biometeorology}, \bold{51}(2), 107--118.
#'   \doi{10.1007/s00484-006-0048-5}.
#'
#'   Zhang, X., Alexander, L., Hegerl, G. C., Jones, P., Tank, A. K.,
#'   Peterson, T. C., Trewin, B. and Zwiers, F. W. (2011)
#'   Indices for monitoring changes in extremes based on daily temperature and
#'   precipitation data.
#'   \emph{Wiley Interdisciplinary Reviews: Climate Change}, \bold{2}(6), 851--870.
#'   \doi{10.1002/wcc.147}.
#'
#' @examples
#' data(goe)
#' vegperiod(dates=goe$date, Tavg=goe$t,
#'           start.method="Menzel", end.method="vonWilpert",
#'           species="Picea abies (frueh)", est.prev=5)
#'
#' # take chill days from first year, which is then dropped
#' vegperiod(dates=goe$date, Tavg=goe$t, start="Menzel", end="vonWilpert",
#'           species="Picea abies (frueh)", est.prev=0)
#'
#' # add column with sum of day degrees in vegetation periods
#' vegperiod(dates=goe$date, Tavg=goe$t, Tsum.out=TRUE,
#'           start="StdMeteo", end="StdMeteo")
#' @md
#' @export
vegperiod <- function(dates, Tavg, start.method, end.method, Tsum.out=FALSE,
                      Tsum.crit=0, species=NULL, est.prev=0, check.data=TRUE){
  # Checks
  #----------------------------------------------------------------------------
  start.method <- match.arg(start.method,
                            choices=c('Menzel', 'StdMeteo', 'ETCCDI',
                                      'Ribes uva-crispa'))
  end.method <- match.arg(end.method,
                          choices=c('vonWilpert', 'LWF-BROOK90', 'NuskeAlbert',
                                    'StdMeteo', 'ETCCDI'))

  if(start.method == 'Menzel'){
    possible.species <- c("Larix decidua", "Picea abies (frueh)",
                          "Picea abies (spaet)", "Picea abies (noerdl.)",
                          "Picea omorika", "Pinus sylvestris",
                          "Betula pubescens", "Quercus robur",
                          "Quercus petraea", "Fagus sylvatica")
    if(is.null(species))
      stop(paste0("If start.method='Menzel', species must be one of '",
                  paste(possible.species, collapse="', '"), "'."))

    species <- match.arg(species, choices=possible.species)
  }

  if(length(dates) != length(Tavg))
    stop("The arguments dates and Tavg must be of same length!")

  # are the temperatures sound
  if(isTRUE(check.data)){
  minimax <- range(Tavg)
  if(minimax[1] < -35 | minimax[2] > 40)
    stop("Your input temperature data exceeds the plausible range of -35 to +40\u00b0C.\n",
         "You may want to double check your data. ",
         "If you still want to use the given data, set 'check.data' to FALSE")
  }

  if(!inherits(dates, "Date")){
    tryCatch(dates <- as.Date(dates),
             error=function(c) stop(paste("'dates' could not be coerced to",
                                          "class Date by as.Date(dates)."),
                                    call.=FALSE)
    )
  }

  # check for consecutive days
  if(any(diff(dates) != 1))
    stop("The dates are not in sequence.")

  # set up the workhorse data.frame
  df <- data.frame(year  = as.integer(format(dates, "%Y")),
                   month = as.integer(format(dates, "%m")),
                   DOY   = as.integer(format(dates, "%j")),
                   Tavg)
  rm(dates, Tavg)

  # determine leap years
  years <- sort(unique(df$year))
  leap <- (years %% 4 == 0 & years %% 100 != 0) | years %% 400 == 0

  # est.prev in valid range?
  # set to a high value if not Menzel
  if(start.method == 'Menzel'){
    est.prev <- as.integer(est.prev)
    if(est.prev < 0 || est.prev > length(years))
      stop("est.prev must be >= 0 and <= number of years.")
    if(est.prev == 0 && length(years) == 1)
      stop(paste("It's not possible to use previous year's chill days",
           "(est.prev=0) if only one year is provided.",
           "Either set est.prev=1 or provide more years."))
  } else {
    est.prev <- 999
  }

  # Do the provided years have the correct length (considering leap years)?
  # last year only till October 5th (DOY=279) needed
  if(est.prev == 0){
    # Menzel: first year discarded -> only Nov+Dec of first year needed
    year.length <- c(61, 365 + leap[c(-1, -length(leap))], 279)
  } else {
    # full first year required
    # either not Menzel or guesstimate first year's NovDecCD from previous yrs
    year.length <- c(365 + leap[-length(leap)], 279)
  }
  if(!all(tapply(df$DOY, df$year, FUN=length) >= year.length))
    stop("Not all years in 'dates' have expected length.")

  if(est.prev == 0 &&
     length(df[df$year == years[1] & df$month >= 11, "DOY"]) != 61)
    stop("First year must at least contain full November & December")


  # calculating start and end
  #----------------------------------------------------------------------------
  start <- switch(start.method,
                  'Menzel'   = .start_menzel(df=df, est.prev=est.prev, species=species),
                  'StdMeteo' = ,
                  'ETCCDI'   = .start_std_meteo(df),
                  'Ribes uva-crispa' = .start_ribes(df))

  # if first year only used for getting previous cold days -> drop it now
  if(est.prev == 0){
    df <- df[df$year != years[1], ]
    years <- years[-1]
  }

  end <- switch(end.method,
                'vonWilpert'  = .end_vonWilpert(df),
                'LWF-BROOK90' = .end_LWF_BROOK90(df),
                'NuskeAlbert' = .end_NuskeAlbert(df, start),
                'StdMeteo'    = ,
                'ETCCDI'      = .end_std_meteo(df))


  # collect results
  #----------------------------------------------------------------------------
  # if indicated calculate day degrees in vegperiod also
  res <- data.frame(year=years, start=start, end=end)

  seq_len

  if(Tsum.out){
    res$Tsum <- numeric(nrow(res))
    for(i in seq_along(res$Tsum)){
      res$Tsum[i] <- sum(df$Tavg[df$year == years[i] &
                                 df$DOY  >= start[i] &
                                 df$DOY  <= end[i]   &
                                 df$Tavg >  Tsum.crit])
    }
  }

  return(res)
}
