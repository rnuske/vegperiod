# Determine vegetation period

Calculate start and end date of vegetation periods based on daily
average air temperature and the day of the year (DOY). The sum of day
degrees within the vegetation period is included for convenience.

## Usage

``` r
vegperiod(
  dates,
  Tavg,
  start.method,
  end.method,
  Tsum.out = FALSE,
  Tsum.crit = 0,
  species = NULL,
  est.prev = 0,
  check.data = TRUE
)
```

## Arguments

- dates:

  vector of calendar dates (objects of class `Date` or something
  understood by [`as.Date()`](https://rdrr.io/r/base/as.Date.html)).
  Must contain entire years if `est.prev > 0` else the first year may
  comprise only November and December. Days must be in correct order.

- Tavg:

  vector of daily average air temperatures in degree Celsius. Same
  length as `dates`.

- start.method:

  name of method to use for vegetation start. One of `"Menzel"` (needs
  additional argument `species`, see below), `"StdMeteo"` resp.
  `"ETCCDI"`, `"Ribes uva-crispa"`. Can be abbreviated (partial
  matching). For further discussion see Details.

- end.method:

  name of method to use for vegetation end. One of `"vonWilpert"`,
  `"LWF-BROOK90"`, `"NuskeAlbert"` and `"StdMeteo"` resp. `"ETCCDI"`.
  Can be abbreviated (partial matching). For further discussion see
  Details.

- Tsum.out:

  boolean. Return the sum of daily mean temperatures above `Tsum.crit`
  within vegetation period, also known as growing day degrees.

- Tsum.crit:

  threshold for sum of day degrees. Only daily mean temperatures
  `> Tsum.crit` will be tallied. The default of `0` prevents negative
  daily temperatures from reducing the sum. Climate change studies often
  use a threshold of `5`.

- species:

  name of tree species (required if `start.method="Menzel"` ignored
  otherwise).

  Must be one of `"Larix decidua"`, `"Picea abies (frueh)"`,
  `"Picea abies (spaet)"`, `"Picea abies (noerdl.)"`, `"Picea omorika"`,
  `"Pinus sylvestris"`, `"Betula pubescens"`, `"Quercus robur"`,
  `"Quercus petraea"`, `"Fagus sylvatica"`.

- est.prev:

  number of years to **est**imate **prev**ious year's chill days for the
  first year (required if `start.method="Menzel"` ignored otherwise).

  `Menzel` requires the number of chill days of previous November and
  December. If `est.prev = 0` the first year is used to get the previous
  year's chill days and dropped afterwards. Thus, a year from the time
  series is lost. To avoid losing a year, `est.prev = n` estimates the
  previous year's chill days for the first year from the average of `n`
  first years of the time series.

- check.data:

  Performs plausibility checks on the temperature data to ensure that
  the temperatures have not been multiplied by ten. Plausible range is
  -35 to +40°C.

## Value

A data.frame with year and DOY of start and end day of vegetation
period. If `Tsum.out=TRUE`, the data.frame contains an additional column
with the sum of day degrees within vegetation periods.

## Details

Common methods for determining the onset and end of thermal vegetation
periods are provided, for details see next sections. Popular choices
with regard to forest trees in Germany are `Menzel` and `vonWilpert`.
Climate change impact studies at NW-FVA are frequently conducted using
`Menzel` with "Picea abies (frueh)" and `NuskeAlbert` for all tree
species; with tree species specifics accounted for in subsequent
statistical models.

### Start methods:

The method
**[`Menzel`](https://rnuske.github.io/vegperiod/reference/method_Menzel.md)**
implements the algorithm described in Menzel (1997). The method is
parameterized for 10 common tree species. It needs previous year's chill
days.
**[`ETCCDI`](https://rnuske.github.io/vegperiod/reference/method_ETCCDI.md)**
resp. `StdMeteo` is a simple threshold based procedure as defined by the
Expert Team on Climate Change Detection and Indices (cf. ETCCDI 2009,
Frich et al. 2002, Zhang et al. 2011) leading to quite early vegetation
starts. This method is widely used in climate change studies. The method
**[`Ribes uva-crispa`](https://rnuske.github.io/vegperiod/reference/method_Ribes.md)**
is based on leaf-out of gooseberry (Janssen 2009). It was developed by
the Germany's National Meteorological Service (Deutscher Wetterdienst,
DWD) and is more robust against early starts than common simple
meteorological procedures.

### End methods:

The end method
**[`vonWilpert`](https://rnuske.github.io/vegperiod/reference/method_vonWilpert.md)**
is based on von Wilpert (1990). It was originally developed for *Picea
abies* in the Black Forest but is commonly used for all tree species
throughout Germany. As usual, the rules regarding the soilmatrix are
neglected in this implementation.
**[`LWF-BROOK90`](https://rnuske.github.io/vegperiod/reference/method_LWF-BROOK90.md)**
is -for the sake of convenience- a reimplementation of the LWF-BROOK90
VBA (version 3.4) variant of "vonWilpert" (Hammel and Kennel 2001).
Their interpretation of von Wilpert (1990) and the somewhat lower
precision of VBA was mimicked.
**[`NuskeAlbert`](https://rnuske.github.io/vegperiod/reference/method_NuskeAlbert.md)**
provide a very simple method which is inspired by standard
climatological procedures but employs a 7 day moving average and a 5 °C
threshold (cf. Walther and Linderholm 2006).
**[`ETCCDI`](https://rnuske.github.io/vegperiod/reference/method_ETCCDI.md)**
resp. `StdMeteo` is a simple threshold based procedure as defined by the
Expert Team on Climate Change Detection and Indices (cf. ETCCDI 2009,
Frich et al. 2002, Zhang et al. 2011) leading to quite late vegetation
ends.

## References

ETCCDI (2009) Climate Change Indices: Definitions of the 27 core
indices.
[http://etccdi.pacificclimate.org/list_27_indices.shtml](http://etccdi.pacificclimate.org/list_27_indices.smd)

Frich, P., Alexander, L., Della-Marta, P., Gleason, B., Haylock, M.,
Klein Tank, A. and Peterson, T. (2002) Observed coherent changes in
climatic extremes during the second half of the twentieth century.
*Climate Research*, **19**, 193–212.
[doi:10.3354/cr019193](https://doi.org/10.3354/cr019193) .

Hammel, K. and Kennel, M. (2001) Charakterisierung und Analyse der
Wasserverfügbarkeit und des Wasserhaushalts von Waldstandorten in Bayern
mit dem Simulationsmodell BROOK90. *Forstliche Forschungsberichte
München*.

Janssen, W. (2009) Definition des Vegetationsanfanges. *Internal Report,
Deutscher Wetterdienst, Abteilung Agrarmeteorologie*.

Menzel, A. (1997) Phänologie von Waldbäumen unter sich ändernden
Klimabedingungen - Auswertung der Beobachtungen in den Internationalen
Phänologischen Gärten und Möglichkeiten der Modellierung von Phänodaten.
*Forstliche Forschungsberichte München*.

von Wilpert, K. (1990) Die Jahrringstruktur von Fichten in Abhängigkeit
vom Bodenwasserhaushalt auf Pseudogley und Parabraunerde: Ein
Methodenkonzept zur Erfassung standortsspezifischer
Wasserstreßdispostion. *Freiburger Bodenkundliche Abhandlungen*.

Walther, A. and Linderholm, H. W. (2006) A comparison of growing season
indices for the Greater Baltic Area. *International Journal of
Biometeorology*, **51**(2), 107–118.
[doi:10.1007/s00484-006-0048-5](https://doi.org/10.1007/s00484-006-0048-5)
.

Zhang, X., Alexander, L., Hegerl, G. C., Jones, P., Tank, A. K.,
Peterson, T. C., Trewin, B. and Zwiers, F. W. (2011) Indices for
monitoring changes in extremes based on daily temperature and
precipitation data. *Wiley Interdisciplinary Reviews: Climate Change*,
**2**(6), 851–870.
[doi:10.1002/wcc.147](https://doi.org/10.1002/wcc.147) .

## Examples

``` r
data(goe)
vegperiod(dates=goe$date, Tavg=goe$t,
          start.method="Menzel", end.method="vonWilpert",
          species="Picea abies (frueh)", est.prev=5)
#>    year start end
#> 1  2001   119 274
#> 2  2002   127 279
#> 3  2003   125 279
#> 4  2004   116 279
#> 5  2005   124 279
#> 6  2006   126 271
#> 7  2007   124 279
#> 8  2008   125 279
#> 9  2009   115 279
#> 10 2010   116 279

# take chill days from first year, which is then dropped
vegperiod(dates=goe$date, Tavg=goe$t, start="Menzel", end="vonWilpert",
          species="Picea abies (frueh)", est.prev=0)
#>   year start end
#> 1 2002   127 279
#> 2 2003   125 279
#> 3 2004   116 279
#> 4 2005   124 279
#> 5 2006   126 271
#> 6 2007   124 279
#> 7 2008   125 279
#> 8 2009   115 279
#> 9 2010   116 279

# add column with sum of day degrees in vegetation periods
vegperiod(dates=goe$date, Tavg=goe$t, Tsum.out=TRUE,
          start="StdMeteo", end="StdMeteo")
#>    year start end   Tsum
#> 1  2001    34 347 3474.7
#> 2  2002    36 329 3273.8
#> 3  2003    65 323 3273.7
#> 4  2004    38 333 3361.4
#> 5  2005    69 327 3229.4
#> 6  2006    18 308 2891.6
#> 7  2007    84 354 3216.4
#> 8  2008    64 322 3273.7
#> 9  2009    48 321 3195.9
#> 10 2010    88 356 3359.8
```
