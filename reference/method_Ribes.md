# Vegetation Start Method "Ribes uva-crispa"

The method `Ribes uva-crispa` is based on leaf-out of gooseberry
(Janssen 2009). It was developed by the Germany's National
Meteorological Service (Deutscher Wetterdienst, DWD) and is more robust
against early starts than common simple meteorological procedures.

## Calculation

starting from 18th February sum all day degrees above 0 °C (daily
average air temperature). Vegetetaion period starts at the day when 164
is crossed.

    # start day: February 18th == 49 doy
    # only Tavg over 0°C are summed
    df[df$DOY < 49 | df$Tavg < 0, 'Tavg'] <- 0

    # find day where cum day degrees cross 164
    start <- tapply(df$Tavg, df$year, FUN=function(x){
      x <- cumsum(x)
      min(which(x > 164))
    })

## References

Janssen, W. (2009) Definition des Vegetationsanfanges. *Internal Report,
Deutscher Wetterdienst, Abteilung Agrarmeteorologie*.
