# Fetch list of DWD weather stations

Get a listing of advertised stations of Germany's National
Meteorological Service (Deutscher Wetterdienst, DWD). The list differs
for station type, time period and resolution. The list of advertised
stations is usually not identical with currently available stations.

## Usage

``` r
read.DWDstations(type = "climate", period = "recent", resolution = "daily")
```

## Arguments

- type:

  string. Stations can be of type `"climate"` offering a range of
  meteorological parameters or of type `"precipitation"` solely
  recording rainfall.

- period:

  string specifying a time span. The last 1.5 years are called
  `"recent"`. Data from the individual beginning of measurements up to a
  year ago are labelled `"historical"`.

- resolution:

  string. Temporal resolution of the data can be `"daily"` or
  `"monthly"`. `"daily"` being the common resolution.

## Value

A data.frame with all weather stations. It's not guaranteed that all
advertised staions are available for download. Watch out for the end
data of observation. Stations with recent data have a current end date.
The data.frame contains the variables id, from, to, elev, lat, long,
name, state.

## Details

The freely accessible part of the Climate Data Center of Germany's
National Meteorological Service (Deutscher Wetterdienst, DWD) is part of
DWD's mandate for basic supply of information (termed
"Grundversorgung"). This service may be used without any restrictions
(no fees will be charged and in general there are no restrictions for
the use the data), provided that the source is indicated as laid down in
the
["GeoNutzV"](http://www.gesetze-im-internet.de/geonutzv/BJNR054700013.md)
ordinance. The source reference shall roughly meet the following rules:

- Where data are used without modification, the source reference shall
  read "Source: Deutscher Wetterdienst" or just consist of the DWD logo.

- If the data are modified, the source reference shall specify as
  precisely as possible the extent of such, e.g. "Based on data from
  Deutscher Wetterdienst, figures rounded".

A more detailed description of the rules can be found in the official
and legally binding German
[Nutzungsbedingungen](https://opendata.dwd.de/climate_environment/CDC/Nutzungsbedingungen_German.pdf)
or the translated [Terms of
use](https://opendata.dwd.de/climate_environment/CDC/Terms_of_use.pdf).

## References

Freely accessible DWD data available via the [Climate Data
Center](https://www.dwd.de/EN/climate_environment/cdc/cdc_node_en.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# stations with daily climate data
stations <- read.DWDstations()

# precipitation stations with monthly historical data
precip <- read.DWDstations(type='precip', period='historical',
                           resolution='monthly')

# list stations with daily data updated within last week
stat.daily <- read.DWDstations(period='recent')
stat.daily.recent <- stat.daily[stat.daily$to > (Sys.Date() - 7), ]
} # }
```
