# Fetch meteo data of DWD weather stations

Fetch observed meteorological data of German weather stations from
freely accessible part of the Climate Data Center of Germany's National
Meteorological Service (Deutscher Wetterdienst, DWD).

## Usage

``` r
read.DWDdata(
  id,
  type = "climate",
  period = "recent",
  resolution = "daily",
  file = NULL,
  destdir = NULL,
  quiet = FALSE
)
```

## Arguments

- id:

  integer. A valid station id (cf.
  [read.DWDstations](https://rnuske.github.io/vegperiod/reference/read.DWDstations.md)).

- type:

  string. Stations can be of type `"climate"` offering a range of
  meteorological parameters or of type `"precipitation"` solely
  recording rainfall. There are about 400 `climate` and 2000
  `precipitation` stations.

- period:

  string specifying a time span. The last 1.5 years are called
  `"recent"`. Data from the individual beginning of measurements up to a
  year ago are labelled `"historical"`. `"recent"` data have not yet
  gone through the full quality control procedure. `"historical"` data
  have completed the operational quality control. But be aware there
  still might be gaps and inhomogeneities.

- resolution:

  string. Temporal resolution of the data can be `"daily"` or
  `"monthly"`. `"daily"` being the common resolution.

- file:

  a string specifying the location of a dataset. `"file"` may point to a
  file on a FTP server or on the local file system. If the file lives on
  a FTP server the string must start with `'ftp://'`. If it is NULL (the
  default), data will be downloaded from DWD's FTP server.

- destdir:

  directory (string) where intermediate data (downloaded \*.zip file)
  are stored. If it is NULL (the default) a subdirectory
  `downloaded_packages` of the session temporary directory will be used
  (and the files will be deleted at the end of the session). It might be
  advisable to keep the .zip file since it contains the stations
  documentation and metadata (e.g. station shift, used devices,
  downtimes etc.).

- quiet:

  If TRUE, suppress status messages (if any), and the progress bar.

## Value

A data.frame with the observed weather data. Beware of gaps and
inhomogeneities! `colnames` contains the original header and hence
German terms.

Content of 'climate'-data.frame:

|             |                                                           |
|-------------|-----------------------------------------------------------|
| **Name**    | **Description**                                           |
| STATIONS_ID | station id                                                |
| MESS_DATUM  | date                                                      |
| QN_3        | quality level of next columns (-)                         |
| FX          | daily maximum of wind gust (m/s)                          |
| FM          | daily mean of wind velocity (m/s)                         |
| QN_4        | quality level of next columns (-)                         |
| RSK         | daily precipitation height (mm)                           |
| RSKF        | precipitation form (-)                                    |
| SDK         | daily sunshine duration (h)                               |
| SHK_TAG     | daily snow depth (cm)                                     |
| NM          | daily mean of cloud cover (1/8)                           |
| VPM         | daily mean of vapor pressure (hPa)                        |
| PM          | daily mean of pressure (hPa)                              |
| TMK         | daily mean of temperature (°C)                            |
| UPM         | daily mean of relative humidity (%)                       |
| TXK         | daily maximum of temperature at 2m height (°C)            |
| TNK         | daily minimum of temperature at 2m height (°C)            |
| TGK         | daily minimum of air temperature at 5cm above ground (°C) |

Content of 'precipitation'-data.frame:

|             |                                   |
|-------------|-----------------------------------|
| **Name**    | **Description**                   |
| STATIONS_ID | station id                        |
| MESS_DATUM  | date                              |
| QN_6        | quality level of next columns (-) |
| RS          | daily precipitation height (mm)   |
| RSF         | precipitation form (-)            |
| SH_TAG      | daily height of snow pack (cm)    |

## Details

An introduction to the data available at the Climate Data Center can be
found in the German
[Liesmich_intro_CDC_ftp](https://opendata.dwd.de/climate_environment/CDC/Liesmich_intro_CDC-FTP.pdf)
or the translated
[Readme_intro_CDC_ftp](https://opendata.dwd.de/climate_environment/CDC/Readme_intro_CDC_ftp.pdf).

The freely accessible part of the Climate Data Center of Germany's
National Meteorological Service (Deutscher Wetterdienst, DWD) is part of
the DWD's mandate for basic supply of information (termed
"Grundversorgung"). These services may be used without any restrictions
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
# fetch last 500 days worth of data from station Göttingen
clim <- read.DWDdata(id=1691)

# save data & metadata (documentation about devices, downtimes etc.)
clim <- read.DWDdata(id=1691, destdir='.')

# find and download historical data from the Brocken
stat.hist <- read.DWDstations(period='historical')
brocken.id <- stat.hist[grep("^Brock", stat.hist$name), ]$id
clim.brocken <- read.DWDdata(id=brocken.id, period='historical')
} # }
```
