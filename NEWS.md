# News for Package vegperiod

## Version 0.3
  * fix `est.prev` parameter used in start method Menzel. Now averages as advertised (Thanks for catching this bug to @awellpott).
  * switched to unit testing framework `tinytest`


## Version 0.2.7
  * changed temperature plausibility checks because of severe winter 1956 with daily mean temperatures of -27 °C in Germany and even lower and higher daily mean temperatures in climate scenarios.


## Version 0.2.6
  * fixed change of base URL of DWD open data (Thanks @ckluss for reporting and pointing in the right direction)
  * fixed download of recent monthly climate data. Breakage caused by DWD file name schema change (#1). Thanks @rwoerde for reporting.
  * corrected minimum R version. Argument `fileEncoding` was introduced to `read.fwf()` in R 3.2


## Version 0.2.5
  * replaced package 'RCurl' with 'curl' for downloading DWD Data
  * stopped importing package 'methods'
  * fixed read.DWDstations() because DWD changed layout of station list
  * updated documentation of read.DWDdata() because DWD changed column order of climate data files


## Version 0.2.4
  * vegperiod() checks if Tavg (daily mean temperature) is between -25 and 35 to catch temperatures multiplied by 10. Sometimes done to store as integers. (thanks to Ronald Bialozyt for reporting a crash)


## Version 0.2.3
  * vegperiod() reports end of vegetation period always as integer
  * new parameter 'quiet' to readDWDdata to suppress messages and progessbar of download
  * started unit testing with package 'testthat'


## Version 0.2.2
  * start.method='Ribes uva-crispa' using leaf out of gooseberry as indicator. Developed by Germany's National Meteorological Service (DWD).
  * est.prev can now be equal to number of provided years
  * proper error message if only one year of data provided and previous year's chill days requested


## Version 0.2.1
  * cleaned up mangled output of start.method='StdMeteo'. StdMeteo now behaves properly and returns a vector.
  * use stats::filter() in the package to protected filter() against getting run over by package dplyr (thanks to Cristabel Duran for reporting).


## Version 0.2.0
### New Features
  * `read.DWDdata()` reads open meteo data from the Climate Data Center (CDC) of Germany's National Meteorological Service (Deutscher Wetterdienst, DWD).
  * `read.DWDstations()` fetches information about available DWD stations

### Major Changes
  * renamed arguments in vegperiod() to clarify meaning / easier tab completion:
    * first.avg -> est.prev 
    * temp.sum -> Tsum.out
    * method.start -> start.method
    * method.end -> end.method
  * renamed method `StdClimatolgy` to `StdMeteo` / `ETCCDI` in vegperiod() and brought calculations into compliance with ETCCDI standard

### Bug fixes and minor improvements
  * added a hack in method LWF-BROOK90 of vegperiod() to adjust for different arithmetic precision in VBA version (thanks to Paul Schmidt-Walter for providing climate.in files for comparision and help finding the bug)
  * fixed bug leading to crash of vegperiod() if argument `first.avg=0` (thanks to Ronald Bialozyt for reporting)
  * the argument "dates" of vegperiod() accepts now also character strings understood by `as.Date()`
  * rewrote help-page of vegperiod() and added references for all methods


## Version 0.1.0
  * Intial version
