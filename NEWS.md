# CHANGES in vegperiod VERSION 0.2.4

## BUG FIXES AND MINOR IMPROVEMENTS
* vegperiod() now checks if Tavg (daily mean temperature) is between -25 and 35 to catch temperatures stored as integers multiplied by 10. Thanks to Ronald Bialozyt, who reported vegperiod() crashes if fed long time series of such temperatures.



# CHANGES in vegperiod VERSION 0.2.3

## BUG FIXES AND MINOR IMPROVEMENTS
* end of vegetation period  as reported by vegperiod() now always integer
* DESCRIPTION now reflects new home at https://www.nw-fva.de/r-pkgs
* new parameter 'quiet' to readDWDdata to suppress messages and progessbar of download
* started unit testing with package 'testthat'



# CHANGES in vegperiod VERSION 0.2.2

## BUG FIXES AND MINOR IMPROVEMENTS
* start.method='Ribes uva-crispa' using leaf out of gooseberry as indicator. Developed by the German Weather Service.
* est.prev can now be equal to number of years provided
* proper error message if only one year of data provided and previous year's chill days reqested



# CHANGES in vegperiod VERSION 0.2.1

## BUG FIXES AND MINOR IMPROVEMENTS
* cleaned up mangled output of start.method='StdMeteo'. StdMeteo now behaves properly and returns a vector.
* use stats::filter() to protected filter() against getting run over by package dplyr



# CHANGES in vegperiod VERSION 0.2.0

## NEW FEATURES
* `read.DWDdata()` reads meteo data from the freely accessable stations of the Climate Data Center (CDC) of the German Weather Service (Deutscher Wetterdienst, DWD).
* `read.DWDstations()` fetches information about available DWD stations


## MAJOR CHANGES
* renamed arguments in function vegperiod to clarify meaning / easier tab completion:
    + first.avg -> est.prev 
    + temp.sum -> Tsum.out
    + method.start -> start.method
    + method.end -> end.method
* renamed method `StdClimatolgy` to `StdMeteo` resp. `ETCCDI` in function vegperiod and rebased implementation on the international standard

## BUG FIXES AND MINOR IMPROVEMENTS
* added a hack in function vegperiod method LWF-BROOK90 to adjust for different arithmetic precision in VBA version (thanks to Paul Schmidt-Walter for providing climate.in files for comparision and help finding the bug)
* fixed bug leading to crash of vegperiod if argument `first.avg=0` (thanks to Ronald Bialozyt pointing it out)
* rewrote help-page of vegperiod and added references for all methods



# CHANGES in vegperiod VERSION 0.1.0

## Intial version
