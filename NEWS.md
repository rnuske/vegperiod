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
