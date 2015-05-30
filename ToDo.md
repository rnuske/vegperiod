# ToDo List
* introduce formal testing (using testthat)
* Get rid of dependence on RCurl. Think harder about downloading historical data from DWD without RCurl. Problem: The file name can not be constructed from information in the station list and `download.file()` does not accept wildcards.
    + file name: `tageswerte_01691_19470101_20141231_hist.zip`
    + corresponding record in 'KL_Tageswerte_Beschreibung_Stationen.txt' `1691 19470101 20150422            167     51.5003    9.9506 Göttingen`
* Add further vegperiod methods (cf. CSC collection of vegetation periods)
* Consider Brandenburger variation of 'Menzel'
