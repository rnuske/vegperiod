# vegperiod

Calculate start and end date of vegetation period of forest trees based on daily average air temperature and the day of the year (DOY). Functions for summing day degrees within vegetation period and reading freely available meteo data from DWD's ftp-Server are provided on top.

## Available Methods for determinig vegetation period
### start.method
* **Menzel** described in Menzel (1997). Parameterized for 10 common tree 
species.
* **StdMeteo** / **ETCCDI** a simple threshold based procedure as defined by 
the Expert Team on Climate Change Detection and Indices (cf. ETCCDI and Ferch 
et al., 2002) leading to quite early vegetation starts.
* **Ribes uva-crispa** using leaf out of gooseberry as indicator. 
It was developed by the German Weather Service (Deutscher Wetterdienst, DWD) 
and is part of the section forestry of DWD's
[German Climate Atlas](http://www.dwd.de/EN/climate_environment/climateatlas/climateatlas_node.html).
It is more robust against early starts than common simple meteorological procedures.

### end.method
* **vonWilpert** based on von Wilpert (1990). Originally developed for 
"Picea abies" in the Black Forest but currently used for all tree species 
throughout Germany.
* **LWF-BROOK90** a reimplementation of the LWF-BROOK90 VBA version 
(Hammel & Kennel, 2001).
* **NuskeAlbert** a very simple method which is inspired by standard 
climatological procedures.
* **StdMeteo** / **ETCCDI** a simple threshold based procedure as defined by 
the Expert Team on Climate Change Detection and Indices (cf. ETCCDI and Ferch 
et al., 2002) leading to quite late vegetation ends.

### The usual suspects
Most commonly used are **Menzel** and **vonWilpert**.


## Downloading data from DWD
The German Weather Service offers meteo data as text files on a FTP-Server. The 
files are organized in deep folder structures and end with arcane/legacy EOF. 
The Function `read.DWDdata()`deals with all of that and provides data.frame. 
Beware there might be missing values.


## ToDo
* enhance formal testing (using testthat)
* Get rid of dependence on curl. Think harder about downloading historical data from DWD without curl. Problem: The file name can not be constructed from information in the station list and `download.file()` does not accept wildcards.
    + file name: `tageswerte_01691_19470101_20141231_hist.zip`
    + corresponding record in 'KL_Tageswerte_Beschreibung_Stationen.txt' `1691 19470101 20150422            167     51.5003    9.9506 Göttingen`
* Add further vegperiod methods (cf. CSC collection of vegetation periods)
* Consider Brandenburger variation of 'Menzel'


## Installation 

### Install released package via
install.packages("vegperiod", repo="https://www.nw-fva.de/r-pkgs")

### Install from source
Download source code and install locally e.g. via devtools::install_local() or
RStudio. Another possibility is to try devtools::install_git(). It might be a 
little complicated because one has to deal with user and password of the 
private repository.
