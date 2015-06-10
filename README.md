# vegperiod

Calculate start and end date of vegetation period of forest trees based on daily average air temperature and the day of the year (DOY). Functions for summing day degrees within vegetation period and reading freely available meteo data from DWD's ftp-Server are provided on top.

## Available Methods for determinig vegetation period
### start.method
* **Menzel** described in Menzel (1997). Parameterized for 10 common tree 
species.
* **StdMeteo** / **ETCCDI** a simple threshold based procedure as defined by 
the Expert Team on Climate Change Detection and Indices (cf. ETCCDI and Ferch 
et al., 2002) leading to quite early vegetation starts.

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


## Installation 

### Install released package via
install.packages("vegperiod", repo="http://computerfoerster.de/r-pkgs")

### Install from source
Download source code and install locally e.g. via devtools::install_local() or
RStudio. Another possibility is to try devtools::install_git(). It might be a 
little complicated because one has to deal with user and password of the 
private repository.
