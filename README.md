# vegperiod

[![Travis-CI Build Status](https://travis-ci.org/rnuske/vegperiod.svg?branch=master)](https://travis-ci.org/rnuske/vegperiod) 
[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1466541.svg)](https://doi.org/10.5281/zenodo.1466541)


The length of the vegetation period is increasingly used to model plant growth. The vegetation period gains further importance in climate change impact models trying to grasp the change of growing conditions, usually along with a measure of temperature and precipitation.

To determine start and end date of the vegetation period in a simple manner, this package uses only mean daily temperature and the day of the year (DOY). Since the geographical position is not taken into account (at the moment), the area of application is determined by the area used to parameterize the start and end methods, which is currently roughly Western Europe.

The package also includes functions for downloading open meteo data from Germany's National Meteorological Service (Deutscher Wetterdienst, DWD).


## Installation
A development version of the package vegperiod can be installed from Github using the package devtools.

```r
# install.packages("devtools")
devtools::install_github("rnuske/vegperiod")
```

## Usage
Vegetation periods a calculated using the function `vegperiod()`.  One has to choose a start and end method. Some methods, such as 'Menzel', need additional arguments.

```
data(goe)
vegperiod(dates=goe$date, Tavg=goe$t, species="Picea abies (frueh)",
          start.method="Menzel", end.method="vonWilpert", est.prev=5)
```

### Implemented start and end methods
Some common methods for determining the start and end date of the vegetation period are already implemented. Popular choices with regard to forest trees are 'Menzel' and 'vonWilpert'.

#### `start.method`
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

#### `end.method`
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

### Downloading data from DWD
Germany's National Meteorological Service offers open meteo data in its [Climate Data Center](https://www.dwd.de/EN/climate_environment/cdc/cdc.html).
The files are organized in deep folder structures and end with arcane/legacy EOF character. 
The Function `read.DWDdata()`deals with all of that and returns a data.frame. Beware there might be missing values.

Note: Downloading 'historical' data from DWD with read.DWDdata() requires the package 'curl'.


## Contributions
Further start and end methods or download functions are more than welcome! 
