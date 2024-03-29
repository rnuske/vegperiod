## vegperiod: Determine Thermal Vegetation Periods 

<!-- badges: start -->
[![R-CMD-check](https://github.com/rnuske/vegperiod/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rnuske/vegperiod/actions/workflows/R-CMD-check.yaml)
[![Package-License](https://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html) 
[![CRAN](https://www.r-pkg.org/badges/version/vegperiod)](https://cran.r-project.org/package=vegperiod) 
[![Dependencies](https://tinyverse.netlify.com/badge/vegperiod)](https://cran.r-project.org/package=vegperiod) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1466541.svg)](https://doi.org/10.5281/zenodo.1466541)
<!-- badges: end -->

The vegetation period, or growing season, is the period of the year when the weather conditions are sufficient for plants to grow. This package provides methods to calculate climatological or thermal growing seasons solely based on daily mean temperatures and the day of the year (DOY). Because of their simplicity, they are commonly used in plant growth models and climate change impact assessments.

The concept of a temperature driven vegetation period holds mostly for the temperate climate zone. At lower latitudes, other factors such as precipitation and evaporation can be more decisive. Some methods such as GSL of `ETCCDI` are employed globally (with a half year shift in the southern hemisphere). Others have a smaller area of application as they have been parameterized with local to regional observations. However, the methods `Menzel` and `vonWilpert` are used throughout Germany.

The package also includes functions for downloading open meteo data from Germany's National Meteorological Service (Deutscher Wetterdienst, DWD).


### Installation
The stable version can be installed from CRAN
```r
install.packages("vegperiod")
```

and the development version is available from Github using the package `remotes`
```r
remotes::install_github("rnuske/vegperiod")
```

### Usage
Vegetation periods are calculated using the function `vegperiod()`.  One has to choose at least a start and an end method. Some methods require additional arguments, such as 'Menzel' which needs 'species'.

```r
data(goe)
vegperiod(dates=goe$date, Tavg=goe$t, 
          start.method="Menzel", end.method="vonWilpert", 
          species="Picea abies (frueh)", est.prev=3)
```

### Implemented start and end methods
Common methods for determining the onset and end of thermal vegetation periods are provided, for details see next sections and documentation. Suggestions or contributions of additional methods are always welcome. Popular choices with regard to forest trees in Germany are `Menzel` and `vonWilpert`. Climate change impact studies at NW-FVA are frequently conducted using `Menzel` with "Picea abies (frueh)" and `NuskeAlbert` for all tree species; with tree species specifics accounted for in subsequent statistical models.

#### Start methods
* **Menzel** implemented as described in Menzel (1997). Parameterized for 10 common tree species. Requires previous years chill days, which can be substituted by the average of first years.
* **StdMeteo** / **ETCCDI** a simple threshold based procedure as defined by the Expert Team on Climate Change Detection and Indices (cf. ETCCDI 2009 and Frich et al. 2002). Leading to quite early vegetation starts.
* **Ribes uva-crispa** using leaf out of gooseberry as indicator. Developed by Germany's National Meteorological Service (Deutscher Wetterdienst, DWD). Presented in the section forestry of DWD's 'Climate Atlas'. It is more robust against early starts than common simple meteorological procedures.

#### End methods
* **vonWilpert** based on von Wilpert (1990). Originally developed for "Picea abies" in the Black Forest but currently used for all tree species throughout Germany.
* **LWF-BROOK90** a reimplementation of the LWF-BROOK90 VBA version of vonWilpert (Hammel and Kennel 2001).
* **NuskeAlbert** a very simple method inspired by standard climatological practices.
* **StdMeteo** / **ETCCDI** a simple threshold based procedure as defined by the Expert Team on Climate Change Detection and Indices (cf. ETCCDI 2009 and Frich et al., 2002). Leading to quite late vegetation ends.

### Download climate data from DWD (German Meteorological Service)
Germany's National Meteorological Service offers open meteo data in its [Climate Data Center](https://cdc.dwd.de/portal/).
The files are organized in deep folder structures and end with an arcane/legacy EOF character. 
The Function `read.DWDdata()`deals with all of that and returns a `data.frame`. Beware there might be missing values and inhomogeneities.

Note: Downloading 'historical' data from DWD with `read.DWDdata()` requires the package 'curl'.


### How to cite this package
If you use the package `vegperiod` for your publication, please cite it as follows:

> Nuske, R. (2022): vegperiod: Determine Thermal Vegetation Periods [Software]. 
> Zenodo. Version 0.4.0. https://doi.org/10.5281/zenodo.1466541


### Contributions
Implementations of further start and end methods or download functions are more than welcome! Please suggest suitable candidates via issue or pull request.
