\name{NEWS}
\title{News for Package 'vegperiod'}
\encoding{UTF-8}
\section{Changes in vegperiod version 0.2.4 (2017-09-19)}{
  \subsection{fixed}{
    \itemize{
      \item \code{vegperiod()} now checks if Tavg (daily mean temperature) is between -25 and 35 to catch temperatures stored as integers multiplied by 10. Thanks to Ronald Bialozyt, who reported \code{vegperiod()} crashes if fed long time series of such temperatures.
    }
  }
}
\section{Changes in vegperiod version 0.2.3 (2017-08-24)}{
  \subsection{fixed}{
    \itemize{
      \item end of vegetation period as reported by \code{vegperiod()} now always integer
      \item DESCRIPTION now reflects new home at https://www.nw-fva.de/r-pkgs
    }
  }
  \subsection{added}{
    \itemize{
      \item new parameter 'quiet' to readDWDdata to suppress messages and progessbar of download
      \item started unit testing with package 'testthat'
    }
  }
}
\section{Changes in vegperiod version 0.2.2 (2016-02-18)}{
  \subsection{fixed}{
    \itemize{
      \item est.prev can now be equal to number of years provided
      \item proper error message if only one year of data provided and previous year's chill days reqested
    }
  }
  \subsection{added}{
    \itemize{
      \item start.method='Ribes uva-crispa' using leaf out of gooseberry as indicator. Developed by the German Weather Service.
    }
  }
}
\section{Changes in vegperiod version 0.2.1 (2015-06-10)}{
  \subsection{fixed}{
    \itemize{
      \item cleaned up mangled output of start.method='StdMeteo'. StdMeteo now behaves properly and returns a vector.
    }
  }
  \subsection{changed}{
    \itemize{
      \item use \code{stats::filter()} to protected \code{filter()} against getting run over by package dplyr
    }
  }
}
\section{Changes in vegperiod version 0.2.0 (2015-05-30)}{
  \subsection{fixed}{
    \itemize{
      \item fixed bug leading to crash of vegperiod if argument "first.avg=0" (thanks
        to Ronald Bialozyt for pointing it out)
      \item added a hack to method "LWF-BROOK90" in function vegperiod to adjust for
        different arithmetic precision in VBA (thanks to Paul Schmidt-Walter for
        providing climate.in files for comparison and help finding the bug)
      \item fixed problems in method \code{"StdClimatolgy"} in function vegperiod (now
        renamed to \code{"StdMeteo"})
    }
  }
  \subsection{changed}{
    \itemize{
      \item renamed arguments in function vegperiod to clarify meaning / easier tab
        completion:
        first.avg -> est.prev,
        temp.sum -> Tsum.out,
        method.start -> start.method,
        method.end -> end.method
      \item renamed method \code{"StdClimatolgy"} to \code{"StdMeteo"} resp. \code{"ETCCDI"} in function
        vegperiod and rebased implementation on international standard
      \item the argument \code{"dates"} of function vegperiod accepts now also character
        strings understood by \code{as.Date()}
      \item rewrote help page of vegperiod and added references for all methods
    }
  }
  \subsection{added}{
    \itemize{
      \item \code{read.DWDdata()} reads meteo data from the freely accessable stations of
        the Climate Data Center of the German Weather Service (Deutscher
        Wetterdienst, DWD).
      \item \code{read.DWDstations()} fetches information about available DWD stations
    }
  }
}
\section{Changes in vegperiod version 0.1.0 (2015-04-06)}{
  \itemize{
    \item initial release
  }
}
