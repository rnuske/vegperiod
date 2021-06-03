#==============================================================================
#
# Fetch climate data from DWD's open data program
#
# Robert Nuske 2015-03-30
#==============================================================================


#' Fetch list of DWD weather stations
#'
#' Get a listing of advertised stations of Germany's National Meteorological
#' Service (Deutscher Wetterdienst, DWD). The list differs for station type,
#' time period and resolution. The list of advertised stations is usually not
#' identical with currently available stations.
#'
#' The freely accessible part of the Climate Data Center of Germany's National
#' Meteorological Service (Deutscher Wetterdienst, DWD) is part of DWD's
#' mandate for basic supply of information (termed "Grundversorgung").
#' This service may be used without any restrictions (no fees will be
#' charged and in general there are no restrictions for the use the data),
#' provided that the source is indicated as laid down in the
#' \href{http://www.gesetze-im-internet.de/geonutzv/BJNR054700013.html}{"GeoNutzV"}
#' ordinance. The source reference shall roughly meet the following rules:
#' \itemize{
#'   \item Where data are used without modification, the source reference shall
#'     read "Source: Deutscher Wetterdienst" or just consist of the DWD logo.
#'   \item If the data are modified, the source reference shall specify as
#'     precisely as possible the extent of such, e.g. "Based on data from
#'     Deutscher Wetterdienst, figures rounded".
#' }
#' A more detailed description of the rules can be found in the official and
#' legally binding German
#' \href{https://opendata.dwd.de/climate_environment/CDC/Nutzungsbedingungen_German.pdf}{Nutzungsbedingungen}
#' or the translated \href{https://opendata.dwd.de/climate_environment/CDC/Terms_of_use.pdf}{Terms
#' of use}.
#'
#' @param type string. Stations can be of type \samp{"climate"} offering a range
#'   of meteorological parameters or of type \samp{"precipitation"} solely
#'   recording rainfall.
#'
#' @param period string specifying a time span. The last 1.5 years are
#'   called \samp{"recent"}. Data from the individual beginning of measurements
#'   up to a year ago are labelled \samp{"historical"}.
#'
#' @param resolution string. Temporal resolution of the data can be
#'   \samp{"daily"} or \samp{"monthly"}. \samp{"daily"} being the common
#'   resolution.
#'
#' @return A data.frame with all weather stations. It's not guaranteed that all
#'   advertised staions are available for download. Watch out for the end data
#'   of observation. Stations with recent data have a current end date.
#'   The data.frame contains the variables id, from, to, elev, lat, long, name,
#'   state.
#'
#' @references
#'   Freely accessible DWD data available via the
#'   \href{https://www.dwd.de/EN/climate_environment/cdc/cdc_node_en.html}{Climate Data Center}.
#'
#' @examples
#' \dontrun{
#' # stations with daily climate data
#' stations <- read.DWDstations()
#'
#' # precipitation stations with monthly historical data
#' precip <- read.DWDstations(type='precip', period='historical',
#'                            resolution='monthly')
#'
#' # list stations with daily data updated within last week
#' stat.daily <- read.DWDstations(period='recent')
#' stat.daily.recent <- stat.daily[stat.daily$to > (Sys.Date() - 7), ]
#' }
#'
#' @export
read.DWDstations <- function(type='climate', period='recent',
                             resolution='daily'){
  # check arguments
  type <- match.arg(type, choices=c('climate','precipitation'))
  period <- match.arg(period, choices=c('historical', 'recent'))
  resolution <- match.arg(resolution, choices=c('daily', 'monthly'))

  # construct url
  baseURL <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate"

  url.type <- switch(type,'climate'='kl','precipitation'='more_precip')
  pre.type <- switch(type, 'climate'='KL','precipitation'='RR')
  pre.res <- switch(resolution, 'daily'='Tageswerte', 'monthly'='Monatswerte')

  fname <- paste0(pre.type, "_", pre.res, "_Beschreibung_Stationen.txt")
  myURL <- paste(baseURL, resolution, url.type, period, fname, sep='/')

  myCols <- c('id', 'from', 'to', 'elev', 'lat', 'long', 'name', 'state')
  myWidths <- switch(resolution,
                     'daily'   = c(6, 9, 9, 15, 12, 10, 41, 24),
                     'monthly' = c(6, 9, 9, 14, 12, 10, 41, 24))

  options(warn=-1) # the warning about last line is wrong -> don't show it
  # treat EOF as comment
  df <- utils::read.fwf(myURL, widths=myWidths, skip=2, comment.char="\032",
                 strip.white=TRUE, na.strings='-999',
                 fileEncoding='ISO-8859-1', col.names=myCols,
                 stringsAsFactors=FALSE)
  df$from <- as.Date(as.character(df$from), "%Y%m%d")
  df$to   <- as.Date(as.character(df$to), "%Y%m%d")
  options(warn=0)

  return(df)
}

#' Fetch meteo data of DWD weather stations
#'
#' Fetch observed meteorological data of German weather stations from freely
#' accessible part of the Climate Data Center of Germany's National
#' Meteorological Service (Deutscher Wetterdienst, DWD).
#'
#' An introduction to the data available at the Climate Data Center can be
#' found in the German
#' \href{https://opendata.dwd.de/climate_environment/CDC/Liesmich_intro_CDC-FTP.pdf}{Liesmich_intro_CDC_ftp}
#' or the translated
#' \href{https://opendata.dwd.de/climate_environment/CDC/Readme_intro_CDC_ftp.pdf}{Readme_intro_CDC_ftp}.
#'
#' The freely accessible part of the Climate Data Center of Germany's National
#' Meteorological Service (Deutscher Wetterdienst, DWD) is part of the DWD's
#' mandate for basic supply of information (termed "Grundversorgung").
#' These services may be used without any restrictions (no fees will be charged
#' and in general there are no restrictions for the use the data), provided
#' that the source is indicated as laid down in the
#' \href{http://www.gesetze-im-internet.de/geonutzv/BJNR054700013.html}{"GeoNutzV"}
#' ordinance. The source reference shall roughly meet the following rules:
#' \itemize{
#'   \item Where data are used without modification, the source reference shall
#'     read "Source: Deutscher Wetterdienst" or just consist of the DWD logo.
#'   \item If the data are modified, the source reference shall specify as
#'     precisely as possible the extent of such, e.g. "Based on data from
#'     Deutscher Wetterdienst, figures rounded".
#' }
#' A more detailed description of the rules can be found in the official and
#' legally binding German
#' \href{https://opendata.dwd.de/climate_environment/CDC/Nutzungsbedingungen_German.pdf}{Nutzungsbedingungen}
#' or the translated \href{https://opendata.dwd.de/climate_environment/CDC/Terms_of_use.pdf}{Terms
#' of use}.
#'
#' @param id integer. A valid station id (cf. \link{read.DWDstations}).
#'
#' @param type string. Stations can be of type \samp{"climate"} offering a range
#'   of meteorological parameters or of type \samp{"precipitation"} solely
#'   recording rainfall. There are about 400 \samp{climate} and 2000
#'   \samp{precipitation} stations.
#'
#' @param period string specifying a time span. The last 1.5 years are
#'   called \samp{"recent"}. Data from the individual beginning of measurements
#'   up to a year ago are labelled \samp{"historical"}.
#'   \samp{"recent"} data have not yet gone through the full quality control
#'   procedure. \samp{"historical"} data have completed the operational quality
#'   control. But be aware there still might be gaps and inhomogeneities.
#'
#' @param resolution string. Temporal resolution of the data can be
#'   \samp{"daily"} or \samp{"monthly"}. \samp{"daily"} being the common
#'   resolution.
#'
#' @param file a string specifying the location of a dataset. \samp{"file"}
#'   may point to a file on a FTP server or on the local file system.
#'   If the file lives on a FTP server the string must start with
#'   \code{'ftp://'}. If it is NULL (the default), data will be downloaded
#'   from DWD's FTP server.
#'
#' @param destdir directory (string) where intermediate data (downloaded *.zip
#'   file) are stored. If it is NULL (the default) a subdirectory
#'   \samp{downloaded_packages} of the session temporary directory will be used
#'   (and the files will be deleted at the end of the session).
#'   It might be advisable to keep the .zip file since it contains the
#'   stations documentation and metadata (e.g. station shift, used devices,
#'   downtimes etc.).
#'
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar.
#'
#' @return A data.frame with the observed weather data. Beware of gaps and
#'   inhomogeneities! \samp{colnames} contains the original header and
#'   hence German terms.
#'
#'   Content of 'climate'-data.frame:
#'   \tabular{ll}{
#'    \strong{Name} \tab \strong{Description}\cr
#'    STATIONS_ID \tab station id\cr
#'    MESS_DATUM  \tab date\cr
#'    QN_3        \tab quality level of next columns (-)\cr
#'    FX          \tab daily maximum of wind gust (m/s)\cr
#'    FM          \tab daily mean of wind velocity (m/s)\cr
#'    QN_4        \tab quality level of next columns (-)\cr
#'    RSK         \tab daily precipitation height (mm)\cr
#'    RSKF        \tab precipitation form (-)\cr
#'    SDK         \tab daily sunshine duration (h)\cr
#'    SHK_TAG     \tab daily snow depth (cm)\cr
#'    NM          \tab daily mean of cloud cover (1/8)\cr
#'    VPM         \tab daily mean of vapor pressure (hPa)\cr
#'    PM          \tab daily mean of pressure (hPa)\cr
#'    TMK         \tab daily mean of temperature (°C)\cr
#'    UPM         \tab daily mean of relative humidity (\%)\cr
#'    TXK         \tab daily maximum of temperature at 2m height (°C)\cr
#'    TNK         \tab daily minimum of temperature at 2m height (°C)\cr
#'    TGK         \tab daily minimum of air temperature at 5cm above ground (°C)
#'   }
#'
#'   Content of 'precipitation'-data.frame:
#'   \tabular{ll}{
#'    \strong{Name} \tab \strong{Description}\cr
#'    STATIONS_ID \tab station id\cr
#'    MESS_DATUM  \tab date\cr
#'    QN_6        \tab quality level of next columns (-)\cr
#'    RS          \tab daily precipitation height (mm)\cr
#'    RSF         \tab precipitation form (-)\cr
#'    SH_TAG      \tab daily height of snow pack (cm)\cr
#'   }
#'
#' @references
#'   Freely accessible DWD data available via the
#'   \href{https://www.dwd.de/EN/climate_environment/cdc/cdc_node_en.html}{Climate Data Center}.
#'
#' @examples
#' \dontrun{
#' # fetch last 500 days worth of data from station Göttingen
#' clim <- read.DWDdata(id=1691)
#'
#' # save data & metadata (documentation about devices, downtimes etc.)
#' clim <- read.DWDdata(id=1691, destdir='.')
#'
#' # find and download historical data from the Brocken
#' stat.hist <- read.DWDstations(period='historical')
#' brocken.id <- stat.hist[grep("^Brock", stat.hist$name), ]$id
#' clim.brocken <- read.DWDdata(id=brocken.id, period='historical')
#' }
#'
#' @export
read.DWDdata <- function(id, type='climate', period='recent',
                         resolution='daily', file=NULL, destdir=NULL,
                         quiet=FALSE){

  if(is.null(file)){
      # construct url from provided pieces of information
      #------------------------------------------------------------------------
      # check arguments
      if(missing(id)) stop('id is required')
      type <- match.arg(type, choices=c('climate','precipitation'))
      period <- match.arg(period, choices=c('historical', 'recent'))
      resolution <- match.arg(resolution, choices=c('daily', 'monthly'))

      # build up URL  & filename
      baseURL <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate"
      url.type <- switch(type,'climate'='kl','precipitation'='more_precip')
      myURL <- file.path(baseURL, resolution, url.type, period)

      if(period == "historical"){
        # historical filenames can't be guessed -> list all files & filter
        if(!quiet)
          message('Searching for file name in list of available files...')
        files <- .list_available_via_FTP(paste0(myURL, '/'))
        fname <- grep(formatC(id, width=5, flag='0'), files, value=TRUE)
        if(length(fname) == 0)
          stop(paste0('File with id ', sQuote(id),' not available at URL ',
                      sQuote(myURL), '.'))
      } else {
        # recent
        fname <- paste(
          switch(resolution, 'daily'='tageswerte', 'monthly'='monatswerte'),
          switch(type,'climate'='KL','precipitation'='RR'),
          formatC(id, width=5, flag='0'), "akt.zip",
          sep='_'
        )
      }
      url <- file.path(myURL, fname)
      pathToZip <- .download_from_ftp(url, destdir, fname, quiet)
      file <- .connection_to_target_within_zip(pathToZip)

    } else {
      # url for download or path in local filesystem
      #------------------------------------------------------------------------
      if(substr(file, 1, 6) == "ftp://"){
        # a url to an ftp service
        fname <- basename(file)
        pathToZip <- .download_from_ftp(file, destdir, fname, quiet)
        file <- .connection_to_target_within_zip(pathToZip)
      } else {
        # path in local file system
        if (!file.exists(file)) stop(paste("Can not find file", sQuote(file)))
        if(file.access(file, 4) != 0) stop("'file' is not readable")

        if(grepl(".zip$", file)) file <- .connection_to_target_within_zip(file)
      }
    }

  df <- .read.obscureDWDfiles(file)

  md_col <- grep("^MESS_DATUM", names(df))
  for(i in md_col){
    df[, i] <- as.Date(as.character(df[, i]), format="%Y%m%d")
  }

  return(df)
}



#==============================================================================
#
#   Internal Helper Functions
#
#==============================================================================

# Download data from FTP Server
#--------------------------------------------------------------------------
.download_from_ftp <- function(url, destdir, fname, quiet){
  # where to put the zip file ?
  if(is.null(destdir)){
    pathToZip <- file.path(tempdir(), fname)
  } else {
    if (!dir.exists(destdir)) stop("'destdir' is not a directory")
    if(file.access(destdir, 2) != 0) stop("'destdir' is not writeable")
    if(!quiet)
      message(paste("Writing downloaded zip to", sQuote(destdir)))
    pathToZip <- file.path(destdir, fname)
  }

  # download
  tryCatch(
    utils::download.file(url, destfile=pathToZip, quiet=quiet),
    error=function(e)
      stop(paste("Could not download file. Try the URL", sQuote(url),
                 "in a Web-Browser. \nPlease inform the package maintainer",
                 "if you did not use the 'url' argument but the URL is",
                 "malformed anyway."))
  )

  return(pathToZip)
}


# create connection to target file in zip file
#------------------------------------------------------------------------------
.connection_to_target_within_zip <- function(pathToZip){
  file.list <- utils::unzip(pathToZip, list=TRUE)
  hidden.file <- grep('^produkt', file.list$Name, value=TRUE)
  tryCatch(connection <- unz(pathToZip, hidden.file),
           error=function(e)
             stop(paste("Could not open", sQuote(hidden.file),
                        "from zip-file", sQuote(pathToZip), "for reading."))
  )
  return(connection)
}


#  argh, DWD-Data end with EOF (=Ctrl-Z, 0x1A, \032)
#    http://en.wikipedia.org/wiki/End-of-file
#    http://de.wikipedia.org/wiki/End_of_File
#
#  Usual Treatment:
#    treat EOF as comment, read file in binary mode, use gsub to replace EOF
#------------------------------------------------------------------------------
.read.obscureDWDfiles <- function(file){
  # treat EOF as comment
  df <- utils::read.table(file, sep=";", dec=".", header=TRUE,
                          comment.char="\032", strip.white=TRUE,
                          na.strings='-999')
  df$eor <- NULL
  return(df)
}

.read.obscureDWDfiles2 <- function(file){
  # read in binary mode, kick EOF out, write clean data, read again
  temp <- tempfile()
  binaer <- readBin(file, what="raw", n=10000000)
  binaer <- binaer[binaer != 0x1a]
  writeBin(binaer, temp)
  df <- utils::read.table(temp, sep=";", dec=".", header=TRUE,
                   strip.white=TRUE, na.strings="-999")
  df$eor <- NULL
  return(df)
}

.read.obscureDWDfiles3 <- function(file){
  # read in binary mode, ignore warnings, replace EOF with nothing while reading
  f <- file(file, "rb")
  zeilen <- readLines(f)
  close(f)
  df <- utils::read.table(textConnection(gsub("\\\032", '', zeilen)),
                   sep=";", dec=".", header=TRUE, strip.white=TRUE,
                   na.strings="-999")
  df$eor <- NULL
  return(df)
}


# List files available at ftp server
#------------------------------------------------------------------------------
.list_available_via_FTP <- function(url){

  if(!requireNamespace('curl', quietly=TRUE))
    stop("Please install curl, e.g. install.packages('curl')")

  # get a list of all files within a directory
  h <- curl::new_handle(dirlistonly=1, ftp_use_epsv=0)
  con <- curl::curl(url, handle=h)
  filenames <- readLines(con)
  close(con)

  return(filenames)
}
