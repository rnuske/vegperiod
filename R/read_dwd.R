#==============================================================================
#
# Fetch climate data from DWD's open data program
#
# Robert Nuske 2015-03-30
#==============================================================================


#' Fetch information about DWD weather stations
#'
#' Get a listing of advertised stations of the German Weather Service (Deutscher
#' Wetterdienst, DWD). The list differs for station type, time period and
#' resolution. The list of advertised stations is usually not identical with
#' the currently available stations.
#'
#'
#' The freely accessible part of the Climate Data Center of the German Weather
#' Service (Deutscher Wetterdienst, DWD) is part of the DWD's mandate for
#' basic supply of information (termed "Grundversorgung"). These services may
#' be used without any restrictions (no fees will be charged and in general
#' there are no restrictions for the use the data), provided that the source is
#' indicated as laid down in the
#' \href{http://www.gesetze-im-internet.de/geonutzv/BJNR054700013.html}{"GeoNutzV"}
#' ordinance. The source reference shall meet the following rules:
#' \itemize{
#'   \item Where data are used without modification, the source reference shall
#'     read "Source: Deutscher Wetterdienst" or just consist of the DWD logo.
#'   \item If the data are modifyed, the source reference shall specify as
#'     precisely as possible the extent of such, e.g. "Based on data from
#'     Deutscher Wetterdienst, figures rounded".
#' }
#' A more detailed description of the rules can be found in the official German
#' \href{../doc/Nutzungsbedingungen_German.pdf}{Nutzungsbedingungen}
#' or the translated \href{../doc/Terms_of_use.pdf}{Terms of use}.
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
#' @return A data.frame with all advertised weather stations. The data.frame
#' contains the variables id, from, to, elev, lat, long, name, state.
#'
#' @references
#'   Freely accessible DWD data are available via the
#'   \href{http://www.dwd.de/cdc}{Climate Data Center}
#'
#' @examples
#' \dontrun{
#' # stations with daily climate data from the last year
#' stations <- read.DWDstations()
#'
#' # precipitation stations with monthly histrical data
#' precip <- read.DWDstations(type='precip', period='historical',
#'                            resolution='monthly')
#'
#' # list stations updated within last week
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
  baseURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"

  url.type <- switch(type,'climate'='kl','precipitation'='more_precip')
  pre.type <- switch(type, 'climate'='KL','precipitation'='RR')
  pre.res <- switch(resolution, 'daily'='Tageswerte', 'monthly'='Monatswerte')

  fname <- paste0(pre.type, "_", pre.res, "_Beschreibung_Stationen.txt")
  myURL <- paste(baseURL, resolution, url.type, period, fname, sep='/')

  myCols <- c('id', 'from', 'to', 'elev', 'lat', 'long', 'name', 'state')
  myWidths <- switch(type,
                     'climate'=c(11, 9, 9, 15, 12, 10, 41, 23),
                     'precipitation'=c(5, 9, 9, 15, 12, 10, 41, 23))

  options(warn=-1) # the warning about last line is wrong -> don't show it
  # treat EOF as comment
  df <- read.fwf(myURL, widths=myWidths, skip=2, comment.char="\032",
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
#' accessible part of the Climate Data Center of the German Weather Service
#' (Deutscher Wetterdienst, DWD).
#'
#' An introduction to the data available at the Climate Data Center can be
#' found in the German
#' \href{../doc/Liesmich_intro_CDC_ftp.pdf}{Liesmich_intro_CDC_ftp}
#' or the translated
#' \href{../doc/Readme_intro_CDC_ftp.pdf}{Readme_intro_CDC_ftp}.
#'
#' The freely accessible part of the Climate Data Center of the German Weather
#' Service (Deutscher Wetterdienst, DWD) is part of the DWD's mandate for
#' basic supply of information (termed "Grundversorgung"). These services may
#' be used without any restrictions (no fees will be charged and in general
#' there are no restrictions for the use the data), provided that the source is
#' indicated as laid down in the
#' \href{http://www.gesetze-im-internet.de/geonutzv/BJNR054700013.html}{"GeoNutzV"}
#' ordinance. The source reference shall meet the following rules:
#' \itemize{
#'   \item Where data are used without modification, the source reference shall
#'     read "Source: Deutscher Wetterdienst" or just consist of the DWD logo.
#'   \item If the data are modifyed, the source reference shall specify as
#'     precisely as possible the extent of such, e.g. "Based on data from
#'     Deutscher Wetterdienst, figures rounded".
#' }
#' A more detailed description of the rules can be found in the official German
#' \href{../doc/Nutzungsbedingungen_German.pdf}{Nutzungsbedingungen}
#' or the translated \href{../doc/Terms_of_use.pdf}{Terms of use}.
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
#'   may point to a file on a FTP-server or on the local file system.
#'   If the file lives on a FTP-server the string must start with
#'   \code{'ftp://'}.
#'
#' @param destdir directory (string) where intermediate data (downloaded *.zip
#'   file) are stored. If it is NULL (the default) a subdirectory
#'   \samp{downloaded_packages} of the session temporary directory will be used
#'   (and the files will be deleted at the end of the session).
#'   It might be advisable to keep the .zip file since it contains the
#'   stations documentation and metadata (eg. station shift, used devices,
#'   downtimes etc.).
#'
#' @return A data.frame with the observed weather data. Be aware there might be
#'   gaps and inhomogeneities! \samp{colnames} contains the original header and
#'   hence German terms. Translation of usual column names:
#'   \tabular{ll}{
#'    \strong{original name} \tab \strong{translation}\cr
#'    STATIONS_ID              \tab id\cr
#'    MESS_DATUM               \tab date\cr
#'    QUALITAETS_NIVEAU        \tab quality level\cr
#'    LUFTTEMPERATUR           \tab average temperature\cr
#'    DAMPFDRUCK               \tab vapor pressure\cr
#'    BEDECKUNGSGRAD           \tab cloud cover\cr
#'    LUFTDRUCK_STATIONSHOEHE  \tab air pressure\cr
#'    REL_FEUCHTE              \tab humidity\cr
#'    WINDGESCHWINDIGKEIT      \tab wind speed\cr
#'    LUFTTEMPERATUR_MAXIMUM   \tab max temperature\cr
#'    LUFTTEMPERATUR_MINIMUM   \tab min temperature\cr
#'    LUFTTEMP_AM_ERDB_MINIMUM \tab min temperature at ground level\cr
#'    WINDSPITZE_MAXIMUM       \tab max wind speed\cr
#'    NIEDERSCHLAGSHOEHE       \tab precipitation\cr
#'    NIEDERSCHLAGSHOEHE_IND   \tab type of precipitation\cr
#'    SONNENSCHEINDAUER        \tab sunshine duration\cr
#'    SCHNEEHOEHE              \tab snow height
#'   }
#'
#' @references
#'   Freely accessible DWD data are available via the
#'   \href{http://www.dwd.de/cdc}{Climate Data Center}.
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
#'
#' # get recent meteorological data fom a randomly selected station
#' require('RCurl')
#' dwdURL <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/",
#'                  "climate/daily/kl/recent/")
#' filenames <- getURL(dwdURL, ftp.use.epsv=FALSE, dirlistonly=TRUE)
#' filenames <- strsplit(filenames, '\r*\n')[[1]]
#' stations <- grep("Beschreibung_Stationen.txt$", filenames,
#'                  invert=TRUE, value=TRUE)
#' rnd.station <- sample(stations, 1)
#' rnd.clim <- read.DWDdata(file=paste0(dwdURL, rnd.station))
#' }
#'
#' @export
read.DWDdata <- function(id, type='climate', period='recent',
                         resolution='daily',  file=NULL, destdir=NULL){

  if(is.null(file)){
      # construct url from provided pieces of information
      #------------------------------------------------------------------------
      # check arguments
      if(missing(id)) stop('id is required')
      type <- match.arg(type, choices=c('climate','precipitation'))
      period <- match.arg(period, choices=c('historical', 'recent'))
      resolution <- match.arg(resolution, choices=c('daily', 'monthly'))

      # build up URL  & filename
      baseURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"
      url.type <- switch(type,'climate'='kl','precipitation'='more_precip')
      myURL <- file.path(baseURL, resolution, url.type, period)

      if(period == "historical"){
        # historical filenames can't be guessed -> list all files & filter
        message('Searching for correct file name in list of available files...')
        files <- .list_available_via_FTP(paste0(myURL, '/'))
        fname <- grep(formatC(id, width=5, flag='0'), files, value=TRUE)
        if(length(fname) == 0)
          stop(paste0('File with id ', sQuote(id),' not available at URL ',
                      sQuote(myURL), '.'))
      } else {
        # recent
        fname <- paste0(formatC(id, width=5, flag='0'), "_akt.zip")

        if(type == 'climate' && resolution == 'monthly'){
          fname <- paste(
            switch(resolution, 'daily'='tageswerte', 'monthly'='monatswerte'),
            fname, sep='_'
          )
        } else {
          fname <- paste(
            switch(resolution, 'daily'='tageswerte', 'monthly'='monatswerte'),
            switch(type,'climate'='KL','precipitation'='RR'),
            fname, sep='_'
          )
        }
      }
      url <- file.path(myURL, fname)
      pathToZip <- .download_from_ftp(url, destdir, fname)
      file <- .connection_to_target_within_zip(pathToZip)

    } else {
      # url for download or path in local filesystem
      #------------------------------------------------------------------------
      if(substr(file, 1, 6) == "ftp://"){
        # a url to an ftp service
        fname <- basename(file)
        pathToZip <- .download_from_ftp(file, destdir, fname)
        file <- .connection_to_target_within_zip(pathToZip)
      } else {
        # path in local file system
        if (!file.exists(file)) stop(paste("Can not find file", sQuote(file)))
        if(file.access(file, 4) != 0) stop("'file' is not readable")

        if(grepl(".zip$", file)) file <- .connection_to_target_within_zip(file)
      }
    }

  df <- .read.obscureDWDfiles(file)

  if('MESS_DATUM' %in% names(df))
    df$MESS_DATUM <- as.Date(as.character(df$MESS_DATUM), format="%Y%m%d")

  return(df)
}



#==============================================================================
#
#   Internal Helper Functions
#
#==============================================================================

# Download data from FTP Server
#--------------------------------------------------------------------------
.download_from_ftp <- function(url, destdir, fname){
  # where to put the zip file ?
  if(is.null(destdir)){
    pathToZip <- file.path(tempdir(), fname)
  } else {
    if (!dir.exists(destdir)) stop("'destdir' is not a directory")
    if(file.access(destdir, 2) != 0) stop("'destdir' is not writeable")
    message(paste("Writing downloaded zip to", sQuote(destdir)))
    pathToZip <- file.path(destdir, fname)
  }

  # download
  tryCatch(
    download.file(url, destfile=pathToZip),
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
  file.list <- unzip(pathToZip, list=TRUE)
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
  df <- read.table(file, sep=";", dec=".", header=TRUE, comment.char="\032",
                   strip.white=TRUE, na.strings='-999')
  df$eor <- NULL
  return(df)
}

.read.obscureDWDfiles2 <- function(file){
  # read in binary mode, kick EOF out, write clean data, read again
  temp <- tempfile()
  binaer <- readBin(file, what="raw", n=10000000)
  binaer <- binaer[binaer != 0x1a]
  writeBin(binaer, temp)
  df <- read.table(temp, sep=";", dec=".", header=TRUE,
                   strip.white=TRUE, na.strings="-999")
  df$eor <- NULL
  return(df)
}

.read.obscureDWDfiles3 <- function(file){
  # read in binary mode, ignore warnings, replace EOF with nothing while reading
  f <- file(file, "rb")
  zeilen <- readLines(f)
  close(f)
  df <- read.table(textConnection(gsub("\\\032", '', zeilen)),
                   sep=";", dec=".", header=TRUE, strip.white=TRUE,
                   na.strings="-999")
  df$eor <- NULL
  return(df)
}


# List files available at ftp server
#------------------------------------------------------------------------------
.list_available_via_FTP <- function(url){

  if(!requireNamespace('RCurl', quietly=TRUE))
    stop("Please install RCurl, eg. install.packages('RCurl')")

  # get a list of all files within a directory
  filenames <- RCurl::getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)

  # deal with newlines as \n or \r\n
  filenames <- strsplit(filenames, "\r*\n")[[1]]

  return(filenames)
}


# Hack to provide dir.exists in R < 3.2
#------------------------------------------------------------------------------
if(!existsFunction('dir.exists')){
  dir.exists <- function(x){ file.exists(x) & file.info(x)$isdir }
}
