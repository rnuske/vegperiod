# startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("Note: Reading 'historical' data with",
          "read.DWDdata() requires the package 'RCurl'.")
    )
}
