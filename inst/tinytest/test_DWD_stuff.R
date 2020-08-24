
# download of list of stations works ------------------------------------------
# don't run download tests on cran, travis, appveyor
if(at_home()){

  # Structure
  # .
  # ├── daily
  # │   ├── climate
  # │   │   ├── historic
  # │   │   └── recent
  # │   └── precip
  # │       ├── historic
  # │       └── recent
  # └── monthly
  #     ├── climate
  #     │   ├── historic
  #     │   └── recent
  #     └── precip
  #         ├── historic
  #         └── recent

  # Test some of the possible lists not all.

  # Column widths, and thus parsing, is different for daily and monthly lists.
  # All monthly station lists have floating point elev.
  # A data.frame with 8 columns ist constructed during reading/parsing.
  # If parsing went well, the columns should have proper types.

  # DAILY =====================================================================
  # daily climate recent ------------------------------------------------------
  expect_silent(stations <- read.DWDstations())
  expect_true(is.data.frame(stations))
  expect_true(length(stations) == 8)
  expect_true(nrow(stations) > 1000)

  expect_true(is.integer(stations$id))
  expect_true(inherits(stations$from, "Date"))
  expect_true(inherits(stations$to, "Date"))
  expect_true(is.integer(stations$elev))
  expect_true(is.numeric(stations$lat))
  expect_true(is.numeric(stations$long))

  # daily climate historic ----------------------------------------------------
  expect_silent(stations <- read.DWDstations(period='historic'))
  expect_true(is.data.frame(stations))
  expect_true(length(stations) == 8)
  expect_true(nrow(stations) > 1000)

  expect_true(is.integer(stations$id))
  expect_true(inherits(stations$from, "Date"))
  expect_true(inherits(stations$to, "Date"))
  expect_true(is.integer(stations$elev))
  expect_true(is.numeric(stations$lat))
  expect_true(is.numeric(stations$long))


  # daily precip recent -------------------------------------------------------
  expect_silent(precip <- read.DWDstations(type='precip'))
  expect_true(is.data.frame(precip))
  expect_true(length(precip) == 8)
  expect_true(nrow(precip) > 3000)

  expect_true(is.integer(precip$id))
  expect_true(inherits(precip$from, "Date"))
  expect_true(inherits(precip$to, "Date"))
  expect_true(is.integer(precip$elev))
  expect_true(is.numeric(precip$lat))
  expect_true(is.numeric(precip$long))


  # MONTHLY ===================================================================
  # monthly climate recent ----------------------------------------------------
  expect_silent(stations <- read.DWDstations(resolution='monthly'))
  expect_true(is.data.frame(stations))
  expect_true(length(stations) == 8)
  expect_true(nrow(stations) > 1000)

  expect_true(is.integer(stations$id))
  expect_true(inherits(stations$from, "Date"))
  expect_true(inherits(stations$to, "Date"))
  expect_true(is.integer(stations$elev))
  expect_true(is.numeric(stations$lat))
  expect_true(is.numeric(stations$long))


  # monthly precip historic ---------------------------------------------------
  expect_silent(precip <- read.DWDstations(type='precip', period='historic',
                                           resolution='monthly'))
  expect_true(is.data.frame(precip))
  expect_true(length(precip) == 8)
  expect_true(nrow(precip) > 3000)

  expect_true(is.integer(precip$id))
  expect_true(inherits(precip$from, "Date"))
  expect_true(inherits(precip$to, "Date"))
  expect_true(is.integer(precip$elev))
  expect_true(is.numeric(precip$lat))
  expect_true(is.numeric(precip$long))
}

# download of data works ------------------------------------------------------
# don't run download tests on cran, travis, appveyor
if(at_home()){

  # fetch last 500 days worth of data from station Göttingen ------------------
  expect_silent(clim <- read.DWDdata(id=1691, quiet=TRUE))
  expect_true(file.exists(file.path(tempdir(), "tageswerte_KL_01691_akt.zip")))

  expect_true(is.data.frame(clim))
  expect_true(length(clim) == 18)
  expect_true(nrow(clim) >= 550)

  expect_true(is.integer(clim$STATIONS_ID))
  expect_equal(unique(clim$STATIONS_ID), 1691L)
  expect_true(inherits(clim$MESS_DATUM, "Date"))
  expect_true(is.integer(clim$QN_3))
  expect_true(is.numeric(clim$RSK))
  expect_true(is.integer(clim$RSKF))
  expect_true(is.numeric(clim$TMK))


  # download monthly climate data from 1270
  expect_silent(clim <- read.DWDdata(id = 1270, type="climate", period="recent",
                                     resolution="monthly", quiet=TRUE))
  expect_true(file.exists(file.path(tempdir(), "monatswerte_KL_01270_akt.zip")))

  expect_true(is.data.frame(clim))
  expect_true(length(clim) == 16)
  expect_true(nrow(clim) >= 19)

  expect_true(is.integer(clim$STATIONS_ID))
  expect_equal(unique(clim$STATIONS_ID), 1270L)
  expect_true(inherits(clim$MESS_DATUM_BEGINN, "Date"))
  expect_true(inherits(clim$MESS_DATUM_ENDE, "Date"))
  expect_true(is.integer(clim$QN_4))
  expect_true(is.numeric(clim$MO_TN))
  expect_true(is.numeric(clim$MO_RR))


  # download historical data from the Brocken if curl available ---------------
  if(requireNamespace("curl", quietly=TRUE)){

    expect_silent(climb <- read.DWDdata(722, period='historical', quiet=TRUE))
    expect_true(is.data.frame(climb))
    expect_true(length(climb) == 18)
    expect_true(nrow(climb) >= 550)

    expect_true(is.integer(climb$STATIONS_ID))
    expect_equal(unique(climb$STATIONS_ID), 722L)
    expect_true(inherits(climb$MESS_DATUM, "Date"))
    expect_true(is.integer(climb$QN_3))
    expect_true(is.numeric(climb$RSK))
    expect_true(is.integer(climb$RSKF))
    expect_true(is.numeric(climb$TMK))
  }
}
