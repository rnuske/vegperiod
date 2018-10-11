context("DWD stuff")

test_that("download of list of stations works", {

  # stations with daily climate data from the last year
  expect_silent(stations <- read.DWDstations())
  expect_is(stations, 'data.frame')
  expect_length(stations, 8)
  expect_gt(nrow(stations), 1000)
  expect_named(stations,
               c("id", "from", "to", "elev", "lat", "long", "name", "state"))

  # precipitation stations with monthly historical data
  expect_silent(precip <- read.DWDstations(type='precip', period='historical',
                                            resolution='monthly'))
  expect_is(precip, 'data.frame')
  expect_gt(nrow(precip), 3000)
})


test_that("download of data works", {
  # fetch last 500 days worth of data from station Göttingen
  clim <- read.DWDdata(id=1691, quiet=TRUE)
  expect_is(clim, 'data.frame')
  expect_length(clim, 18)
  expect_gt(nrow(clim), 366)
  expect_true(file.exists(file.path(tempdir(), "tageswerte_KL_01691_akt.zip")))

  # download historical data from the Brocken
  skip_if_not_installed('curl')
  {
    climb <- read.DWDdata(722, period='historical', quiet=TRUE)
    expect_is(climb, 'data.frame')
    expect_length(climb, 18)
    expect_gt(nrow(climb), 366)
    expect_true(file.exists(file.path(tempdir(), "tageswerte_KL_01691_akt.zip")))
  }
})
