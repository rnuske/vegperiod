context("vegperiod")

data(goe)


test_that("vegperiod throws erors", {
  expect_equal(vegperiod(dates=goe$date, Tavg=goe$t, start.method="Menzel",
                         end.method="vonWilpert", species="Picea abies (spaet)",
                         est.prev=0, Tsum.out=FALSE),
               data.frame(
                 year  = 2002:2010,
                 start = c(134L, 131L, 125L, 131L, 135L, 132L, 130L, 121L, 121L),
                 end   = c(279L, 279L, 279L, 279L, 271L, 279L, 279L, 279L, 279L)
                 )
  )
})


test_that("vegperiod standard works", {
  expect_equal(vegperiod(dates=goe$date, Tavg=goe$t, start.method="Menzel",
                         end.method="vonWilpert", species="Picea abies (spaet)",
                         est.prev=0, Tsum.out=FALSE),
               data.frame(
                 year = 2002:2010,
                 start = c(134L, 131L, 125L, 131L, 135L, 132L, 130L, 121L, 121L),
                 end = c(279L, 279L, 279L, 279L, 271L, 279L, 279L, 279L, 279L)
                 )
               )
})


