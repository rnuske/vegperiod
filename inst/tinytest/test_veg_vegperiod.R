
data(goe)


# vegperiod throws errors
expect_equal(vegperiod(dates=goe$date, Tavg=goe$t, start.method="Menzel",
                       end.method="vonWilpert", species="Picea abies (spaet)",
                       est.prev=0, Tsum.out=FALSE),
             data.frame(
               year  = 2002:2010,
               start = c(134L, 131L, 125L, 131L, 135L, 132L, 130L, 121L, 121L),
               end   = c(279L, 279L, 279L, 279L, 271L, 279L, 279L, 279L, 279L)
             )
)


# vegperiod standard works
expect_equal(vegperiod(dates=goe$date, Tavg=goe$t, start.method="Menzel",
                       end.method="vonWilpert", species="Picea abies (spaet)",
                       est.prev=0, Tsum.out=FALSE),
             data.frame(
               year  = 2002:2010,
               start = c(134L, 131L, 125L, 131L, 135L, 132L, 130L, 121L, 121L),
               end   = c(279L, 279L, 279L, 279L, 271L, 279L, 279L, 279L, 279L)
             )
)


# temperature is sound (not multiplied by 10)
# problem showed in this case by time series > 26 years
# example data set with 30 yrs from Ronald Bialozyt
load('./rb30.RData')

expect_error(
  vegperiod(dates=rb30$date, Tavg=rb30$tm, start.method="Menzel",
            end.method="vonWilpert", species="Picea abies (spaet)",
            est.prev=3, Tsum.out=FALSE),
  "Daily mean temperatures are too small/large")

expect_equal(
  vegperiod(dates=rb30$date, Tavg=rb30$tm/10, start.method="Menzel",
            end.method="vonWilpert", species="Picea abies (spaet)",
            est.prev=3, Tsum.out=FALSE),
  data.frame(
    year  = 2021:2050,
    start = c(134L, 122L, 125L, 137L, 133L, 127L, 121L, 123L, 132L, 120L,
              132L, 134L, 116L, 132L, 107L, 119L, 119L, 120L, 120L, 123L,
              123L, 131L, 128L, 120L, 130L, 117L, 129L, 119L, 125L, 120L),
    end   = c(279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L,
              279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L,
              279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L)
  )
)

# there should be an error with shorter times series as well
rb10 <- rb30[rb30$date <= as.Date('2030-12-31'), ]
expect_error(
  vegperiod(dates=rb10$date, Tavg=rb10$tm, start.method="Menzel",
            end.method="vonWilpert", species="Picea abies (spaet)",
            est.prev=3, Tsum.out=FALSE),
  "Daily mean temperatures are too small/large")

expect_equal(
  vegperiod(dates=rb10$date, Tavg=rb10$tm/10, start.method="Menzel",
            end.method="vonWilpert", species="Picea abies (spaet)",
            est.prev=3, Tsum.out=FALSE),
  data.frame(
    year  = 2021:2030,
    start = c(134L, 122L, 125L, 137L, 133L, 127L, 121L, 123L, 132L, 120L),
    end   = c(279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L, 279L)
  )
)


# Bug report by S. Timmke
# StdMeteo did report 0 if it did not find an end (eg. Magdeburg in 2006)
# now it returns last DOY in that case

load("./md_2006.RData")

expect_equal(
  vegperiod(dates=md_2006$date, Tavg=md_2006$t, start="StdMeteo", end="StdMeteo",
            Tsum.out=TRUE),
  data.frame(
    year  = 2005:2007,
    start = c(9L, 89L, 8L),
    end   = c(324L, 365L, 319L),
    Tsum  = c(3437.1, 3913.5, 3829.3)
    )
)
