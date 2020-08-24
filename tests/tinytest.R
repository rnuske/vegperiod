
if ( requireNamespace("tinytest", quietly=TRUE) ){
  # run tests requiring downloads only on my hardware
  home <- (unname(Sys.info()["user"]) == "rnuske")
  tinytest::test_package("vegperiod", at_home=home)
}

