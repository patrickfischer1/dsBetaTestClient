library(devtools)

devtools::check()
devtools::test(filter="ds.")

quit(status=0)
