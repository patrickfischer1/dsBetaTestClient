#
# Set up
#

context("dsClient::ds.dataFrame")

options(datashield.variables=list('LAB_TSC','LAB_HDL'))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.dataFrame() create a dataframe")
myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.dataFrame.o(x=myvectors)
res <- ds.ls()
test_that("dataframe_exists", {
    expect_equal(ds.ls()$sim1[2], "dframe")
    expect_equal(ds.ls()$sim2[2], "dframe")
    expect_equal(ds.ls()$sim3[2], "dframe")
})


context("dsBetaTestClient::ds.dataFrame() errors")
test_that("dataframe_errors", {
    expect_error(ds.dataframe(), "argument is of length zero", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")
