#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2018 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("dsBetaTestClient::ds.dataFrame.o")

options(opal.server1="sim1", opal.server2="sim2", opal.server3="sim3")
options(opal.table1="CNSIM.CNSIM1", opal.table2="CNSIM.CNSIM2", opal.table3="CNSIM.CNSIM3")
options(datashield.variables=list('LAB_TSC','LAB_HDL'))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.dataFrame.o() create a dataframe")
test_that("dataframe_exists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame.o(x=myvectors)
    res <- ds.ls()

    expect_equal("D", res$sim1[1])
    expect_equal("df_new", res$sim1[2])
    expect_equal("D", res$sim2[1])
    expect_equal("df_new", res$sim2[2])
    expect_equal("D", res$sim3[1])
    expect_equal("df_new", res$sim3[2])
})


context("dsBetaTestClient::ds.dataFrame.o() errors")
test_that("dataframe_errors", {
    expect_error(ds.dataframe(), "Please provide the name of the list that holds the input vectors!", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")

context("dsBetaTestClient::ds.dataFrame.o done")
