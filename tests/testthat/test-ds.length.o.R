#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019 University of Newcastle upon Tyne. All rights reserved.
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

context("dsBetaTestClient::ds.length.o")

options(opal.server1="sim1", opal.server2="sim2", opal.server3="sim3")
options(opal.table1="CNSIM.CNSIM1", opal.table2="CNSIM.CNSIM2", opal.table3="CNSIM.CNSIM3")
options(datashield.variables=list("LAB_TSC"))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.length.o()")
test_that("basic length", {
    res.length <- ds.length.o('D$LAB_TSC')

    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

#
# Tear down
#

source("teardown.R")

context("dsBetaTestClient::ds.length.o done")
