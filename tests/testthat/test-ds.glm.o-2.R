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

context("dsBetaTestClient::ds.glm.o 2")

options(opal.server1="sim1", opal.server2="sim2", opal.server3="sim3")
options(opal.table1="CNSIM.CNSIM1", opal.table2="CNSIM.CNSIM2", opal.table3="CNSIM.CNSIM3")
options(datashield.variables=list("LAB_TSC", "LAB_TRIG"))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.glm.o(): Standard Gaussian regression model for piecewise exponential regression analysis")

mod.D<-ds.glm.o('D$LAB_TSC~D$LAB_TRIG',family="gaussian")
output.D<-c(mod.D$coefficients[,1],mod.D$coefficients[,2])

output.R<-NULL

test_that("glm_gaussian", {
    expect_equal(ds.ls()$sim1[2],output.D,output.R)
})

#
# Tear down
#

source("teardown.R")
