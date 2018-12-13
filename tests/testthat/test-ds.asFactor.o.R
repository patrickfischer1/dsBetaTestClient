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

context("dsBetaTestClient::ds.asFactor.o")

options(opal.server1="survival1", opal.server2="survival2", opal.server3="survival3")
options(opal.table1='SURVIVAL.EXPAND_WITH_MISSING1', opal.table2='SURVIVAL.EXPAND_WITH_MISSING2', opal.table3='SURVIVAL.EXPAND_WITH_MISSING3')
options(datashield.variables=list('survtime', 'time.id', 'female', 'age.60'))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.asFactor.o()")

ds.asNumeric("EM$time.id","TID")

context("dsBetaTestClient::ds.asFactor.o(force.factor.levels)")

ds.asFactor.o("TID","TID.f")
test_that("with no force.factor.levels", {
  expect_true(ds.class("TID.f"))
  expect_true(ds.table1D("TID.f"))
})

context("dsBetaTestClient::ds.asFactor.o(fixed.dummy.vars)")

#
# Tear down
#

source("teardown.R")
