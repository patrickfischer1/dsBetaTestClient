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

ds.asNumeric("time.id","TID")

context("dsBetaTestClient::ds.asFactor.o(force.factor.levels)")

ds.asFactor.o("TID","TID.f")
test_that("with no force.factor.levels", {
  expect_true(ds.class("TID.f"))
  expect_true(ds.table1D("TID.f"))
})

ds.asFactor.o("TID","TID.f2",forced.factor.levels=1:6)
test_that("with forced.factor.levels of 1:6", {
  expect_true(ds.class("TID.f2"))
  expect_true(ds.table1D("TID.f2"))
})

ds.asFactor.o("TID","TID.f3",forced.factor.levels=0:10)
test_that("with force.factor.levels of 0:10", {
  expect_true(ds.class("TID.f3"))
  expect_true(ds.table1D("TID.f3"))
})

ds.asFactor.o("TID","TID.f4",forced.factor.levels=2:3)
test_that("with force.factor.levels of 2:3", {
  expect_true(ds.class("TID.f4"))
  expect_true(ds.table1D("TID.f4"))
})

ds.asFactor.o("TID","TID.f5",forced.factor.levels=c(1,2,3,4,'a','h',5))
test_that("with force.factor.levels of c(1,2,3,4,'a','h',5)", {
  expect_true(ds.class("TID.f5"))
  expect_true(ds.table1D("TID.f5"))
})

context("dsBetaTestClient::ds.asFactor.o(fixed.dummy.vars)")

ds.asFactor.o("TID","TID.mat1",fixed.dummy.vars=TRUE)
test_that("with fixed.dummy.vars of TRUE", {
  expect_true(ds.class("TID.mat1"))
})

ds.asFactor.o("TID","TID.mat6",fixed.dummy.vars=TRUE,baseline.level=6)
test_that("with fixed.dummy.vars of TRUE and baseline.level of 6", {
  expect_true(ds.class("TID.mat6"))
})

#
# Tear down
#

source("teardown.R")
