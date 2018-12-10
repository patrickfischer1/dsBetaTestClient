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

context("dsBetaTestClient::ds.glm.o 1")

options(opal.server1="survival1", opal.server2="survival2", opal.server3="survival3")
options(opal.table1='SURVIVAL.EXPAND_WITH_MISSING1', opal.table2='SURVIVAL.EXPAND_WITH_MISSING2', opal.table3='SURVIVAL.EXPAND_WITH_MISSING3')
options(datashield.variables=list('survtime', 'time.id', 'female', 'age.60'))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.glm.o(): Standard Poisson regression model for piecewise exponential regression analysis")

mod.D<-ds.glm.o("survtime~1+time.id+female+age.60",family="poisson",offset="log.surv")
output.D<-c(mod.D$coefficients[,1],mod.D$coefficients[,2])

mod.R<-glm(SURVTIME~1+TIME.ID+FEMALE+AGE.60,family="poisson",offset=LOG.SURV)
output.R<-c(summary(mod.R)$coefficients[,1],summary(mod.R)$coefficients[,2])

test_that("glm_poisson", {
    expect_equal(ds.ls()$sim1[2],output.D,output.R)
})

context("dsBetaTestClient::ds.glm.o() errors")
test_that("glm_errors", {
    expect_error(ds.glm.o(), "argument is of length zero", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")
