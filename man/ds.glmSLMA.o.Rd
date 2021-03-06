% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.glmSLMA.o.R
\name{ds.glmSLMA.o}
\alias{ds.glmSLMA.o}
\title{ds.glmSLMA.o calling glmDS1.o, glmSLMADS2.o}
\usage{
ds.glmSLMA.o(formula = NULL, family = NULL, offset = NULL,
  weights = NULL, combine.with.metafor = TRUE, dataName = NULL,
  checks = FALSE, maxit = 15, datasources = NULL)
}
\arguments{
\item{formula}{Denotes an object of class formula which is a character string which describes
the model to be fitted. Most shortcut notation allowed by R's standard glm() function is
also allowed by ds.glmSLMA. Many glms can be fitted very simply using a formula like:
"y~a+b+c+d" which simply means fit a glm with y as the outcome variable with a, b, c and d as
covariates. By default all such models also include an intercept (regression constant) term.
If all you need to fit are straightforward models such as these, you do not need to read the
remainder of this information about "formula". But if you need to fit a more complex model in a
customised way, the next paragraph gives a few additional pointers.
As an example, the formula: "EVENT~1+TID+SEXF*AGE.60" denotes fit a glm with the
variable "EVENT" as its
outcome with covariates TID (in this case a 6 level factor [categorical] variable denoting
"time period" with values between 1 and 6), SEXF (also a factor variable denoting sex
and AGE.60 (a quantitative variable representing age-60 in years). The term "1" forces
the model to include an intercept term which it would also have done by default (see above)
but using "1" may usefully be contrasted with using "0" (as explained below).
The "*" between SEXF and AGE.60
means fit all possible main effects and interactions for and between those two covariates.
As SEXF is a factor this is equivalent to writing SEXF+AGE.60+SEXF1:AGE.60 (the
last element being
the interaction term representing the product of SEXF level 1 (in this case female)
and AGE.60).
If the formula had instead been written as :
"EVENT~0+TID+SEXF*AGE.60" the 0 would mean do NOT fit
an intercept term and, because TID happens to be a six level factor this would mean
that the first six model parameters which were originally intercept+TID2+TID3+TID4+TID5+TID6
using the first formula will now become TID1+TID2+TID3+TID4+TID5+TID6.
Conveniently, this means
that the effect of each
time period may now be estimated directly. For example, the effect of time
period 3 is now obtained
directly as TID3 rather than intercept+TID3 as was the case using the original formula.}

\item{family}{This argument identifies the error distribution function to use in
the model. At present
ds.glm has been written to fit family="gaussian" (i.e. a
conventional linear model, family="binomial"
(i.e. a conventional
unconditional logistic regression model), and family = "poisson" (i.e. a
Poisson regression model - of which perhaps the most commonly used application
is for survival analysis
using Piecewise Exponential Regression (PER) which
typically closely approximates Cox regression in its
main estimates and standard
errors. More information about PER can be found in the help folder for
the ds.lexis function which sets up the data structure for a PER. At present the
gaussian family is
automatically coupled with an 'identity' link function, the binomial family with a
'logistic' link
function and the poisson family with a 'log' link function. For the majority of
applications typically
encountered in epidemiology and medical statistics, one  these three classes of
models will
generally be what you need. However, if a particular user wishes us to implement
an alternative family
(e.g. 'gamma') or an alternative family/link combination (e.g. binomial with
probit) we can discuss
how best to meet that request: it will almost certainly be possible, but we may
seek a small amount
of funding or practical programming support from the user in order to ensure that
it can be carried out
in a timely manner}

\item{offset}{A character string specifying the name of a variable to be used as
an offset (effectively
a component of the glm which has a known coefficient a-priori and so does not need to be 
estimated by the model). As an example, an offset is needed to fit a piecewise
exponential regression model. Unlike the standard glm() function in R, ds.glm()
only allows an offset
to be set using the offset= argument, it CANNOT be included directly in the
formula via notation
such as  "y~a+b+c+d+offset(offset.vector.name)". In ds.glmSLMA this must be specified as:
formula="y~a+b+c+d", ..., offset="offset.vector.name" and ds.glmSLMA then incorporates
it appropriately
into the formula itself.}

\item{weights}{A character string specifying the name of a variable containing
prior regression
weights for the fitting process. Like offset, ds.glmSLMA does not allow a weights vector to be
written directly into the glm formula. Using weights provides an alternative way
to fit PER models
if you want to avoid using an offset, but this approach may be viewed as less 'elegant'}

\item{combine.with.metafor}{This argument is Boolean. If TRUE (the default) the
estimates and standard errors for each regression coefficient are pooled across
studies using random effects meta-analysis under maximum likelihood (ML),
restricted maximum likelihood (REML), or fixed effects meta-analysis (FE).}

\item{dataName}{A character string specifying the name of an (optional) dataframe that contains
all of the variables in the glm formula. This avoids you having to specify the name of the
dataframe in front of each covariate in the formula e.g. if the dataframe is
called 'DataFrame' you 
avoid having to write: "DataFrame$y~DataFrame$a+DataFrame$b+DataFrame$c+DataFrame$d"
Processing stops if a non existing data frame is indicated.}

\item{checks}{This argument is a boolean. If TRUE ds.glm then undertakes a series
of checks of
the structural integrity of the model that can take several minutes. Specifically
it verifies that the 
variables in the model are all defined (exist) on the server site at every study
and that they have the correct characteristics required to fit a GLM. The default
value is FALSE
and so it is suggested that they are only made TRUE
if an unexplained problem in the model fit is encountered.}

\item{maxit}{A numeric scalar denoting the maximum number of iterations that are permitted
before ds.glm declares that the model has failed to converge. Logistic regression
and Poisson regression
models can require many iterations, particularly if the starting value of the
regression constant is
far away from its actual value that the glm is trying to estimate. In consequence
we choose to set
maxit=30 - but depending on the nature of the models you wish to fit, you may wish
to be alerted
much more quickly than this if there is a delay in convergence, or you may wish to
allow MORE iterations.}

\item{datasources}{specifies the particular opal object(s) to use, if it is not specified
the default set of opals will be used. The default opals are always called default.opals.
This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
If you wish to specify the second opal server in a set of three, the parameter is specified:
e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
e.g. datasources=opals.em[2,3]}
}
\value{
many of the elements of the output list returned by ds.glmSLMA.o from
each study separately are 
equivalent to those from glm() in native R with potentially disclosive elements
such as individual-level residuals and linear predictors blocked. 
The return results from each study appear first in the return list with one
block of results from each study in the order they appear in datasources.
As regards the elements within each study the most important
elements are included last in the return list because they then appear at the
bottom of a simple print out of the return object. In reverse order, these
key elements in reverse order are:

coefficients:- a matrix in which the first column contains the names of
all of the regression parameters (coefficients) in the model, the second column
contains the estimated values, the third their corresponding standard errors,
the fourth the ratio of estimate/standard error and the fifth the p-value
treating that as a standardised normal deviate

family:- indicates the error distribution and link function used
in the glm

formula:- see description of formula as an input parameter (above)

df.resid:- the residual degrees of freedom around the model

deviance.resid:- the residual deviance around the model

df.null:- the degrees of freedom around the null model (with just an intercept)

dev.null:- the deviance around the null model (with just an intercept)

CorrMatrix:- the correlation matrix of parameter estimates

VarCovMatrix:- the variance covariance matrix of parameter estimates

weights:- the vector (if any) holding regression weights

offset:- the vector (if any) holding an offset (enters glm with a coefficient of 1.0)

cov.scaled:- equivalent to VarCovMatrix

cov.unscaled:- equivalent to VarCovMatrix but assuming dispersion (scale) parameter is 1

Nmissing:- the number of missing observations in the given study

Nvalid:- the number of valid (non-missing) observations in the given study

Ntotal:- the total number of observations in the given study (Nvalid+Nmissing)

data:- - equivalent to input parameter dataName (above)

dispersion:- - the estimated dispersion parameter: deviance.resid/df.resid for
a gaussian family multiple regression model, 1.0 for logistic and poisson regression

call:- - summary of key elements of the call to fit the model

na.action:- - chosen method of dealing with NAs. Commonly na.action=nam.omit
indicating any individual with any data missing that are needed for the model is
exluded from the fit. This includes the outcome variable, covariates,
or any values in a regression weight vector or offset vector. By including
more covariates in a model you may delete extra individuals from an analysis
and this can severely distort inferential tests based on assuming models are
nested (eg likelihood ratio tests)

iter:- the number of iterations required to achieve convergence

there are a small number of more esoteric items of information returned
by ds.glmSLMA.o. Additional information about these can be found in the help
file for the glm() function in native R.

input.beta.matrix.for.SLMA:- a matrix containing the vector of coefficient
estimates from each study. In combination with the corresponding standard errors
(see input.se.matrix.for.SLMA) these can be imported directly into a study level
meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA

input.se.matrix.for.SLMA:- a matrix containing the vector of standard error
estimates for coefficients from each study. In combination with the coefficients
(see input.beta.matrix.for.SLMA) these can be imported directly into a study level
meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA

SLMA.pooled.estimates:- a matrix containing pooled estimates for each
regression coefficient across all studies with pooling under SLMA via 
random effects meta-analysis under maximum likelihood (ML), restricted maximum
likelihood (REML) or via fixed effects meta-analysis (FE)
}
\description{
Fits a generalized linear model (glm) on data from a single or multiple sources
}
\details{
Fits a glm on data from a single source or from multiple sources. In the latter case 
(when using ds.glmSLMA.o), the glm is fitted to convergence in each data source and the
estimates and standard errors
returned from each study separately. When these are then pooled using a function such as 
ds.metafor, this is a form of study level meta-analysis (SLMA). This contrasts with the setting
when ds.glm is used to fit a glm to multiple sources. In that case the update at each
iteration uses data from all sources simultaneously and returns the updated parameter
estimates to all sources to start the next iteration. The approach (using ds.glm) is
mathematically equivalent to placing all individual-level data from all sources in
one central warehouse and analysing those data as one simultaneous block using the conventional
glm() function in R. The SLMA approach offers some advantages when there is marked heterogeneity
between sources that cannot simply be corrected with fixed effects each reflecting a study
or centre effect. In particular fixed effects cannot simply be used in this way when there
there is heterogeneity in the effect of scientific interest.
}
\seealso{
\link{ds.lexis} for survival analysis using piecewise exponential regression
}
\author{
DataSHIELD Development Team
}
