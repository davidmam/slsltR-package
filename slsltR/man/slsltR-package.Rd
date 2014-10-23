\name{slsltR-package}
\alias{slsltR-package}
\alias{slsltR}
\docType{package}
\title{
SLSLT Module data management and reporting
}
\description{
This module provides utilities to read and report in graphical and table form on a SLSLT Module Results spreadsheet.
}
\details{
\tabular{ll}{
Package: \tab slsltR\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2014-10-22\cr
License: \tab CC-BY\cr
}

#read the spreadsheet into the session \cr
bs21002 <- readMuduleResults('BS21002 Module results 1314.xlsm')\cr
#produce a summary table\cr
summary(bs21002) \cr
#produce a summary plot\cr
plot(bs21002)\cr
#produce a summary table for the Diet 1 exams\cr
summary(bs21002$diet1)\cr
#produce summary plots for the BS21002 diet 1 exams\cr
plot(bs21002$diet1)\cr

See the readModuleResults() method for detaisl of the SLTModule object.

}
\author{
Maintainer: Dr David Martin <d.m.a.martin@dundee.ac.uk> 
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
}
