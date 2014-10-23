\name{plot.SLTModule}
\alias{plot.SLTModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convenience method to produce a module report plot
%%  ~~function to do ... ~~
}
\description{
A wrapper for the SLSLT spreadsheet object that presents the data in a coherent and accessible way. \code{plot} produces a bargraph based grade profile for overall, coursework and exam grades.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.SLTModule(module)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{
%%     ~~Describe \code{module} here~~
An SLSLT module.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Dr David Martin d.m.a.martin@dundee.ac.uk
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (module) 
{
    par(ask = F)
    repgrad <- data.frame(Diet.1 = rev(tapply(module$grades$SPR.code, 
        module$grades$Agreed.Overall.Grade.Diet.1, length)), 
        Diet.2 = rev(tapply(module$grades$SPR.code, module$grades$Agreed.Overall.Grade.Diet.2, 
            length)), Coursework = rev(tapply(module$grades$SPR.code, 
            module$grades$Cwk.RAvG, length)), Exam = rev(tapply(module$grades$SPR.code, 
            module$grades$Diet.1.EXAM, length)))
    for (n in names(repgrad)) {
        repgrad[, n][is.na(repgrad[, n])] <- 0
    }
    barplot(t(as.matrix(repgrad[, c(1, 3, 4)])), beside = T, 
        col = c("lightblue", "lightpink", "lightgreen"), main = paste(module$info$modulecode, 
            module$info$year, "grade distribution", collapse = " "), 
        xlab = "Grade", las = 2, ylab = "Students")
    legend(35, max(repgrad$Diet.1, repgrad$Coursework, repgrad$Exam), 
        c("Overall", "Coursework", "Exam"), fill = c("lightblue", 
            "lightpink", "lightgreen"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plot }
\keyword{ Module }% __ONLY ONE__ keyword per line
