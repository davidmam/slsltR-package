\name{summary.SLTModule}
\alias{summary.SLTModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Provides a summary report for the module
%%  ~~function to do ... ~~
}
\description{
Provides two tables. The first is student status with the number taking the module and passing/withdrawing at each diet.
The second is a breakdown of grades with final agreed grade for each diet, exam grade (diet 1) and coursework grade.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.SLTModule(module, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{
%%     ~~Describe \code{module} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
Dr David Martin  d.m.a.martin@dundee.ac.uk
%%  ~~who you are~~
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
function (module, ...) 
{
    alpha2numeric <- c(A1 = 21, A2 = 20, A3 = 19, AB = 0, B1 = 18, 
        B2 = 17, B3 = 16, BF = 2, C1 = 15, C2 = 14, C3 = 13, 
        CA = 0, CF = 6, D1 = 12, D2 = 11, D3 = 10, MC = 0, MF = 9)
    scount <- length(module$students$SPR.code)
    wd <- length(module$students$Diet.1.Status[module$students$Diet.1.Status == 
        "WD"])
    dc <- length(module$students$Diet.1.Status[module$students$Diet.1.Status == 
        "DC"])
    st <- length(module$students$Diet.1.Status[module$students$Diet.1.Status == 
        "ST"])
    wd2 <- length(module$students$Diet.2.Status[module$students$Diet.2.Status == 
        "WD"])
    dc2 <- length(module$students$Diet.2.Status[module$students$Diet.2.Status == 
        "DC"])
    st2 <- length(module$students$Diet.2.Status[module$students$Diet.2.Status == 
        "ST"])
    d1 <- module$grades$Agreed.Overall.Grade.Diet.1[!is.na(module$grades$Agreed.Overall.Grade.Diet.1)]
    d2 <- module$grades$Agreed.Overall.Grade.Diet.2[!is.na(module$grades$Agreed.Overall.Grade.Diet.2)]
    pass1 <- length(d1[d1 >= "D3"])
    mc.ca1 <- length(module$students$SPR.code[module$grades$Diet.1.EXAM == 
        "CA" | module$grades$Diet.1.EXAM == "MC"])
    fail1 <- scount - (wd + st + dc + pass1 + mc.ca1)
    pass2 <- length(d2[d2 >= "D3"])
    mc.ca2 <- length(module$students$SPR.code[module$grades$Diet.2.EXAM == 
        "CA" | module$grades$Diet.2.EXAM == "MC"])
    fail2 <- scount - (wd2 + st2 + dc2 + pass2 + mc.ca2)
    repdf <- data.frame(Diet.1 = c(scount, pass1, fail1, mc.ca1, 
        wd, st, dc), Diet.2 = c(scount, pass2, fail2, mc.ca2, 
        wd2, st2, dc2), row.names = c("Students registered", 
        "Passed", "Failed", "MC/CA", "WD", "ST", "DC"), stringsAsFactors = F)
    cat("Student Status:\n")
    print(repdf)
    cat("Grade Summary:\n")
    repgrad <- data.frame(Diet.1 = rev(tapply(module$grades$SPR.code, 
        module$grades$Agreed.Overall.Grade.Diet.1, length)), 
        Diet.2 = rev(tapply(module$grades$SPR.code, module$grades$Agreed.Overall.Grade.Diet.2, 
            length)), Coursework = rev(tapply(module$grades$SPR.code, 
            module$grades$Cwk.RAvG, length)), Exam = rev(tapply(module$grades$SPR.code, 
            module$grades$Diet.1.EXAM, length)))
    for (n in names(repgrad)) {
        repgrad[, n][is.na(repgrad[, n])] <- 0
    }
    print(repgrad)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
