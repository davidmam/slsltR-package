\name{summary.SLTexam}
\alias{summary.SLTexam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce a summary table for an exam diet.
%%  ~~function to do ... ~~
}
\description{
Provides mean/median/dispersion/passrate for scores for the QMP portion, and a per-question breakdown of scores with mean/top/median/sd, numbers selecting that question and passrate.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.SLTexam(exam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exam}{
%%     ~~Describe \code{exam} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A text output that gives two tables, one for the QMP results and one for the SAQ.
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
Dr David Martin d.m.a.martin@dundee.ac.uk
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
function (exam) 
{
    res <- list()
    if (exam$config$QMP) {
        res$QMP <- data.frame(meanscore = mean(exam$qmp$QMPexam, 
            na.rm = T), sdscore = sd(exam$qmp$QMPexam, na.rm = T), 
            topscore = max(exam$qmp$QMPexam, na.rm = T), sitting = length(exam$qmp$QMPexam[!is.na(exam$qmp$QMPexam)]), 
            passing = length(exam$qmp$QMPexam[exam$qmp$QMPexam >= 
                40]), passrate = 100 * length(exam$qmp$QMPexam[exam$qmp$QMPexam >= 
                40])/length(exam$qmp$QMPexam[!is.na(exam$qmp$QMPexam)]))
        cat("Summary results for QMP exam:\n")
        print(res$QMP, collapse = " ")
        cat("\n")
    }
    if (exam$config$SAQ) {
        qdat <- melt(exam$saq_questions, id.var = "grades.SPR.code")
        res$SAQ <- list(byquestion = data.frame(mean = tapply(as.integer(qdat$value), 
            qdat$variable, mean, na.rm = T), sd = tapply(qdat$value, 
            qdat$variable, sd, na.rm = T), topscore = tapply(qdat$value, 
            qdat$variable, max, na.rm = T), median = tapply(as.integer(qdat$value), 
            qdat$variable, median, na.rm = T), sitting = tapply(qdat$value[!is.na(qdat$value)], 
            qdat$variable[!is.na(qdat$value)], length), passing = tapply(qdat$value[qdat$value > 
            0.4 * exam$config$SAQ_marks], qdat$variable[qdat$value > 
            0.4 * exam$config$SAQ_marks], length)))
        res$SAQ$byquestion$passrate <- 100 * res$SAQ$byquestion$passing/res$SAQ$byquestion$sitting
        cat("Summary results for SAQ exam:\n")
        print(res$SAQ$byquestion, collapse = " ")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
