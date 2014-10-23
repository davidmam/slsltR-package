\name{plot.SLTexam}
\alias{plot.SLTexam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convenience plot function for the SLTexam.
%%  ~~function to do ... ~~
}
\description{
Produces plots to describe the response for QMP and multiple freeform question (SAQ) based exams. 
The plot for QMPs is a histogram across the grade profile.
The plot for SAQs is a box plot of score across questions. It is intended to be interactive and requires a carriage return (ENTER key) press between plots. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.SLTexam(exam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exam}{
%%     ~~Describe \code{exam} here~~
\code{exam} is an exam object form the \code{SLTModule$diet1} or \code{SLTModule$diet2}
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
function (exam) 
{
    par(ask = T)
    if (exam$config$QMP) {
        hist(as.integer(exam$qmp$QMPexam), main = paste("QMP exam", 
            exam$config$title, collapse = " "), xlab = "Score", 
            breaks = 20, col = "lightgrey")
    }
    if (exam$config$SAQ) {
        qdat <- melt(exam$saq_questions, id.var = "grades.SPR.code")
        boxplot(as.integer(qdat$value) ~ qdat$variable, main = "SAQ response by question", 
            sub = exam$config$title, ylab = "Score", col = c("antiquewhite2", 
                "cadetblue1", "darkseagreen2", "orchid1", "darkgoldenrod1", 
                "khaki1", "lavender"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Plot }
\keyword{ Exam }% __ONLY ONE__ keyword per line
