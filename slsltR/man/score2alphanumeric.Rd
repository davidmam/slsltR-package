\name{score2alphanumeric}
\alias{score2alphanumeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Utility function to convert grades from a numeric score to an alphanumeric grade
%%  ~~function to do ... ~~
}
\description{
Converts the numerical score to the alphanumeric grade using the appropriate conversion table. Scores can be scaled appropriately.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
score2alphanumeric(x, scale='stringent', max=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A vector of numeric grades. For 'stringent' and 'standard' scales these are scaled to percentages then rounded to integers and the appropriate grade is returned. For the '21point' scale the grade is returend directly.
  0 is recorded as AB.
%%     ~~Describe \code{x} here~~
}
  \item{scale}{
  The scale which should be used. Select from 'stringent' (the default for coursework), 'standard' (usually for exams or immediate response coursework), or '21point' where the standard 21 point grading has been used. 
%%     ~~Describe \code{strict} here~~
}
\item{max}{Where a score is not percentage based, the maximum value can be given as \code{max} and it will be converted to a percentage appropriately.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
21point grade scale:\cr
AGGREGATION  GRADE\cr
0	AB\cr
1-3 BF\cr
4-6	CF\cr
7-9	MF\cr
10	D3\cr
11	D2\cr
12	D1\cr
13	C3\cr
14	C2\cr
15	C1\cr
16	B3\cr
17	B2\cr
18	B1\cr
19	A3\cr
20	A2\cr
21	A1\cr

Percentage scales
STANDARD  STRINGENT  GRADE
0	0 AB
1-19  1-19	BF
20-34	20-34 CF
35-39 35-39	MF
40-42 40-44	D3
43-45 45-49	D2
46-49 50-54	D1
50-52 55-59	C3
53-55	60-64 C2
56-59 65-69	C1
60-62 70-74	B3
63-65 75-79	B2
66-69 80-84	B1
70-79	85-89 A3
80-89 90-94	A2
90-100  95-100	A1

}
\value{
A list of ordered factors corresponding to the alphanumeric grades. Factors are ordered from CA, MC, AB through to A1.
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
Dr David Martin <d.m.a.martin@dundee.ac.uk>
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
function (x, strict = T) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
