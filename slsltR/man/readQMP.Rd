\name{readQMP}
\alias{readQMP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Method to read a QMP results spreadsheet. 
%%  ~~function to do ... ~~
}
\description{
This method reads a QMP Exam Results spreadsheet in the format that contains all details for all questions into an accesisble data structure. Data is returned as an object of class QMPtest which is a list of data (see below)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readQMP(filename = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  Filename for the QMP results spreadsheet. If not given then file.choose() is called to select the file interactively.
%%     ~~Describe \code{filename} here~~
}
}
\details{
This does presume that the correct format spreadsheet is loaded.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The QMPtest is a LIST.
\item{test_name}{The title of the test}
\item{max_score}{ Highest possible score from the test}
\item{results}{Raw score for each student who took the test. 
}
\item{scores}{Scores for each question in long format. Id variable is \code{Participant}. \code{Question} and \code{Score} comprise the group variable and the value}
\item{questions}{The raw question names as abstracted from the exam rubric}
\item{byquestion}{A data frame with five columns. \code{name} - the question identifier, \code{max} - th emaximmum points available for that question, \code{total} - total points awarded for that question, \code{response} - percentage of possible maximum points for that question achieved and \code{rank} - rank of the question (lowest first) in terms of response.%%  ~Describe the value returned
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
\code{\link{SLTExam}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename = NULL, withQMP = T, withSAQ = T, SAQscore = 10, 
    SAQcount = 4) 
{
    if (is.null(filename)) 
        filename <- file.choose()
    wb <- loadWorkbook(filename)
    file <- readWorksheet(wb, "DATA ENTRY", endRow = 350, startRow = 4, 
        header = T, useCachedValues = T)
    modulename <- readWorksheet(wb, "DATA ENTRY", 1, 1, 1, 1, 
        header = F, simp = T)
    moduleinfo <- strsplit(modulename, " ")[[1]]
    info <- list(modulecode = moduleinfo[1], modulename = paste(moduleinfo[2:(length(moduleinfo) - 
        1)], collapse = " "), year = moduleinfo[length(moduleinfo)])
    weightings <- readWorksheet(wb, "DATA ENTRY", startRow = 3, 
        endRow = 3, startCol = 10, endCol = 38, header = F, useCachedValues = T)
    endoffile = length(file[, 1]) - 50
    checkrow <- function(x, y) {
        x[apply(x, 1, function(z, w) {
            sum(is.na(z)) < w
        }, w = y), ]
    }
    data <- checkrow(file, 39)
    checkcolumn <- function(x) {
        substr(x, 1, 1) != "X" & x != "NA" & !is.na(x) & substr(x, 
            1, 4) != "c..P"
    }
    an_grades <- rev(c("A1", "A2", "A3", "B1", "B2", "B3", "C1", 
        "C2", "C3", "D1", "D2", "D3", "MF", "CF", "BF", "AB", 
        "MC", "CA"))
    attendances <- names(data)[107:136]
    attendances <- attendances[checkcolumn(attendances)]
    coursework <- names(data)[10:38]
    coursework <- coursework[checkcolumn(coursework)]
    exam1 <- names(data[39:72])
    exam1 <- exam1[checkcolumn(exam1)]
    exam2 <- names(data[73:110])
    exam2 <- exam2[checkcolumn(exam2)]
    names(weightings) <- coursework
    cw_weight = apply(weightings, 1, sum)
    exam_weight = 100 - cw_weight
    thisobject <- list(info = info, coursework = weightings, 
        structure = list(coursework = cw_weight, exam = exam_weight), 
        registerable = attendances, data = data)
    class(thisobject) <- "SLTModule"
    grades <- data[is.na(data[, 1]) == F, checkcolumn(names(data))]
    copygrades <- function(x, y) {
        for (e in 1:length(x)) {
            if (is.na(y[e])) 
                y[e] <- x[e]
        }
        y
    }
    alpha2numeric <- c(A1 = 21, A2 = 20, A3 = 19, AB = 0, B1 = 18, 
        B2 = 17, B3 = 16, BF = 2, C1 = 15, C2 = 14, C3 = 13, 
        CA = 0, CF = 6, D1 = 12, D2 = 11, D3 = 10, MC = 0, MF = 9)
    calc.cwk <- function(x) {
        sum(alpha2numeric[x[x != "CA" & x != "MC"]] * weightings[x != 
            "CA" & x != "MC"])/sum(weightings[x != "CA" & x != 
            "MC"])
    }
    calc.cwk.weight <- function(x) {
        sum(weightings[x != "CA" & x != "MC"])
    }
    numeric2alpha21 <- function(x) {
        na_grades = c(rep("BF", 2), rep("CF", 4), rep("MF", 3), 
            "D3", "D2", "D1", "C3", "C2", "C1", "B3", "B2", "B1", 
            "A3", "A2", "A1")
        na_grades[as.integer(floor(x))]
    }
    copygrades(grades$Diet.1.EXAM, grades$Diet.2.EXAM)
    grades$Cwk.RAvB <- apply(grades[, coursework], 1, calc.cwk)
    grades$Cwk.RAvG <- factor(numeric2alpha21(grades$Cwk.RAvB), 
        levels = an_grades, ordered = T)
    grades$Cwk.weight <- apply(grades[, coursework], 1, calc.cwk.weight)
    grades$Exam.RAvB <- alpha2numeric[grades$Diet.1.EXAM]
    grades$RAvB <- (grades$Cwk.RAvB * grades$Cwk.weight + grades$Exam.RAvB * 
        exam_weight)/(grades$Cwk.weight + exam_weight)
    grades$Exam.RAvB2 <- alpha2numeric[grades$Diet.2.EXAM]
    grades$RAvB2 <- (grades$Cwk.RAvB * grades$Cwk.weight + grades$Exam.RAvB2 * 
        exam_weight)/(grades$Cwk.weight + exam_weight)
    grades$Agreed.Overall.Grade.Diet.1 <- factor(grades$Agreed.Overall.Grade.Diet.1, 
        levels = an_grades, ordered = T)
    grades$Agreed.Overall.Grade.Diet.2 <- factor(grades$Agreed.Overall.Grade.Diet.2, 
        levels = an_grades, ordered = T)
    grades$Diet.1.EXAM <- factor(grades$Diet.1.EXAM, levels = an_grades, 
        ordered = T)
    grades$Diet.2.EXAM <- factor(grades$Diet.2.EXAM, levels = an_grades, 
        ordered = T)
    gradefactor <- function(x) {
        factor(x, levels = an_grades, ordered = T)
    }
    absence <- melt(grades[, c("SPR.code", attendances)], id.var = "SPR.code")
    missedwork <- melt(grades[, c("SPR.code", coursework)], id.var = "SPR.code")
    d2 <- merge(grades, tapply(absence$SPR.code, list(absence$SPR.code, 
        absence$value), length), by.x = "SPR.code", by.y = "row.names")
    d2$MC[is.na(d2$MC)] <- 0
    d2$CA[is.na(d2$CA)] <- 0
    d2$AB[is.na(d2$AB)] <- 0
    d2$all <- d2$AB + d2$MC + d2$CA
    thisobject$absences <- d2[, c("SPR.code", "AB", "MC", "CA", 
        "all")]
    thisobject$grades <- grades[, c("SPR.code", "Cwk.RAvB", "Exam.RAvB", 
        "RAvB", "Cwk.RAvG", "Diet.1.EXAM", "Agreed.Overall.Grade.Diet.1", 
        "Agreed.Overall.Grade.Diet.2")]
    thisobject$students <- grades[, c("SPR.code", "Sort.Name", 
        "First.Name", "Route", "Email", "Diet.1.Status", "Diet.2.Status", 
        "COMMENTS")]
    processgrades <- function(x, title = "Exam") {
        exam <- list(total = rep(0, length(x[, 1])), config = list(SAQ = withSAQ, 
            QMP = withQMP, title = title))
        if (withQMP) {
            qmpscores <- x[, grep("QMP.Exam", names(x))]
            qmpscores[qmpscores == "Pending"] <- NA
            exam$qmp = data.frame(SPR.code = x[, 1], QMPexam = as.integer(qmpscores))
            exam$total <- exam$total + exam$qmp[, 2]
        }
        if (withSAQ) {
            exam$saq = data.frame(SPR.code = x[, 1])
            qcol = grep("Q[[:digit:]]+", names(x))
            for (col in qcol) {
                x[x[, col] == "Pending", col] <- NA
            }
            exam$saq_questions <- x[, c(1, qcol)]
            exam$saq$SAQ.Exam = apply(x[, qcol], 1, function(y) sum(sort(as.integer(y), 
                decreasing = T, na.last = T)[1:SAQcount], na.rm = T) * 
                100/(SAQcount * SAQscore))
            exam$saq$qcount = apply(x[, qcol], 1, function(y) length(y[!is.na(y)]))
            exam$total <- exam$total + exam$saq$SAQ.Exam
            exam$config$SAQ_required <- SAQcount
            exam$config$SAQ_marks <- SAQscore
        }
        exam$total <- exam$total/sum(c(1, 1)[c(withQMP, withSAQ)])
        class(exam) <- "SLTexam"
        return(exam)
    }
    if (exam_weight > 0) {
        exam1grades <- data.frame(grades$SPR.code, grades[, exam1])
        exam1grades <- processgrades(exam1grades, title = paste(thisobject$info$modulecode, 
            "Diet 1", thisobject$info$year, collapse = " "))
        thisobject$diet1 <- exam1grades
        exam2grades <- data.frame(grades$SPR.code, grades[, exam2])
        exam2grades <- processgrades(exam2grades, title = "Diet 2")
        thisobject$diet2 <- exam2grades
    }
    return(thisobject)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Module }
