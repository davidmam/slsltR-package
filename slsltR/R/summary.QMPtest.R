
summary.QMPtest <- function (x) {
  
  cat('Summary statisitcs for QMP test',x$test_name,"\n")
  cat('\n')
  cat("Participant statistics:\n")
  cat('Number of participants:',length(x$results),"\n")
  cat('Mean score achieved:', 100*mean(x$results)/x$max_score,'\n')
  cat('Standard deviation:', 100*sd(x$results)/x$max_score, "\n")
  cat('Students passing:', length(x$results[x$results>0.4*x$max_score]),"\n")
  cat('Pass rate:' ,100*length(x$results[x$results>0.4*x$max_score])/length(x$results),"\n")
  cat('Grade Distribution\n')
  print(tapply(x$results, score2alphanumeric(x$results, scale='standard', max=x$max_score), length))
  cat("\n")
  cat("Question statistics:\n")
  cat("Number of questions:",length(x$byquestion$max[!is.na(x$byquestion$max)]),"\n")
  cat("Mean response:", mean(x$byquestion$response, na.rm=T),"\n")
  cat("Well answered questions (>90%): ", as.character(x$byquestion$name[x$byquestion$response>90 & !is.na(x$byquestion$response)]),"\n")
  cat("Poorly answered questions (<10%): ", as.character(x$byquestion$name[x$byquestion$response<10 & !is.na(x$byquestion$response)]),"\n")
  
}
