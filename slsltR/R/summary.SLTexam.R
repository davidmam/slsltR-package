summary.SLTexam <-
function (exam) {
  res<-list()
  if (exam$config$QMP){
    res$QMP<-data.frame(meanscore=mean(exam$qmp$QMPexam, na.rm=T),
                  sdscore=sd(exam$qmp$QMPexam, na.rm=T),
                  topscore=max(exam$qmp$QMPexam, na.rm=T),
                  sitting=length(exam$qmp$QMPexam[!is.na(exam$qmp$QMPexam)]),
                  passing=length(exam$qmp$QMPexam[exam$qmp$QMPexam >=40]),
                  passrate=100*length(exam$qmp$QMPexam[exam$qmp$QMPexam >=40])/length(exam$qmp$QMPexam[!is.na(exam$qmp$QMPexam)])
    )
    cat("Summary results for QMP exam:\n")
    print(res$QMP, collapse=' ')
    cat("\n")
  }
  if (exam$config$SAQ){
    qdat<-melt(exam$saq_questions, id.var='grades.SPR.code')
    res$SAQ<-list(byquestion=data.frame( mean=tapply(as.integer(qdat$value), qdat$variable, mean, na.rm=T),
                  sd=tapply(qdat$value, qdat$variable, sd, na.rm=T),
                  topscore=tapply(qdat$value, qdat$variable, max, na.rm=T),
                  median=tapply(as.integer(qdat$value), qdat$variable, median, na.rm=T),
                  sitting=tapply(qdat$value[!is.na(qdat$value)], qdat$variable[!is.na(qdat$value)], length),
                  passing=tapply(qdat$value[qdat$value>0.4*exam$config$SAQ_marks], qdat$variable[qdat$value>0.4*exam$config$SAQ_marks], length))
    )
    res$SAQ$byquestion$passrate<-100*res$SAQ$byquestion$passing/res$SAQ$byquestion$sitting
    cat("Summary results for SAQ exam:\n")
    print(res$SAQ$byquestion, collapse=' ')
  }
  #return(res)
  
}
