plot.SLTexam <-
function (exam) {
  par(ask=T)
  if (exam$config$QMP){
    hist(as.integer(exam$qmp$QMPexam),main=paste('QMP exam', exam$config$title, collapse=' '), xlab='Score', breaks=20 , col='lightgrey')
  }
  if (exam$config$SAQ) {
    qdat<-melt(exam$saq_questions, id.var='grades.SPR.code')
    boxplot(as.integer(qdat$value)~qdat$variable, main="SAQ response by question", sub=exam$config$title, ylab='Score', 
                                                             col=c('antiquewhite2','cadetblue1','darkseagreen2','orchid1',
                                                                   'darkgoldenrod1','khaki1','lavender'))
  }
}
