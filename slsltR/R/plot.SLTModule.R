plot.SLTModule <-
function (module) {
  par(ask=F)
  repgrad<-data.frame(Diet.1=rev(tapply(module$grades$SPR.code, module$grades$Agreed.Overall.Grade.Diet.1, length)),
                     Diet.2=rev(tapply(module$grades$SPR.code, module$grades$Agreed.Overall.Grade.Diet.2, length)),
                     Coursework=rev(tapply(module$grades$SPR.code, module$grades$Cwk.RAvG, length)),
                     Exam=rev(tapply(module$grades$SPR.code, module$grades$Diet.1.EXAM, length))
  )
  for (n in names(repgrad)) {repgrad[,n][is.na(repgrad[,n])]<-0}
  #gdat<-melt(repgrad,id.var=row.names(repgrad))
  barplot(t(as.matrix(repgrad[,c(1,3,4)])),beside=T,col=c('lightblue','lightpink','lightgreen'), 
          main=paste(module$info$modulecode, module$info$year, 'grade distribution', collapse=' '), 
          xlab='Grade', las=2, ylab='Students')
  legend(35, max(repgrad$Diet.1,repgrad$Coursework,repgrad$Exam), c('Overall','Coursework','Exam'), fill=c('lightblue','lightpink','lightgreen'))
  
}
