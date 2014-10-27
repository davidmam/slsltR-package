

plot.QMPtest <- function(x) {
  par(ask=F)
  barplot(tapply(qmp$results, score2alphanumeric(qmp$results, scale='standard', max=qmp$max_score), length), las=2,
     main=paste(c("Score distribution for", x$test_name),collapse=' '), 
        xlab='Grade', ylab='Students',      col=c(rep('lightgray',2),'black', rep('red',3), rep('sandybrown',3),rep('khaki',3),rep('lightblue',3),rep('lightgreen',3)))
  plot(x$byquestion$rank[x$byquestion$rank], x$byquestion$response[x$byquestion$rank], type='l', 
       main=paste(c('Question performance profile for',x$test_name),collapse=' '),
       xlab='Question rank', ylab='% max mark obtained', col='red', lwd=5, ylim=c(0,100))
  xco<-x$byquestion$rank[x$byquestion$rank]
  yco<-x$byquestion$response[x$byquestion$rank]
  for (c in 1:(length(xco)-1)) {
    polygon(c(rep(xco[c],2),rep(xco[c+1],2)), c(0,yco[c],yco[c+1],0), col='lightblue', border='lightblue')
  }
  
  par(ask=F)
}

