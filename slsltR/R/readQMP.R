
readQMP <- function (x=NULL) {
    if (is.null(x)){ 
      x <-file.choose()
    }
    qmp <- read.table(x, sep="\t", header=T, fill=T,  quote='', stringsAsFactors=F)
    data<- qmp[4:length(qmp[,1]),]
    names(data)[1:11] <-t(qmp[2,1:11])
    max.score<-as.integer(data$"Maximum score"[1])
    
clean.names <- function(x){
  substr(x,1,1)!='X'
}    
    
  qnames<-names(qmp)[c(F, clean.names(names(qmp)[2:(length(names(qmp))-1)]))]
  print(length(qnames))
  print(qnames)
  qmax <-rep(0,length(qnames))
  qsum <-rep(0,length(qnames)) 
  qres <-rep(0,length(qnames))

  for (q in 1:length(qnames)){
    col=10+3*(q)
    print(names(data)[col])
    data[,col]<-as.integer(data[,col])
    qmax[q]=as.integer(max(as.integer(data[,col])))
    qsum[q]=sum(as.integer(data[,col]))
    qres[q]=100*qsum[q]/(qmax[q]*length(data[,1]))
    names(data)[9+(((3*q):((3*q)+2)))]<-c(paste(c("Question",q), collapse='.'),paste(c("Score",q), collapse='.'),paste(c("Response",q), collapse='.')) 
  }
  qdata<-melt(data[,c(2, 10+3*(1:(length(qnames))))],id.vars='Participant')
  names(qdata)<-c('Paticipant','Question','Score')
  obj<-list(questions=data[1,9+(3*(1:(length(qnames))))],
            results=tapply(as.integer(data$"Total score"), data$Participant, max),
            scores=qdata,
            max_score=max.score,
            test_name=data[1,1],
            byquestion=data.frame(name=qnames, max=qmax, total=qsum, response=qres, rank=rank(qres))[order(rank(qres)),]
            )
  class(obj) <-'QMPtest'
  return(obj)
              
}
