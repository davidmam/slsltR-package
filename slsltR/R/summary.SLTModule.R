summary.SLTModule <-
function(module ,...) {
  alpha2numeric <- c(A1=21,A2=20,A3=19,AB=0,B1=18,B2=17,B3=16,BF=2,C1=15,C2=14,C3=13,CA=0,CF=6,D1=12,D2=11,D3=10,MC=0,MF=9)
  scount<-length(module$students$SPR.code)
  wd<-length(module$students$Diet.1.Status[module$students$Diet.1.Status=='WD'])
  dc<-length(module$students$Diet.1.Status[module$students$Diet.1.Status=='DC'])  
  st<-length(module$students$Diet.1.Status[module$students$Diet.1.Status=='ST'])
  wd2<-length(module$students$Diet.2.Status[module$students$Diet.2.Status=='WD'])
  dc2<-length(module$students$Diet.2.Status[module$students$Diet.2.Status=='DC'])  
  st2<-length(module$students$Diet.2.Status[module$students$Diet.2.Status=='ST'])
  d1<-module$grades$Agreed.Overall.Grade.Diet.1[!is.na(module$grades$Agreed.Overall.Grade.Diet.1)]
  d2<-module$grades$Agreed.Overall.Grade.Diet.2[!is.na(module$grades$Agreed.Overall.Grade.Diet.2)]
  pass1<-length(d1[d1>='D3'])  
  mc.ca1<-length(module$students$SPR.code[module$grades$Diet.1.EXAM=='CA' | module$grades$Diet.1.EXAM=='MC' ])
  fail1<-scount-(wd+st+dc+pass1+mc.ca1)
  pass2<-length(d2[d2>='D3'])
  mc.ca2<-length(module$students$SPR.code[module$grades$Diet.2.EXAM=='CA' | module$grades$Diet.2.EXAM=='MC' ])
  fail2<-scount-(wd2+st2+dc2+pass2+mc.ca2)
  
  
  # Now build report.
  repdf<-data.frame(Diet.1=c(scount,pass1,fail1,mc.ca1,wd,st,dc),Diet.2=c(scount,pass2,fail2,mc.ca2,wd2,st2,dc2),
                    row.names=c("Students registered","Passed","Failed","MC/CA","WD","ST","DC"), stringsAsFactors=F)
  cat("Student Status:\n")
  print(repdf)
  cat("Grade Summary:\n")
  repgrad<-data.frame(Diet.1=rev(tapply(module$grades$SPR.code, module$grades$Agreed.Overall.Grade.Diet.1, length)),
                      Diet.2=rev(tapply(module$grades$SPR.code, module$grades$Agreed.Overall.Grade.Diet.2, length)),
                      Coursework=rev(tapply(module$grades$SPR.code, module$grades$Cwk.RAvG, length)),
                      Exam=rev(tapply(module$grades$SPR.code, module$grades$Diet.1.EXAM, length))
  )
  for (n in names(repgrad)) {repgrad[,n][is.na(repgrad[,n])]<-0}
  
  print(repgrad)
  
}
