score2alphanumeric <- function (x, scale='stringent', maxscore=100) {
  x<-x+1
  an_grades<-rev(c("A1" ,"A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3", "D1", "D2", "D3", "MF", "CF", "BF", "AB", "MC", "CA"))
  gradescale=c('AB',rep('BF',19),rep('CF',15), rep('MF',5), rep('D3',5), rep('D2',5), rep('D1',5), 
               rep('C3',5), rep('C2',5), rep('C1',5), rep('B3',5), rep('B2',5), rep('B1',5), 
               rep('A3',5), rep('A2',5), rep('A1',6))  
  if (scale =='stringent'){
    
    x <- as.integer(100*x/maxscore)
  }
  if (scale =='standard'){
    gradescale=c('AB',rep('BF',19),rep('CF',15), rep('MF',5), rep('D3',3), rep('D2',3), rep('D1',4), 
                 rep('C3',3), rep('C2',3), rep('C1',4), rep('B3',3), rep('B2',3), rep('B1',4), 
                 rep('A3',10), rep('A2',10), rep('A1',11))  
    x <- as.integer(100*x/maxscore)
  }
  if (scale == '21point'){
    gradescale=c('AB',rep('BF',3),rep('CF',3), rep('MF',3), 'D3','D2','D1','C3','C2','C1','B3','B2','B1','A3','A2','A1')  
  }
  
  
  an_x<-gradescale[x]
  an_x<-factor(an_x, levels=an_grades, ordered=T)
  return(an_x)
}
