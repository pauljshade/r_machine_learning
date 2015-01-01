regPred<-function(result,x) {
  
  trial<-c(1,x,0)
  
  y<-nrow(result)

  prediction<-sum(((trial-result[,2])/result[,3])*result[,1])*result[y,3]+result[y,2]
  
  return(prediction)
  
}