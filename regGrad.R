## This is a function to perform gradient decent to solve a linear regression problem
## Mutliple regression

## Parameters
 # data - matrix or data.frame of the training set
 # x    - vector of columns to regress on
 # y    - column number for target
 # iterations - number of iterations
 # alpha - descent
regGrad<-function(data,x,y,iterations=1500,alpha=0.01,scale=FALSE,...) {
    
  col<-length(x)+1           # number of coefficents to find
  m<-nrow(data)              # number of training examples
  
  # form X the matrix of training examples for x's
  tx<-as.matrix(data[,x])    
  X<-matrix(c(rep(1,m),unlist(tx)), nrow=m,ncol=col)
  
  # form Y the vector of targets
  Y<-data[,y]
  
  # set up the coefficents
  theta<-matrix(rep(0,col),nrow=col,ncol=1)

  # feature scale if needed
  the.mean<-rep(0,col+1)
  the.sd<-rep(1,col+1)
  
  if(scale==TRUE) {
    x.mean<-apply(X,2,mean)
    x.mean[1]<-0    # dont scale the first column
    X.mean<-matrix(rep(x.mean,m),nrow=m,ncol=col, byrow=TRUE)
    x.sd<-apply(X,2,sd)
    x.sd[1]<-1     # dont scale the first column
    X.sd<-matrix(rep(x.sd,m),nrow=m,ncol=col, byrow=TRUE)
    X<-(X-X.mean)/X.sd   # scale
    
    y.mean<-mean(Y)
    y.sd<-sd(Y)
    Y<-(Y-y.mean)/y.sd
    
    the.mean<-c(x.mean,y.mean)   # put the means together
    the.sd<-c(x.sd,y.sd)       # put the sds together
    
  }  
  
  for(i in 1:iterations) {
    theta<-theta-(alpha/m)*t((t(X %*% theta - Y) %*% X))
  }
  
  the.theta<-c(theta,0)
  
  # the values to return
  the.return<-matrix(c(the.theta,the.mean,the.sd),nrow=col+1,ncol=3)
  
  return(the.return)
  
}

