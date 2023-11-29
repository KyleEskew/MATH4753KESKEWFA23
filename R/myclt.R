#' myclt
#'
#' @param n number per sample
#' @param iter number of samples
#' @param a lower bound
#' @param b upper bound
#'
#' @return histogram of sum of samples
#' @export
#'
#' @examples
#' myclt(10,1000)
myclt=function(n,iter,a=0,b=5){
  runif <- NULL
  hist <- NULL
  curve <- NULL
  dnorm <- NULL
  x <- NULL
  rainbow <- NULL
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
}
