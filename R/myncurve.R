#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a upper bound
#'
#' @return probability with shaded region on graph
#' @export
#'
#' @examples
#' myncurve(10,5,6)
myncurve=function(mu, sigma,a){
  curve <- NULL
  dnorm <- NULL
  x <- NULL
  polygon <- NULL
  pnorm <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  return (prob)
}
