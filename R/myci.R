#' myci
#'
#' @param x input vector
#'
#' @return 95% confidence interval
#' @export
#'
#' @examples
#' myci(rnorm(30,mean=10,sd=12))
myci=function(x){
  qt=NULL
  sd=NULL
  t_halfalpha_.95=qt(1-0.05/2,length(x)-1)
  L=round(mean(x)-(t_halfalpha_.95*sd(x)/sqrt(length(x))),4)
  U=round(mean(x)+(t_halfalpha_.95*sd(x)/sqrt(length(x))),4)
  print(paste0('95% ci: (', L,', ', U,')'),quote = FALSE)
}
