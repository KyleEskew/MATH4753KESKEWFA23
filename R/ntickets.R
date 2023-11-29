#' ntickets
#'
#' @param N number of seats on aircraft
#' @param gamma probability of overbooking
#' @param p probability that ticketed passenger boards
#' @import ggplot2
#'
#' @return optimal number of tickets to sell, as determined by a discrete distribution and the associated normal approximation; two graphs are also returned to visually demonstrate the optimal number of tickets to sell
#' @export
#'
#' @examples
#' ntickets(200,.02,.95)
#' ntickets(225,.025,.985)
ntickets=function(N,gamma,p){
  pbinom <- NULL
  pnorm <- NULL
  #Discrete case
  sbd = N:(1.1*N)
  n_disc = 1-gamma-pbinom(N,sbd,p)
  an_disc = abs(n_disc)
  nd_i = which.min(an_disc)
  nd = sbd[nd_i]

  #Normal approximation
  sbc=seq(N,(N*1.1),.0001)
  n_norm = 1-gamma-pnorm(N+.5,sbc*p,sqrt(sbc*p*(1-p)))
  an_norm = abs(n_norm)
  nc_i = which.min(an_norm)
  nc = sbc[nc_i]

  #Print named list
  namedList = noquote(c(paste('nd =',nd),paste('nc =',nc),paste('N =',N),paste('p =',p),paste('gamma =',gamma)))
  print(namedList)

  #Plots

  #Discrete
  #Employ :: method instead of library() for package etiquette
  dfd = data.frame(sbd,n_disc)
  gd=ggplot2::ggplot(dfd, ggplot2::aes(x=sbd,y=n_disc))
  gd=gd+ggplot2::geom_point()+ggplot2::geom_line()
  gd=gd+ggplot2::ggtitle(paste0('Discrete: Objective Vs. n to find optimal tickets sold (',nd,'), gamma=',gamma, ', N=',N))
  gd=gd+ggplot2::theme(plot.title=ggplot2::element_text(size=10))
  gd=gd+ggplot2::xlab('n')
  gd=gd+ggplot2::ylab('Objective')
  gd=gd+ggplot2::geom_vline(xintercept=nd,col='Blue')
  gd=gd+ggplot2::geom_hline(yintercept=0,col='Green')
  print(gd)

  #Continuous
  dfc = data.frame(sbc,n_norm)
  gc=ggplot2::ggplot(dfc, ggplot2::aes(x=sbc,y=n_norm))
  gc=gc+ggplot2::geom_line()
  gc=gc+ggplot2::ggtitle(paste0('Continuous: Objective Vs. n to find optimal tickets sold (',nc,'), gamma=',gamma, ', N=',N))
  gc=gc+ggplot2::theme(plot.title=ggplot2::element_text(size=10))
  gc=gc+ggplot2::xlab('n')
  gc=gc+ggplot2::ylab('Objective')
  gc=gc+ggplot2::geom_vline(xintercept=nc,col='Blue')
  gc=gc+ggplot2::geom_hline(yintercept=0,col='Green')
  print(gc)
}
