matplot.r4dt <- function(spc
                         , ppp = NA
                         , derivative = "spc"
                         , xlab = lambda
                         , ylab = NA
                         , seqp = 1
                         , lty = 1
                         , type = "l"
                         , main = ""
                         , colp = NA){

  if( is.na(ppp)) x <- spc$wl
  if( !is.na(ppp)) x <- ppp$wl

  if( is.na(ppp)) y <- spc[[ grep(derivative, names( spc ))[ 1 ] ]]
  if( !is.na(ppp)) y <- spc[ , ppp$numcol, with = F]

  if( is.na( ylab)) ylab <- ifelse(sum(y) / nrow(y) > 5000, "Counts", "AU")

  if( derivative == "1st") ylab = ylab_1st
  if( derivative == "2nd") ylab = ylab_2nd

  rowp <- seq( 1, nrow(y), seqp)

  if( all(is.na(colp))) colp = colorRamps::blue2red( length( rowp ))

  matplot(x, t(y[rowp , ])
          , lty = lty
          , type = type
          , xlab = xlab
          , ylab = ylab
          , main = main
          , col = colp)

}
