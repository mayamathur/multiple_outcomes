
#' Compute E-value for a difference of means and its confidence interval limits
#' 
#' Returns a data frame containing point estimates, the lower confidence limit, and the upper confidence limit
#' on the risk ratio scale (through an approximate conversion) as well as E-values for the point estimate and the confidence interval
#' limit closer to the null.  
#' @param est The point estimate as a standardized difference (i.e., Cohen's d)
#' @param se The standard error of the point estimate
#' @param true The true standardized mean difference to which to shift the observed point estimate. Typically set to 0 to consider a null true effect. 
#' @export
#' @examples
#' # compute E-value if Cohen's d = 0.5 with SE = 0.25
#' evalues.MD( .5, .25 )

evalues.MD = function( est, se = NA, true = 0 ) {
  
  if ( !is.na(se) ) {
    if ( se < 0 ) stop("Standard error cannot be negative")
  }

  values = c()
  values[1] = exp( 0.91 * est )
  values[2] = exp( 0.91 * est - 1.78 * se ) 
  values[3] = exp( 0.91 * est + 1.78 * se )
  
  # convert true value to which to shift observed point estimate
  #  to RR scale
  true.RR = exp( 0.91 * true )
  
  return( evalues.RR( values[1], values[2], values[3], true = true.RR ) )
}
