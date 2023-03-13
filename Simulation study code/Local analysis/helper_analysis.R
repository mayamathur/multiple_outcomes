# Note that these access global variables for, e.g., method.label


ntrues_plot = function(dat, benchmark.line = FALSE) {
  library(ggplot2)
  p = ggplot( data = dat, aes( x = rho.YY, y = n.true,
                               color = method,
                               label = method ) ) +
    geom_text( aes( label = method.label) ) +
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +  # for changing rows/columns
    ylab("Number of nulls rejected") +
    scale_color_manual( values = colors) +
    scale_x_continuous( limits = c( min(x.breaks), max(x.breaks) ), breaks = x.breaks ) +
    scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
    xlab( "Correlation between each pair of outcomes" ) +
    theme(legend.position="none") # remove legend
  
  if ( benchmark.line == FALSE) return(p)
  else return( p + geom_hline( aes(yintercept = benchmark ), color="red", lty=2 ) )
}



pwr_plot = function(dat) {
  library(ggplot2)
  ggplot( data = dat, aes( x = rho.YY, y = power,
                           fill = method.label ) ) +
    #geom_text( aes( label = method.label) ) +
    geom_bar(position = "dodge", stat = "identity") + 
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +  # for changing rows/columns
    ylab("Power") +
    #scale_fill_manual( values = colors) +
    myFillScale + 
    scale_x_continuous( limits = c( min(x.breaks) - 0.05, max(x.breaks) + 0.05 ), breaks = x.breaks ) +
    scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
    xlab( "Correlation between each pair of outcomes" ) +
    #ggtitle("Power of bootstrapped hypothesis test of joint null") +
    theme(legend.position="bottom") # remove legend
}

ci_plot = function(dat) {
  library(ggplot2)
  ggplot( data = dat ) +
    # bootstrap results
    geom_point( aes( x = rho.YY, y = n.rej.bt.mn, color = method ), size=2.5 ) +
    geom_errorbar( aes( x=rho.YY, ymin = bt.lo.mn,
                        ymax = bt.hi.mn, color = method ), width=0.02, size=1.05 ) +
    
    # original dataset results
    geom_point( aes( x = rho.YY, y = n.rej.mn, shape="the shape" ), color = "black", size=3.5 ) +
    scale_shape_manual( values = c('the shape' = 4),
                        name = "Original dataset", guide = 'legend', labels = c("Mean rejections")) +
    
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +
    ylab("Average rejections") +
    scale_color_manual( name="Joint test", values = colors, labels = legend.labs ) +
    
    xlab( "Correlation between each pair of outcomes" ) +
    #ggtitle("Average CI limits and rejections in resamples and originals") +
    theme(legend.position="none") # remove legend
}


