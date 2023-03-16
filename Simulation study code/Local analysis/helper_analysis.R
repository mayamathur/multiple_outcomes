# Note that these access global variables for, e.g., method.label


ntrues_plot = function(dat, benchmark.line = FALSE) {
  library(ggplot2)
  p = ggplot( data = dat, aes( x = rho.YY, y = n.true,
                               fill = method.label ) ) +
    geom_bar(position = "dodge", stat = "identity") + 
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +  # for changing rows/columns
    ylab("Number of nulls rejected") +
    myFillScale + 
    scale_x_continuous( limits = c( min(x.breaks) - 0.05, max(x.breaks) + 0.05 ), breaks = x.breaks ) +
    scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
    xlab( "Correlation between each pair of outcomes" ) +
    theme(legend.position="bottom")
  
  if ( benchmark.line == FALSE) return(p)
  else return( p + geom_hline( aes(yintercept = benchmark ), color="red", lty=2 ) )
}



pwr_plot = function(dat) {
  library(ggplot2)
  ggplot( data = dat, aes( x = rho.YY, y = power,
                           fill = method.label ) ) +
    geom_bar(position = "dodge", stat = "identity") + 
    theme_bw() +
    facet_wrap(~ group, ncol = 3 ) +  # for changing rows/columns
    ylab("Power") +
    myFillScale + 
    scale_x_continuous( limits = c( min(x.breaks) - 0.05, max(x.breaks) + 0.05 ), breaks = x.breaks ) +
    scale_y_continuous( limits = c( min(y.breaks), max(y.breaks) ), breaks = y.breaks ) +
    xlab( "Correlation between each pair of outcomes" ) +
    theme(legend.position="bottom") 
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


make_methods_labels = function(dat) {
  
  
  # for more plotting joy
  labels = c("Bonferroni",
             "Holm",
             "minP",
             "meanP",
             "Global (alpha=0.01)",
             "Global (alpha=0.05)",
             "Wstep",
             "Romano")
  dat$method.label = NA
  dat$method.label[ dat$method == "bonf.naive" ] = labels[1]
  dat$method.label[ dat$method == "holm" ] = labels[2]
  dat$method.label[ dat$method == "minP" ] = labels[3]
  dat$method.label[ dat$method == "meanP" ] = labels[4]  # for "log-P"
  dat$method.label[ dat$method == "ours.0.01" ] = labels[5]
  dat$method.label[ dat$method == "ours.0.05" ] = labels[6]
  dat$method.label[ dat$method == "Wstep" ] = labels[7]
  dat$method.label[ dat$method == "Romano" ] = labels[8]
  
  # remove experimental method
  dat = dat[ dat$method != "meanP", ]
  
  dat = order_methods_labels(dat)
  
  return(dat)
}


order_methods_labels = function(dat){
  # set method ordering for plot
  correct.order = rev( c( "Bonferroni",
                          "Holm",
                          "minP",
                          "Romano",
                          "Wstep",
                          "Global (alpha=0.01)",
                          "Global (alpha=0.05)" ) )
  
  
  dat$method.label = factor(dat$method.label, levels = rev(correct.order))
  
  return(dat)
}



order_panel_labels = function(dat){
  # set method ordering for plot
  
  # relevel to get facets in correct order
  dat = dat[ order( dat$rho.XY, dat$prop.corr, dat$rho.YY ), ]
  ordered.levels = unique(dat$group)
  # put the strong null scenario last for prettiness
  ordered.levels = c( as.character(ordered.levels), as.character(ordered.levels[1]) )
  ordered.levels = ordered.levels[-1]
  dat$group = factor( dat$group, levels = ordered.levels )
  #levels(dat$group)
  
  return(dat)
}



nuni = function(x) length(unique(x))









