utils = new.env()

utils$img_w = 500
utils$img_h = 750/2
utils$pdf_w = 10.02
utils$pdf_h = 7.27
utils$par.def = par()

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
# saves current  contents of the current device to a file if gen_slide_plot is ste to TRUE
utils$save_png = function(path = './', name, lwidth = utils$img_w, lheight = utils$img_h)
{
  dev.copy(png,paste(path, paste('/',name,".png", sep="") ,sep=""), width = lwidth, height = lheight); 
  dev.off();
}

utils$save_pdf = function(path = './', name, lwidth = utils$pdf_w, lheight = utils$pdf_h)
{
  print(paste(path, paste(name,".pdf", sep="") ,sep=""))
  dev.copy(pdf,paste(path, paste(name,".pdf", sep="") ,sep=""), width = lwidth, height = lheight); 
  dev.off();
}

utils$fortify.ts <- function(x){
  time <- as.numeric(time(x))
  if(is.matrix(x)){
    df <- as.data.frame(x)
  }
  else {
    x <- as.numeric(x)
    df <- as.data.frame(x)
  }
  df$time <- time
  return(df)
}

utils$fortify.acf <- function(acf){
  n = length(acf$acf)
  range = 1:n
  if (acf$type=="correlation")
    range=2:n
  type <- switch(acf$type, correlation = "ACF", covariance = "ACF (cov)", 
                 partial = "Partial ACF")
  data.frame(acf = acf$acf[range], lag =  acf$lag[range], type = type, n.used = acf$n.used)
}

utils$acf_plot <- function(df, ...){
  clim <- qnorm((1 + 0.975)/2)/sqrt(df$n.used)
  qplot(lag, ymin = 0, ymax = acf, data = df, geom = "linerange") + 
    facet_grid(type ~ .) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5) +
    geom_hline(yintercept = c(-clim, clim), linetype = "dashed", colour = "grey50", size = 0.5)
}

utils$examine_corr <- function(x, ...){
  acfs <- rbind(fortify.acf(acf(x, plot = FALSE,...)), fortify.acf(pacf(x, plot = FALSE,...)))
  print(acf_plot(acfs))
  invisible(acfs)
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
utils$multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
