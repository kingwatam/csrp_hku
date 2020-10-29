library(magrittr)
#
# add all arguments
#
TestAdd <- function(...){
  sum(...)
}

# countrycode <- function(sourcevar, origin, destination, warn = TRUE, nomatch = NA, custom_dict = NULL, custom_match = NULL, origin_regex = FALSE){
# countrycode::countrycode(sourcevar, origin, destination, warn, nomatch, custom_dict, custom_match, origin_regex)
# }

iso.expand <- function(code){
  maps::iso.expand(code)
}

graph.plot <- function(data, label=NULL){  
  # link the graphics to the calling cell, so it won't get erased 
  # when another cell draws a plot
  BERT.graphics.device(cell=T);
  
  # flatten the data and make sure it's numeric
  data = as.numeric( unlist( data ));

  # draw the plot
  plot(data, pch=21, bg="pink", col="red", main=label );
  lines( lowess( data ));

  # we're done with the graphics device, so shut it off for now
  dev.off();

  # this is a convenient return value for the calling cell
  T;
}

kernel_smooth <- function(object, startyear, startmonth, endyear, endmonth, frequency, kernel_type, bandwidth){
  object <- as.data.frame(object) # convert from list to df
  object[object == ''] <- NA # NULL values to NAs
  object <- zoo::na.locf(object) # last non-missing value carried forward
  ts_object <- ts(object, start = c(startyear, startmonth), end = c(endyear, endmonth), frequency)
  return(ksmooth(time(ts_object), ts_object, kernel_type, bandwidth, x.points = time(ts_object))$y)
}

StatsGini <- function(x, w = rep(1, length(x))){
  # x and w are vectors
  # w can be left blank when calling the fn (i.e. no weighting)
  # Examples:
  # x <- c(3, 1, 7, 2, 5)
  # w <- c(1, 2, 3, 4, 5)
  # StatsGini(x, w) should yield 0.2983050847
  # StatsGini(c(0.25, 0.75), c(1, 1)) should yield 0.25
  n <- length(x)
  wxsum <- sum(w * x)
  wsum <- sum(w)
  sxw <- order(x, w) # Ascending order sort
  sx <- w[sxw] * x[sxw]
  sw <- w[sxw]
  pxi <- vector(mode = "numeric", length = n)
  pci <- vector(mode = "numeric", length = n)
  pxi <- cumsum(sx) / wxsum
  pci <- cumsum(sw) / wsum
  G <- 0.0
  for (i in 2:n){
    G <- G - (pci[i] * pxi[i - 1] - pxi[i] * pci[i - 1] )
  }
  return(G)
}

getFX <- function(currency_1, currency_2){
  url <- paste0('https://www.freeforexapi.com/api/live?pairs=', currency_1, currency_2)
  rate <- rjson::fromJSON(readLines(url))$rates[[1]][[1]]
  return(rate)
}

interpolate <- function(x, y, xout){
  approx(x, y, n = length(x))$y[which(x %in% xout)]
}


