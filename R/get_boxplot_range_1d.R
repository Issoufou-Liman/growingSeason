get_boxplot_range_1d <- function(x, split_IQR =TRUE, na.rm=TRUE , type=5, ...) {
  # Lower hinge to upper hinge using 25% and 75% percentile
  qnt <- quantile(x, probs=c(0.25, 0.5, 0.75), na.rm = na.rm, type = type, ...)
  # interquartile range
  H <- 1.5 * IQR(x, na.rm = na.rm, type = type)
  # H <- 1.58 * IQR(x, na.rm = na.rm, type = type) / sqrt(length(x))
  # y_min: lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR
  min_x <- min(x, na.rm = TRUE)
  y_min <- qnt[[1]] - H
  #y_min <- ifelse(y_min >= min_x, y_min, min_x)
  y_min <- min(x[which(x >= y_min)])

  # y_max: upper whisker = largest observation less than or equal to upper hinge + 1.5 * IQR
  max_x <- max(x, na.rm = TRUE)
  y_max <- qnt[[2]] + H
  #y_max <- ifelse(y_max <= max_x, y_max, max_x)
  y_max <- max(x[which(x <= y_max)])
  if(split_IQR){
    out <-  c('out_min' = min_x, # lowest values will be the lowest outliers
              'ymin' = y_min,
              'lower' = qnt[[1]],
              'middle' = qnt[[2]],
              'upper' = qnt[[3]],
              'ymax' = y_max,
              'out_max' = max_x) # highest value will be the highest outliers
  } else{
    c('out_min' = min_x, # lowest values will be the lowest outliers
      'ymin' = y_min,
      'lower' = qnt[[1]],
      'upper' = qnt[[2]],
      'ymax' = y_max,
      'out_max' = max_x) # highest value will be the highest outliers
  }

}


seasonal_ranges <- function(x, options=c('stats', 'out'), size=1000){
  options <- match.arg(options)
  tmp <- 1:nlayers(x); names(tmp) <- names(x)
  out <- sapply(tmp, function(i){
    if(canProcessInMemory(x)){
      vals <- sampleRandom(x[[i]], size = size)
    } else {
      vals <- getValues(x[[i]])
    }
    ranges <- get_boxplot_range_1d(vals)
    if (options == 'stats'){
      out <- as.data.frame(as.list(ranges[c(-1, -length(ranges))]))
    } else if (options == 'out') {
      out <- vals[(vals <= ranges["ymin"]) & (vals >= ranges["out_min"]) | (vals <= ranges["out_max"]) & (vals >= ranges["ymax"])]
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
  if (options == 'stats'){
    reshape2::melt(out, id.vars = c("ymin", "lower", "middle", "upper", "ymax"))
  } else if (options == 'out') {
    reshape2::melt(out)
  }
}

ggplot_seasonal_ranges <- function(x, size=1000, outlier_size = 0.00001){
  stats <- seasonal_ranges(x, 'stats', size)
  ggplot() +
    geom_boxplot(data=seasonal_ranges(x, 'stats', size), aes(x=L1, ymin = ymin, lower = lower, middle = middle,
                                                             upper = upper,  ymax = ymax), stat = "identity")+
    geom_point(data = seasonal_ranges(x, 'out', size), aes(x=L1, y=value), size = outlier_size)

}
