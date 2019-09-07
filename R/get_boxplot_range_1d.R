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


seasonal_ranges <- function(x, size=1000, sample=FALSE, melt = TRUE){
  if (!(class(x) %in% c("RasterBrick", "RasterStack", "matrix", "data.frame"))){
    stop(paste(deparse(substitute(x)), 'must be a rasterStack, rasterBrick, matrix or data.frame'))
  }
  if (class(x) %in% c("RasterBrick", "RasterStack")){
    if((!canProcessInMemory(x)) | sample){
      vals <- sampleRandom(x, size = size)
    } else {
      vals <- getValues(x)
    }
  } else if (class(x) %in% c("matrix", "data.frame")){
    vals <- as.matrix(x)
  }
  tmp <- 1:ncol(vals); names(tmp) <- colnames(vals)
  ranges <- sapply(tmp, function(i){
    get_boxplot_range_1d(vals[, i])
  }, simplify = FALSE, USE.NAMES = TRUE)

  stats <- sapply(tmp, function(i){
    as.data.frame(as.list(ranges[[i]][c(-1, -length(ranges[[i]]))]))
  }, simplify = FALSE, USE.NAMES = TRUE)

  if(melt){
    stats <- reshape2::melt(stats, id.vars = c("ymin", "lower", "middle", "upper", "ymax"))
    stats$year <- substring(stats$L1, 2, 5)
    stats$L1 <- as.Date(substring(stats$L1, 2), format = "%Y.%m.%d")
  }


  out <- sapply(tmp, function(i){
    vals <- vals[, i]
    vals[(vals <= ranges[[i]]["ymin"]) & (vals >= ranges[[i]]["out_min"]) | (vals <= ranges[[i]]["out_max"]) & (vals >= ranges[[i]]["ymax"])]
  }, simplify = FALSE, USE.NAMES = TRUE)
  if (melt){
    out <- reshape2::melt(out)
    out$year <- substring(out$L1, 2, 5)
    out$L1 <- as.Date(substring(out$L1, 2), format = "%Y.%m.%d")
  }


  ranges <- list(stats = stats, out = out)
  class(ranges) <- "seasonal_ranges"
  return(ranges)
}


ggplot_seasonal_ranges <- function(x, ...){
  UseMethod("ggplot_seasonal_ranges")
}

ggplot_seasonal_ranges.default <- function(x, ...){
  x <- seasonal_ranges (x)
  ggplot() +
    geom_boxplot(data=x$stats,
                 aes(x=L1, ymin = ymin, lower = lower, middle = middle, upper = upper,  ymax = ymax),
                 stat = "identity")+
    geom_point(data=x$out,
               aes(x=L1, y=value))
}

ggplot_seasonal_ranges.seasonal_ranges <- function(x, ...){
  ggplot() +
    geom_boxplot(data=x$stats,
                 aes(x=L1, ymin = ymin, lower = lower, middle = middle, upper = upper,  ymax = ymax,
                     fill=year, color=year), stat = "identity", lwd=0.001, notchwidth = 0.0001, na.rm = TRUE)+
    geom_point(data=x$out,
               aes(x=L1, y=value), size = 0.00001, color = 'black', alpha = 0.01)+
    scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels =  "%b \n%Y", expand = c(0,0),
                 labels=function(x) sub(" ","d",x,fixed=TRUE))
}

ggplot_seasonal_ranges.list <- function(x, ...){
  if (!all(lapply(x, class) %in% c("RasterBrick", "RasterStack", "matrix", "data.frame", "seasonal_ranges"))){
    stop(paste(deparse(substitute(x)), 'must be a list of RasterBrick, RasterStack, matrix, data.frame or "seasonal_ranges'))
  }
  if (length(unique(sapply(x, class)))!=1){ # class mixtures should not make sense
    stop("list must not contain objects of different class")
  }
  if (class(x[[1]]) != "seasonal_ranges"){ # testing the other classes
    x <- sapply(X=x, FUN = seasonal_ranges, melt = FALSE, ..., simplify = FALSE, USE.NAMES = TRUE)
    # x <- reshape2::melt(x, id.vars = names(x[[1]]))
  }
  data <- reshape2::melt(data=sapply(X=x, '[[', 1, simplify = FALSE, USE.NAMES = TRUE), id.vars = c("ymin", "lower", "middle", "upper", "ymax"))
  data$year <- substring(data$L2, 2, 5)
  data$L2 <- as.Date(substring(data$L2, 2), format = "%Y.%m.%d")
  p <- ggplot() +
    geom_boxplot(data=data,
                 aes(x=L2, ymin = ymin, lower = lower, middle = middle, upper = upper,  ymax = ymax,
                     fill=year, color=year), stat = "identity", lwd=0.001, notchwidth = 0.0001, na.rm = TRUE)+
    facet_wrap(L1~., nrow = 2)+
    scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels =  "%b \n%Y", expand = c(0,0),
                 labels=function(x) sub(" ","d",x,fixed=TRUE))

  data <- reshape2::melt(data=sapply(X=x, '[[', 2, simplify = FALSE, USE.NAMES = TRUE))
  data$year <- substring(data$L2, 2, 5)
  data$L2 <- as.Date(substring(data$L2, 2), format = "%Y.%m.%d")
  p +
    geom_point(data=data,
               aes(x=L2, y=value), size = 0.00001, color = 'black', alpha = 0.01)
}
test <- seasonal_ranges(ndvi_ts)
seasonal_ranges(list(NDVI=ndvi_ts,
                     #mcfeeters_ndwi = mcfeeters_NDMI_ts,
                     'MDWI (mcfeeters, 1996)' = mcfeeters_NDMI_ts,
                     NDFI=Boschetti_NDFI_ts,
                     #gao_ndwi = GAO_NDWI_ts,
                     'MDWI (Gao, 1996)' = GAO_NDWI_ts), 100)

p=ggplot_seasonal_ranges (test)

test <- list(NDVI=ndvi_ts,
             #mcfeeters_ndwi = mcfeeters_NDMI_ts,
             'MDWI (mcfeeters, 1996)' = mcfeeters_NDMI_ts,
             NDFI=Boschetti_NDFI_ts,
             #gao_ndwi = GAO_NDWI_ts,
             'MDWI (Gao, 1996)' = GAO_NDWI_ts)
p=ggplot_seasonal_ranges (test)+
  my_theme +
  scale_color_manual(values = c('gray', 'gray', 'gray'))+
  guides(color = FALSE)+
  theme(panel.grid.minor.y = element_line(size = 0.02, linetype = 'solid',color = 'darkgray'),
        axis.title = element_blank())+
  scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels =  "%b \n%Y", expand = c(0,0),
               labels=function(x) sub(" ","d",x,fixed=TRUE))+
  labs(title='Kisumu County, Kenya',
       subtitle=paste0('MODIS', ' : ', '2014-01-01', ' ', '/', ' ', '2016-12-26'))
