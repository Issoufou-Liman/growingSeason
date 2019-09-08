#' ggplot seasonal ranges
#'
#' ggplot seasonal ranges
#' @inheritParams seasonal_ranges
#' @param show_limits Logical.Should the differents ranges be delineated with lines?
#' @inheritParams ggplot2::geom_boxplot
#' @param lwd Numeric. line width
#' @param ... Additional arguments passed to seasonal_ranges. This is not used with `ggplot_seasonal_ranges.seasonal_ranges`.
#' @importFrom ggplot2 aes_string facet_wrap geom_boxplot geom_point ggplot scale_x_date stat_summary
#' @export
ggplot_seasonal_ranges <- function(x, show_limits=TRUE, ...,
                                   outlier.colour = 'black', outlier.color = 'black',
                                   outlier.fill = 'black', outlier.shape = 19, outlier.size = 0.00001,
                                   outlier.stroke = 0.5, outlier.alpha = 0.01,
                                   na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, lwd=0.1){
  UseMethod("ggplot_seasonal_ranges")
}

#' @rdname ggplot_seasonal_ranges
#' @export
ggplot_seasonal_ranges.default <- function(x, show_limits=TRUE, ...,
                                           outlier.colour = 'black', outlier.color = 'black',
                                           outlier.fill = 'black', outlier.shape = 19, outlier.size = 0.00001,
                                           outlier.stroke = 0.5, outlier.alpha = 0.01,
                                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, lwd=0.1){
  x <- seasonal_ranges (x, ...)
  p <- ggplot(data=x$stats) +
    geom_boxplot(aes_string(x='L1', ymin = 'ymin', lower = 'lower', middle = 'middle', upper = 'upper',  ymax = 'ymax', fill='year', color='year'),
                 stat = "identity", position = "dodge2", na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes,
                 lwd=lwd)
  if(show_limits){
    p <- p +
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='out_max', group=1), colour="red", size=0.2)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='ymax', group=1), colour="black", size=0.3)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='middle', group=1), colour ="lightgreen", size=0.5)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='ymin', group=1), colour="black", size=0.3)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='out_min', group=1), colour="red", size=0.2)
  }
  p <- p+ geom_point(data=x$out,
                     aes_string(x='L1', y='value'),
                     colour = outlier.colour, color = outlier.color,
                     fill = outlier.fill, shape = outlier.shape, size = outlier.size,
                     stroke = outlier.stroke, alpha = outlier.alpha)+
    scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels =  "%b \n%Y", expand = c(0,0),
                 labels=function(x) sub(" ","d",x,fixed=TRUE))
  return(p)
}

#' @rdname ggplot_seasonal_ranges
#' @export
ggplot_seasonal_ranges.seasonal_ranges <- function(x, show_limits=TRUE, ...,
                                                   outlier.colour = 'black', outlier.color = 'black',
                                                   outlier.fill = 'black', outlier.shape = 19, outlier.size = 0.00001,
                                                   outlier.stroke = 0.5, outlier.alpha = 0.01,
                                                   na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, lwd=0.1){
  p <- ggplot(data=x$stats) +
    geom_boxplot(aes_string(x='L1', ymin = 'ymin', lower = 'lower', middle = 'middle', upper = 'upper',  ymax = 'ymax', fill = 'year', color = 'year'),
                 stat = "identity", position = "dodge2", na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes,
                 lwd=lwd)
  if(show_limits){
    p <- p +
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='out_max', group=1), colour="red", size=0.2)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='ymax', group=1), colour="black", size=0.3)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='middle', group=1), colour ="lightgreen", size=0.5)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='ymin', group=1), colour="black", size=0.3)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L1', y='out_min', group=1), colour="red", size=0.2)
  }
  p <- p+ geom_point(data=x$out,
                     aes_string(x = 'L1', y = 'value'),
                     colour = outlier.colour, color = outlier.color,
                     fill = outlier.fill, shape = outlier.shape, size = outlier.size,
                     stroke = outlier.stroke, alpha = outlier.alpha)+
    scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels =  "%b \n%Y", expand = c(0,0),
                 labels=function(x) sub(" ","d",x,fixed=TRUE))
  return(p)
}

#' @rdname ggplot_seasonal_ranges
#' @export
ggplot_seasonal_ranges.list <- function(x, show_limits=TRUE, ...,
                                        outlier.colour = 'black', outlier.color = 'black',
                                        outlier.fill = 'black', outlier.shape = 19, outlier.size = 0.00001,
                                        outlier.stroke = 0.5, outlier.alpha = 0.01,
                                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, lwd=0.1){
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
  data <- reshape2::melt(data=sapply(X=x, '[[', 1, simplify = FALSE, USE.NAMES = TRUE), id.vars = c("out_min", "ymin", "lower", "middle", "upper", "ymax", "out_max"))
  data$year <- substring(data$L2, 2, 5)
  data$L2 <- as.Date(substring(data$L2, 2), format = "%Y.%m.%d")
  p <- ggplot(data=data) +
    geom_boxplot(aes_string(x = 'L2', ymin = 'ymin', lower = 'lower', middle = 'middle', upper = 'upper',  ymax = 'ymax', fill = 'year', color = 'year'),
                 stat = "identity", position = "dodge2", na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes,
                 lwd=lwd)
  if(show_limits){
    p <- p +
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L2', y='out_max', group=1), colour="red", size=0.2)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L2', y='ymax', group=1), colour="black", size=0.3)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L2', y='middle', group=1), colour ="lightgreen", size=0.5)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L2', y='ymin', group=1), colour="black", size=0.3)+
      stat_summary(fun.y='identity', geom="smooth", aes_string(x='L2', y='out_min', group=1), colour="red", size=0.2)
  }

  data <- reshape2::melt(data=sapply(X=x, '[[', 2, simplify = FALSE, USE.NAMES = TRUE))
  data$year <- substring(data$L2, 2, 5)
  data$L2 <- as.Date(substring(data$L2, 2), format = "%Y.%m.%d")

  p <- p+ geom_point(data=data,
                     aes_string(x='L2', y='value'),
                     colour = outlier.colour, color = outlier.color,
                     fill = outlier.fill, shape = outlier.shape, size = outlier.size,
                     stroke = outlier.stroke, alpha = outlier.alpha)+
    facet_wrap(L1~., nrow = 2)+
    scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels =  "%b \n%Y", expand = c(0,0),
                 labels=function(x) sub(" ","d",x,fixed=TRUE))
  return(p)
}
