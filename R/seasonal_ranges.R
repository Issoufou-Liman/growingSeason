#' Benchmark seasonal data by ranges based boxplots statistics and outliers
#'
#' Benchmark time series data based ranges following boxplot statistics and outliers.
#' @author Issoufou Liman
#' @param x an object of class dataframe, matrix, or RasterStack. Names of x should
#' be dates in the format `X%Y.%m.%d`.
#' @param size Integer. Sample size (if sample is TRUE or if the data cannot
#' be process in memory).
#' @param sample Logical. Should the data be sampled? If TRUE, the result is
#' based on a ramdon sampling of the original data.
#' @param melt Logical. Should the data be melted to long format? If False, the result is a list.
#' @return data.frame or list (if melt is false).
#' @details To be documented
#' @importFrom raster getValues canProcessInMemory sampleRandom
#' @importFrom reshape2 melt
#' @export
seasonal_ranges <- function(x, size = 1000, sample = FALSE, melt = TRUE){
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
    # as.data.frame(as.list(ranges[[i]][c(-1, -length(ranges[[i]]))]))
    as.data.frame(as.list(ranges[[i]]))
  }, simplify = FALSE, USE.NAMES = TRUE)
  if(melt){
    stats <- melt(stats, id.vars = c("out_min", "ymin", "lower", "middle", "upper", "ymax", "out_max"))
    stats$year <- substring(stats$L1, 2, 5)
    stats$L1 <- as.Date(substring(stats$L1, 2), format = "%Y.%m.%d")
  }
  out <- sapply(tmp, function(i){
    vals <- vals[, i]
    vals[(vals <= ranges[[i]]["ymin"]) & (vals >= ranges[[i]]["out_min"]) | (vals <= ranges[[i]]["out_max"]) & (vals >= ranges[[i]]["ymax"])]
  }, simplify = FALSE, USE.NAMES = TRUE)
  if (melt){
    out <- melt(out)
    out$year <- substring(out$L1, 2, 5)
    out$L1 <- as.Date(substring(out$L1, 2), format = "%Y.%m.%d")
  }
  ranges <- list(stats = stats, out = out)
  class(ranges) <- "seasonal_ranges"
  return(ranges)
}
