#' Estimate and stack the beginning and end of growing seasons using parallel processing.
#'
#' @author Issoufou Liman
#' @param x A Raster \code{\link[raster]{brick}} or \code{\link[raster]{stack}}
#' @inheritParams phases
#' @param valid_range A numeric vector of lenght 2. Data cleaning parameter specifying the validity domain of the input data are valid.
#' Values outside this range are set NA before any processing. In the case of normalized difference indices (e.g. NDVI, SAVI) for example,
#' one should set these values to be between.
#' @inheritParams raster::writeRaster
#' @return A \code{\link[raster]{stack}} object of biginning and end dates of the season for each pixel.
#' @seealso \code{\link[growingSeason]{phases}}, \code{\link[growingSeason]{seasons}}, \code{\link[growingSeason]{date_seasons}}
#' @importFrom raster raster stack getValues setValues nlayers addLayer blockSize brick
#'  canProcessInMemory extension getCluster nlayers pbClose pbCreate pbStep rasterTmpFile
#'  returnCluster trim writeStart writeStop writeValues
#' @importFrom snow sendCall recvOneData
#' @importFrom plyr rbind.fill
#' @importFrom utils flush.console
#' @export
par_stack_season_dates <- function(x, type = "v_points",
                               n_criticals = 1, steps = 2, ts_freq = 23,
                               returned = c("ts_seasonal", "original"),
                               valid_range = NULL,
                               filename="", ...) {
  filename_last <- filename
  out <- raster(x)
  out0 <- out; out0 [] <- NA
  cl <- getCluster()
  on.exit( returnCluster() )
  nodes <- length(cl)
  bs <- blockSize(x=x,  minblocks=nodes*4)
  pb <- pbCreate(bs$n)
  clus_fun <- function (block_i) {
    # #---------------------------------------
    # seasons
    # #---------------------------------------
    # date_seasons
    # #---------------------------------------
    val <- getValues (x, bs$row[block_i], bs$nrows[block_i])
    colnames(val) <- as.character(as.Date(substring(colnames(val),
                                                    2), format = "%Y.%m.%d"))
    if(!is.null(valid_range)){
      val <- ifelse(val < valid_range[1],NA,val)
      val <- ifelse(val > valid_range[2],NA,val)
    }

    val <- lapply (X=seq_len(nrow(val)), FUN = function(i) val[i, ])

    seas <- lapply(X = val, FUN = function (i){
      if((all(is.na(i)))|(is.null(i))){
        tmp <- NA
      } else {
        tmp <- lapply(X=get_phases(x=i,type = type, n_criticals = n_criticals,
                                   ts_freq = ts_freq, steps = steps,
                                   returned = returned),
                      FUN = date_seasons)
      }
      unlist(tmp)
    })
  }

  # get all nodes going
  for (i in 1:nodes) {
    sendCall(cl[[i]], clus_fun, i, tag=i)
  }

  files_tracker <- NULL
  base_filename <- trim(filename)
  file_ext <- ifelse (extension(base_filename) == "", '.grd', extension(base_filename))
  base_filename <- ifelse(extension(base_filename) == "", paste0(base_filename, file_ext), base_filename)
  # print(file_ext)
  if (!canProcessInMemory(out, 8) & base_filename == "") {
    base_filename <- rasterTmpFile()
  }
  this_filename <- gsub(file_ext, paste0('_', 1, file_ext), base_filename)
  # print(this_filename)
  filename <- this_filename
  if (filename != "") {
    out_tmp <- paste0('out', '_', '1')
    files_tracker[out_tmp] <- filename
    # out_tmp <- writeStart(out, filename = filename )
    assign(out_tmp, writeStart(out, filename = filename))
  } else {
    vv <- vector('list', length = bs$n)
  }
  for (i in 1:bs$n) {
    # receive results from a node
    d <- recvOneData(cl)
    # print(d)
    # error?
    if (! d$value$success) {
      stop('cluster error')
    }
    # which block is this?
    b <- d$value$tag
    cat('received block: ',b,'\n'); flush.console();
    computed_values <- d$value$value
    # computed_values <-  do.call(rbind, computed_values)
    computed_values <- do.call(rbind.fill, lapply(computed_values, function (i) as.data.frame(t(as.matrix(i)))))
    # print(head(computed_values))
    if (filename != "") {
      if (ncol(computed_values) > length(files_tracker)){ # when there more data then placeholder
        for (j in (length(files_tracker)+1) : ncol(computed_values)) {
          # file_ext <- ifelse (is.null(extension(base_filename)), '.grd', extension(base_filename))
          file_edits <- paste0('_', j, file_ext)
          this_filename <- gsub(pattern = file_ext, replacement = file_edits, x = base_filename)
          # print(this_filename)
          out_tmp <- paste0('out', '_', j)
          files_tracker[out_tmp] <- this_filename
          # out_tmp <- writeStart(out, filename = files_tracker[j] )
          assign(out_tmp, writeStart(out, filename = files_tracker[j] ))
        }
      }
      for (k in 1:length(files_tracker)) {
        out_tmp <- names(files_tracker)[k]
        # out_tmp <- writeValues(eval(parse(text = out_tmp)), computed_values[, k], bs$row[b])
        assign(out_tmp, writeValues(eval(parse(text = out_tmp)), computed_values[, k], bs$row[b]))
      }
    } else {
      vv[[b]] <- computed_values
    }
    # need to send more data?
    ni <- nodes + i
    if (ni <= bs$n) {
      sendCall(cl[[d$node]], clus_fun, ni, tag=ni)
    }
    pbStep(pb)
  }
  pbClose(pb)
  if (filename != "") {
    for (i in 1:length(files_tracker)) {
      out_tmp <- names(files_tracker)[i]
      # out_tmp <- writeStop(eval(parse(text = out_tmp)))
      assign(out_tmp, writeStop(eval(parse(text = out_tmp))))
    }
    # names(files_tracker) <- NULL
    # print(list.files())

    out <- stack(files_tracker)
    # filename <- filename_last
    # print(filename)
    out <- brick(out, filename = filename_last, ...)
    # out <- lapply(files_tracker, raster)
  } else {
    vv <- do.call(rbind.fill, vv)
    repeat{
      out <- stack(addLayer(out, out0))
      if(nlayers(out) == ncol(vv)){
        break
      }
    }
    # out <- setValues(out, vv, filename = filename_last, ...)
    out <- setValues(out, vv)
  }
  return(out)
}

