conv_time <- function(x){
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

#' Get NCoV data
#'
#' @param method character.
#' @param port character. The port(s). Valid only for the 'api' method
#' @param base character. The link of the database api.
#'
#' @return a list
#' @export
#'
#' @examples
#' get_ncov('overall')
get_ncov <- function(method = c('ncovr', 'tidy', 'api'),
                     port = c('area', 'overall', 'provinceName', 'news', 'rumors'),
                     base = 'https://lab.isaaclin.cn/nCoV/api/'){
  method <- match.arg(method)
  if(method == 'tidy'){
    ncov <- readRDS(gzcon(url('https://github.com/pzhaonet/travis-ncov/raw/master/data/ncov_tidy.RDS')))
  }
  if(method == 'ncovr'){
    ncov <- readRDS(gzcon(url('https://github.com/pzhaonet/travis-ncov/raw/master/data/ncov.RDS')))
  }
  if(method == 'api'){
    ncov <- lapply(port,
                   function(x) {
                     get_raw <- httr::GET(paste0(base, x))
                     get_text <- httr::content(get_raw, "text")
                     jsonlite::fromJSON(get_text)$results
                   })
    names(ncov) <- port
  }
  ncov
}


conv_ncov <- function(ncov){
  # convert time stamps
  ncov$area$updateTime <- conv_time(ncov$area$updateTime)
  ncov$area$createTime <- conv_time(ncov$area$createTime)
  ncov$area$modifyTime <- conv_time(ncov$area$modifyTime)
  ncov$overall$updateTime <- conv_time(ncov$overall$updateTime)

  # convert area into city
  ncov_area <- ncov$area
  for(i in 1:nrow(ncov_area)){
    if(!is.null(ncov_area$cities[i][[1]])){
      ncov_area$cities[i][[1]]$country <- ncov_area$country[i]
      ncov_area$cities[i][[1]]$provinceName <- ncov_area$provinceName[i]
      ncov_area$cities[i][[1]]$provinceShortName <- ncov_area$provinceShortName[i]
      ncov_area$cities[i][[1]]$updateTime <- ncov_area$updateTime[i]
      ncov_area$cities[i][[1]]$createTime <- ncov_area$createTime[i]
      ncov_area$cities[i][[1]]$modifyTime <- ncov_area$modifyTime[i]
    }
  }
  ncov$area <- dplyr::bind_rows(ncov_area$cities)
  ncov
}
