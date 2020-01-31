conv_time <- function(x){
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

#' Get NCoV data
#'
#' @param port character. The port(s).
#' @param base character. The link of the database api.
#'
#' @return a list
#' @export
#'
#' @examples
#' get_ncov('overall')
get_ncov <- function(port = c('area', 'overall', 'provinceName', 'news', 'rumors'),
                     method = c('ncovr', 'api'),
                     base = 'https://lab.isaaclin.cn/nCoV/api/'){
  # port <- match.arg(port)
  method <- match.arg(method)
  if(method == 'api'){
    ncov <- lapply(port,
                   function(x) {
                     get_raw <- httr::GET(paste0(base, x))
                     get_text <- httr::content(get_raw, "text")
                     jsonlite::fromJSON(get_text)$results
                   })
    names(ncov) <- port
  }
  if(method == 'ncovr'){
    ncov <- readRDS('https://raw.githubusercontent.com/pzhaonet/ncovr/master/inst2/ncov_tidy.RDS')
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

# Sys.setlocale('LC_CTYPE', 'Chinese')
# ncov <- get_ncov(port = c('area?latest=0', 'overall', 'provinceName', 'news', 'rumors'))
# names(ncov)[1] <- 'area'
# ncov_tidy <- conv_ncov(ncov)
# saveRDS(ncov_tidy, 'inst2/ncov_tidy.RDS')
# saveRDS(ncov, 'inst2/ncov.RDS')
