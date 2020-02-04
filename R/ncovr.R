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
get_ncov <- function(method = c('ncovr', 'tidy', 'api', 'china'),
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
  if(method == 'china'){
    ncov <- jsonlite::fromJSON('https://raw.githubusercontent.com/JackieZheng/2019-nCoV/master/Json/data.json')
    ncov[, 2:9] <- apply(ncov[,2:9], c(1,2), as.numeric)
    ncov[, 1] <- as.Date(ncov[, 1])
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


##' @title Load amap to leaflet
##'
##' @description Modified from leafletCN::geojsonMap()
##'
##' @usage
##' geojsonMap_legendless(dat, mapName, namevar=NULL, valuevar=NULL,
##'   palette = "Blues", colorMethod = "numeric",
##'   na.color = "#808080", popup = NULL, stroke = T, smoothFactor = 1,
##'    weight = 1, fillOpacity = 0.7, tileType, ...)
##'
##' @param dat a data.frame contain regions and values
##' @param mapName mapName for loading, eg. 'china', 'city', ...
##' @param namevar a formula show which feature is chosen for name variable
##' @param valuevar a formula show which feature is chosen for value variable
##' @param palette The colors or color function that values will be mapped to, see RColorBrewer::display.brewer.all()
##' @param colorMethod set one of the coloe mapping in c("numeric", "bin", "quantile", "Factor")
##' @param na.color The color to return for NA values. Note that na.color=NA is valid.
##' @param popup a character vector of the HTML content for the popups (you are recommended to escape the text using htmlEscape() for security reasons)
##' @param stroke whether to draw stroke along the path (e.g. the borders of polygons or circles)
##' @param smoothFactor how much to simplify the polyline on each zoom level (more means better performance and less accurate representation)
##' @param weight stroke width in pixels
##' @param fillOpacity fill opacity
##' @param tileType function to define tile like amap or leaflet::addTiles
##' @param ... other paramter pass to the color mapping function
##'
##' @examples
##' dat = data.frame(name = regionNames("china"),
##'                  value = runif(34))
##' geojsonMap(dat,"china")
##'
##' dat$value2 = cut(dat$value, c(0, 0.25, 0.5, 1))
##' geojsonMap(dat,"china",
##'   namevar = ~name,
##'   valuevar = ~value2,
##'   palette="Reds",
##'   colorMethod="factor")
##'
##' geojsonMap(dat,"china",
##'   namevar = ~name,
##'   valuevar = ~value2,
##'   palette = topo.colors(3),
##'   colorMethod="factor")
##' @export
geojsonMap_legendless = function(dat,
                                 mapName,
                                 namevar=NULL,
                                 valuevar=NULL,
                                 palette = "Blues",
                                 colorMethod = "numeric",
                                 na.color = "#808080",
                                 popup = NULL,
                                 stroke = T,
                                 smoothFactor = 1,
                                 weight = 1,
                                 fillOpacity = 0.7,
                                 legendTitle = "Legend",
                                 tileType = amap,
                                 ...){
  if(class(dat) != 'data.frame'){
    stop("dat should be a data.frame")
  }
  if(is.null(namevar)){
    name = dat[, 1] %>% leafletCN:::toLabel()
  }else{
    name = leaflet::evalFormula(namevar,dat) %>% leafletCN:::toLabel()
  }
  name = as.character(name)

  if(is.null(valuevar)){
    value = dat[, 2]
  }else{
    value = leaflet::evalFormula(valuevar,dat)
  }


  countries <- leafletCN:::readGeoLocal(mapName)
  countries$label = leafletCN:::toLabel(countries$name)
  index = sapply(countries$label,function(x) which(name==x)[1])

  if(is.null(popup)){
    countries$popup = countries$name
  }else if(length(popup)!=dim(dat)[1]){
    warning("Length of popup and data don't match, use names instead!")
    countries$popup = countries$name
  }else{
    countries$popup = popup[index]
  }

  countries$value = value[index]

  ##
  if(colorMethod == "numeric"){
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else if( colorMethod == "bin" ){
    pal <- leaflet::colorBin(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else if(colorMethod == "quantile"){
    pal <- leaflet::colorQuantile(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else if(colorMethod == "factor"){
    pal <- leaflet::colorFactor(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else{
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }


  map <- leaflet::leaflet(countries)
  map %>% tileType %>%
    leaflet::addPolygons(stroke = stroke,
                         smoothFactor = smoothFactor,
                         fillOpacity = fillOpacity,
                         weight = weight,
                         color = ~pal(value),
                         popup = ~htmltools::htmlEscape(popup)
    )
}

#' Plot maps for nCoV in China
#'
#' @param ncov the dataset obtained by get_ncov()
#' @param key the column name
#' @param legend_title legend title
#' @param color The colors or color function that values will be mapped to, see RColorBrewer::display.brewer.all()
#' @param scale "cat" for category, 'log' for numeric
#' @param method 'province' or 'city'.
#' @param filter the rows to remove.
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' ncov <- get_ncov()
#' plot_map(ncov)
plot_map <- function(x,
                     key = c("confirmedCount", "suspectedCount", "curedCount", "deadCount"),
                     filter = 'unkown',
                     legend_title ='Confirmed',
                     color = "Reds",
                     scale = c("cat", "log"),
                     method = c('province', 'city')){
  key <- match.arg(key)
  scale <- match.arg(scale)
  method <- match.arg(method)

  count_cut <- c(0, 9, 99, 999, Inf)

  x <- x[x$provinceName != filter,]
  x$key <- x[, key]

  if(method == "province"){
    if(scale == "cat") {
      x$key_level <- cut(
        x$key,
        count_cut,
        labels = c('< 10', '10-99', '100-999', '>999'))
      mymap <-
        leafletCN::geojsonMap(
          dat = x,
          mapName = "china",
          colorMethod = "factor",
          palette=color,
          namevar = ~provinceName,
          valuevar = ~key_level,
          popup =  paste(
            x$provinceName,
            x$key),
          legendTitle = legend_title)
    }
    if(scale == 'log'){
      x$key_log <- log10(x$key)
      x$key_log[x$key == 0] <- NA
      mymap <-
        geojsonMap_legendless(
        dat = x,
        mapName = "china",
        palette = color,
        namevar = ~provinceName,
        valuevar = ~key_log,
        popup =  paste(
          x$provinceName,
          x$key)) %>%
        leaflet::addLegend(
          "bottomright",
          bins = 4,
          pal = leaflet::colorNumeric(
            palette = color,
            domain = x$key_log
          ),
          values = x$key_log,
          title = legend_title,
          labFormat = leaflet::labelFormat(
            digits = 0,
            transform = function(x) 10 ^ x),
          opacity = 1)
    }
  }

  if(method == "city") {
    x_city <- dplyr::bind_rows(x$cities)
    if(nrow(x_city) != 0) x_city <- x_city[, 1:5]
    x_area <- x[, 2:6]
    names(x_area)[1] <- 'cityName'
    x_cities <- rbind(x_city, x_area)
    cities <- leafletCN::regionNames(mapName = 'city')
    x_cities <- x_cities[x_cities$cityName %in% cities, ]
    x_cities[, 'key'] <- x_cities[, key]

    if(scale == 'cat'){
      x_cities$key_level <- cut(
        unlist(x_cities[, key]),
        count_cut,
        labels = c('< 10', '10-99', '100-999', '>999'))
      mymap <- leafletCN::geojsonMap(
        dat = as.data.frame(x_cities),
        mapName = "city",
        colorMethod = "factor",
        palette=color,
        namevar = ~cityName,
        valuevar = ~key_level,
        popup =  paste(
          x_cities$cityName,
          x_cities$key),
        legendTitle = legend_title)
    }
    if(scale == 'log'){
      x_cities$key_log <- log10(x_cities$key)
      x_cities$key_log[x_cities$key == 0] <- NA
      mymap <- geojsonMap_legendless(
        dat = as.data.frame(x_cities),
        mapName = "city",
        palette = color,
        namevar = ~cityName,
        valuevar = ~key_log,
        popup =  paste(x_cities$cityName,
                       x_cities$key)) %>%
        leaflet::addLegend(
          "bottomright",
          bins = 4,
          pal = leaflet::colorNumeric(
            palette = color,
            domain = x_cities$key_log
            ),
          values = x_cities$key_log,
          title = legend_title,
          labFormat = leaflet::labelFormat(
            digits = 0,
            transform = function(x) 10 ^ x),
          opacity = 1)
    }
  }
  mymap
}

#' Predict
#'
#' @param province province short name
#' @param ncov ncov data
#' @param ifplot logic.
#'
#' @return a figure.
#' @export
#'
#' @examples
#' Sys.setlocale('LC_CTYPE', 'Chinese')
#' ncov <- get_ncov()#Get the data
#' myfig <- predict_date(province = "provincenameinchinease", ncov = ncov)
predict_date <- function(province, ncov = ncov, ifplot = TRUE){
  #Dataset For a specific area
  Area <- ncov$area
  Area$updateTime <- ncovr:::conv_time(Area$updateTime)#Correct the time

  Region_all <- unique(Area$provinceShortName)
  Region_name <- Region_all[match(province, Region_all)]#Match regional name
  Region <- subset(Area,provinceShortName==Region_name)

  RegionTime <- aggregate(confirmedCount~updateTime,data=Region,sum)

  RegionTime$Date <- format(RegionTime$updateTime,"%m-%d")

  RegionDat <- aggregate(confirmedCount~Date,data=RegionTime,max)

  #No data availalbe for specific date
  RegionDat$Day <- 1:nrow(RegionDat)
  RegionDat$New <- with(RegionDat,confirmedCount-c(0,confirmedCount[-nrow(RegionDat)]))
  RegionDat$Date <-  as.Date(RegionDat$Date,"%m-%d")
  Length <- round(2.2*nrow(RegionDat),0)
  Dseq <- format(seq(RegionDat$Date[1], by = "day", length.out = Length ),"%m-%d")

  #Model logistic regression
  md <- nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat)
  Coe <- summary(md)$coefficients
  a <- Coe[1,1]
  xmax <-  2*Coe[2,1]

  #End date
  END <- Dseq [round(xmax,0)]
  #Predict
  Input=nrow(RegionDat)+1
  Predict <- round(a/(1+exp((Coe[2,1]-Input)/Coe[3,1])),0)

  if(ifplot){
    #Plot the results
    myplot <- function(){
      par(mgp = c(2.5, 1, 0))
      with(RegionDat,plot(y=confirmedCount,x=Day,xlim=c(0,1.8*xmax),ylim=c(0,1.3*a),ylab="Number of cases",xlab="",bty='n',xaxt = "n"));title(Region_name)

      with(RegionDat,points(y=New,x=Day,col="grey",pch=19))

      #Smooth predict line
      PredS <- seq(0, 1.3*xmax, length = 100)
      lines(PredS, predict(md, list(Day = PredS )),col="red")

      #The daily increase case
      Lth<- round(1.3*xmax,0)
      newdat <- data.frame(Pred=1:Lth)
      newdat <- within(newdat, ypred <- predict(md,  list(Day = Pred )))
      newdat$Prednew <- with(newdat,ypred-c(0,ypred[-nrow(newdat)]))
      lines(x=newdat$Pred,y=newdat$Prednew,col="blue")

      axis(1, at=1:Length , labels=Dseq,cex.axis = 0.6,las=2)

      points(2,0.8*a)
      text(3,0.8*a,"Confirmed",cex=0.8,adj=0)
      segments(1.5,0.7*a,2.5,0.7*a,col="red")
      text(3,0.7*a,"Model fit",cex=0.8,adj=0)
      points(2,0.6*a,col="grey",pch=19)
      text(3,0.6*a,"Daily increase",cex=0.8,adj=0)

      segments(0,a,xmax,a,lty="dotted")
      segments(xmax,0,xmax,a,lty="dotted")
    }
    return(list(fig = myplot(), enddate = END, tomorrow = Dseq[nrow(RegionDat)+1], tomorrowcount = Predict))

  }
  return(list(enddate = END, tomorrow = Dseq[nrow(RegionDat)+1], tomorrowcount = Predict))
}

conv_time <- function(x){
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}
