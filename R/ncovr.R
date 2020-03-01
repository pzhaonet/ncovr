conv_time <- function(x){
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

conv_firstletter <- function(x){
  paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep="")
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
get_ncov <- function(method = c('ncovr', 'tidy', 'api', 'china', 'csv'),
                     port = c('area', 'overall', 'news', 'rumors'),
                     base = 'https://lab.isaaclin.cn/nCoV/api/',
                     if_en = FALSE){
  method <- match.arg(method)

  if(if_en) dic <- readr::read_csv(system.file('dic.csv', package = 'ncovr'))

  if(method == 'ncovr'){
    if(class(try(readRDS(gzcon(url('https://github.com/pzhaonet/ncov/raw/master/static/data-download/ncov.RDS'))))) == 'try-error'){
      method <- 'api'
    } else{
      ncov <- readRDS(gzcon(url('https://github.com/pzhaonet/ncov/raw/master/static/data-download/ncov.RDS')))
    }
  }
  if(method == 'tidy'){
    if(class(try(readRDS(gzcon(url('https://github.com/pzhaonet/ncov/raw/master/static/data-download/ncov_tidy.RDS'))))) == 'try-error'){
      method <- 'api'
    } else{
      ncov <- readRDS(gzcon(url('https://github.com/pzhaonet/ncov/raw/master/static/data-download/ncov_tidy.RDS')))
    }
  }
  if(method == 'api'){
    ncov <- lapply(port,
                   function(x) {
                     get_raw <- httr::GET(paste0(base, x))
                     get_text <- httr::content(get_raw, "text")
                     y <- jsonlite::fromJSON(get_text)$results
                     if(grepl('area', x)) {
                       dic_city <-  readr::read_csv(system.file('china_city_list.csv', package = 'ncovr'))
                       y$province_en <- dic_city[match(y$provinceShortName, dic_city$Province), 'Province_EN']
                     }
                     y
                   })
    names(ncov) <- port

  }
  if(method == 'china'){
    ncov <- jsonlite::fromJSON('https://raw.githubusercontent.com/JackieZheng/2019-nCoV/master/Json/data.json')
    ncov[, 2:9] <- apply(ncov[,2:9], c(1,2), as.numeric)
    ncov[, 1] <- as.Date(ncov[, 1])
    if(if_en) names(ncov) <- dic$en[match(names(ncov), dic$zh)]
  }
  if(method == 'csv') {
    port_upper <- conv_firstletter(port)
    ncov <- lapply(port_upper,
                   function(x) {
                     readr::read_csv(paste0('https://raw.githubusercontent.com/BlankerL/DXY-2019-nCoV-Data/master/csv/DXY', x,'.csv'))
                   }
    )
    names(ncov) <- port
  }
  ncov
}


conv_ncov <- function(ncov){
  dic_city <-  readr::read_csv(system.file('china_city_list.csv', package = 'ncovr'))

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
      ncov_area$cities[i][[1]]$province_en <- unlist(dic_city[match(ncov_area$provinceShortName[i], dic_city$Province), 'Province_EN'])
    }
  }
  ncov$area <- dplyr::bind_rows(ncov_area$cities)
  ncov$area$city_en <- unlist(dic_city[match(ncov$area$cityName, dic_city$City), 'City_EN'])
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
                                 tileType = NULL,
                                 ...){
  if(class(dat) != 'data.frame'){
    stop("dat should be a data.frame")
  }
  if(is.null(namevar)){
    name = dat[, 1]
  }else{
    name = leaflet::evalFormula(namevar,dat)
  }
  name = as.character(name)

  if(is.null(valuevar)){
    value = dat[, 2]
  }else{
    value = leaflet::evalFormula(valuevar,dat)
  }


  countries <- leafletCN:::readGeoLocal(mapName)
  countries$label = countries$name
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

  if (is.null(tileType)) {
    if (is.null(tileType)) {
      map %>%
        leaflet::addPolygons(
          stroke = stroke,
          smoothFactor = smoothFactor,
          fillOpacity = fillOpacity,
          weight = weight,
          color = ~pal(value),
          popup = ~htmltools::htmlEscape(popup)
        )
    }
  } else {
    map %>% tileType %>%
      leaflet::addPolygons(
        stroke = stroke,
        smoothFactor = smoothFactor,
        fillOpacity = fillOpacity,
        weight = weight,
        color = ~pal(value),
        popup = ~htmltools::htmlEscape(popup)
    )
  }
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
                     method = c('province', 'city', 'country')){
  key <- match.arg(key)
  scale <- match.arg(scale)
  method <- match.arg(method)

  ## add nanhai
  nanhai_label <- dplyr::filter(leafletCN::mapNames, name_en == "Nanhai")
  nanhai <- data.frame(
    provinceName = nanhai_label$name,
    provinceShortName = nanhai_label$name,
    provinceEnglishName = "Nanhai",
    stringsAsFactors = FALSE
  )
  x$province_en <- unlist(x$province_en)
  x <- dplyr::bind_rows(x, nanhai) %>%
    dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.x), 0, .x))

  count_cut <- c(0, 9, 99, 999, Inf)

  if("provinceName" %in% names(x)) x <- x[x$provinceName != filter,]
  x$key <- x[, key]

  if(method == "province"){
    if(scale == "cat") {
      x$key_level <- cut(
        x$key,
        count_cut,
        include.lowest = TRUE,
        labels = c('< 10', '10-99', '100-999', '>999'))
      mymap <-
        leafletCN::geojsonMap(
          dat = x,
          mapName = "china",
          colorMethod = "factor",
          palette=color,
          namevar = ~provinceShortName,
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
          namevar = ~provinceShortName,
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

  if(method == "country"){
    if(scale == "cat") {
      x$key_level <- cut(
        x$key,
        count_cut,
        labels = c('< 10', '10-99', '100-999', '>999'))
      mymap <-
        leafletCN::geojsonMap(
          dat = x,
          mapName = "world",
          colorMethod = "factor",
          palette=color,
          namevar = ~countryEnglishName2,
          valuevar = ~key_level,
          popup =  paste(
            x$countryEnglishName,
            x$key,
            x$countryName),
          legendTitle = legend_title)
    }
    if(scale == 'log'){
      x$key_log <- log10(x$key)
      x$key_log[x$key == 0] <- NA
      mymap <-
        geojsonMap_legendless(
          dat = x,
          mapName = "world",
          palette = color,
          namevar = ~countryEnglishName2,
          valuevar = ~key_log,
          popup =  paste(
            x$countryEnglishName,
            x$key,
            x$countryName)) %>%
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

  mymap
}

#' Previous version of predict_date()
#'
#' @param province province short name
#' @param ncov ncov data
#' @param ifplot logic.
#'
#' @return a figure.
# #' @export
#'
#' @examples
#' Sys.setlocale('LC_CTYPE', 'Chinese')
#' ncov <- get_ncov()#Get the data
#' ncovChina <-get_ncov('china')
#' predict_date(province = "jiangsuinchinese",  ncov = ncov)
#' predict_date(province = "meiguoinchinese",  ncov = ncov)
#' predict_date(province = "zhongguoinchinese",  ncov = ncovChina)
predict_date2 <- function(province, ncov = c(ncov,ncovChina), ifplot = TRUE, addtitle = NA, ifchinese = FALSE, fontfamily = NA){
  dic <- readr::read_csv(system.file('dic.csv', package = 'ncovr'))
  #Dataset for a specific area
  if (province!=dic$zh[1])
  {
    Area <- ncov$area
    Area$updateTime <- ncovr:::conv_time(Area$updateTime)#Correct the time
    Area$Date <- format(Area$updateTime,"%m-%d")
    Region_all <- unique(Area$provinceShortName)
    Region_name <- Region_all[match(province, Region_all)]#Match regional name
    Region <- subset(Area,provinceShortName==Region_name)
    RegionDat <- aggregate(confirmedCount~Date,data=Region,max)
    RegionDat$Date <- as.Date(RegionDat$Date,"%m-%d")
    RegionDat <- RegionDat[RegionDat$Date<Sys.Date(),]
  }

  if (province==dic$zh[1])
  {
    col_confirmed <- dic$zh[c(2, 11)][dic$zh[c(2, 11)] %in% names(ncovChina)]
    RegionDat <- data.frame(confirmedCount=c(58, 136, 198, ncovChina[,col_confirmed]))
    RegionDat$Date <- seq(as.Date("2020-01-17",format="%Y-%m-%d"),by = "day", length.out = nrow(RegionDat))

  }

  #No data availalbe for specific date
  RegionDat$Day <- as.numeric(RegionDat$Date-RegionDat$Date[1])+1
  RegionDat$New <- with(RegionDat,confirmedCount-c(0,confirmedCount[-nrow(RegionDat)]))
  Length <- as.numeric(Sys.Date()-as.Date(RegionDat$Date[1],"%m-%d")+20)#x axis from today to 20 days later
  Dseq <- format(seq(RegionDat$Date[1], by = "day", length.out = Length ),"%m-%d")

  END <- NA
  Predict <- NA
  NewIncrease <- NA
  a <- NA
  xmax <- NA
  r.sq <- NA

  # #Model logistic regression
  an.error.occured <- 0
  tryCatch({
    md <- nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat)
    Coe <- summary(md)$coefficients
    a <- Coe[1,1]
    xmax <-  2*Coe[2,1]

    #End date
    END = Dseq [round(xmax,0)]
    #Predict
    Input=nrow(RegionDat)+1
    Predict <- round(a/(1+exp((Coe[2,1]-Input)/Coe[3,1])),0)
    #Model fit

    X1 <- RegionDat[complete.cases(RegionDat$confirmedCount),"confirmedCount"]
    Y1 <- predict(md,X1)
    r.sq <- max(cor(X1,Y1),0)^2

    #The daily increase case
    Lth <- as.numeric(as.Date(END,"%m-%d")-as.Date(Dseq[1],"%m-%d"))
    newdat <-  data.frame(Date=as.Date(Dseq[1:Lth],"%m-%d"),Pred=1:Lth)
    newdat <- within(newdat, ypred <- predict(md,  list(Day = Pred )))
    newdat$Prednew <- with(newdat,ypred-c(0,ypred[-nrow(newdat)]))
    newdat$Judge <- ifelse(newdat$Date ==Sys.Date(),"Tmr",ifelse(newdat$Date < Sys.Date(),"Obs","Pre"))
    if (Sys.Date()+1>as.Date(END,"%m-%d")){NewIncrease <- 0}else
    {NewIncrease <- round(subset(newdat,Judge=="Tmr")[,"Prednew"],0)}

  }, error = function(e) {an.error.occured <<- 1})

  if(ifplot){
    #Plot the results
    par(mgp = c(2.5, 1, 0))
    if(ifchinese & !is.na(fontfamily)) par(family=fontfamily)
    with(RegionDat,plot(y=confirmedCount,x=Day,xlim=c(0,Length),ylim=c(0,2*max(confirmedCount)),ylab=ifelse(ifchinese, dic$zh[9], dic$en[9]),xlab="",bty='n',xaxt = "n"))
    title(province)
    with(RegionDat,points(y=New,x=Day,col="grey",pch=19))
    Dseq <- format(seq(RegionDat$Date[1],by = "day", length.out = Length ),"%m-%d")
    axis(1, at=1:Length, labels=Dseq,cex.axis = 0.6,las=2)

    legend("topleft", legend=c(ifelse(ifchinese, dic$zh[4], dic$en[4]),ifelse(ifchinese, dic$zh[5], dic$en[5]),ifelse(ifchinese, dic$zh[6], dic$en[6])),bty='n',col=c("black", "grey","red"), pch=c(1,19,19), cex=0.6)

    if(an.error.occured == 0 & r.sq>0.90)
    {
      with(subset(newdat,Judge=="Obs"),lines(x=Pred,y=ypred,col="red"))
      with(subset(newdat,Judge=="Pre"),lines(x=Pred,y=ypred,col="red",lty="dotted"))
      with(subset(newdat,Judge=="Obs"),lines(x=Pred,y=Prednew,col="blue"))
      with(subset(newdat,Judge=="Pre"),lines(x=Pred,y=Prednew,col="blue",lty="dotted"))
      with(subset(newdat,Judge=="Tmr"),points(x=Pred,y=ypred,col="red",pch=19))
      with(subset(newdat,Judge=="Tmr"),points(x=Pred,y=Prednew,col="red",pch=19))

      #Modelling legend
      legend("top", legend=c(ifelse(ifchinese, dic$zh[7], dic$en[7]),ifelse(ifchinese, dic$zh[8], dic$en[8])), col=c("black", "black"), lty = 1:2, cex=0.6, bty='n')
    }
  } else {
    list(area = province,
         enddate = END,
         tomorrow = Dseq[nrow(RegionDat)+1],
         tomorrowcount = Predict,
         tomorrowNew=NewIncrease)
  }
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
#' ncovChina <-get_ncov('china')
#' predict_date(province = "jiangsuinchinese",  ncov = ncov)
#' predict_date(province = "meiguoinchinese",  ncov = ncov)
#' predict_date(province = "zhongguoinchinese",  ncov = ncovChina)
predict_date <- function(province, ncov = c(ncov,ncovChina), ifplot = TRUE, addtitle = NA, ifchinese = FALSE, fontfamily = NA,predDate=Sys.Date()){
  dic <- readr::read_csv(system.file('dic.csv', package = 'ncovr'))
  #Dataset for a specific area
  if (province!=dic$zh[1])
  {
    Area <- ncov$area
    Area$updateTime <- ncovr:::conv_time(Area$updateTime)#Correct the time
    Area$Date <- format(Area$updateTime,"%m-%d")
    Region_all <- unique(Area$provinceShortName)
    Region_name <- Region_all[match(province, Region_all)]#Match regional name
    Region <- subset(Area,provinceShortName==Region_name)
    RegionDat <- aggregate(confirmedCount~Date,data=Region,max)
    RegionDat$Date <- as.Date(RegionDat$Date,"%m-%d")
    RegionDat <- RegionDat[RegionDat$Date<predDate,]
  }

  if (province==dic$zh[1])
  {
    #   col_confirmed <- dic$zh[c(2, 11)][dic$zh[c(2, 11)] %in% names(ncovChina)]
    #  RegionDat <- data.frame(confirmedCount=c(58, 136, 198, ncovChina[,col_confirmed]))
    RegionDat <- data.frame(confirmedCount=c(58, 136, 198, ncovChina[,2]))
    RegionDat$Date <- seq(as.Date("2020-01-17",format="%Y-%m-%d"),by = "day", length.out = nrow(RegionDat))

  }

  #No data availalbe for specific date
  RegionDat$Day <- as.numeric(RegionDat$Date-RegionDat$Date[1])+1
  RegionDat$New <- with(RegionDat,confirmedCount-c(0,confirmedCount[-nrow(RegionDat)]))
  Length <- as.numeric(predDate-as.Date(RegionDat$Date[1],"%m-%d")+20)#x axis from today to 20 days later
  Dseq <- format(seq(RegionDat$Date[1], by = "day", length.out = Length ),"%m-%d")

  END <- NA
  Predict <- NA
  NewIncrease <- NA
  a <- NA
  xmax <- NA
  r.sq <- NA

  # #Model logistic regression
  an.error.occured <- 0
  tryCatch({
    md <- nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat)
    Coe <- summary(md)$coefficients
    a <- Coe[1,1]
    xmax <-  2*Coe[2,1]

    #End date
    END = Dseq [round(xmax,0)]
    #Predict
    Input=nrow(RegionDat)+1
    Predict <- round(a/(1+exp((Coe[2,1]-Input)/Coe[3,1])),0)
    #Model fit

    X1 <- RegionDat[complete.cases(RegionDat$confirmedCount),"confirmedCount"]
    Y1 <- predict(md,X1)
    r.sq <- max(cor(X1,Y1),0)^2

    #The daily increase case
    Lth <- as.numeric(as.Date(END,"%m-%d")-as.Date(Dseq[1],"%m-%d"))
    newdat <-  data.frame(Date=as.Date(Dseq[1:Lth],"%m-%d"),Pred=1:Lth)
    newdat <- within(newdat, ypred <- predict(md,  list(Day = Pred )))
    newdat$Prednew <- with(newdat,ypred-c(0,ypred[-nrow(newdat)]))
    newdat$Judge <- ifelse(newdat$Date ==predDate,"Tmr",ifelse(newdat$Date < predDate,"Obs","Pre"))
    if (predDate+1>as.Date(END,"%m-%d")){NewIncrease <- 0}else
    {NewIncrease <- round(subset(newdat,Judge=="Tmr")[,"Prednew"],0)}

  }, error = function(e) {an.error.occured <<- 1})

  if(ifplot){
    #Plot the results
    par(mgp = c(2.5, 1, 0))
    if(ifchinese & !is.na(fontfamily)) par(family=fontfamily)
    with(RegionDat,plot(y=confirmedCount,x=Day,xlim=c(0,Length),ylim=c(0,2*max(confirmedCount)),ylab=ifelse(ifchinese, dic$zh[9], dic$en[9]),xlab="",bty='n',xaxt = "n"))
    title(province)
    with(RegionDat,points(y=New,x=Day,col="grey",pch=19))
    Dseq <- format(seq(RegionDat$Date[1],by = "day", length.out = Length ),"%m-%d")
    axis(1, at=1:Length, labels=Dseq,cex.axis = 0.6,las=2)

    legend("topleft", legend=c(ifelse(ifchinese, dic$zh[4], dic$en[4]),ifelse(ifchinese, dic$zh[5], dic$en[5]),ifelse(ifchinese, dic$zh[6], dic$en[6])),bty='n',col=c("black", "grey","red"), pch=c(1,19,19), cex=0.6)

    if(an.error.occured == 0 & r.sq>0.90)
    {
      with(subset(newdat,Judge=="Obs"),lines(x=Pred,y=ypred,col="red"))
      with(subset(newdat,Judge=="Pre"),lines(x=Pred,y=ypred,col="red",lty="dotted"))
      with(subset(newdat,Judge=="Obs"),lines(x=Pred,y=Prednew,col="blue"))
      with(subset(newdat,Judge=="Pre"),lines(x=Pred,y=Prednew,col="blue",lty="dotted"))
      with(subset(newdat,Judge=="Tmr"),points(x=Pred,y=ypred,col="red",pch=19))
      with(subset(newdat,Judge=="Tmr"),points(x=Pred,y=Prednew,col="red",pch=19))

      #Modelling legend
      legend("top", legend=c(ifelse(ifchinese, dic$zh[7], dic$en[7]),ifelse(ifchinese, dic$zh[8], dic$en[8])), col=c("black", "black"), lty = 1:2, cex=0.6, bty='n')
    }
  } else {
    list(area = province,
         enddate = END,
         tomorrow = predDate,
         tomorrowcount = Predict,
         tomorrowNew=NewIncrease)
  }
}


#' Plot map with ggplot
#'
#' @param x data frame with province name and value
#' @param col_name column whose value is to display
#' @param province_language chinese, english or NA (not shown)
#' @param show_capitials logical.
#' @param add_title title
#'
#' @return a ggplot map
#' @export
#'
#' @examples
#' require(ncovr)
#' library(ggplot2)
#' library(dplyr)
#' library(scales)
#' Sys.setlocale("LC_CTYPE", "Chinese")
#' ncov <- get_ncov()
#' ncov$area$date <- as.Date(ncovr:::conv_time(ncov$area$updateTime))
#'
#' choose_date <- "2020-02-10"
#' x <- ncov$area[ncov$area$date <= as.Date(choose_date), ]
#' x <- x[!duplicated(x$provinceName), ]
#' plot_ggmap(x)
#' plot_ggmap(x, province_language = "english", show_capitials = TRUE) +
#'   annotate(geom = "text", 105, y = 35, label = "ncov2020.org", color = "white",
#'            fontface = "bold", size = 10, alpha = 0.5)
#'
#' library(animation)
#' ncov_dates <- as.character(rev(unique(ncov$area$date)))
#' ani.options(interval = 0.5, ani.width = 1000, ani.height = 600)
#' saveGIF({
#'   for (i in rev(ncov_dates)) {
#'     x <- ncov$area[ncov$area$date <= as.Date(i), ]
#'     x <- x[!duplicated(x$provinceName), ]
#'     print(plot_ggmap(x,))
#'   }
#' }, movie.name = "map_animation.gif")

plot_ggmap <- function(x, col_name = 'confirmedCount', province_language = 'chinese', show_capitials = TRUE, add_title = NA){
  load(system.file('ProvinceMapDatas.Rda', package = 'ncovr'))
  city_position = read.csv(system.file("city_position.csv", package = 'ncovr'))
  x$value <- x[, col_name]

  china_map_virus = left_join(df_China, x[, c('provinceShortName', 'value')], by = c("NAME" = "provinceShortName"))

  p <-
    ggplot() +
    geom_polygon(data = china_map_virus,
                 aes(x = long, y = lat, group = interaction(class, group), fill = value),
                 colour = "black", size = 0.25) +
    geom_rect(aes(xmin = 124, xmax = 124 + 9.3, ymin = 16 - 0.3, ymax = 16 + 9),
              fill = NA, colour = "black", size = 0.25) +
    geom_line(data = df_NanHaiLine, aes(x = long, y = lat, group = ID),
              colour = "grey40", size = 1) +
    scale_fill_gradient(low = "white", high = "darkred", na.value = "white", trans = "log", limits = c(1, 10^5), breaks = 10 ^(0:5), labels = c('1', '10', '100', '1,000', '10,000', '100,000'))  +
    coord_map() +
    ylim(14, 54) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.85, 0.4),
          legend.title = element_blank(),
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size = 16,
                                    face = "bold",
                                    color = "black"))
  if(!is.na(add_title)){
    p <- p +
      geom_text(data = data.frame(long = 105, lat = 50), aes(x = long, y = lat, label = add_title),
                colour = "black", size = 5)
  }

  if(show_capitials){
    p <- p +  geom_point(data = city_position, aes(x = long, y = lat),
                         colour = "black", size = 1)
  }

  if(!is.na(province_language)){
    if(province_language == 'english') {
      dic_city <-  readr::read_csv(system.file('china_city_list.csv', package = 'ncovr'))
      city_position$province <- dic_city$Province_EN[match(city_position$province, dic_city$Province)]
    }
    p <- p +
      geom_text(data = city_position, aes(x = long, y = lat, label = province),
                colour = "black", size = 3, vjust = 0, nudge_y = 0.5)
  }
  p
}


#' Plot time series of a country's cases
#'
#' @param x a datafram with "countryEnglishName", "date", "confirmed", "cured", "dead" columns
#' @param area name of an selected area
#' @param area_col the column name of the area in x
#' @param date_col the column name of the date in x
#' @param ts_col the column names of the time series in x
#'
#' @return a figure
#' @export
#' @examples
#' ncov_raw <- get_ncov()
#' ncov_raw$area$date <- as.Date(ncovr:::conv_time(ncov_raw$area$updateTime))
#' x <- ncov_raw$area[, c('countryEnglishName', 'countryName', 'date', 'confirmedCount', 'curedCount', 'deadCount')] %>%
#'   roup_by(countryEnglishName, date) %>%
#'   summarise(confirmed = sum(confirmedCount),
#'             cured = sum(curedCount),
#'             dead = sum(deadCount)) %>%
#'   ungroup()
#' x <- x[!is.na(x$countryEnglishName) & !x$countryEnglishName == 'China', ]
#' plot_ts(x, area = 'Korea')
plot_ts <- function(x, area, area_col = "countryEnglishName", date_col = "date", ts_col = c("confirmedCount", "curedCount", "deadCount")){
  x_ts <- data.frame(Date = seq.Date(from = min(x[, date_col]), to = max(x[, date_col]), by = 1))
  for(i in ts_col) x_ts[, i] <- NA
  x <- x[x[, area_col] == area, ]
  x_ts[match(x[, date_col], x_ts$Date), ts_col] <- x[, ts_col]

  x_ts <- tidyr::fill(x_ts, -1, .direction = "down")
  x_ts[is.na(x_ts)] <- 0
  x_ts <- xts::xts(x_ts[, 2:4], x_ts$Date)
  dygraphs::dygraph(x_ts, main = paste("COVID-19 in", area), ylab = 'Cases') %>%
    dygraphs::dyOptions(fillGraph = TRUE) %>%
    dygraphs::dyRangeSelector()
}
