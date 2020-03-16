#' Plot maps for nCoV
#'
#' @param ncov ncov data
#' @param province which province to plot
#' @param key the feature to plot
#' @param legend_title legend title
#' @param legend_position legend position
#' @param color color palette
#' @param scale category or factor
#' @param bins a numberic vector to cut the value to format the value to
#' @param map_title map title
#' category, only worked while \code{scale} is "cat"
#' @details For \code{plot_province_map()}, the color is set to white manually if
#'   there is no ncov.
#' @importFrom magrittr %>%
#' @import leafletCN
#' @author Yang Cao
#' @export
plot_province_map <- function(ncov,
                              province,
                              key = c("confirmedCount", "suspectedCount", "curedCount", "deadCount"),
                              legend_position = c("bottomright", "topright", "bottomleft", "topleft"),
                              legend_title ='Confirmed',
                              color = "Reds",
                              scale = c("cat", "log"),
                              bins = c(0, 10, 100, 1000),
                              map_title = paste0(province, "nCoV")) {
  key <- match.arg(key)
  scale <- match.arg(scale)
  legend_position <- match.arg(legend_position)

  province_cities_ncov <- tidy_province_ncov(ncov, province)
  province_cities_ncov$key <- province_cities_ncov[, key]

  if (scale == "cat") {
    bins <- setdiff(bins, c(0, 1)) %>%
      c(0, 1, .)

    province_cities_ncov$key_level <-  cut(
      province_cities_ncov$key,
      breaks = c(bins, Inf),
      labels = format_labels(bins),
      include.lowest = TRUE,
      right = FALSE)
    province_map <- leafletCN::leafletGeo(
      province,
      province_cities_ncov,
      namevar = ~cityName,
      valuevar = ~key_level)

    # sort the `province_cities_ncov` according to city names in `province_map`
    # province_cities_ncov <- sort_province_ncov_map(province_cities_ncov, province_map)

    pal <- leaflet::colorFactor(palette = color, domain = province_map$value)

    # if the count is 0, manual set the color as white
    colors <- pal(province_map$value)
    colors[province_map$value == 0] = "#FFFFFF"
    map_colors <- colors
    names(colors) <- province_map$value
    legend_colors <- colors[!duplicated(colors)] %>%
      sort(decreasing = TRUE)

    res <- leaflet::leaflet(province_map) %>%
      leaflet::addPolygons(
        stroke = TRUE,
        smoothFactor = 1,
        fillOpacity = 0.7,
        weight = 1,
        color = map_colors,
        popup =  paste(
          province_cities_ncov$cityName,
          province_cities_ncov$key)
      ) %>%
      leaflet::addLegend(
        legend_position,
        colors = legend_colors,
        labels = names(legend_colors),
        labFormat = leaflet::labelFormat(prefix = ""),
        opacity = 1
      )
  }
  if (scale == "log") {
    province_cities_ncov <- dplyr::mutate(
      province_cities_ncov,
      key_log = ifelse(key == 0, 0, log10(key))
    )
    province_map <- leafletCN::leafletGeo(
      province,
      province_cities_ncov,
      valuevar = ~key_log
    )

    # sort the `province_cities_ncov` according to city names in `province_map`
    # province_cities_ncov <- sort_province_ncov_map(province_cities_ncov, province_map)

    pal <- leaflet::colorNumeric(palette = color, domain = province_map$value)
    # pal <- leaflet::colorBin(color, province_map$value)

    res <- leaflet::leaflet(province_map) %>%
      leaflet::addPolygons(
        stroke = TRUE,
        smoothFactor = 1,
        fillOpacity = 0.7,
        weight = 1,
        color = ~ pal(value),
        popup =  paste(
          province_cities_ncov$cityName,
          province_cities_ncov$key)
      ) %>%
      leaflet::addLegend(
        legend_position,
        bins = 4,
        pal = pal,
        values = ~value,
        title = legend_title,
        labFormat = leaflet::labelFormat(
          digits = 0,
          transform = function(x) 10 ^ x),
        opacity = 1)
  }

  # #' @importFrom htmltools tags HTML
  # add title
  # title <- htmltools::tags$div(
  #   tag.map.title,
  #   htmltools::HTML(map_title)
  # )
  # res %>% leaflet::addControl(
  #   title, position = "topleft",
  #   className = "map-title"
  # )

  res
}


#' @param tile_type function to define tile like amap or
#' \code{\link[leaflet]{addTiles}}, default NULL
#' @rdname plot_province_map
#' @export
plot_province_map2 <- function(ncov,
                               province,
                               key = c("confirmedCount", "suspectedCount", "curedCount", "deadCount"),
                               legend_position = c("bottomright", "topright", "bottomleft", "topleft"),
                               legend_title ='Confirmed',
                               color = "Reds",
                               scale = c("cat", "log"),
                               bins = c(0, 10, 100, 1000),
                               tile_type = NULL,
                               map_title = paste0(province, "nCoV")) {
  key <- match.arg(key)
  scale <- match.arg(scale)
  legend_position <- match.arg(legend_position)

  province_cities_ncov <- tidy_province_ncov(ncov, province)
  province_cities_ncov$key <- province_cities_ncov[, key]

  if (scale == "cat") {
    bins <- setdiff(bins, c(0, 1)) %>%
      c(0, 1, .)

    province_cities_ncov$key_level <-  cut(
      province_cities_ncov$key,
      breaks = c(bins, Inf),
      labels = format_labels(bins),
      include.lowest = TRUE,
      right = FALSE)

    # sort the `province_cities_ncov` according to city names in `province_map`
    # province_map <- leafletCN::leafletGeo(province)
    # province_cities_ncov <- sort_province_ncov_map(province_cities_ncov, province_map)
    # province_cities_ncov$cityName <- province_map$name

    res <- leafletCN::geojsonMap(
      dat = province_cities_ncov,
      mapName = province,
      colorMethod = "factor",
      palette = color,
      namevar = ~ cityName,
      valuevar = ~ key_level,
      popup =  paste(
        province_cities_ncov$cityName,
        province_cities_ncov$key),
      legendTitle = legend_title,
      tileType = tile_type)
  }
  if (scale == "log") {
    province_cities_ncov$key_log <- log10(province_cities_ncov$key)
    province_cities_ncov$key_log[province_cities_ncov$key == 0] <- NA

    # sort the `province_cities_ncov` according to city names in `province_map`
    # province_map <- leafletCN::leafletGeo(province)
    # province_cities_ncov <- sort_province_ncov_map(province_cities_ncov, province_map)
    # province_cities_ncov$cityName <- province_map$name

    res <- geojsonMap_legendless(
      dat = province_cities_ncov,
      mapName = province,
      palette = color,
      namevar = ~ cityName,
      valuevar = ~ key_log,
      popup =  paste(
        province_cities_ncov$cityName,
        province_cities_ncov$key),
      tileType = tile_type) %>%
      leaflet::addLegend(
        "bottomright",
        bins = 4,
        pal = leaflet::colorNumeric(
          palette = color,
          domain = province_cities_ncov$key_log
        ),
        values = province_cities_ncov$key_log,
        title = legend_title,
        labFormat = leaflet::labelFormat(
          digits = 0,
          transform = function(x) 10 ^ x),
        opacity = 1)
  }

  res

  # title <- htmltools::tags$div(
  #   tag.map.title,
  #   htmltools::HTML(map_title)
  # )
  #
  # res %>% leaflet::addControl(
  #   title,
  #   position = "topleft",
  #   className="map-title"
  # )
}

#' Format legend labels
#' @noRd
format_labels <- function(bins, sep = "~") {
  bins <- setdiff(bins, c(0, 1)) %>%
    c(0, 1, .)
  n <- length(bins)
  labels <- vector("character", n -1)
  labels[1] <- 0
  labels[n] <- paste(">=", bins[n])
  for (i in 2:(n-1)) {
    if (bins[i] == bins[i + 1] - 1) {
      labels[i] = bins[i]
    } else {
      labels[i] <- paste0(bins[i], sep, bins[i + 1]  - 1)
    }
  }

  labels
}


#' Add cities in which the count of ncov is 0
#' @noRd
tidy_province_ncov <- function(ncov, province) {
  province_cities_ncov <- correct_ncov_cities(ncov, province)
  province_cities <- leafletCN::regionNames(province)

  city_zero <- setdiff(province_cities, province_cities_ncov$cityName)

  # bind the cities which has no ncov
  if (length(city_zero)) {
    city_zero <- data.frame(cityName = city_zero, stringsAsFactors = FALSE)
    province_cities_ncov <- dplyr::bind_rows(
      province_cities_ncov,
      city_zero
    ) %>%
      dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.x), 0, .x))
  }
  # order the data acccording to regionNames
  province_cities_ncov <- province_cities_ncov[
    match(province_cities, province_cities_ncov$cityName), ]

  province_cities_ncov
}

#' Title css style
#' @references https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map
#' @noRd
# tag.map.title <- htmltools::tags$style(htmltools::HTML("
#     .leaflet-control.map-title {
#       background: rgba(255,255,255,0.75);
#       font-weight: bold;
#       font-size: 28px;
#     }
#   "))

#' Correct names of cities in ncov data to consistent with the cities names in
#'  leafletCN map
#'
#' Since the latest data was uesed for visualization, only correct the latest data
#'
#' @param ncov ncov data
#' @importFrom dplyr filter inner_join mutate select
#' @noRd
correct_ncov_cities <- function(ncov, province) {
  # xianggang aomen and taiwan, no cities ncov data
  ref_names <- leafletCN::leafletcn.map.names
  no_cities <- match(
    c("Hong Kong", "Macau", "Taiwan"),
    ref_names$name_en
  ) %>%
    ref_names[c("name", "label")][., ] %>%
    unlist()
  if (province %in% no_cities) {
    stop("ncov does not contian data on Hong Kang, Macau, or Taiwan")
  }

  res <- dplyr::inner_join(
    as.data.frame(ncov),
    city_reference,
    by = c("cityName" = "origin")
  ) %>%
    dplyr::mutate(cityName = corrected)

  res
}

