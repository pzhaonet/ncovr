#' Foreign ncov map
#' @param ncov ncov data
#' @param country country name
#' @param key the feature to plot
#' @param legend_title legend title
#' @param legend_position legend position
#' @param color color palette
#' @param bins a numberic vector to cut the value to format the value to
#' @export
plot_foreign_map <- function(ncov,
                             country,
                             key = c("confirmedCount", "suspectedCount",
                                     "curedCount", "deadCount"),
                             bins = c(0, 10, 100, 500, 1000),
                             legend_title ='Confirmed',
                             legend_position = c("bottomright", "topright",
                                                 "bottomleft", "topleft"),
                             color = "Reds") {
  key <- match.arg(key)
  legend_position <- match.arg(legend_position)

  country <- tolower(country)

  country_states <- system.file(
    "foreign_cities.csv",
    package = "ncovr"
  ) %>%
    readr::read_csv() %>%
    dplyr::filter(name_zh == country | name == country)

  if (grepl(country, "korea")) {
    country <- "south korea"
  }

  bins <- setdiff(bins, c(0, 1)) %>%
    c(0, 1, .)

  if (country == "italy") {
    json_file <- system.file(
      paste0("json/", country, ".json"),
      package = "ncovr"
    )
    country_map <- leafletCN::read.geoShape(json_file)
  } else {
    country_map <- rnaturalearth::ne_states(country)
    # country_states <- system.file(
    #   paste0(sub(" ", "_", country), "_states.csv"),
    #   package = "ncovmap"
    # ) %>%
    #   readr::read_csv()
  }

  if (country == "south korea") {
    country_map$name <- country_map$name_de
  }

  country_map$provinceName <- country_states$provinceName[
    match(country_map$name, country_states$provinceEnglishName)
    ]
  # country_map$name_zh <- country_states$name_zh[
  #  match(country_map$name, country_states$name)
  #  ]

  ncov <- dplyr::right_join(
    ncov,
    country_states,
    by = c("provinceName" = "provinceName")
  )
  ncov <-  dplyr::mutate_if(ncov, is.numeric, ~ ifelse(is.na(.x), 0, .x))

  ncov$key <- ncov[[key]]
  ncov$key_level <-  cut(
    ncov$key,
    breaks = c(bins, Inf),
    labels = format_labels(bins),
    include.lowest = TRUE,
    right = FALSE
  )

  index <- match(country_map$provinceName, ncov$provinceName)
  country_map$value <- ncov[["key_level"]][index]
  ncov <- ncov[index, ]

  pal <- leaflet::colorFactor(
    palette = "Reds",
    domain = country_map$value
  )

  colors <- pal(country_map$value)
  # if the count is 0, manual set the color as white
  colors[country_map$value == 0] <-  "#FFFFFF"
  map_colors <- colors
  names(colors) <- country_map$value
  legend_colors <- colors[!duplicated(colors)] %>%
    sort(decreasing = TRUE)


  leaflet::leaflet(country_map) %>%
    leaflet::addPolygons(
      smoothFactor = 1,
      fillOpacity = 0.7,
      weight = 1,
      color = map_colors,
      popup =  paste(
        ncov$provinceName,
        ncov$key)
    ) %>%
    leaflet::addLegend(
      "bottomleft",
      colors = legend_colors,
      labels = names(legend_colors),
      labFormat = leaflet::labelFormat(prefix = ""),
      opacity = 1
    )
}

#' Download ncov 2019 area and overall data from 163
#'
#' @param country foeign country name
#' @export
get_foreign_ncov <- function(country) {
  wy_ncov <- jsonlite::fromJSON("https://c.m.163.com/ug/api/wuhan/app/data/list-total")
  wy_ncov <- wy_ncov$data$areaTree
  foreign_ncov <- wy_ncov[wy_ncov$name == country, ]

  if ("children" %in% names(foreign_ncov)) {
    child <- foreign_ncov$children[[1]]
    child <- data.frame(
      confirmedCount = child$total$confirm,
      suspectedCount = child$total$suspect,
      curedCount = child$total$heal,
      deadCount =  child$total$dead,
      provinceName = child$name,
      updateTime = child$lastUpdateTime,
      stringsAsFactors = FALSE
    )
  } else {
    stop("No province/city ncov data of", country)
  }

  child
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
