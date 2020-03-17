#' @export
#' @rdname plot_province_map
plot_world_map <- function(x,
                           key = c("confirmedCount", "suspectedCount", "curedCount", "deadCount"),
                           bins = c(0, 10, 100, 500, 1000, 10000),
                           legend_title ='Confirmed',
                           legend_position = c("bottomright", "topright", "bottomleft", "topleft"),
                           color = "Reds") {


  key <- match.arg(key)
  legend_position <- match.arg(legend_position)

  # dplyr verbs require ncov to data frame since dplyr 1.0.0
  x <- data.frame(x)

  # filter Antarctica
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent != "Antarctica")
  # merge taiwan to china
  world$geometry[[41]] <- sf::st_union(
    world$geometry[[41]],
    world$geometry[[221]]
  )
  world <- world[-221, ]

  countries_en_zh <- system.file(
    "counties_en_zh.csv",
    package = "ncovmap"
  ) %>%
    readr::read_csv()
  world <- merge(world, countries_en_zh)

  # correct countries names according to world map
  world_ncov <-
    dplyr::filter(
      x,
      countryEnglishName != "Diamond Princess Cruise Ship"
    ) %>%
    dplyr::mutate(countryEnglishName = dplyr::case_when(
      countryEnglishName == "United States of America" ~ "United States",
      countryEnglishName == "Kampuchea (Cambodia )" ~   "Cambodia",
      countryEnglishName == "SriLanka" ~ "Sri Lanka",
      countryEnglishName == "United Kiongdom"~ "United Kingdom",
      TRUE ~ countryEnglishName
    ))

  bins <- setdiff(bins, c(0, 1)) %>%
    c(0, 1, .)
  world_ncov$key <- world_ncov[[key]]

  map_dat <-  merge(
    world,
    world_ncov,
    by.x  = "name",
    by.y = "countryEnglishName",
    all.x = TRUE
  )
  map_dat <- dplyr::mutate_if(map_dat, is.numeric, ~ ifelse(is.na(.x), 0, .x))
  map_dat$key_level <-  cut(
    map_dat$key,
    breaks = c(bins, Inf),
    labels = format_labels(bins),
    include.lowest = TRUE,
    right = FALSE
  )
  # map_dat[is.na(map_dat)] <- 0

  pal <- leaflet::colorFactor(
    palette = "Reds",
    domain = map_dat$key_level
  )
  colors <- pal(map_dat$key_level)
  # if the count is 0, manual set the color as white
  colors[map_dat$key == 0] <-  "#FFFFFF"

  legend_colors <- colors[!duplicated(colors)]
  names(legend_colors) <- map_dat$key_level[!duplicated(colors)]
  legend_colors <- sort(legend_colors, decreasing = TRUE)

  leaflet::leaflet(map_dat) %>%
    leaflet::addPolygons(
      smoothFactor = 1,
      fillOpacity = 0.7,
      weight = 1,
      popup = paste(map_dat$name_zh, map_dat$key),
      color = colors
    ) %>%
    leaflet::addLegend(
      legend_position,
      colors = legend_colors,
      labels = names(legend_colors),
      labFormat = leaflet::labelFormat(prefix = ""),
      opacity = 1
    )

}
