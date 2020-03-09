#' Download ncov 2019 area and overall data from http://lab.isaaclin.cn/nCoV/
#'
#' @param latest logical, download the latest or all ncov data, default \code{TRUE}
#'
#' @export
get_ncov2 <- function(latest = TRUE) {
  api <- "https://lab.isaaclin.cn/nCoV/api/"
  para <- ifelse(latest, "?latest=1", "?latest=0")

  ncov <- jsonlite::fromJSON(paste0(api, "area", para))$results
  ncov$updateTime <- conv_time(ncov$updateTime)
  ncov <- purrr::map_dfr(
    1:nrow(ncov),
    ~ unnest_province_ncov(ncov[.x, ])
  )

  overall <- jsonlite::fromJSON(paste0(api, "overall", para))$results
  overall$updateTime <- conv_time(overall$updateTime)

  structure(
    ncov,
    overall = overall,
    class = c("ncov", "data.frame"),
    type = "All",
    from = api
  )
}

# unnest the cities data
unnest_province_ncov <- function(x) {
  p_ncov <- dplyr::select(
    x,
    dplyr::starts_with(c("province", "country", "continent")),
    # -countryShortCode,
    # cityName,
    cities,
    currentConfirmedCount:deadCount,
    updateTime
  )

  # no cities data, such as beijing, foregin countries
  if (length(p_ncov$cities[[1]]) == 0) {
    p_ncov$cities <- NULL
    p_ncov$cityEnglishName <- NA
    res <- p_ncov
  } else {
    c_ncov <- p_ncov$cities[[1]]
    # data may be incomplete, not contain locationID
    if ("locationId" %in% names(c_ncov)) {
      c_ncov$locationId <- NULL
    }
    # missing some vars
    c_var <- names(c_ncov)
    if (!"cityName" %in% c_var) {
      stop("City name must be listed in the cities ncov")
    }
    count_names <- c(
      "currentConfirmedCount", "confirmedCount",
      "suspectedCount", "curedCount", "deadCount"
    )
    lack_var <- setdiff(count_names, c_var)
    if (length(lack_var) > 0) {
      for (v in lack_var) {
        c_ncov[[v]] <- NA
      }
    }
    if (!"cityEnglishName" %in% names(c_ncov)) {
      city_name_map <- system.file(
        "china_city_list.csv",
        package = "ncovmap") %>%
        readr::read_csv()

      index <-
        match(c_ncov$cityName, city_name_map$City, nomatch = 0) +
        match(c_ncov$cityName, city_name_map$City_Admaster, nomatch = 0)

      c_ncov$cityEnglishName <- purrr::map_chr(
        index,
        function(x) ifelse(x == 0, NA, city_name_map$City_EN[x])
      )
    }

    p_ncov$cities <- NULL
    p_var <- setdiff(
      names(p_ncov),
      names(c_ncov)
    )

    for (v in p_var) {
      c_ncov[[v]] <- p_ncov[[v]]
    }

    res <- dplyr::bind_rows(p_ncov, c_ncov)
  }

  res
}

#' Show ncov
#' @export
print.ncov <- function(x) {
  type <- attr(x, "type")
  cat(type, "COVID 2019 Data\n")

  if (type == "All") {
    overall <- attr(x, "overall")
    update_time <- as.character(overall$updateTime[1])
  } else {
    update_time <- as.character(x$updateTime[1])
  }

  cat("Updated at", update_time, "\n")
  cat("From", attr(x, "from"))
}

#' Subset ncov data
#'
#' Subset world, china, province, and other countries ncov data
#'
#' @param x ncov data, return from `get_ncov()`
#' @param i word
#'
#' @export
`[.ncov` <- function(x, i, j, latest = TRUE) {
  if (length(i) == 1) {
    if (i %in% c("world", "World")) {
      res <- subset_world_ncov(x, latest = latest)
      type <-  "World"
    } else if (i %in% c("China", "china")) {
      res <- subset_china_ncov(x, latest)
      type <- "China"
    } else {
      res <- subset_province_ncov(x, i, latest)
      type <- res$provinceEnglishName[1]
    }
  } else {
    res <- purrr::map_df(
      i,
      ~ subset_province_ncov(ncov, .x, latest)
    )
    type <- paste(unique(res$provinceEnglishName), collapse = ", ")
  }

  res <- res[, j, drop = FALSE]

  structure(
    res,
    class = c("ncov", "data.frame"),
    type = type,
    from = attr(x, "from")
  )
}

#' Subset china ncov
#' @noRd
subset_china_ncov <- function(ncov, latest = TRUE) {
  china_ncov <- dplyr::filter(
    ncov,
    countryEnglishName == "China"
  )

  if (latest) {
    china_ncov <- dplyr::group_by(china_ncov, provinceName) %>%
      dplyr::group_modify(~ head(.x, 1L)) %>%
      dplyr::ungroup()
  }
  china_ncov <- dplyr::select(
    china_ncov,
    dplyr::starts_with("province"),
    currentConfirmedCount:updateTime
  )

  china_ncov
}

#' Subset province ncov, as well as foreign country
#' @noRd
subset_province_ncov <- function(ncov, i, latest = TRUE) {
  province_ncov <- dplyr::filter(
    ncov,
    provinceName == i | provinceEnglishName == i | provinceShortName == i,
  )


  if (latest) {
    province_ncov <- dplyr::filter(
      province_ncov,
      !is.na(cityName),
      updateTime == max(province_ncov$updateTime)
    )
  }

  province_ncov <- dplyr::select(
    province_ncov,
    dplyr::starts_with(c("city", "province")),
    currentConfirmedCount:updateTime
  )

  province_ncov
}

#' Subset world ncov
#' @noRd
subset_world_ncov <- function(ncov, latest = TRUE) {
  # ncov in other countries except china
  other_ncov <- dplyr::filter(
    ncov,
    countryEnglishName != "China"
  )

  countries <- system.file("countries_list.csv", package = "ncovr") %>%
    readr::read_csv()
  china_zh <- countries$countryName[countries$countryEnglishName == "China"]
  china_ncov <- attr(ncov, "overall") %>%
    dplyr::select(currentConfirmedCount:deadCount)
  china_ncov$countryName <- china_zh
  china_ncov$countryEnglishName <- "China"

  world_ncov <- dplyr::bind_rows(china_ncov, other_ncov)

  if (latest) {
    world_ncov <- dplyr::group_by(world_ncov, countryEnglishName) %>%
      dplyr::group_modify(~ head(.x, 1L)) %>%
      dplyr::ungroup()
  }

  world_ncov <- dplyr::select(
    world_ncov,
    dplyr::starts_with("country"),
    currentConfirmedCount:deadCount,
    updateTime
  )

  world_ncov
}
