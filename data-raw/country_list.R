library(dplyr)

# download country data from naturalearchdata
download.file(
  "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
  "data-raw/country_list.zip"
)
unzip("data-raw/country_list.zip", exdir = "data-raw/country_list")
countries <- sf::read_sf("data-raw/country_list/ne_50m_admin_0_countries.shp") %>%
  select(NAME, NAME_ZH)

# correct countries names to inconsistent with data from
# `rnaturalearth::ne_countries(type = "medium")`
countries_corrected <- dplyr::transmute(
  countries,
  countryEnglishName = dplyr::case_when(
    NAME ==  "United States of America" ~ "United States",
    NAME == "Cabo Verde" ~ "Cape Verde",
    NAME == "Åland" ~ "Aland",
    NAME == "Czechia" ~ "Czech Rep.",
    NAME == "South Korea" ~ "Korea",
    NAME == "Laos" ~ "Lao PDR",
    NAME == "North Korea" ~ "Dem. Rep. Korea",
    NAME == "eSwatini" ~ "Swaziland",
    NAME == "S. Geo. and the Is." ~ "S. Geo. and S. Sandw. Is.",
    TRUE ~ NAME
  ),
  countryName = NAME_ZH
)

# 中华人民共和国 to 中国, remove taiwan
countries_corrected <- mutate(
  countries_corrected,
  countryName = ifelse(countryEnglishName == "China", "中国", countryName)
) %>%
  filter(countryEnglishName != "Taiwan")
readr::write_csv(
  countries_corrected[, c("countryName", "countryEnglishName"), drop = TRUE],
  "inst/countries_list.csv"
)
unlink(c("data-raw/country_list/", "data-raw/country_list.zip"), recursive = TRUE)
