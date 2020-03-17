# correct ncov cities, create city_referece as internal data

library(magrittr)
# library(dplyr)
library(purrr)
library(leafletCN)

china_cities <- readr::read_csv("inst/china_city_list.csv")
# # data correction
# china_cities <- dplyr::mutate(china_cities,
#                               Province_EN = dplyr::case_when(
#                                 Province_EN == "anhui" ~ "Anhui",
#                                 Province_EN == "guizhou" ~ "Guizhou",
#                                 Province_EN == "hubei" ~ "Hubei",
#                                 Province_EN == "xinjiang" ~ "Xinjiang",
#                                 TRUE ~ Province_EN
#                               ))
# readr::write_csv(china_cities, "inst/china_city_list.csv", )
# usethis::use_data(china_cities, internal = TRUE, overwrite = TRUE)


# correct province name
province_map <- leafletCN::regionNames("china")
ncov <- ncovr::get_ncov2()
# ncov <- readr::read_csv("https://github.com/yiluheihei/nCoV-2019-Data/raw/master/ncov_area.csv")
# ncov province
province_ncov <- ncov$provinceShortName
province_need_correct <- setdiff(province_map, province_ncov)
# province[match(province_need_correct, province_map)] <- c("台湾", "香港", "澳门")

# cities does not need info: taiwan, xianggang, aomen
province_has_cities <- setdiff(province_map, c("台湾", "香港", "澳门", "南海诸岛"))
cities_list <- purrr::map(province_has_cities, regionNames)
names(cities_list) <- province_has_cities
province_cities <- purrr::map2(
  cities_list, province_has_cities,
  ~ data.frame(Province = .y, City_Admaster = .x)
) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Province = substr(Province, 1, 2)) %>%
  dplyr::mutate(Province = dplyr::case_when(
    Province == "黑龙" ~ "黑龙江",
    Province == "内蒙" ~ "内蒙古",
    TRUE ~ Province
  ))
province_cities$Province_EN <- china_cities$Province_EN[
  match(province_cities$Province, china_cities$Province)]

## correct city name

#' extract province cities in ncov
#'
#' @param ncov ncov data
#' @param p province name
get_province_cities <- function(ncov, p) {
  province_dat <- dplyr::filter(ncov, provinceShortName == p)
  cities <- province_dat$cityName %>%
    unique() %>%
    na.omit()
}
# ncov cities
ncov_cities_list <- purrr::map(
  province_has_cities,
  get_province_cities,
  ncov = ncov
)
names(ncov_cities_list) <- province_has_cities

#' distinguish cities between consistent and inconsistent
distinguish_cities <- function(city, ncov_city) {
  index <- purrr::map_lgl(ncov_city, ~ any(grepl(.x, city)))
  index2 <- purrr::map(ncov_city, ~ which(grepl(.x, city))) %>% unlist()
  res <- list(consistent_ncov = ncov_city[index],
              consistent = city[index2],
              inconsistent = ncov_city[!index])

  res
}
city_distg <- purrr::map2(cities_list, ncov_cities_list, distinguish_cities)

# cities is consistent
purrr::map(city_distg, ~.x$consistent) %>% lengths %>% sum



city_need_corrected <- tibble::tribble(
  ~ origin,           ~corrected,               ~Province,
  "伊犁州",           "伊犁哈萨克自治州",    "新疆维吾尔自治区",
  "昌吉州",           "昌吉回族自治州",      "新疆维吾尔自治区",
  "巴州",             "巴音郭楞蒙古自治州",  "新疆维吾尔自治区",
  "兵团第八师石河子市","石河子市",           "新疆维吾尔自治区",
  "兵团第六师五家渠市","五家渠市",           "新疆维吾尔自治区",
  "海北州",            "海北藏族自治州",       "青海省",
  "甘孜州",            "甘孜藏族自治州",       "四川省",
  "凉山州",           "凉山彝族自治州",        "四川省",
  "阿坝州",            "阿坝藏族羌族自治州",   "四川省",
  "大理州",            "大理白族自治州",       "云南省",
  "红河州",            "红河哈尼族彝族自治州", "云南省",
  "德宏州",            "德宏傣族景颇族自治州", "云南省",
  "楚雄州",            "楚雄彝族自治州",       "云南省",
  "文山州",            "文山壮族苗族自治州",   "云南省",
  "湘西自治州",        "湘西土家族苗族自治州", "湖南省",
  "韩城",              "韩城(渭南)",           "陕西省",
  "杨凌",             "杨凌(咸阳)",            "陕西省",
  "公主岭",            "(四平市)",             "吉林省",
  "梅河口",           "(通化市)",              "吉林省",
  "恩施州",            "恩施土家族苗族自治州", "湖北省",
  "黔南州",           "黔南布依族苗族自治州",  "贵州省",
  "黔东南州",          "黔东南苗族侗族自治州", "贵州省",
  "黔西南州",         "黔西南布依族苗族自治州","贵州省",
  "赣江新区",          "(新区)",               "江西省",
  "两江新区",         "(新区)",                "重庆市",
  "高新区",           "(高新区)",              "重庆市",
  "石柱县",           "石柱土家族自治县",      "重庆市",
  "彭水县",           "彭水苗族土家族自治县",  "重庆市",
  "秀山县",           "秀山土家族苗族自治县",  "重庆市",
  "万盛经开区",        "(开发区)",             "重庆市",
  "宁东",                "(宁东)",             "宁夏回族自治区"
)
city_need_corrected$type <-  "error"
city_correct <- purrr::map2(
  city_distg, province_has_cities,
  ~ data.frame(
    origin = .x$consistent_ncov,
    corrected = .x$consistent,
    Province = .y)
) %>%
  dplyr::bind_rows()
city_correct$type <-  "correct"
city_reference <- dplyr::bind_rows(city_correct, city_need_corrected)
usethis::use_data(city_reference, internal = TRUE, overwrite = TRUE)

# not used now
uncertain_dat <- tibble::tribble(
  ~city,              ~province,
  "兵团第四师",    "新疆维吾尔自治区",
  "兵团第九师",    "新疆维吾尔自治区",
  "兵团第七师",    "新疆维吾尔自治区",
  "兵团第十二师",  "新疆维吾尔自治区",
  "待明确地区",    "甘肃省",
  "待明确地区",    "云南省",
  "待明确地区",    "广东省",
  "待明确地区",    "河南省",
  "待明确地区",    "北京市",
  "外地来京人员",  "北京市",
  "待明确地区",    "天津市",
  "外地来津人员",  "天津市",
  "外地来沪人员",   "上海市"
)
