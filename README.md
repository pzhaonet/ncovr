ncovr: Read and process nCoV data 新型冠状病毒数据获取和可视化
================
2020-03-09

这是一个 R 语言包，使用教程详见 <https://openr.pzhao.org/zh/tags/ncovr/>。这里是个简介。

ncovr 包是方便 R 用户获取新型冠状病毒（2019-nCoV）数据而开发的，后续增添了数据处理、建模、可视化等功能。

## 数据获取途径

ncovr 包获取数据的主要途径是
[BlankerL/DXY-2019-nCoV-Crawler](https://github.com/BlankerL/DXY-2019-nCoV-Crawler)。这个项目提供了
api 接口和 csv 文件。为了减轻 api 的流量压力， ncovr 每天将每天自动从这个 api 读一次数据，保存成 R 语言直接读取的
.RDS 格式，方便 R 语言用户调用。详见下面的示例。

## 安装

1.  安装 R。在 [CRAN](http://cran.r-project.org) 上选择适合你操作系统的安装包来安装。

2.  安装 remotes 包：`install.packages('remotes')`

3.  安装 ncovr 包：`remotes::install_github('pzhaonet/ncovr')`

## 获取数据

``` r
# Sys.setlocale('LC_CTYPE', 'Chinese') # windows 用户设置中文环境
require("ncovr")
```

    ## Loading required package: ncovr

``` r
require("leafletCN")
```

    ## Loading required package: leafletCN

``` r
ncov <- get_ncov() # 读取 RDS数据（推荐）

# get_ncov(method = 'csv') # 从 csv 文件读取（推荐）
# get_ncov(method = 'api') # 从 api 接口读取
```

另外提供了下载数据的函数`get_ncov2()`，定义新的 ncov 类(实际也是data frame)，提供了`subset()`方法,
用于提取 ncov 子集，参数可以是省份、china或者world

``` r
ncov2 <- get_ncov2()
ncov2
```

    ## All COVID 2019 Data
    ## Updated at 2020-03-09 12:00:55 
    ## From https://lab.isaaclin.cn/nCoV/api/

``` r
# 湖北 ncov, 按市统计
hubei_ncov <- ncov2["湖北"]
hubei_ncov
```

    ## Hubei COVID 2019 Data
    ## Updated at 2020-03-09 11:42:37 
    ## From https://lab.isaaclin.cn/nCoV/api/

``` r
head(data.frame(hubei_ncov), 5)
```

    ##   cityEnglishName cityName provinceName provinceShortName provinceEnglishName
    ## 1           Wuhan     武汉       湖北省              湖北               Hubei
    ## 2         Xiaogan     孝感       湖北省              湖北               Hubei
    ## 3           Ezhou     鄂州       湖北省              湖北               Hubei
    ## 4         Suizhou     随州       湖北省              湖北               Hubei
    ## 5         Yichang     宜昌       湖北省              湖北               Hubei
    ##   currentConfirmedCount confirmedCount suspectedCount curedCount deadCount
    ## 1                 16572          49948              0      30987      2389
    ## 2                   335           3518              0       3057       126
    ## 3                   310           1394              0       1030        54
    ## 4                   159           1307              0       1104        44
    ## 5                   144            931              0        753        34
    ##            updateTime
    ## 1 2020-03-09 11:42:37
    ## 2 2020-03-09 11:42:37
    ## 3 2020-03-09 11:42:37
    ## 4 2020-03-09 11:42:37
    ## 5 2020-03-09 11:42:37

``` r
# china，按省统计
china_ncov <- ncov2["china"]
head(data.frame(china_ncov), 5)
```

    ##   provinceName provinceShortName provinceEnglishName currentConfirmedCount
    ## 1       安徽省              安徽               Anhui                     0
    ## 2         澳门              澳门               Macau                     0
    ## 3       北京市              北京             Beijing                   105
    ## 4       重庆市              重庆           Chongqing                    42
    ## 5       福建省              福建              Fujian                     0
    ##   confirmedCount suspectedCount curedCount deadCount          updateTime
    ## 1            990              0        984         6 2020-03-08 05:19:01
    ## 2             10              0         10         0 2020-03-06 10:21:01
    ## 3            428              0        315         8 2020-03-09 04:09:24
    ## 4            576              0        528         6 2020-03-09 07:13:20
    ## 5            296              0        295         1 2020-03-07 13:54:02

``` r
# world， 按国家统计
world_ncov <- ncov2["world"]
```

    ## Parsed with column specification:
    ## cols(
    ##   countryName = col_character(),
    ##   countryEnglishName = col_character()
    ## )

``` r
head(data.frame(world_ncov), 5)
```

    ##   countryEnglishName countryName currentConfirmedCount confirmedCount
    ## 1        Afghanistan      阿富汗                     4              4
    ## 2            Algeria  阿尔及利亚                    19             19
    ## 3            Andorra      安道尔                     1              1
    ## 4          Argentina      阿根廷                    11             12
    ## 5            Armenia    亚美尼亚                     1              1
    ##   suspectedCount curedCount deadCount          updateTime
    ## 1              0          0         0 2020-03-09 04:56:18
    ## 2              0          0         0 2020-03-09 04:56:18
    ## 3              0          0         0 2020-03-05 02:39:09
    ## 4              0          0         1 2020-03-09 10:07:37
    ## 5              0          0         0 2020-03-05 02:39:09

## 国家地图:省级疫情图

按省级显示

``` r
plot_map(ncov$area)
```

![](man/figures/leaflet-map-1.png)<!-- -->

``` r
# log scale
plot_map(ncov$area, scale = "log")
```

![](man/figures/leaflet-map-2.png)<!-- -->

按城市显示

``` r
plot_map(ncov$area, method = "city", scale = "log")
```

![](man/figures/map-city-1.png)<!-- -->

ggplot

``` r
library(ggplot2)
ncov$area$date <- as.Date(ncovr:::conv_time(ncov$area$updateTime))
choose_date <- "2020-02-10"
x <- ncov$area[ncov$area$date <= as.Date(choose_date), ]
x <- x[!duplicated(x$provinceName), ]
plot_ggmap(x)
```

    ## Parsed with column specification:
    ## cols(
    ##   province = col_character(),
    ##   city = col_character(),
    ##   long = col_double(),
    ##   lat = col_double()
    ## )

![](man/figures/gg-map-1.png)<!-- -->

## 世界地图:各国疫情图

``` r
ncov$area$date <- as.Date(ncovr:::conv_time(ncov$area$updateTime))
ncov$area <- ncov$area[rev(order(ncov$area$date)), ]

ncov_date <- as.character(Sys.Date())
y <- ncov$area[ncov$area$date <= as.Date(ncov_date), ]
y <- y[!duplicated(y$provinceName), ]

countryname <- data.frame(
  ncovr = c("United Kiongdom", "United States of America", 
            "New Zealand", "Kampuchea (Cambodia )"
          ),
  leafletNC = c("UnitedKingdom", "UnitedStates", 
                "NewZealand", "Cambodia"
  ), 
  stringsAsFactors = FALSE
)

x <- data.frame(
  countryEnglishName = y$countryEnglishName,
  countryName = y$countryName, 
  confirmedCount = y$confirmedCount, 
  stringsAsFactors = FALSE
)


loc <- which(x$countryEnglishName %in% countryname$ncovr)
x$countryEnglishName[loc] <- countryname$leafletNC[
  match(x$countryEnglishName[loc], countryname$ncovr)
]

x$countryEnglishName2 = x$countryEnglishName # for taiwan

x_other <- x[!is.na(x$countryEnglishName) & x$countryEnglishName != 'China', ]
x_china <- data.frame(
  countryEnglishName = 'China',
  countryName = unique(x[!is.na(x$countryEnglishName) & x$countryEnglishName == 'China', 'countryName']),
  confirmedCount = sum(x[!is.na(x$countryEnglishName) & x$countryEnglishName == 'China', 'confirmedCount']),
  countryEnglishName2 = 'China'
) 
x_taiwan <- x_china
x_taiwan$countryEnglishName2 = "Taiwan"
x <- rbind(x_other, x_china, x_taiwan)


plot_map(
  x = x, 
  key = "confirmedCount", 
  scale = "log", 
  method = 'country', 
  legend_title = paste0("Cnfrm 确诊"), 
  filter = '待明确地区'
)
```

![](man/figures/world-map-1.png)<!-- -->

## ts

``` r
x_ts <- ncov$area[, c('countryEnglishName', 'countryName', 'date', 'confirmedCount', 'curedCount', 'deadCount')] %>% 
  dplyr::group_by(countryEnglishName, date) %>% 
  dplyr::summarise(
    confirmed = max(confirmedCount), 
    cured = max(curedCount), 
    dead = max(deadCount)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(countryEnglishName) & !countryEnglishName == 'China') %>% 
  as.data.frame()
loc <- which(x_ts$countryEnglishName %in% countryname$ncovr)
x_ts$countryEnglishName[loc] <-
  countryname$leafletNC[
    match(x_ts$countryEnglishName[loc], countryname$ncovr)
  ]

plot_ts(
  x_ts, 
  area = "Italy", 
  area_col = "countryEnglishName", 
  date_col = "date", 
  ts_col = c("confirmed", "cured", "dead")
) 
```

![](man/figures/ts-1.png)<!-- -->

## 国外疫情图

### 韩国疫情图

``` r
korea_ncov <- get_foreign_ncov("韩国")
plot_foreign_map(korea_ncov, "korea")
```

    ## Parsed with column specification:
    ## cols(
    ##   name = col_character(),
    ##   name_zh = col_character(),
    ##   provinceName = col_character(),
    ##   provinceEnglishName = col_character()
    ## )

![](man/figures/korea-map-1.png)<!-- -->

### 日本疫情图

``` r
jp_ncov <- get_foreign_ncov("日本")
plot_foreign_map(jp_ncov, "japan")
```

    ## Parsed with column specification:
    ## cols(
    ##   name = col_character(),
    ##   name_zh = col_character(),
    ##   provinceName = col_character(),
    ##   provinceEnglishName = col_character()
    ## )

![](man/figures/jp-map-1.png)<!-- -->

### 伊朗疫情图

``` r
iran_ncov <- get_foreign_ncov("伊朗")
plot_foreign_map(iran_ncov, "iran")
```

    ## Parsed with column specification:
    ## cols(
    ##   name = col_character(),
    ##   name_zh = col_character(),
    ##   provinceName = col_character(),
    ##   provinceEnglishName = col_character()
    ## )

![](man/figures/iran-map-1.png)<!-- -->

### 意大利疫情图

``` r
italy_ncov <- get_foreign_ncov("意大利")
plot_foreign_map(italy_ncov, "italy")
```

    ## Parsed with column specification:
    ## cols(
    ##   name = col_character(),
    ##   name_zh = col_character(),
    ##   provinceName = col_character(),
    ##   provinceEnglishName = col_character()
    ## )

![](man/figures/italy-map-1.png)<!-- -->

### 直接画这四个国家的疫情图

``` r
foreign_countries <- c("韩国", "伊朗", "日本", "意大利")
names(foreign_countries) <- c("korea", "iran", "japan", "italy")
htmltools::tagList(purrr::imap(
  foreign_countries, 
  ~ get_foreign_ncov(.x) %>% 
    plot_foreign_map(.y)
))
```

更多功能请参看函数的帮助信息

# License

Copyright [Peng Zhao](http://pzhao.org).

Released under the GPL-3 license.
