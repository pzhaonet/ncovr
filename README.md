
# ncovr: Read and process nCoV data 新型冠状病毒数据获取和可视化

这是一个 R 语言包，使用教程详见 <https://openr.pzhao.org/zh/tags/ncovr/>。这里是个简介。

ncovr 包是方便 R 用户获取新型冠状病毒（2019-nCoV）数据而开发的，后续增添了数据处理、建模、可视化等功能。

## 数据获取途径

ncovr 包获取数据的主要途径是 [BlankerL/DXY-2019-nCoV-Crawler](https://github.com/BlankerL/DXY-2019-nCoV-Crawler)。这个项目提供了 api 接口和 csv 文件。为了减轻 api 的流量压力， ncovr 每天将每天自动从这个 api 读一次数据，保存成 R 语言直接读取的 .RDS 格式，方便 R 语言用户调用。详见下面的示例。

## 安装

1. 安装 R。在 [CRAN](http://cran.r-project.org) 上选择适合你操作系统的安装包来安装。

2. 安装 remotes 包：`install.packages('remotes')`

3. 安装 ncovr 包：`remotes::install_github('pzhaonet/ncovr')`

## 获取数据

```r
Sys.setlocale('LC_CTYPE', 'Chinese') # windows 用户设置中文环境
require("ncovr")
get_ncov() # 读取 RDS数据（推荐）
get_ncov(method = 'csv') # 从 csv 文件读取（推荐）
get_ncov(method = 'api') # 从 api 接口读取
```

更多功能请参看函数的帮助信息

# License

Copyright [Peng Zhao](http://pzhao.org).

Released under the GPL-3 license.
