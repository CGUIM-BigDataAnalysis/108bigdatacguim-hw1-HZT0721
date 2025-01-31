108-2 大數據分析方法 作業一
================
TZU-TAO,HUANG

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**-
（107年）<https://data.gov.tw/dataset/6647>
（104-105年）<http://ipgod.nchc.org.tw/dataset/a17000000j-020066>
，可初步了解台灣近幾年各行各業、各學歷的起薪。

## 比較104年度和107年度大學畢業者的薪資資料

### 資料匯入與處理

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(knitr)
library(scales)#for percent()
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
#資料匯入
salary_107_edu <- read_csv("C:/Users/admin/Desktop/salary data/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_character(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所-薪資` = col_character(),
    ##   `研究所-女/男` = col_character()
    ## )

``` r
salary_104_edu <- read_csv("C:/Users/admin/Desktop/salary data/a17000000j-020066-mah.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#比對後發現職業別雖文字不同,但所指職業相同 直接將104資料取代為與107相同
salary_104_edu$`大職業別`<-salary_107_edu$`大職業別`
#join 104與107的資料表by職業別
salary_edu_join<-inner_join(salary_104_edu,salary_107_edu,by="大職業別")
#選出分析所需欄位
salary_compare_uni<-salary_edu_join[,c(2,11,24)]

#重新命名欄位
colnames(salary_compare_uni) <- c("大職業別", "104大學薪資","107大學薪資")

#資料清洗
salary_compare_uni<-salary_compare_uni[(-grep("—",salary_compare_uni$`104大學薪資`)),]
salary_compare_uni<-salary_compare_uni[(-grep("—",salary_compare_uni$`107大學薪資`)),]
salary_compare_uni<-salary_compare_uni[(-grep("…",salary_compare_uni$`107大學薪資`)),]

salary_compare_uni$`104大學薪資`<-as.numeric(salary_compare_uni$`104大學薪資`)
salary_compare_uni$`107大學薪資`<-as.numeric(salary_compare_uni$`107大學薪資`)

#新增計算薪資漲幅欄位
salary_compare_uni$`薪資漲幅d`<-round((salary_compare_uni$`107大學薪資`/salary_compare_uni$`104大學薪資`),digits=3)-1
salary_compare_uni$`薪資漲幅`<-salary_compare_uni$`薪資漲幅d`%>%percent(accuracy = 0.1)
```

### 107年度薪資較104年度薪資高的職業有哪些?

``` r
salary_compare_uni<-salary_compare_uni[order(salary_compare_uni$`薪資漲幅d`,decreasing=T),]

head(salary_compare_uni[salary_compare_uni$`薪資漲幅d`>0,c(1,2,3,5)],10)%>%kable()
```

| 大職業別                   | 104大學薪資 | 107大學薪資 | 薪資漲幅  |
| :--------------------- | :-----: | :-----: | :---- |
| 專業\_科學及技術服務業-服務及銷售工作人員 |  24423  |  28304  | 15.9% |
| 教育業-服務及銷售工作人員          |  24324  |  27543  | 13.2% |
| 不動產業-技術員及助理專業人員        |  27338  |  30914  | 13.1% |
| 住宿及餐飲業-服務及銷售工作人員       |  23548  |  26111  | 10.9% |
| 藝術\_娛樂及休閒服務業-事務支援人員    |  24244  |  26611  | 9.8%  |
| 金融及保險業-技藝\_機械設備操作及組裝人員 |  30074  |  32997  | 9.7%  |
| 教育業                    |  25162  |  27582  | 9.6%  |
| 教育業-事務支援人員             |  23643  |  25908  | 9.6%  |
| 用水供應及污染整治業-專業人員        |  32179  |  34957  | 8.6%  |
| 教育業-專業人員               |  26615  |  28911  | 8.6%  |

#### 說明

上表是前十名107比104薪資高的職業

可以從表中看到照漲幅排序的職業並沒有集中在特定產業

但其中可看到教育業在前十名中出現了四次或許可能與107的軍公教調薪3%有關

(行政院107年1月31日院授人給字第10700000011號函)

### 提高超過5%的的職業有哪些?

``` r
#nrow(salary_compare_uni[salary_compare_uni$`薪資漲幅`>1.05,])
#共52筆漲幅大於5% (在140筆資料中)

salary_compare_uni[salary_compare_uni$`薪資漲幅d`>0.05,c(1,2,3,5)]%>%kable()
```

| 大職業別                         | 104大學薪資 | 107大學薪資 | 薪資漲幅  |
| :--------------------------- | :-----: | :-----: | :---- |
| 專業\_科學及技術服務業-服務及銷售工作人員       |  24423  |  28304  | 15.9% |
| 教育業-服務及銷售工作人員                |  24324  |  27543  | 13.2% |
| 不動產業-技術員及助理專業人員              |  27338  |  30914  | 13.1% |
| 住宿及餐飲業-服務及銷售工作人員             |  23548  |  26111  | 10.9% |
| 藝術\_娛樂及休閒服務業-事務支援人員          |  24244  |  26611  | 9.8%  |
| 金融及保險業-技藝\_機械設備操作及組裝人員       |  30074  |  32997  | 9.7%  |
| 教育業                          |  25162  |  27582  | 9.6%  |
| 教育業-事務支援人員                   |  23643  |  25908  | 9.6%  |
| 用水供應及污染整治業-專業人員              |  32179  |  34957  | 8.6%  |
| 教育業-專業人員                     |  26615  |  28911  | 8.6%  |
| 出版、影音製作、傳播及資通訊服務業-專業人員       |  29352  |  31763  | 8.2%  |
| 住宿及餐飲業                       |  25167  |  27213  | 8.1%  |
| 用水供應及污染整治業-技藝\_機械設備操作及組裝人員   |  27887  |  29909  | 7.3%  |
| 營建工程-服務及銷售工作人員               |  26782  |  28707  | 7.2%  |
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員     |  28039  |  30031  | 7.1%  |
| 住宿及餐飲業-事務支援人員                |  24786  |  26538  | 7.1%  |
| 運輸及倉儲業-技術員及助理專業人員            |  29670  |  31747  | 7.0%  |
| 服務業-服務及銷售工作人員                |  24812  |  26527  | 6.9%  |
| 金融及保險業-技術員及助理專業人員            |  30495  |  32602  | 6.9%  |
| 金融及保險業-服務及銷售工作人員             |  28213  |  30134  | 6.8%  |
| 工業及服務業-服務及銷售工作人員             |  25831  |  27551  | 6.7%  |
| 出版、影音製作、傳播及資通訊服務業-事務支援人員     |  25728  |  27464  | 6.7%  |
| 運輸及倉儲業-技藝\_機械設備操作及組裝人員       |  29168  |  31085  | 6.6%  |
| 不動產業                         |  27657  |  29494  | 6.6%  |
| 不動產業-技藝\_機械設備操作及組裝人員         |  25204  |  26802  | 6.3%  |
| 住宿及餐飲業-技藝\_機械設備操作及組裝人員       |  26276  |  27913  | 6.2%  |
| 不動產業-服務及銷售工作人員               |  25806  |  27409  | 6.2%  |
| 營建工程-專業人員                    |  31014  |  32897  | 6.1%  |
| 出版、影音製作、傳播及資通訊服務業            |  27478  |  29143  | 6.1%  |
| 支援服務業-服務及銷售工作人員              |  24810  |  26321  | 6.1%  |
| 運輸及倉儲業-事務支援人員                |  26079  |  27650  | 6.0%  |
| 藝術\_娛樂及休閒服務業                 |  25615  |  27147  | 6.0%  |
| 營建工程-事務支援人員                  |  25281  |  26721  | 5.7%  |
| 醫療保健業-服務及銷售工作人員              |  25106  |  26549  | 5.7%  |
| 藝術\_娛樂及休閒服務業-專業人員            |  28351  |  29955  | 5.7%  |
| 其他服務業-服務及銷售工作人員              |  22179  |  23443  | 5.7%  |
| 運輸及倉儲業                       |  28404  |  30002  | 5.6%  |
| 藝術\_娛樂及休閒服務業-技藝\_機械設備操作及組裝人員 |  25739  |  27180  | 5.6%  |
| 製造業-技術員及助理專業人員               |  27247  |  28746  | 5.5%  |
| 運輸及倉儲業-服務及銷售工作人員             |  27757  |  29262  | 5.4%  |
| 金融及保險業                       |  30787  |  32448  | 5.4%  |
| 工業-技術員及助理專業人員                |  27429  |  28894  | 5.3%  |
| 製造業-專業人員                     |  30377  |  31972  | 5.3%  |
| 批發及零售業-服務及銷售工作人員             |  24329  |  25614  | 5.3%  |
| 金融及保險業-專業人員                  |  33706  |  35490  | 5.3%  |
| 不動產業-事務支援人員                  |  25632  |  27001  | 5.3%  |
| 工業-專業人員                      |  30546  |  32145  | 5.2%  |
| 用水供應及污染整治業                   |  28464  |  29943  | 5.2%  |
| 出版、影音製作、傳播及資通訊服務業-技術員及助理專業人員 |  27470  |  28910  | 5.2%  |
| 出版、影音製作、傳播及資通訊服務業-服務及銷售工作人員  |  26210  |  27586  | 5.2%  |
| 營建工程                         |  27748  |  29154  | 5.1%  |
| 藝術\_娛樂及休閒服務業-技術員及助理專業人員      |  26523  |  27887  | 5.1%  |
| 工業-技藝\_機械設備操作及組裝人員           |  25890  |  27193  | 5.0%  |

#### 說明

在140筆資料中 共有52個職業薪資漲幅大於5% (約為37%)

### 主要的職業種別是哪些種類呢?

``` r
#取得漲幅大於5%的資料
job_increase_5percent<-salary_compare_uni[salary_compare_uni$`薪資漲幅d`>0.05,]

#宣告一個用來取的strsplit後list中每個詞的第一個元素
get_jobcat<-function(x){
  
  return(strsplit(job_increase_5percent$`大職業別`,"-")[[x]][1])
}
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:scales':
    ## 
    ##     discard

``` r
library(knitr)

#利用上面宣告的方法取得list後unlist成vector做table找各產業出現次數
map(c(1:52),get_jobcat)%>%unlist()%>%table()%>%sort(decreasing = T)%>%kable()
```

| .                 | Freq |
| :---------------- | ---: |
| 不動產業              |    5 |
| 出版、影音製作、傳播及資通訊服務業 |    5 |
| 金融及保險業            |    5 |
| 運輸及倉儲業            |    5 |
| 藝術\_娛樂及休閒服務業      |    5 |
| 住宿及餐飲業            |    4 |
| 教育業               |    4 |
| 營建工程              |    4 |
| 用水供應及污染整治業        |    3 |
| 工業                |    2 |
| 製造業               |    2 |
| 工業及服務業            |    1 |
| 支援服務業             |    1 |
| 批發及零售業            |    1 |
| 其他服務業             |    1 |
| 服務業               |    1 |
| 專業\_科學及技術服務業      |    1 |
| 電力及燃氣供應業          |    1 |
| 醫療保健業             |    1 |

#### 說明

在52個漲幅大於5%職業中 前五名的產業都出現5次

## 男女同工不同酬現況分析

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 資料處理

``` r
#選擇所需欄位與資料清洗

salary_FM_104<-salary_104_edu[,c(1,2,12)]
salary_FM_104<-salary_FM_104[(-grep("—",salary_FM_104$`大學-女/男`)),]
salary_FM_104<-salary_FM_104[(-grep("…",salary_FM_104$`大學-女/男`)),]
salary_FM_104$`大學-女/男`<-as.numeric(salary_FM_104$`大學-女/男`)


salary_FM_107<-salary_107_edu[,c(1,2,12)]
salary_FM_107<-salary_FM_107[(-grep("—",salary_FM_107$`大學-女/男`)),]
salary_FM_107<-salary_FM_107[(-grep("…",salary_FM_107$`大學-女/男`)),]
salary_FM_107$`大學-女/男`<-as.numeric(salary_FM_107$`大學-女/男`)
```

### 104和107年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#104 M>F
salary_FM_104_M<-salary_FM_104[order(salary_FM_104$`大學-女/男`),]
salary_FM_104_M[salary_FM_104_M$`大學-女/男`<100,]$`大職業別`%>%head(10)%>%kable()
```

| x                          |
| :------------------------- |
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員   |
| 教育業-服務及銷售工作人員              |
| 礦業及土石採取業-技術員及助理專業人員        |
| 礦業及土石採取業-技藝\_機械設備操作及組裝人員   |
| 礦業及土石採取業                   |
| 其他服務業-事務支援人員               |
| 營建工程-技藝\_機械設備操作及組裝人員       |
| 用水供應及污染整治業-技藝\_機械設備操作及組裝人員 |
| 營建工程                       |
| 教育業                        |

``` r
#107 M>F
salary_FM_107_M<-salary_FM_107[order(salary_FM_107$`大學-女/男`),]
salary_FM_107_M[salary_FM_107_M$`大學-女/男`<100,]$`大職業別`%>%head(10)%>%kable()
```

| x                    |
| :------------------- |
| 礦業及土石採取業-專業人員        |
| 電力及燃氣供應業-事務支援人員      |
| 礦業及土石採取業             |
| 營建工程                 |
| 教育業-事務支援人員           |
| 電力及燃氣供應業-服務及銷售工作人員   |
| 營建工程-技術員及助理專業人員      |
| 用水供應及污染整治業-專業人員      |
| 用水供應及污染整治業-服務及銷售工作人員 |
| 電力及燃氣供應業-專業人員        |

#### 說明

在上面104和107的資料中 前十名男生薪資高於女生的職業

可以看到大部分為營建、礦業、水供應及電力等普遍認為需要更多勞力的工作

### 哪些行業女生薪資比男生薪資多?

``` r
#104 F>M
salary_FM_104_F<-salary_FM_104[order(salary_FM_104$`大學-女/男`,decreasing = T),]
salary_FM_104_F[salary_FM_104_F$`大學-女/男`>100,]$`大職業別`%>%head()%>%kable()
```

| x                            |
| :--------------------------- |
| 專業\_科學及技術服務業-技藝\_機械設備操作及組裝人員 |

``` r
#107 F>M
salary_FM_107_F<-salary_FM_107[order(salary_FM_107$`大學-女/男`,decreasing = T),]
salary_FM_107_F[salary_FM_107_F$`大學-女/男`>100,]$`大職業別`%>%head()%>%kable()
```

| x |
| :- |

#### 說明

這邊可以看到在104的資料中僅有一筆女生薪資高於男生的職業

而在107甚至是不進反退 一個都沒有 可見男女同工不同酬的議題仍然嚴重

## 研究所薪資差異

以107年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#選擇所需欄位
salary_107_BS_MS<-salary_107_edu[,c(1,2,11,13)]
#資料清洗與處理
salary_107_BS_MS<-salary_107_BS_MS[(-grep("—",salary_107_BS_MS$`大學-薪資`)),]
salary_107_BS_MS<-salary_107_BS_MS[(-grep("—",salary_107_BS_MS$`研究所-薪資`)),]
salary_107_BS_MS<-salary_107_BS_MS[(-grep("…",salary_107_BS_MS$`大學-薪資`)),]
salary_107_BS_MS$`大學-薪資`<-as.numeric(salary_107_BS_MS$`大學-薪資`)
salary_107_BS_MS$`研究所-薪資`<-as.numeric(salary_107_BS_MS$`研究所-薪資`)

#新增增加比例"數值"欄位
salary_107_BS_MS$`增加比例d`<-round((salary_107_BS_MS$`研究所-薪資`/salary_107_BS_MS$`大學-薪資`),digits=3)-1
#新增增加比例"%數"欄位
salary_107_BS_MS$`增加比例`<-salary_107_BS_MS$`增加比例d`%>%percent(accuracy = 0.1)
#新增增加比例"排名"欄位
salary_107_BS_MS$`增加比例排名`<-rank(-salary_107_BS_MS$`增加比例d`,ties.method ="min")

#新增薪資差額欄位
salary_107_BS_MS$`薪資差額`<-salary_107_BS_MS$`研究所-薪資`-salary_107_BS_MS$`大學-薪資`
#新增薪資差額"排名"欄位
salary_107_BS_MS$`差額排名`<-rank(-salary_107_BS_MS$`薪資差額`,ties.method ="min")

#新增薪資排名
salary_107_BS_MS$`大學-薪資排名`<-rank(-salary_107_BS_MS$`大學-薪資`)
salary_107_BS_MS$`研究所-薪資排名`<-rank(-salary_107_BS_MS$`研究所-薪資`)

#依增加比例排序 找出前10
salary_107_BS_MS[order(salary_107_BS_MS$`增加比例d`,decreasing = T),c(1,2,3,4,6)]%>%head(10)%>%kable()
```

|  年度  | 大職業別                   | 大學-薪資 | 研究所-薪資 | 增加比例  |
| :--: | :--------------------- | :---: | :----: | :---- |
| 2018 | 其他服務業                  | 25781 | 31909  | 23.8% |
| 2018 | 專業\_科學及技術服務業           | 29353 | 35381  | 20.5% |
| 2018 | 專業\_科學及技術服務業-專業人員      | 32460 | 39103  | 20.5% |
| 2018 | 教育業-事務支援人員             | 25908 | 30827  | 19.0% |
| 2018 | 出版、影音製作、傳播及資通訊服務業      | 29143 | 34503  | 18.4% |
| 2018 | 其他服務業-事務支援人員           | 26115 | 30830  | 18.1% |
| 2018 | 出版、影音製作、傳播及資通訊服務業-專業人員 | 31763 | 37479  | 18.0% |
| 2018 | 製造業                    | 28777 | 33916  | 17.9% |
| 2018 | 工業                     | 28850 | 33893  | 17.5% |
| 2018 | 工業及服務業                 | 28849 | 33880  | 17.4% |

#### 說明

除了其他服務業有最高的增加比例以外

科學及技術業的薪資增加比例在前三名

而資通訊服務業則在第五名和第七名

## 我有興趣的職業別薪資狀況分析

### 有興趣的職業別篩選，呈現薪資

``` r
sa<-c(2:4)

salary_107_BS_MS[grep("教育業",salary_107_BS_MS$`大職業別`),sa]%>%kable()
```

| 大職業別       | 大學-薪資 | 研究所-薪資 |
| :--------- | :---: | :----: |
| 教育業        | 27582 | 30509  |
| 教育業-專業人員   | 28911 | 30025  |
| 教育業-事務支援人員 | 25908 | 30827  |

``` r
salary_107_BS_MS[grep("資通訊服務業",salary_107_BS_MS$`大職業別`),sa]%>%kable()
```

| 大職業別                         | 大學-薪資 | 研究所-薪資 |
| :--------------------------- | :---: | :----: |
| 出版、影音製作、傳播及資通訊服務業            | 29143 | 34503  |
| 出版、影音製作、傳播及資通訊服務業-專業人員       | 31763 | 37479  |
| 出版、影音製作、傳播及資通訊服務業-技術員及助理專業人員 | 28910 | 33643  |
| 出版、影音製作、傳播及資通訊服務業-事務支援人員     | 27464 | 30950  |

``` r
salary_107_BS_MS[grep("金融及保險業",salary_107_BS_MS$`大職業別`),sa]%>%kable()
```

| 大職業別              | 大學-薪資 | 研究所-薪資 |
| :---------------- | :---: | :----: |
| 金融及保險業            | 32448 | 37228  |
| 金融及保險業-專業人員       | 35490 | 39868  |
| 金融及保險業-技術員及助理專業人員 | 32602 | 36312  |
| 金融及保險業-事務支援人員     | 30515 | 35274  |

``` r
salary_107_BS_MS[grep("製造業",salary_107_BS_MS$`大職業別`),sa]%>%kable()
```

| 大職業別           | 大學-薪資 | 研究所-薪資 |
| :------------- | :---: | :----: |
| 製造業            | 28777 | 33916  |
| 製造業-專業人員       | 31972 | 37170  |
| 製造業-技術員及助理專業人員 | 28746 | 32487  |
| 製造業-事務支援人員     | 27405 | 31033  |

``` r
salary_107_BS_MS[grep("科學及技術服務業",salary_107_BS_MS$`大職業別`),sa]%>%kable()
```

| 大職業別                    | 大學-薪資 | 研究所-薪資 |
| :---------------------- | :---: | :----: |
| 專業\_科學及技術服務業            | 29353 | 35381  |
| 專業\_科學及技術服務業-專業人員       | 32460 | 39103  |
| 專業\_科學及技術服務業-技術員及助理專業人員 | 29371 | 33601  |
| 專業\_科學及技術服務業-事務支援人員     | 26999 | 31629  |

### 這些職業別研究所薪資與大學薪資差多少呢？

#### 新增排名

``` r
#宣告所需欄位向量
sall<-c(2:3,10,4,11,6:9)

salary_107_BS_MS<-salary_107_BS_MS[order(salary_107_BS_MS$`增加比例d`,decreasing = T),]
```

## 教育業

``` r
salary_107_BS_MS[grep("教育業",salary_107_BS_MS$`大職業別`),sall]%>%kable()
```

| 大職業別       | 大學-薪資 | 大學-薪資排名 | 研究所-薪資 | 研究所-薪資排名 | 增加比例  | 增加比例排名 | 薪資差額 | 差額排名 |
| :--------- | :---: | :-----: | :----: | :------: | :---- | :----: | :--: | :--: |
| 教育業-事務支援人員 | 25908 |   77    | 30827  |    62    | 19.0% |   4    | 4919 |  14  |
| 教育業        | 27582 |   57    | 30509  |    64    | 10.6% |   56   | 2927 |  58  |
| 教育業-專業人員   | 28911 |   41    | 30025  |    68    | 3.9%  |   76   | 1114 |  77  |

在教育業中 可以看到事務支援人員的增加比例排名第4 但專業人員卻在排名76(倒數10名)

這是一個很有趣的現象 跟我想像的不一樣

我們通常會認為研究所能讓人更加鑽研一門學問

但在教育業中 研究所的學歷卻在專業人員中影響不大 卻讓事務支援人員薪資提升最多

不過就金額來看 或許30k是這個行業平均願意給到的最高金額

## 出版、影音製作、傳播及資通訊服務業

``` r
salary_107_BS_MS[grep("資通訊服務業",salary_107_BS_MS$`大職業別`),sall]%>%kable()
```

| 大職業別                         | 大學-薪資 | 大學-薪資排名 | 研究所-薪資 | 研究所-薪資排名 | 增加比例  | 增加比例排名 | 薪資差額 | 差額排名 |
| :--------------------------- | :---: | :-----: | :----: | :------: | :---- | :----: | :--: | :--: |
| 出版、影音製作、傳播及資通訊服務業            | 29143 |   39    | 34503  |    24    | 18.4% |   5    | 5360 |  5   |
| 出版、影音製作、傳播及資通訊服務業-專業人員       | 31763 |   17    | 37479  |    7     | 18.0% |   7    | 5716 |  4   |
| 出版、影音製作、傳播及資通訊服務業-技術員及助理專業人員 | 28910 |   42    | 33643  |    31    | 16.4% |   15   | 4733 |  21  |
| 出版、影音製作、傳播及資通訊服務業-事務支援人員     | 27464 |   58    | 30950  |    59    | 12.7% |   42   | 3486 |  46  |

在資通訊服務業中 整體的增加比例算是高 而增加比例依序由專業到事務支援遞減也符合我的想像

同時專業人員的研究所薪資、增加比例、差額的排名都在前面

對我來說 結果算是合乎我的觀念 並不影響我念碩班的意願

## 金融及保險業

``` r
salary_107_BS_MS[grep("金融及保險業",salary_107_BS_MS$`大職業別`),sall]%>%kable()
```

| 大職業別              | 大學-薪資 | 大學-薪資排名 | 研究所-薪資 | 研究所-薪資排名 | 增加比例  | 增加比例排名 | 薪資差額 | 差額排名 |
| :---------------- | :---: | :-----: | :----: | :------: | :---- | :----: | :--: | :--: |
| 金融及保險業-事務支援人員     | 30515 |   25    | 35274  |    20    | 15.6% |   20   | 4759 |  20  |
| 金融及保險業            | 32448 |   12    | 37228  |    10    | 14.7% |   25   | 4780 |  19  |
| 金融及保險業-專業人員       | 35490 |    1    | 39868  |    1     | 12.3% |   46   | 4378 |  27  |
| 金融及保險業-技術員及助理專業人員 | 32602 |    9    | 36312  |    15    | 11.4% |   51   | 3710 |  39  |

平常和以前同學討論時 會知道通常商院念碩班的比例較低 除了本身名額較少外 也常聽到是因為薪資差異不大

根據107資料 金融保險業的增加比例雖然不是很前面

但他的薪資相較其他產業來說本來就更高 專業人員甚至是薪資排名最高

## 製造業

``` r
salary_107_BS_MS[grep("製造業",salary_107_BS_MS$`大職業別`),sall]%>%kable()
```

| 大職業別           | 大學-薪資 | 大學-薪資排名 | 研究所-薪資 | 研究所-薪資排名 | 增加比例  | 增加比例排名 | 薪資差額 | 差額排名 |
| :------------- | :---: | :-----: | :----: | :------: | :---- | :----: | :--: | :--: |
| 製造業            | 28777 |   47    | 33916  |    26    | 17.9% |   8    | 5139 |  8   |
| 製造業-專業人員       | 31972 |   16    | 37170  |    12    | 16.3% |   18   | 5198 |  7   |
| 製造業-事務支援人員     | 27405 |   59    | 31033  |    57    | 13.2% |   35   | 3628 |  44  |
| 製造業-技術員及助理專業人員 | 28746 |   48    | 32487  |    44    | 13.0% |   37   | 3741 |  37  |

製造業整體的增加比例都有在前50%

而在薪資方面讀研究所後

在增加比例上雖然事務支援人員略多於技術員及助理專業人員 但基本上算是同一個檔次

而薪資依照專業人員、技術員及助理專業人員、事務支援人員的順序遞減

整體而言算是中規中矩 可能與製造業這個分類本身包含範圍大 在更細的產業分類中有差異 製造業算是平均下的結果

## 專業\_科學及技術服務業

``` r
salary_107_BS_MS[grep("科學及技術服務業",salary_107_BS_MS$`大職業別`),sall]%>%kable()
```

| 大職業別                    | 大學-薪資 | 大學-薪資排名 | 研究所-薪資 | 研究所-薪資排名 | 增加比例  | 增加比例排名 | 薪資差額 | 差額排名 |
| :---------------------- | :---: | :-----: | :----: | :------: | :---- | :----: | :--: | :--: |
| 專業\_科學及技術服務業            | 29353 |   36    | 35381  |    18    | 20.5% |   2    | 6028 |  3   |
| 專業\_科學及技術服務業-專業人員       | 32460 |   11    | 39103  |    3     | 20.5% |   2    | 6643 |  1   |
| 專業\_科學及技術服務業-事務支援人員     | 26999 |   68    | 31629  |    54    | 17.1% |   14   | 4630 |  23  |
| 專業\_科學及技術服務業-技術員及助理專業人員 | 29371 |   35    | 33601  |    33    | 14.4% |   26   | 4230 |  30  |

在這個產業中的專業人員 在研究所的薪資、增加比例、差額排名都在前三名 整體而言的增加比例排名也都在前面

可以說是讀碩班最有“錢途”的職業了
