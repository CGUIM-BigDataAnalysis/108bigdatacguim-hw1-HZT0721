library(jsonlite)
library(dplyr)
library(readr)


salary_107_edu <- read_csv("C:/Users/admin/Desktop/salary data/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
salary_104_edu <- read_csv("C:/Users/admin/Desktop/salary data/a17000000j-020066-mah.csv")
salary_104_edu$大職業別<-salary_107_edu$大職業別



salary_edu_join<-inner_join(salary_104_edu,salary_107_edu,by="大職業別")
salary_compare_uni<-salary_edu_join[,c(2,11,24)]

colnames(salary_compare_uni) <- c("大職業別", "104大學薪資","107大學薪資")

salary_compare_uni<-salary_compare_uni[(-grep("—",salary_compare_uni$`104大學薪資`)),]
salary_compare_uni<-salary_compare_uni[(-grep("—",salary_compare_uni$`107大學薪資`)),]
salary_compare_uni<-salary_compare_uni[(-grep("…",salary_compare_uni$`107大學薪資`)),]

salary_compare_uni$`104大學薪資`<-as.numeric(salary_compare_uni$`104大學薪資`)
salary_compare_uni$`107大學薪資`<-as.numeric(salary_compare_uni$`107大學薪資`)

salary_compare_uni$`薪資漲幅d`<-round((salary_compare_uni$`107大學薪資`/salary_compare_uni$`104大學薪資`)-1,digits=5)
salary_compare_uni<-salary_compare_uni[order(salary_compare_uni$薪資漲幅d,decreasing=T),]
library('scales')#for percent()
salary_compare_uni$`薪資漲幅`<-salary_compare_uni$`薪資漲幅d`%>%percent(accuracy =0.01)



head(salary_compare_uni[salary_compare_uni$薪資漲幅d>0,c(1,2,3,5)],10)
salary_compare_uni[salary_compare_uni$薪資漲幅d>0.05,c(1,2,3,5)]

job_increase_5percent<-salary_compare_uni[salary_compare_uni$薪資漲幅d>0.05,]

get_jobcat<-function(x){
  
  return(strsplit(job_increase_5percent$大職業別,"-")[[x]][1])
}
library(purrr)
map(c(1:52),get_jobcat)%>%unlist()%>%table()%>%sort(decreasing = T)


salary_FM_104<-salary_104_edu[,c(1,2,12)]
salary_FM_104<-salary_FM_104[(-grep("—",salary_FM_104$`大學-女/男`)),]
salary_FM_104<-salary_FM_104[(-grep("…",salary_FM_104$`大學-女/男`)),]
salary_FM_104$`大學-女/男`<-as.numeric(salary_FM_104$`大學-女/男`)

salary_FM_107<-salary_107_edu[,c(1,2,12)]
salary_FM_107<-salary_FM_107[(-grep("—",salary_FM_107$`大學-女/男`)),]
salary_FM_107<-salary_FM_107[(-grep("…",salary_FM_107$`大學-女/男`)),]
salary_FM_107$`大學-女/男`<-as.numeric(salary_FM_107$`大學-女/男`)


salary_FM_104_F<-salary_FM_104[order(salary_FM_104$`大學-女/男`,decreasing = T),]
salary_FM_104_F[salary_FM_104_F$`大學-女/男`>100,]$大職業別%>%head()

salary_FM_104_M<-salary_FM_104[order(salary_FM_104$`大學-女/男`),]
salary_FM_104_M[salary_FM_104_M$`大學-女/男`<100,]$`大職業別`%>%head(10)


salary_FM_107_F<-salary_FM_107[order(salary_FM_107$`大學-女/男`,decreasing = T),]
salary_FM_107_F[salary_FM_107_F$`大學-女/男`>100,]$大職業別%>%head()

salary_FM_107_M<-salary_FM_107[order(salary_FM_107$`大學-女/男`),]
salary_FM_107_M[salary_FM_107_M$`大學-女/男`<100,]$大職業別%>%head(10)

salary_107_BS_MS<-salary_107_edu[,c(1,2,11,13)]
salary_107_BS_MS<-salary_107_BS_MS[(-grep("—",salary_107_BS_MS$`大學-薪資`)),]
salary_107_BS_MS<-salary_107_BS_MS[(-grep("—",salary_107_BS_MS$`研究所-薪資`)),]
salary_107_BS_MS<-salary_107_BS_MS[(-grep("…",salary_107_BS_MS$`大學-薪資`)),]
salary_107_BS_MS$`大學-薪資`<-as.numeric(salary_107_BS_MS$`大學-薪資`)
salary_107_BS_MS$`研究所-薪資`<-as.numeric(salary_107_BS_MS$`研究所-薪資`)
salary_107_BS_MS$薪資漲幅<-round((salary_107_BS_MS$`研究所-薪資`/salary_107_BS_MS$`大學-薪資`),digits=3)
salary_107_BS_MS[order(salary_107_BS_MS$薪資漲幅,decreasing = T),]%>%head(10)

salary_107_BS_MS$薪資差額<-salary_107_BS_MS$`研究所-薪資`-salary_107_BS_MS$`大學-薪資`
salary_107_BS_MS[order(salary_107_BS_MS$薪資差額,decreasing = T),]%>%head(10)





