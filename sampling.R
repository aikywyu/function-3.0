#2021一纲多卷抽样
##！！！请先另存为一份总体数据之后再进行如下步骤的处理工作，确保不在对方提供的原始数据上进行操作！！！
#1. 【转分】拿到总体原始数据之后，先检查二级计分题目是否有分数，如果只是提供了选项答案，需要先进行赋分转换工作。
#2. 【总分处理】如果数据文件没有总分，则对所有题目进行加总，在数据文件中的最后一列形成名字为“total”的数据。如果数据文件中已经有加总的分数，则把该列数据名改为“total”。
#3. 【统计】统计原始数据的总人数，总分为零分的人数。P.S：如有人口学变量，可根据人口学变量的情况进一步统计。
#4. 【转存为CSV格式】另存为名字格式是“省份(拼音首字母小写)”的CSV格式的数据文件。例如，广东——gd。

#One、Reset folder path
setwd("E://sampling")

#Two、Read raw data file
options(scipen=200)#Avoid presenting the candidate number in the form of scientific counting method
gd<- read.csv(file = "gd.csv", header = TRUE, stringsAsFactors = FALSE)

#3、Sort and tier the whole data
#1.Loading the software package $$requires installing the package xxirt in advance or directly defining the following functions
#library(xxIRT)
freq <- function(x, values=NULL, rounding=NULL){
 if(is.null(values)) values <- sort(unique(x))
  rs <- data.frame(table(factor(x, levels=values, labels=values)), stringsAsFactors=F)
  colnames(rs) <- c("value", "freq")
  rs$perc <- rs$freq / sum(rs$freq)
  rs$cum_freq <- cumsum(rs$freq)
  rs$cum_perc <- cumsum(rs$perc) 
  if(!is.null(rounding)){
    rs$perc <- round(rs$perc, rounding)
    rs$cum_perc <- round(rs$cum_perc, rounding)
  }
  rs
}

#2.Divide the population into ten sub samples
cum_perc <- seq(0, 1, 0.1)

#Three.Replace each sub sample with 1-10, i.e. 0-100% population division in the report
ss <- c(1 : 10)

#4.Establish a function to divide the population into ten sub samples
ss.convert <- function(x_rs, ss, ss_cum_perc){
  x_ss <- rep(x = NA, length(x_rs))
  freq_x_rs <- freq(x_rs)
  rs_cum_perc <- freq_x_rs$cum_perc
  rs_score_point <- as.numeric(levels(freq_x_rs$value))
  for (i in 1 : length(ss)) {
    cutoff_low <- rs_score_point[min(which(rs_cum_perc > ss_cum_perc[i]))]
    cutoff_high <- rs_score_point[max(which(rs_cum_perc <= ss_cum_perc[i + 1]))]
    x_ss[x_rs >= cutoff_low & x_rs <= cutoff_high] <- ss[i]
  }
  return(x_ss)
}

#5.Add one more column of hierarchical results to the original overall data file
gd$strat <- ss.convert(x_rs = gd$total, ss = ss, ss_cum_perc = cum_perc)
write.csv(x = gd, file = "gd.csv", row.names = FALSE)

#Four、Set sampling conditions
sps <- 50000  #sample size
sps_strat <- sps / 10  #Stratified sample size
gd_sp <- gd[1 : sps, ]#Replace the original overall data to generate sample data

#Five、Sampling process
for (i in 1 : 10) {
gd_strat <- gd[which(gd$strat == i), ]
gd_strat_s <- gd_strat[sample(nrow(gd_strat), sps_strat, replace = FALSE, set.seed(1)), ]
rownum_l <- (i - 1) * sps_strat + 1
rownum_h <- sps_strat * i
gd_sp[rownum_l : rownum_h, ] <- gd_strat_s
 }
write.csv(x = gd_sp, file = "gd_sp.csv", row.names = FALSE)
