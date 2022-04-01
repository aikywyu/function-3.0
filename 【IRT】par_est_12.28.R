### convert data to IRT analysis
### 20.12.01-04

rm(list = ls())
########################################################################################
path0 <- "J:/【2】【一纲多卷】/【3】代码讲解/【4】IRT代码演示/【0】原始文件"
path1 <- "J:/【2】【一纲多卷】/【3】代码讲解/【4】IRT代码演示/【1】结果文件"
setwd(path0)
########################################################################################

library(openxlsx)
library(mirt)
library(purrr)


# Variables to be modified
dq_all <- c("广东")
# wl_all <- c("w")
# dq_all <- c("广东","河北","重庆","陕西","四川","I卷","II卷")
wl_all <- c("w","l")


### Two level scoring level conversion function
class_tra_2 <- function(x){
  if(x == 0){
    x <- x
  }else{
    x <- 1
  }
}

### Multi level scoring level conversion function
class_tra_m <- function(x){
  if(x > 0 & x <= 2){
    x <- 1
  }else if(x > 2 & x <= 4){
    x <- 2
  }else if(x > 4 & x <= 6){
    x <- 3
  }else if(x > 6 & x <= 8){
    x <- 4
  }else if(x > 8 & x <= 10){
    x <- 5
  }else if(x > 10 & x <= 12){
    x <- 6
  }else{
    x <- x
  }
}

### Data level conversion
class_tra <- function(dq,wl){
  dat <- dat_raw[,-1]
  # dat <- dat[order(dat[,ncol(dat)], decreasing = T),]
  lim_2 <- 1:16; lim_m <- 17:22
  for (i in lim_2) {
    dat[,i] <- map_dbl(dat[,i], class_tra_2)
  }
  
  for (i in lim_m) {
    dat[,i] <- map_dbl(dat[,i], class_tra_m)
  }
  # write.xlsx(dat, file = sprintf("【%s】22题样本数据-%s_等级转换.xlsx",dq,wl))
  return(dat)
}

### Calculate item parameters and information
IRT <- function(dat, lim, lim_2, lim_m, itm_m, dq, wl){
  
  dat <- dat[,lim]
  mod <- mirt(dat, model = 1,itemtype = "gpcm")                ## parameter estimation
  itm_par <- coef(mod, IRTpars = TRUE)                           ## item parameters
  theta <- sort(fscores(mod, method = "ML", max_theta = 4))  ## person parameters
  theta[which(theta == Inf)] <- 4

  # Save the required parameters into a matrix
  para_2 <- matrix(data=NA, nrow = length(lim_2), ncol=2)
  colnames(para_2) <- c("区分度", "difficulty")
  for (i in lim_2) {
    para_2[i,1] <- round(itm_par[[i]][1],2)
    para_2[i,2] <- round(itm_par[[i]][2],2)
  }

  para_m <- matrix(data=NA, nrow = length(lim_m), ncol=7)
  colnames(para_m) <- c("discrimination ", "difficulty1", "difficulty2", "difficulty3", "difficulty4", "difficulty5", "difficulty6")
  for (i in 1:length(lim_m)) {
    for (j in 1:7) {
      para_m[i,j] <- round(itm_par[[i+length(lim_2)]][j],2)
    }
  }
  dif_sum <- round(rowMeans(para_m[,2:7], na.rm = T),2)
  para_m <- cbind("discrimination "=para_m[,1], "total_difficulty"=dif_sum, para_m[,2:7])
  para_2 <- as.data.frame(para_2); para_m <- as.data.frame(para_m)

  ###test_information#####################################################################
  test_info <- testinfo(mod, theta)
  # plot(theta, test_info, type = 'l', ylab = "Information", main = 'Test information')

  ### Item_infor_matrix###################################################################
  inf_m <- matrix(NA, nrow(dat), length(lim))
  for(i in 1:length(lim)) {
    extr <- extract.item(mod, i)
    inf_m[, i] <- iteminfo(extr, theta)
  }
  ### Merge data and save file
  itm_inf <- data.frame("item_information" = round(colMeans(inf_m),2))
  para_inf_2 <- cbind("item_num" = lim_2, para_2, "item_information" = itm_inf[lim_2,])
  para_inf_m <- cbind("item_num" = itm_m, para_m, "item_information" = itm_inf[lim_m,])
  inf_test_info <- cbind("ability" = theta, "test_information" = test_info)

  setwd(path1)

  write.xlsx(list("item_parameter+information_two_level"=as.data.frame(para_inf_2),
                  "item_parameter+information_multilevel"=as.data.frame(para_inf_m),
                  "ability+test_information"=as.data.frame(inf_test_info)),
             file = sprintf("%s-%s-IRTparameter.xlsx",dq,wl))

  dir.create(sprintf("%s-%s-ICC",dq,wl))
  setwd(sprintf("%s-%s-ICC",dq,wl))

  wl_1 <- wl
  wl_1 <- ifelse(wl_1 == "w","文科","理科")
  ### Item trace plot#####################################################################
  for(i in 1:length(lim)){
    if(i == 22){
      itm <- i+1
    }else{
      itm <- i
    }
    jpeg(filename = sprintf("%s-%s-%d.jpeg",dq,wl,itm),width = 1800,height = 1200,res = 200)
    print(itemplot(mod, i, type = 'trace', theta_lim = c(-4,4), main = sprintf("%s%s第%d题 ICC",dq,wl_1,itm)))
    dev.off()
  }
  
  setwd(path0)
}


for (dq in dq_all) {
  for (wl in wl_all) {
    t1 <- proc.time()
    dat_raw <- read.xlsx(sprintf("【%s】22题样本数据-%s.xlsx",dq,wl))
    dat <- class_tra(dq,wl)
    lim <- 1:22; lim_2 <- 1:16; lim_m <- 17:22; itm_m <- c(17:21,22)
    IRT(dat, lim, lim_2, lim_m, itm_m, dq, wl)
    t2 <- proc.time()
    print(sprintf("%s-%s-runningtime：%.2fs",dq,wl,(t2-t1)[3][[1]]))
  }
}

########################################################################################
