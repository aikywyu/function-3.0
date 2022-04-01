### Code of example for 【20.11.21_YGDJ_function】
### 20.11.21


###########################################################################################
rm(list = ls()) # 清除环境中的所有变量        
path0 <- "J:/【2】【一纲多卷】/【3】代码讲解/【1】数据演示"
setwd(path0) # 设置工作路径
source('J:/【2】【一纲多卷】/【3】代码讲解/【0】R代码/直上青天_函数3.0_12.24.R', encoding = 'UTF-8')
###########################################################################################

library(openxlsx)
library(writexl)
library(psychometric)


# dq_all <- c("广东","河北","重庆","陕西","四川","I卷","II卷")
dq_all <- c("广东")   # dq：地区
wl_all <- c("w","l")   # wl：文理


main <- function(dq, wl, itm){
  t1 <- proc.time()

  ifelse(dq %in% c("海南", "山东"), lim_2 <- 1:12, lim_2 <- 1:16) #!!! Number of columns of two level scoring questions
  ifelse(dq %in% c("海南", "山东"), lim_m <- 13:22, lim_m <- 17:22) #!!! Number of columns of multi-level scoring questions
  lim <- 1:22 #Total number of items
  ifelse(dq %in% c("海南", "山东"),
         itm_s <- c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,10,12,12,12,12,12),
         itm_s <- c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,12,12,12,12,12,10)) #!!! Full marks for each item
  
  ################################################################################
  
  ### 5-segment layering [0,100][0,90][88,100][94,100][98,100][0,40][40,100]
  min <- c(0,0,0.88,0.94,0.98,0,0.4) # Upper limit of each layer
  max <- c(1,0.9,1,1,1,0.4,1) # Lower limit of each layer
  # Store each layer into an excel worksheet
  dat_s1_1 <- list()
  for (i in 1:7) {
    dat_s1 <- del_dat(dat_raw, min[i], max[i]) # This function is in the function 3.0 file
    dat_s1_1[[sprintf("%d%%-%d%%",min[i]*100,max[i]*100)]] <- dat_s1 # Treat each layer as a worksheet
  }
  
  # Calculate the difficulty and discrimination of each layer respectively
  par_1 <- matrix(data = NA,nrow = length(itm_s), ncol = 15) # To create an empty matrix, you need to determine the number of rows and columns
  colnames(par_1) <- c(" ",rep("0-100%",2),rep("0-90%",2),rep("88%-100%",2),
                      rep("94%-100%",2),rep("98%-100%",2),rep("0-40%",2),rep("40%-100%",2))
  for (i in 1:7) {
    dat_part <- dat_s1_1[[i]]
    dis_hl <- dis_cal_hl(dat_part, itm_s, lim_2, lim_m) # This function is in the function 3.0 file
    dif_hl <- dif_cal_hl(dat_part, itm_s, lim_2, lim_m) # This function is in the function 3.0 file
    par_1[,i*2] <- dis_hl[,1]; par_1[,i*2+1] <- dif_hl[,1]
  }
  par_1[,1] <- c(1:21, itm)
  if(dq %in% c("海南", "山东")){par_1[,1][9:12] <- 13:16; par_1[,1][13:16] <- 9:12}
  
  par_1 <- rbind(c("item_num",rep(c("discrimination","difficulty"),7)), par_1)
  par_1 <- as.data.frame(par_1)
  
  print("5+2 completed")
  ################################################################################
  
  # 各层分组（[0,10%]、(10%-20%]、(20%-30%]……(90%,100%]） (返回列表)
  # dat_s2 <- mul_dat(dat_raw)
  # dat_s2_1 <- list()
  # for (i in 1:10) {
  #   dat_s2_1[[sprintf("%d%%-%d%%",(i-1)*10,i*10)]] <- dat_s2[[i]]
  # }
  # 
  # 
  # para_2 <- matrix(data = NA,nrow = length(itm_s), ncol = 21)
  # colnames(para_2) <- c("题号", rep(c("区分度","难度"),10))
  # for (i in 1:10) {
  #   dat_part <- dat_s2_1[[i]]
  #   dis_hl <- dis_cal(dat_part, itm_s, lim_2, lim_m)
  #   dif_hl <- dif_cal(dat_part, itm_s, lim_2, lim_m)
  #   para[,i*2] <- dis_hl[,1]; para[,i*2+1] <- dif_hl[,1]
  # }
  # 
  # para[,1] <- c(1:22, itm)
  # 
  # 
  # para <- rbind(para, c("","0-10%","","10%-20%","","20%-30%","","30%-40%","","40%-50%","",
  #                       "50%-60%","","60%-70%","","70%-80%","","80%-90%","","90%-100%",""))
  # write.xlsx(para, file = sprintf("%s-%s-各分层项目参数.xlsx",dq,wl))
  # 
  # print("各层分组已完成")
  ################################################################################
  
  ### Cumulative stratification
  ### Bottom up accumulation [0,10],[0,20]……[0,100]
  dat_s3_bt <- acc_dat_bt(dat_raw)
  # Store each layer into an excel worksheet
  dat_bt <- list()
  for (i in 1:10) {
    dat_bt[[sprintf("0-%d%%",i*10)]] <- dat_s3_bt[[i]]
  }

  
  # Calculate the difficulty and discrimination of each layer respectively
  par_bt <- matrix(data = NA,nrow = length(itm_s), ncol = 21)
  colnames(par_bt) <- c(" ",rep("0-10%",2),rep("0-20%",2),rep("0-30%",2),
                      rep("0-40%",2),rep("0-50%",2),rep("0-60%",2),
                      rep("0-70%",2),rep("0-80%",2),rep("0-90%",2),rep("0-100%",2))
  for (i in 1:10) {
    dat_part <- dat_bt[[i]]
    dis_hl <- dis_cal_hl(dat_part, itm_s, lim_2, lim_m)
    dif_hl <- dif_cal_hl(dat_part, itm_s, lim_2, lim_m)
    par_bt[,i*2] <- dis_hl[,1]; par_bt[,i*2+1] <- dif_hl[,1]
  }
  par_bt[,1] <- c(1:21, itm)
  if(dq %in% c("海南", "山东")){par_bt[,1][9:12] <- 13:16; par_bt[,1][13:16] <- 9:12}
  
  par_bt <- rbind(c("item_num", rep(c("discrimination","difficulty"),10)), par_bt)
  par_bt <- as.data.frame(par_bt)
  
  print("Upward accumulation completed")
  
  ################################################################################
  
  ### 自上而下累积[100,90],[100,80]……[100,0]
  dat_s3_tb <- acc_dat_tb(dat_raw)
  # 将每一层分别存入一个excel工作表中
  dat_tb <- list()
  for (i in 1:10) {
    dat_tb[[sprintf("100%%-%d%%",(10-i)*10)]] <- dat_s3_tb[[i]]
  }

  
  # Calculate the difficulty and discrimination of each layer respectively
  par_tb <- matrix(data = NA,nrow = length(itm_s), ncol = 21)
  colnames(par_tb) <- c(" ",rep("100%-90%",2),rep("100%-80%",2),rep("100%-70%",2),
                      rep("100%-60%",2),rep("100%-50%",2),rep("100%-40%",2),
                      rep("100%-30%",2),rep("100%-20%",2),rep("100%-10%",2),rep("100%-0",2))
  for (i in 1:10) {
    dat_part <- dat_tb[[i]]
    dis_hl <- dis_cal_hl(dat_part, itm_s, lim_2, lim_m)
    dif_hl <- dif_cal_hl(dat_part, itm_s, lim_2, lim_m)
    par_tb[,i*2] <- dis_hl[,1]; par_tb[,i*2+1] <- dif_hl[,1]
  }
  par_tb[,1] <- c(1:21, itm)
  if(dq %in% c("海南", "山东")){par_tb[,1][9:12] <- 13:16; par_tb[,1][13:16] <- 9:12}
  
  par_tb <- rbind(c("item_num", rep(c("discrinmination","difficulty"),10)), par_tb)
  par_tb <- as.data.frame(par_tb)
  
  # Create a new folder for each province and modify the path
  setwd(paste0(path0,"/【1】过程性文件"))
  dir.create(sprintf("%s-%s",dq,wl))
  setwd(sprintf("%s-%s",dq,wl))
  
  # 将三个文件存入同一个文件夹中
  write_xlsx(dat_s1_1, path = sprintf("%s-%s-5+2分段（7个表）.xlsx",dq,wl))
  write_xlsx(dat_bt, path = sprintf("%s-%s-向上累积（10个表）.xlsx",dq,wl))
  write_xlsx(dat_tb, path = sprintf("%s-%s-向下累积（10个表）.xlsx",dq,wl))
  
  # 将所有项目参数存入一个文件中
  setwd(paste0(path0,"/【2】结果文件"))
  write.xlsx(list("5+2分层"=par_1, "向上累积"=par_bt, "向下累积"=par_tb),
             file = sprintf("%s-%s-项目参数.xlsx",dq,wl))
  
  print("Downward accumulation completed")
  print("run successfully!！")
  
  
  ### Calculate run time
  t2 <- proc.time()
  print(sprintf("%s-%s-runingtime：%.2fs",dq,wl,(t2-t1)[3][[1]]))
  print("")
}


t1 <- proc.time()

for (dq in dq_all) {
  for (wl in wl_all) {
    setwd(paste0(path0,"/【0】原始文件"))
    dat_raw <- read.xlsx(sprintf("【%s】22题样本数据-%s.xlsx",dq,wl))
    main(dq, wl, itm = 22)
  }
}

t2 <- proc.time()
print(paste0("total_runningtime：",sprintf("%.2f",(t2-t1)[3][[1]]),"s"))

# # 海南和山东的参数与全国卷的有所不同，主要不同点是【excel文件名】和【itm参数】
# dq_all <- c("山东","海南")
# dq_all <- c("广东", "海南")
# wl_all <- c("整卷")
# 
# t1 <- proc.time()
# 
# for (dq in dq_all) {
#   for (wl in wl_all) {
#     setwd(paste0(path0,"/【0】原始文件"))
#     dat_raw <- read.xlsx(sprintf("【%s】%s数据.xlsx",dq,wl))
#     main(dq, wl, itm = 22)
#   }
# }
# 
# t2 <- proc.time()
# print(paste0("总运行时间：",sprintf("%.2f",(t2-t1)[3][[1]]),"s"))
