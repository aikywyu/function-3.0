### The function Code for Data cleaning/converting/hierarchy
### 20.12.01

### Convert the raw data to the opt-analysis data------------------------------------------
na_dat <- function(dat,lim){
  for(i in 2:length(lim)){
    dat[,i][which(dat[,i]!=0&dat[,i]!=1&dat[,i]!=2&dat[,i]!=4&dat[,i]!=8)] <- 99
    dat[,i][which(dat[,i] == 0)] <- NA
  }
  aa <- rowSums(is.na(dat))
  bb <- which(aa == length(lim)-1)                               ## eliminate all NA response
  cc <- dat[-bb,]
  return(cc)
}


### Convert the opt-analysis to Score data------------------------------------------------
sco_dat <- function(dat_all, ans, sco) {
  for(i in 1:length(ans)) {
    a <- which(dat_all[, (i+1)] == ans[i])
    b <- setdiff(1:nrow(dat_all), a)
    dat_all[a, (i+1)] <- sco                          ## score for correct answer a opt-item
    dat_all[b, (i+1)] <- 0
  }
  aa <- dat_all[, -1]
  aa <- as.data.frame(lapply(aa, as.numeric))
  zf <- matrix(apply(aa, 1, sum), ncol = 1)
  dq <- dat_all[, 1]
  dat_sco <- cbind(dq, aa, zf)
  return(dat_sco)
}


### Uni-dimensional grouping--------------------------------------------------------------
del_dat <- function(dat, min, max){
  dat_ord <- dat[order(dat[, ncol(dat)], decreasing = FALSE), ]
  stu <- nrow(dat_ord); dat_zf <- dat_ord[, ncol(dat_ord)]
  val_min <- dat_ord[round(quantile(1:stu, min)),ncol(dat_ord)]
  val_max <- dat_ord[round(quantile(1:stu, max)),ncol(dat_ord)]
  if (min == 0.4){
    dir_del <- which(dat_zf <= val_max & dat_zf > val_min)
  }else{
    dir_del <- which(dat_zf <= val_max & dat_zf >= val_min)
  }
  dat_sel <- dat_ord[dir_del, ]
}


### Multi-dimensional grouping (return a list)--------------------------------------------
mul_dat <- function(dat){
  dat_ord <- dat[order(dat[, ncol(dat)], decreasing = FALSE), ]
  dat_g <- list(); stu <- nrow(dat_ord); dat_zf <- dat_ord[, ncol(dat_ord)]
  for(i in 1 : 10) {
    gap <- i/10
    dir <- which(dat_zf <= dat_ord[round(quantile(1:stu, gap)),ncol(dat_ord)])
    dat_g[[i]] <- dat_ord[dir, ]
  }
  dat_g1 <- dat_g
  for(j in 10:2){
    dat_g1[[j]] <- dat_g[[j]][-(1:nrow(dat_g[[j-1]])),]
  }
  return(dat_g1)
}

### From bottom to top [0,10],[0,20]……[0,100]
### Accumulate grouping--------------------------------------------------------------------
acc_dat_bt <- function(dat){
  dat_ord <- dat[order(dat[, ncol(dat)], decreasing = FALSE), ]
  dat_g <- list(); stu <- nrow(dat_ord); dat_zf <- dat_ord[, ncol(dat_ord)]
  for(i in 1 : 10) {
    gap <- i/10
    dir <- which(dat_zf <= dat_ord[round(quantile(1:stu, gap)),ncol(dat_ord)])
    dat_g[[i]] <- dat_ord[dir, ]
  }
## rev(dat_g)               ### From bottom to top [0,100],[0,90]……[0,10]
  return(dat_g)
}

### From top to bottom [100,90],[100,80]……[100,0]
acc_dat_tb <- function(dat){
  dat_ord <- dat[order(dat[, ncol(dat)], decreasing = TRUE), ]
  dat_g <- list(); stu <- nrow(dat_ord); dat_zf <- dat_ord[, ncol(dat_ord)]
  for(i in 1 : 10) {
    gap <- i/10
    dir <- which(dat_zf >= dat_ord[round(quantile(1:stu, gap)),ncol(dat_ord)])
    dat_g[[i]] <- dat_ord[dir, ]
  }
  ## rev(dat_g)               ### From bottom to top [0,100],[0,90]……[0,10]
  return(dat_g)
}



### Discrimination calculate (dat_s: response score matrix)-----------------------------------------------------
dis_cal_hl <- function(dat_s,itm_s, lim_2, lim_m){
  dat_ord <- dat_s[order(dat_s[, ncol(dat_s)], decreasing = TRUE), ]
  dat_ord <- dat_ord[,-1]
  dat_zf <- dat_ord[ ,ncol(dat_ord)]; stu <- nrow(dat_ord)
  dir_h <- which(dat_zf >= dat_ord[round(quantile(1:stu, 0.27)),ncol(dat_ord)])
  dir_l <- which(dat_zf <= dat_ord[round(quantile(1:stu, 0.73)),ncol(dat_ord)])
  hig_sco_2 <- dat_ord[dir_h, lim_2]; hig_sco_m <- dat_ord[dir_h, lim_m]
  low_sco_2 <- dat_ord[dir_l, lim_2]; low_sco_m <- dat_ord[dir_l, lim_m]
  PH_2 <- (colSums(hig_sco_2)/nrow(hig_sco_2))/itm_s[lim_2]                             ## the scoring rate of high-group
  PL_2 <- (colSums(low_sco_2)/nrow(low_sco_2))/itm_s[lim_2]                             ## the scoring rate of low-group
  D_2 <- data.frame("D"=round(PH_2 - PL_2, 2))
  PH_m <- colMeans(hig_sco_m)/itm_s[lim_m]
  PL_m <- colMeans(low_sco_m)/itm_s[lim_m]
  D_m <- data.frame("D"=round(PH_m - PL_m, 2))
  D <- rbind(D_2, D_m)
  return(D)
}

dis_cal_cor <- function(dat_s,itm_s, lim){
  dat_ord <- dat_s[order(dat_s[, ncol(dat_s)], decreasing = TRUE), ]
  dat_ord <- dat_ord[,-1]
  dat_zf <- dat_ord[ ,ncol(dat_ord)]
  D <- 0
  for (i in lim) {
    cor <- cor.test(dat_ord[,i], dat_zf)
    D[i] <- cor[["estimate"]]
  }
  D <- data.frame("D"=round(D, 2))
  return(D)
}


### Difficulty calculate-----------------------------------------------------------------------------------
dif_cal_hl <- function(dat_s,itm_s, lim_2, lim_m){
  dat_ord <- dat_s[order(dat_s[, ncol(dat_s)], decreasing = TRUE), ]
  dat_ord <- dat_ord[,-1]
  dat_zf <- dat_ord[ ,ncol(dat_ord)]; stu <- nrow(dat_ord)
  dir_h <- which(dat_zf >= dat_ord[round(quantile(1:stu, 0.27)),ncol(dat_ord)])
  dir_l <- which(dat_zf <= dat_ord[round(quantile(1:stu, 0.73)),ncol(dat_ord)])
  hig_sco_2 <- dat_ord[dir_h, lim_2]; hig_sco_m <- dat_ord[dir_h, lim_m]
  low_sco_2 <- dat_ord[dir_l, lim_2]; low_sco_m <- dat_ord[dir_l, lim_m]
  PH_2 <- (colSums(hig_sco_2)/nrow(hig_sco_2))/itm_s[lim_2]                             ## the scoring rate of high-group
  PL_2 <- (colSums(low_sco_2)/nrow(low_sco_2))/itm_s[lim_2]                             ## the scoring rate of low-group
  dif_2 <- data.frame("dif"=round((PH_2 + PL_2)/2, 2))
  PH_m <- colMeans(hig_sco_m)/itm_s[lim_m]
  PL_m <- colMeans(low_sco_m)/itm_s[lim_m]
  dif_m <- data.frame("dif"=round((PH_m + PL_m)/2, 2))
  dif <- rbind(dif_2, dif_m)
  return(dif)
}

dif_cal_p <- function(dat_s,itm_s, lim){
  dat_ord <- dat_s[order(dat_s[, ncol(dat_s)], decreasing = TRUE), ]
  dat_ord <- dat_ord[,-1]
  dif <- data.frame("dif"=round(colMeans(dat_ord[,lim])/itm_s[lim],2))
  return(dif)
}