#################
### Panel Generalization of Durbin-Watson Statistic
### p-values and test from:
###  Bhargava, Franzini & Narendranathan
###  Serial Correlation and the Fixed Effects Model
###  Review of Economic Studies (1982), XLIX, 533-549
### implementation by w10 2014-jun thesis EUR
#
# load by: source('dwpaneltest.R')
# methods: dwpaneltest (res, indiv, time, n)
#          print.dwtest (dw, ...)
#          cvbhargava (H, T, n)
#
# Generalization of DW for a panel of dimensions T x H 
# d_P = \frac{ \sum_{i=1}^H \sum_{t=2}^T \left( \tilde{u}_{it} - \tilde{u}_{it-1} \right)^2 }
#          { \sum_{i=1}^H \sum_{t=1}^T \tilde{u}_{it}^2 }
#                                                                             (Eqn.4 p.535)
#
# the P-values are sumulated and depend on time dimension T, inidividual dimension H and number of regressors n
# Critical values from the tables I and II in Bhargava, Franzini & Narendranathan p.537
# TABLE I
# Five per cent significance points of d_PL and d_PU when T = 6
# H  50            100           150
# n  d_PL   d_PU   d_PL   d_PU   d_PL   d_PU   
# 1  1.8091 1.8231 1.8660 1.8731 1.8907 1.8958
# 3  1.7954 1.8376 1.8592 1.8799 1.8859 1.8998
# 5  1.7805 1.8517 1.8523 1.8867 1.8819 1.9046
# 7  1.7665 1.8657 1.8444 1.8937 1.8770 1.9094
# 9  1.7523 1.8812 1.8386 1.9018 1.8720 1.9145
# 11 1.7380 1.8956 1.8304 1.9086 1.8671 1.9185
# 13 1.7211 1.9118 1.8283 1.9158 1.8631 1.9233
# 15 1.7063 1.9267 1.8163 1.9227 1.8580 1.9284
# H  250           500           1000
# 1  1.9158 1.9183 1.9409 1.9413 1.9579 1.9586
# 3  1.9135 1.9217 1.9393 1.9427 1.9573 1.9593
# 5  1.9100 1.9240 1.9378 1.9441 1.9567 1.9560
# 7  1.9070 1.9265 1.9363 1.9455 1.9551 1.9606
# 9  1.9044 1.9296 1.9349 1.9468 1.9544 1.9614
# 11 1.9021 1.9321 1.9333 1.9482 1.9538 1.9621
# 13 1.8987 1.9354 1.9319 1.9497 1.9531 1.9628
# 15 1.8956 1.9379 1.9304 1.9511 1.9524 1.9630
# 
# TABLE II
# Five per cent significance points of d_PL and d_PU when T = 10
# H  50            100           150
# n  d_PL   d_PU   d_PL   d_PU   d_PL   d_PU  
# 1  1.8512 1.8596 1.8953 1.8991 1.9156 1.9160
# 3  1.8421 1.8688 1.8907 1.9037 1.9117 1.9206
# 5  1.8338 1.8769 1.8862 1.9081 1.9076 1.9244
# 7  1.8258 1.8851 1.8826 1.9118 1.9059 1.9245
# 9  1.8164 1.8945 1.8780 1.9164 1.9047 1.9284
# 11 1.8072 1.9029 1.8734 1.9209 1.9003 1.9318
# 13 1.7999 1.9126 1.8698 1.9275 1.8971 1.9341
# 15 1.7903 1.9209 1.8651 1.9294 1.8946 1.9375
# H  250           500           1000
# n  d_PL   d_PU   d_PL   d_PU   d_PL   d_PU  
# 1  1.9336 1.9354 1.9528 1.9536 1.9668 1.9677
# 3  1.9321 1.9370 1.9520 1.9544 1.9667 1.9679
# 5  1.9303 1.9390 1.9509 1.9552 1.9663 1.9682
# 7  1.9286 1.9405 1.9501 1.9561 1.9657 1.9686
# 9  1.9270 1.9421 1.9492 1.9572 1.9652 1.9700
# 11 1.9255 1.9445 1.9484 1.9580 1.9648 1.9705
# 13 1.9241 1.9459 1.9476 1.9588 1.9642 1.9710
# 15 1.9217 1.9474 1.9468 1.9569 1.9639 1.9712
 
# TEST H0: (no residual autocorrelation)
# U <- data.table(res=rep(NaN, 750), indiv=round(110*runif(750)))[, list(res=rnorm(length(res)), time=round(runif(1)*10)+1:length(res)), keyby=indiv][sample(750)]
# TEST Ha: (I(1) random walk)
# U <- data.table(res=rep(NaN, 750), indiv=round(110*runif(750)))[, list(res=diffinv(rnorm(length(res)-1)), time=round(runif(1)*10)+1:length(res)), keyby=indiv][sample(750)]
# TEST H0/Ha: inconclusinve
# U <- data.table(res=rep(NaN, 750), indiv=round(110*runif(750)))[, list(res=rnorm(length(res))+diffinv(rnorm(length(res)-1)), time=round(runif(1)*10)+1:length(res)), keyby=indiv][sample(750)]
# dwpaneltest(U$res, U$indiv, U$time, n=2)
 
 
# calculate the DW statistic and p-value
# res is the the residuals vector (numeric) - length(res)  =sum_H(T_i) #not nescesarily balanced!
# indiv is the individuals vector (numeric) - length(indiv)=sum_H(T_i)
# time is the time vector (numeric/integer) - length(time) =sum_H(T_i)
# n is the number of regressors estimated in the model (scalar numeric) #used for pvalues
require(data.table) # fast unbalanced dw calculation
require(pracma) #interp2 critical value matrix interpolation
'dwpaneltest' <- function (res, indiv, time, n){
  if (length(res)!=length(indiv) | length(res)!=length(time)){
    warning('dwpaneltest: length(res)!=length(indiv) | length(res)!=length(time)')
  }
  dw <- list() # return object
  class(dw) <- "dwtest"
  dw$testname <- 'Panel Generalization of Durbin-Watson'
  dw$H0 <- 'H0: Zero residual autocorrelation rho = 0'
  dw$Ha <- 'Ha: Nonzero residual autocorrelation rho != 0'
  sum_H_T_i <- length(res)
  dw$U <- data.table(res=res, indiv=indiv, time=time)
  setkey(dw$U, indiv, time) #sort ascending by time and allow keying on individals
  # calculate by individual demeaned residuals
  dw$U[, resdm:=res-mean(res), by=indiv]
  # calculate squares of first differences BY INDIVIDUAL
  dw$U[, ':='(dres2=c(NA, diff(c(resdm)))^2, res2=c(resdm)^2), by=indiv]
  # calculate statistitic by summing over all time periods and than over all individuals
  dw$d_P <- dw$U[, list(sumdres2=sum(dres2, na.rm=T), sumres2=sum(res2)), by=indiv][, sum(sumdres2)/sum(sumres2)]
  dw$is.dir.pos <- (dw$d_P<2) #is the direction of the autocorrelation positive
  dw$sign <- ifelse(dw$is.dir.pos, '+', '-')
  
  # critical value calculation
  dw$pvalsource <- 'Bhargava, Franzini & Narendranathan\nReview of Economic Studies (1982), XLIX, p.533-549'
  dw$H <- length(unique(indiv))
  dw$T <- dw$U[, list(T_i=length(time)), by=indiv][, mean(T_i)]
  dw$n <- n
  dw$cv <- cvbhargava(dw$H, dw$T, dw$n)
  if (is.na(dw$cv)[1]){
    # critical values cannot be determined apperantly
    return(dw)
  }
  dw$cv.pos <- dw$cv[, 1:2, with=F] #cv.pos: if d_P < d_PL postitive residual autocorr, d_PL < d_P < d_PU inconclusive
  dw$cv.neg <- 4 - dw$cv.pos #invert critcal values cv.neg d_P > d_PU negative residual autocorr, d_PL > d_P > d_PU inconclusive
  setnames(dw$cv.neg, names(dw$cv.neg), names(dw$cv.neg)[2:1]) #invert names
  dw$star <- ' '
  if (dw$is.dir.pos){ #check for postive residual a.c.
    if (dw$d_P < dw$cv.pos$d_PL){
      dw$concl.str <- 'Significant (5%) postitive residual autocorrelation.'
      dw$concl.code <- list(paste('d_P  < cv(d_PL+)'), 
                            paste(sprintf('%.2f', dw$d_P), '<', sprintf('%.2f', dw$cv.pos$d_PL)))
      dw$star <- '*'
    } else if (dw$d_P < dw$cv.pos$d_PU) {
      dw$concl.str <- 'Insignificant (5%) postitive residual autocorrelation.'
      dw$concl.code <- list(paste('cv(d_PL+) < d_P < cv(d_PU+)'), 
                            paste(sprintf('%.2f', dw$cv.pos$d_PL), '<', sprintf('%.2f', dw$d_P), 
                                  '<',sprintf('%.2f', dw$cv.pos$d_PU)))
    } else {dw$concl.str <- 'No residual autocorrelation.'
            dw$concl.code <- list(paste('2 >  d_P  > cv(d_PU+)'), 
                                  paste(2, '> ', sprintf('%.2f', dw$d_P), '>', sprintf('%.2f', dw$cv.pos$d_PU)))
    }
  } else { #check for negative residual a.c.
    if (dw$d_P > dw$cv.neg$d_PU){
      dw$concl.str <- 'Significant (5%) negative residual autocorrelation.'
      dw$concl.code <- list(paste('d_P > cv(d_PU-)'), 
                            paste(sprintf('%.2f', dw$d_P), '>', sprintf('%.2f', dw$cv.neg$d_PU)))
      dw$star <- '*'
    } else if (dw$d_P > dw$cv.neg$d_PL) {
      dw$concl.str <- 'Insignificant (5%) negative residual autocorrelation.'
      dw$concl.code <- list(paste('cv(d_PL-) < d_P < cv(d_PU-)'), 
                            paste(sprintf('%.2f', dw$cv.neg$d_PL), '<', sprintf('%.2f', dw$d_P), 
                                  '<', sprintf('%.2f', dw$cv.neg$d_PU)))
    } else {dw$concl.str <- 'No residual autocorrelation.'
            dw$concl.code <- list(paste('2 <  d_P  < cv(d_PL-)'), 
                                  paste(2, '< ', sprintf('%.2f', dw$d_P), '<', sprintf('%.2f', dw$cv.neg$d_PU)))
    }
  }
  return(dw)
}
 
# interpolate the critical values from the tables in Bhargava, Franzini & Narendranathan p.537
# reported 5% critical values: T=6/10 n=1/3/5/7/9/11/13/15 H=50/100/150/250/500/1000
# so only for shallow, wide and balanced panels
'cvbhargava' <- function (H, T, n, extrap=F){
  T.cv <- T
  if (T<6 | T>10){
    warning(paste0('T = ', T, ', recomended (T<6 | T>10): Critical Values can not be interpolated (Bhargava, Franzini & Narendranathan p.537)\n  Only T=6 and T=10 are simulated. If extrap=T attempt to calculate by extrapolating linearly, else take T=6 or T=10.'))
    if (T<6 & extrap==F){
      T.cv <- 6
    }
    if (T>10 & extrap==F){
      T.cv <- 10
    }
  }
  if (n<1 | n>15){
    warning(paste0('n = ', n, ', required (n<1 | n>15): Critical Values can not be interpolated (Bhargava, Franzini & Narendranathan p.537)'))
    return (NA)
  }
  if (H<50 | H>1000){
    warning(paste0('H = ', H, ', required (H<50 | H>1000): Critical Values can not be interpolated (Bhargava, Franzini & Narendranathan p.537)'))
    return (NA)
  }
  H_vec <- c(50,100,150,250,500,1000)
  n_vec <- c(1,3,5,7,9,11,13,15)
  Hn <- list(H_vec, n_vec)
  d_PL_T6 <- t(matrix(c(
    1.8091,1.8660,1.8907,1.9158,1.9409,1.9579,
    1.7954,1.8592,1.8859,1.9135,1.9393,1.9573,
    1.7805,1.8523,1.8819,1.9100,1.9378,1.9567,
    1.7665,1.8444,1.8770,1.9070,1.9363,1.9551,
    1.7523,1.8386,1.8720,1.9044,1.9349,1.9544,
    1.7380,1.8304,1.8671,1.9021,1.9333,1.9538,
    1.7211,1.8283,1.8631,1.8987,1.9319,1.9531,
    1.7063,1.8163,1.8580,1.8956,1.9304,1.9524),
    nrow=6, ncol=8, dimnames=Hn))
  d_PU_T6 <- t(matrix(c(
    1.8231,1.8731,1.8958,1.9183,1.9413,1.9586,
    1.8376,1.8799,1.8998,1.9217,1.9427,1.9593,
    1.8517,1.8867,1.9046,1.9240,1.9441,1.9560,
    1.8657,1.8937,1.9094,1.9265,1.9455,1.9606,
    1.8812,1.9018,1.9145,1.9296,1.9468,1.9614,
    1.8956,1.9086,1.9185,1.9321,1.9482,1.9621,
    1.9118,1.9158,1.9233,1.9354,1.9497,1.9628,
    1.9267,1.9227,1.9284,1.9379,1.9511,1.9630),
    nrow=6, ncol=8, dimnames=Hn))
  d_PL_T10 <- t(matrix(c(
    1.8512,1.8953,1.9156,1.9336,1.9528,1.9668,
    1.8421,1.8907,1.9117,1.9321,1.9520,1.9667,
    1.8338,1.8862,1.9076,1.9303,1.9509,1.9663,
    1.8258,1.8826,1.9059,1.9286,1.9501,1.9657,
    1.8164,1.8780,1.9047,1.9270,1.9492,1.9652,
    1.8072,1.8734,1.9003,1.9255,1.9484,1.9648,
    1.7999,1.8698,1.8971,1.9241,1.9476,1.9642,
    1.7903,1.8651,1.8946,1.9217,1.9468,1.9639),
    nrow=6, ncol=8, dimnames=Hn))
  d_PU_T10 <- t(matrix(c(
    1.8596,1.8991,1.9160,1.9354,1.9536,1.9677,
    1.8688,1.9037,1.9206,1.9370,1.9544,1.9679,
    1.8769,1.9081,1.9244,1.9390,1.9552,1.9682,
    1.8851,1.9118,1.9245,1.9405,1.9561,1.9686,
    1.8945,1.9164,1.9284,1.9421,1.9572,1.9700,
    1.9029,1.9209,1.9318,1.9445,1.9580,1.9705,
    1.9126,1.9275,1.9341,1.9459,1.9588,1.9710,
    1.9209,1.9294,1.9375,1.9474,1.9569,1.9712),
    nrow=6, ncol=8, dimnames=Hn))
  d_PL_T6.Hn <- interp2(H_vec, n_vec, d_PL_T6, H, n)
  d_PL_T10.Hn <- interp2(H_vec, n_vec, d_PL_T10, H, n)
  d_PL.HTn <- predict(lm(d_PL ~ T + 1, data=data.table(T=c(6, 10), d_PL=c(d_PL_T6.Hn, d_PL_T10.Hn))), 
                      data.frame(T=T.cv))[[1]]
  d_PU_T6.Hn <- interp2(H_vec, n_vec, d_PU_T6, H, n)
  d_PU_T10.Hn <- interp2(H_vec, n_vec, d_PU_T10, H, n)
  d_PU.HTn <- predict(lm(d_PU ~ T + 1, data=data.table(T=c(6, 10), d_PU=c(d_PU_T6.Hn, d_PU_T10.Hn))), 
                      data.frame(T=T.cv))[[1]]
  return(data.table(d_PL=d_PL.HTn, d_PU=d_PU.HTn, T.cv=T.cv))
}
 
print.dwtest <- function (dw, ...){
  cat(paste(paste(rep('-', 50), collapse=''), '\n'))
  cat(paste('Test:',dw$testname, '\n'))
  cat(paste(' ', dw$H0, '\n'))
  cat(paste(' ', dw$Ha, '\n'))
  cat(paste0('Indiv. H = ', dw$H, ', (mean) T = ', sprintf('%.2f', dw$T), 
             ' [T.cv=',  sprintf('%.2f', dw$cv$T.cv), '], regressors n = ', dw$n, '\n'))
  cat(paste0(' cv(d_PL+) = ', sprintf('%.4f', dw$cv.pos$d_PL), ', cv(d_PU+) = ', sprintf('%.4f', dw$cv.pos$d_PU), '\n'))
  cat(paste0(' cv(d_PL-) = ', sprintf('%.4f', dw$cv.neg$d_PL), ', cv(d_PU-) = ', sprintf('%.4f', dw$cv.neg$d_PU), '\n'))
  cat(paste('Statistic d_P =', sprintf('%.2f', dw$d_P), dw$star, '\n'))
  cat(paste(' ', dw$concl.code[[1]], '\n'))
  cat(paste(' ', dw$concl.code[[2]], '\n'))
  cat(paste(dw$concl.str, '\n'))
  cat(paste(paste(rep('-', 50), collapse=''), '\n'))
  cat(paste('P-values from:', dw$pvalsource, '\n'))
}

