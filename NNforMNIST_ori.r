rm(list=ls())
setwd("c:/usr/doc/")
## 教師データ，testデータの読み込み
dt.tr <- read.csv("mnist_train.csv",header=F)
res.tr <- dt.tr[,1]; let.tr <- dt.tr[,2:(28*28+1)]
ns.tr <- 60000
dt.te <- read.csv("mnist_test.csv",header=F)
res.te <- dt.te[,1]; let.te <- dt.te[,2:(28*28+1)]
ns.te <- 10000

## グレースケールを，0.01-1 の範囲値に変換
let.tr <- let.tr/255*0.99 + 0.01
let.te <- let.te/255*0.99 + 0.01

for(ii in 3:10){alp <- ii/10
## 入力層ノード数; 隠れ層ノード数; 出力層ノード数
 nn.inp <- 784; nn.hid <- 100; nn.out <- 10
## 入力層→隠れ層重み行列
 w.ih <- array(rnorm(nn.inp*nn.hid, mean=0, sd=0.3),c(nn.inp,nn.hid))
## 隠れ層→出力層重み行列
 w.ho <- array(rnorm(nn.hid*nn.out, mean=0, sd=0.3),c(nn.hid,nn.out))
## 入力層，出力層の値
 inp    <- array(0,c(ns.tr,nn.inp)); inp <- as.matrix(let.tr)
 out    <- array(0,c(ns.tr,nn.out)); for(n in 1:ns.tr) out[n,res.tr[n]+1]    <- 1
 inp.te <- array(0,c(ns.te,nn.inp)); inp.te <- as.matrix(let.te)
 out.te <- array(0,c(ns.te,nn.out)); for(n in 1:ns.te) out.te[n,res.te[n]+1] <- 1
## sigmoid関数の定義
 sig <-function(x) return(1/(1+exp(-x)))

## 学習率
alp <- 0.2
for(it in 1:10){ err <- 0
 for(n in 1:ns.tr){
   ## 隠れ層，出力層ノード値の計算
   nd.hid <- sig(t(w.ih) %*% inp[n,])
   nd.out <- sig(t(w.ho) %*% nd.hid)
   ## 出力層，隠れ層エラー値の計算
   er.out <- out[n,] - nd.out
   er.hid <- w.ho %*% er.out
   ## 入力層→隠れ層，隠れ層→出力層の重み行列更新
   w.ih <- w.ih + alp*t((er.hid*nd.hid*(1-nd.hid)) %*% inp[n,])
   w.ho <- w.ho + alp*t((er.out*nd.out*(1-nd.out)) %*% t(nd.hid))
   ## 出力層エラー相対値の計算
   err <- err + sum(abs(er.out))/nn.out
 }
 err <- err/ns.tr
 cat("IT=",it," err=",err,"\n")
  ## 的中率の計算 for 教師データ
  res <- array(0,c(ns.tr,1)); hit <- array(0,c(10,10))
  for(n in 1:ns.tr){
    nd.hid <- sig(t(w.ih) %*% inp[n,])
    nd.out <- sig(t(w.ho) %*% nd.hid)
    res[n] <- which.max(nd.out)-1
    hit[res.tr[n]+1,res[n]+1] <- hit[res.tr[n]+1,res[n]+1] + 1
  }
  print(hit)
  rt <- 0; for(n in 1:10) rt <- rt + hit[n,n]
  rt <- rt*100/ns.tr; cat("教師: Hit rate[%]: ",rt,"\n")
  ## 的中率の計算 for testデータ
  res <- array(0,c(ns.te,1)); hit <- array(0,c(10,10))
  for(n in 1:ns.te){
    nd.hid <- sig(t(w.ih) %*% inp.te[n,])
    nd.out <- sig(t(w.ho) %*% nd.hid)
    res[n] <- which.max(nd.out)-1
    hit[res.te[n]+1,res[n]+1] <- hit[res.te[n]+1,res[n]+1] + 1
  }
  print(hit)
  rt <- 0; for(n in 1:10) rt <- rt + hit[n,n]
  rt <- rt*100/ns.te; cat("test: Hit rate[%]: ",rt,"\n")
}



#########################################
## 描画用にデータを作成
library(ggplot2)
mn <- array(0,c(100,28,28))
for(i in 1:100){ nn <- 0
  for(j in 28:1){
    for(k in 1:28){
      nn <- nn+1; mn[i,k,j] <- let[i,nn]
}}}
par(ask=T)
for(n in 1:100){ 
  gr <- array(0,c(28*28,3))
  nn <- 0
  for(i in 1:28){
    for(j in 1:28){nn <- nn +1
      gr[nn,1] <- i; gr[nn,2] <- j; gr[nn,3] <- mn[n,i,j]
  }}
  gr <- data.frame(gr); num <- as.character(res[n])
  gm <- ggplot(data=gr, aes(x=gr[,1], y=gr[,2], color=255-gr[,3]))+geom_point(size=5) +
         annotate("text", x=2, y=2, label=num,size=10)
  print(gm)
} ## 描画修了

