rm(list=ls())
## “ü—Í‘wƒm[ƒh”; ‰B‚ê‘wƒm[ƒh”; o—Í‘wƒm[ƒh”
 nn.inp <- 4; nn.hid <- 3; nn.out <- 2
## “ü—Í‘w¨‰B‚ê‘wd‚İs—ñ
 w.ih <- array(rnorm(nn.inp*nn.hid, mean=0, sd=0.1),c(nn.inp,nn.hid))
## ‰B‚ê‘w¨o—Í‘wd‚İs—ñ
 w.ho <- array(rnorm(nn.hid*nn.out, mean=0, sd=0.1),c(nn.hid,nn.out))
## “ü—Í‘wCo—Í‘w‚Ì’l
 inp <- numeric(nn.inp); inp[1]<-0.8;inp[2]<-0.3;inp[3]<-0.1;inp[4]<-0.9
 out <- numeric(nn.out); out[1] <- 0.2; out[2] <- 0.8
## sigmoidŠÖ”‚Ì’è‹`
 sig <-function(x) return(1/(1+exp(-x)))

err <- 999; it <- 0; alp <- 0.9 ##©ŠwK—¦
while(err>1e-2){it <- it+1
  ## ‰B‚ê‘wCo—Í‘wƒm[ƒh’l‚ÌŒvZ
  nd.hid <- sig(t(w.ih) %*% inp)
  nd.out <- sig(t(w.ho) %*% nd.hid)
  ## o—Í‘wC‰B‚ê‘wƒGƒ‰[’l‚ÌŒvZ
  er.out <- out - nd.out
  er.hid <- w.ho %*% er.out
  ## “ü—Í‘w¨‰B‚ê‘wC‰B‚ê‘w¨o—Í‘w‚Ìd‚İs—ñXV
  w.ih <- w.ih + alp*t( (er.hid*nd.hid*(1-nd.hid)) %*% inp )
  w.ho <- w.ho + alp*t( (er.out*nd.out*(1-nd.out)) %*% t(nd.hid) )
  ## o—Í‘wƒGƒ‰[‘Š‘Î’l‚ÌŒvZ
  err <- sum(abs(er.out))/nn.out
  cat("IT=",it," err=",err,"\n")
}

