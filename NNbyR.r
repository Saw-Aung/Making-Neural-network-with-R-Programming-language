rm(list=ls())
## ���͑w�m�[�h��; �B��w�m�[�h��; �o�͑w�m�[�h��
 nn.inp <- 4; nn.hid <- 3; nn.out <- 2
## ���͑w���B��w�d�ݍs��
 w.ih <- array(rnorm(nn.inp*nn.hid, mean=0, sd=0.1),c(nn.inp,nn.hid))
## �B��w���o�͑w�d�ݍs��
 w.ho <- array(rnorm(nn.hid*nn.out, mean=0, sd=0.1),c(nn.hid,nn.out))
## ���͑w�C�o�͑w�̒l
 inp <- numeric(nn.inp); inp[1]<-0.8;inp[2]<-0.3;inp[3]<-0.1;inp[4]<-0.9
 out <- numeric(nn.out); out[1] <- 0.2; out[2] <- 0.8
## sigmoid�֐��̒�`
 sig <-function(x) return(1/(1+exp(-x)))

err <- 999; it <- 0; alp <- 0.9 ##���w�K��
while(err>1e-2){it <- it+1
  ## �B��w�C�o�͑w�m�[�h�l�̌v�Z
  nd.hid <- sig(t(w.ih) %*% inp)
  nd.out <- sig(t(w.ho) %*% nd.hid)
  ## �o�͑w�C�B��w�G���[�l�̌v�Z
  er.out <- out - nd.out
  er.hid <- w.ho %*% er.out
  ## ���͑w���B��w�C�B��w���o�͑w�̏d�ݍs��X�V
  w.ih <- w.ih + alp*t( (er.hid*nd.hid*(1-nd.hid)) %*% inp )
  w.ho <- w.ho + alp*t( (er.out*nd.out*(1-nd.out)) %*% t(nd.hid) )
  ## �o�͑w�G���[���Βl�̌v�Z
  err <- sum(abs(er.out))/nn.out
  cat("IT=",it," err=",err,"\n")
}

