################################################
# Pedro Henrique Pavan Gonçalves
# Códigos TCC
################################################

# Pacotes
library(tidyverse)
library(survival)

# Lendo os dados e analisando sua estrutura
path <- 'base_de_dados/hantavir2.n280.2.08.05.17.csv'
dados <- read.csv(path, 
    header = TRUE, sep = ';')
summary(dados)
str(dados)



tempo <- as.numeric(as.Date(dat$DATAOBITO,"%d/%m/%Y")-as.Date(dat$DATA1SINT,"%d/%m/%Y"))
aux=dat$CASOCONTROLE==2

tempo[aux] <- as.numeric(as.Date(dat$DATAALTA[aux],"%d/%m/%Y")-as.Date(dat$DATA1SINT[aux],"%d/%m/%Y"))

dat$tempo=tempo

aux <- is.na(dat$tempo)

dat$tempo[aux] <- as.numeric(as.Date(dat$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dat$DATA1SINT[aux],"%d/%m/%Y"))
aux <- is.na(dat$tempo)
dat$tempo[aux] <- 60
aux <- is.na(dat$tempo)

aux <- dat$tempo<0
dat[aux,]

dat$tempo[aux] <- as.numeric(as.Date(dat$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dat$DATA1SINT[aux],"%d/%m/%Y"))
aux <- dat$tempo==0
sum(aux)
dat$tempo[aux]=1
dat$censura=ifelse(dat$CASOCONTROLE==1,1,0)

summary(dat$tempo)
hist(dat$tempo)

aux=dat$tempo>100
dat$tempo[aux]=as.numeric(as.Date(dat$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dat$DATA1SINT[aux],"%d/%m/%Y"))

aux=dat$tempo>100
dat$tempo[aux]=80

require(survival)
plot(survfit(Surv(tempo,censura)~1,data=dat))

#vermelho:nao azul:sim
plot(survfit(Surv(tempo,censura)~SANGRESREG,data=dat),col=c(2,4))
plot(survfit(Surv(tempo,censura)~HIPOTENSAOREG,data=dat),col=c(2,4))
plot(survfit(Surv(tempo,censura)~TONTURAREG,data=dat),col=c(2,4))

fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+INSUFRENALREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+LEUCCDEREG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG+EDEMAPULMREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+INSUFRENALREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+LEUCCDEREG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+INSUFRENALREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+INFPULDIFREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+INFPULDIFREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+HEMMAIOR46REG+AUMENTOUREREG,data=dat)
summary(fit)

#AUMENTOUREREG tem 109 dados omissos
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+HEMMAIOR46REG,data=dat)
summary(fit)
fit=coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,data=dat)
summary(fit)


require(smcure)
pd <- smcure(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,cureform=~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,data=dat,model='ph',nboot=500)

pd <- smcure(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,cureform=~IDADE+TONTURAREG+SANGRESREG+HIPOTENSAOREG,data=dat,model='ph',nboot=500)

