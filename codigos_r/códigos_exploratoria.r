################################################
# Pedro Henrique Pavan Gonçalves
# Códigos TCC
################################################

# Pacotes
library(tidyverse)
library(survival)
library(ggfortify)
library(knitr)


# Lendo os dados e analisando sua estrutura
path <- 'base_de_dados/hantavir.csv'
dados <- read.csv(path, 
    header = TRUE, sep = ';')
summary(dados)
str(dados)



tempo <- as.numeric(as.Date(dados$DATAOBITO,"%d/%m/%Y")-as.Date(dados$DATA1SINT,"%d/%m/%Y"))
aux=dados$CASOCONTROLE==2

tempo[aux] <- as.numeric(as.Date(dados$DATAALTA[aux],"%d/%m/%Y")-as.Date(dados$DATA1SINT[aux],"%d/%m/%Y"))

dados$tempo <- tempo

aux <- is.na(dados$tempo)

dados$tempo[aux] <- as.numeric(as.Date(dados$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dados$DATA1SINT[aux],"%d/%m/%Y"))
aux <- is.na(dados$tempo)

dados$tempo[aux] <- 60
aux <- is.na(dados$tempo)

aux <- dados$tempo < 0
dados[aux,]

dados$tempo[aux] <- as.numeric(as.Date(dados$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dados$DATA1SINT[aux],"%d/%m/%Y"))
aux <- dados$tempo == 0
sum(aux)
dados$tempo[aux] <- 1
dados$censura = ifelse(dados$CASOCONTROLE == 1,1,0)

summary(dados$tempo)

par(mfrow = c(1,1))
hist(dados$tempo)

aux=dados$tempo>100
dados$tempo[aux] <- as.numeric(as.Date(dados$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dados$DATA1SINT[aux],"%d/%m/%Y"))

aux <- dados$tempo>100
dados$tempo[aux]<- 80


# Alteradas e padronizadas todas as variáveis, temos:
summary(dados)

kable(table(dados$tempo))## Quanto maior o tempo, menos a quantidade de observações na variável
kable(prop.table(table(dados$tempo))) ##


ekm <- survfit(Surv(tempo,censura) ~ 1,
            data=dados, type=c("kaplan-meier")) 
summary(ekm) # IC EXTREMAMENTE espaçado
plot(ekm) # Comportamento estranho explicado pela falta de observações


# Curvas de sobrevivência para as variáveis: 
# SANGRESREG
ekm_SANGRESREG <- survfit(Surv(tempo,censura) ~ SANGRESREG, data=dados)
summary(ekm_SANGRESREG)
plot(ekm_SANGRESREG)

# HIPOTENSAOREG
ekm_HIPOTENSAOREG <- survfit(Surv(tempo,censura) ~ HIPOTENSAOREG, data=dados)
summary(ekm_HIPOTENSAOREG)
plot(ekm_HIPOTENSAOREG)
# TONTURAREG
ekm_TONTURAREG <- survfit(Surv(tempo,censura) ~ TONTURAREG, data=dados)
summary(ekm_TONTURAREG)



# Seleção de Variáveis para o modelo de cox
fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+INSUFRENALREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+LEUCCDEREG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG+EDEMAPULMREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+INSUFRENALREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+LEUCCDEREG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+INSUFRENALREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+CEFALEIAREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+DERPLEURALREG+INFPULDIFREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+MIALGIASREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+INFPULDIFREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG+INFPULDIFREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+SINAISHEMOREG+HEMMAIOR46REG+AUMENTOUREREG,data=dados)
summary(fit)

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+HEMMAIOR46REG+AUMENTOUREREG,data=dados)
summary(fit)

#AUMENTOUREREG tem 109 dados omissos
fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG+HEMMAIOR46REG,
            data=dados)
summary(fit)
fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,
            data=dados)
summary(fit)


require(smcure)
pd <- smcure(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,cureform=~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,
            data=dados,model='ph',nboot=500)

pd <- smcure(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,cureform=~IDADE+TONTURAREG+SANGRESREG+HIPOTENSAOREG,
            data=dados,model='ph',nboot=500)


