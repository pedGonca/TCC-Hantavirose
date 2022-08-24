################################################
# Pedro Henrique Pavan Gonçalves
# Códigos TCC
################################################

rm(list = ls())

# Pacotes
library(tidyverse)
library(survival)
library(survminer)
library(ggfortify)
library(knitr)


# Lendo os dados e analisando sua estrutura
path <- 'base_de_dados/hantavir2.csv'
dados <- read.csv(path, 
    header = TRUE, sep = ';')
summary(dados)
str(dados)
colnames(dados)

length(dados$data)


# Criando a variável tempo

# Data de óbito menos o tempo do primeiro sintoma apresentado para casos onde houve óbito
tempo <- as.numeric(as.Date(dados$DATAOBITO,"%d/%m/%Y")-as.Date(dados$DATA1SINT,"%d/%m/%Y"))
aux = dados$CASOCONTROLE==2

# Data de alta do paciente menos o tempo do primeiro sintoma apresentado para casos onde não houve óbito
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

# Histograma da variável tempo
par(mfrow = c(1,1))
hist(dados$tempo)

aux=dados$tempo>100
dados$tempo[aux] <- as.numeric(as.Date(dados$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dados$DATA1SINT[aux],"%d/%m/%Y"))

aux <- dados$tempo>100
dados$tempo[aux]<- 80
dados$tempo <- dados$tempo[dados$tempo > 0]

dados[dados$tempo < 0]


# Criando a variável de censura
# 0 para censura e 1 para os pacientes com data de óbito registrada
dados$censura = ifelse(dados$CASOCONTROLE == 1,1,0)

# Histograma da variável tempo
par(mfrow = c(1,1))
hist(dados$tempo)

# Alteradas e padronizadas todas as variáveis, temos:
summary(dados)

kable(table(dados$tempo))## Quanto maior o tempo, menor a quantidade de observações na variável
kable(prop.table(table(dados$tempo))) ##

# retirando tempo negativo presente na base
dados <- filter(dados, tempo >= 0)
length(dados$tempo)


ggplot(dados, aes(y = tempo)) +
  geom_boxplot()


ekm <- survfit(Surv(tempo,censura) ~ 1,
            data=dados, type=c("kaplan-meier")) 
summary(ekm) # IC EXTREMAMENTE espaçado
ggsurvplot(ekm) # Comportamento estranho explicado pela falta de observações


# Curvas de sobrevivência para as variáveis: 
unique(dados$IDADE)
hist(dados$IDADE)
# IDADE_CAT
# Categorizando a idade. Será categorizado de 25 em 25 anos
dados$IDADE_CAT <- ""
dados$IDADE_CAT[dados$IDADE < 20] <- '[0;20]'
dados$IDADE_CAT[dados$IDADE >= 20 & dados$IDADE <= 29] <- '[20;29]'
dados$IDADE_CAT[dados$IDADE >= 30 & dados$IDADE <= 39] <- '[30;39]'
dados$IDADE_CAT[dados$IDADE >= 40 & dados$IDADE <= 49] <- '[40;49]'
dados$IDADE_CAT[dados$IDADE >= 50 & dados$IDADE <= 59] <- '[50;59]'
dados$IDADE_CAT[dados$IDADE >= 60] <- '[60['


ekm_IDADE_CAT <- survfit(Surv(tempo,censura) ~ IDADE_CAT, data=dados)
summary(ekm_IDADE_CAT)
ggsurvplot(ekm_IDADE_CAT)

# SEXOREG
ekm_SEXOREG <- survfit(Surv(tempo,censura) ~ SEXOREG, data=dados)
summary(ekm_SEXOREG)
ggsurvplot(ekm_SEXOREG)

# SANGRESREG
ekm_SANGRESREG <- survfit(Surv(tempo,censura) ~ SANGRESREG, data=dados)
summary(ekm_SANGRESREG)
ggsurvplot(ekm_SANGRESREG)

# HIPOTENSAOREG
ekm_HIPOTENSAOREG <- survfit(Surv(tempo,censura) ~ HIPOTENSAOREG, data=dados)
summary(ekm_HIPOTENSAOREG)
ggsurvplot(ekm_HIPOTENSAOREG)

# TONTURAREG
ekm_TONTURAREG <- survfit(Surv(tempo,censura) ~ TONTURAREG, data=dados)
summary(ekm_TONTURAREG)
ggsurvplot(ekm_TONTURAREG)


# Seleção de Variáveis para o modelo de cox
fit <- coxph(Surv(tempo,censura)~IDADE + SEXOREG + TONTURAREG + INSUFRENALREG + SANGRESREG + CEFALEIAREG + HIPOTENSAOREG + MIALGIASREG +
            SINAISHEMOREG + HEMMAIOR46REG + LEUCCDEREG + AUMENTOUREREG + DERPLEURALREG + INFPULDIFREG + EDEMAPULMREG + RSRESREG +
            ZONALOCINF, data = dados)
summary(fit)

## Testei os modelos retirando as não significativas e não relevantes para o estudo

fit <- coxph(Surv(tempo,censura)~IDADE + SEXOREG + TONTURAREG + SANGRESREG + HIPOTENSAOREG + MIALGIASREG + RSRESREG, 
            data=dados)
summary(fit) # Tentei rodar com a idade categórica, mas o resultado foi menos satisfatório


### Ajustando o modelos com o pacote smcure
# dados2 contém as variáveis IDADE, SEXOREG, TONTURAREG, SANGRESREG, HIPOTENSAOREG, MIALGIASREG, RSRESREG e SINAISHEMOREG
dados2 <- dados[,c('tempo', 'censura','IDADE', 'SEXO12', 'TONTURAREG', 'SANGRESREG', 'HIPOTENSAOREG', 'MIALGIASREG','RSRESREG',
                    'SINAISHEMOREG','RESPMECANREG')]

dados2$SEXO = ifelse(dados2$SEXO12 == 1,'M','F')


dados2$tempo <- as.integer(dados2$tempo)
dados2$IDADE <- as.integer(dados2$IDADE)
dados2$censura <- as.integer(dados2$censura)


dados2 <- dados2 %>% mutate_at(c('TONTURAREG', 'SANGRESREG', 'HIPOTENSAOREG', 'MIALGIASREG',
                    'SINAISHEMOREG','RESPMECANREG'), as.logical)

str(dados2)



head(dados2)
str(dados2)


# Modelo de COX com os dados2
fit <- coxph(Surv(tempo,censura)~ IDADE + SEXO + TONTURAREG + SANGRESREG + HIPOTENSAOREG + MIALGIASREG + RSRESREG +
                SINAISHEMOREG + RESPMECANREG,
            data=dados2)
summary(fit)


require(smcure)
# Deixar sempre a idade e o sexo e testar as variáveis uma a uma
pd_idade <- smcure(Surv(tempo,censura)~IDADE,
            cureform = ~IDADE,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_idade)

# converge
pd_SEXOREG <- smcure(Surv(tempo,censura) ~ SEXO,
            cureform = ~ SEXO,
            data = dados2, model = 'ph', nboot = 150)
printsmcure(pd_SEXOREG)


pd_TONTURAREG <- smcure(Surv(tempo,censura)~TONTURAREG,
            cureform = ~TONTURAREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_TONTURAREG)

# converge
pd_SANGRESREG <- smcure(Surv(tempo,censura)~SANGRESREG,
            cureform = ~SANGRESREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_SANGRESREG)

# converge
pd_HIPOTENSAOREG <- smcure(Surv(tempo,censura)~HIPOTENSAOREG,
            cureform = ~HIPOTENSAOREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_HIPOTENSAOREG)


pd_MIALGIASREG <- smcure(Surv(tempo,censura)~MIALGIASREG,
            cureform = ~MIALGIASREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_MIALGIASREG)


pd_SINAISHEMOREG <- smcure(Surv(tempo,censura)~SINAISHEMOREG,
            cureform = ~MIALGIASREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_SINAISHEMOREG)

## Apresenta "NA"
pd_RESPMECANREG <- smcure(Surv(tempo,censura)~RESPMECANREG,
            cureform = ~RESPMECANREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_RESPMECANREG)
table(dados2$RESPMECANREG)

# Modelo de cura como se fosse uma Reg LOGISTICA
# Modelo de sobrevida como se fosse COX

## Smcure com todas variáveis
colnames(dados2)
head(dados2)
str(dados2)
dados2


pd_todas_variaveis <- smcure(Surv(tempo,censura) ~ IDADE + TONTURAREG + SANGRESREG + HIPOTENSAOREG + MIALGIASREG +
                SINAISHEMOREG + RESPMECANREG,
            cureform = ~ IDADE + TONTURAREG + SANGRESREG + HIPOTENSAOREG + MIALGIASREG +
                SINAISHEMOREG + RESPMECANREG,
            data = dados2, model = 'ph', nboot = 400)

pd_todas_variaveis <- smcure(Surv(tempo,censura) ~ SANGRESREG+MIALGIASREG,
            cureform = ~ IDADE+SANGRESREG+MIALGIASREG,
            data = dados2, model = 'ph', nboot = 300)


pd_ph_hipotensao <- smcure(Surv(tempo,censura)~IDADE+SEXO+TONTURAREG+SANGRESREG+MIALGIASREG+HIPOTENSAOREG,
            cureform = ~ IDADE+SEXO+TONTURAREG+SANGRESREG+MIALGIASREG,
            data = dados2, model = 'ph', nboot = 300)
printsmcure(pd_ph_hipotensao)


pd_ph <- smcure(Surv(tempo,censura)~IDADE+SEXO+TONTURAREG+SANGRESREG+MIALGIASREG,
            cureform = ~ IDADE+SEXO+TONTURAREG+SANGRESREG+MIALGIASREG,
            data = dados2, model = 'ph', nboot = 300)
printsmcure(pd_ph)


pd_ph <- smcure(Surv(tempo,censura)~IDADE+SEXO+TONTURAREG+SANGRESREG+HIPOTENSAOREG,
            cureform = ~ IDADE+SEXO+TONTURAREG+SANGRESREG,
            data = dados2, model = 'ph', nboot = 300)
printsmcure(pd_ph)


pd_teste <- smcure(Surv(tempo,censura) ~ SANGRESREG+MIALGIASREG,
            cureform = ~ IDADE+SANGRESREG+MIALGIASREG,
            data = dados2, model = 'ph', nboot = 300)
printsmcure(pd_teste)

# Sangramento e tontura estão confundindo o modelo
# Possivelmente colineares

table(dados2$SANGRESREG, dados2$TONTURAREG)
chisq.test(table(dados2$SANGRESREG, dados2$TONTURAREG))
fisher.test(table(dados2$SANGRESREG, dados2$TONTURAREG))

chisq.test(table(dados2$SANGRESREG, dados2$MIALGIASREG))
fisher.test(table(dados2$SANGRESREG, dados2$MIALGIASREG))


## Plotar as curvas
# Exemplo do pacote

## Utilizou o cbind para comtemplar todas as variáveis do ajuste

# predf = predictsmcure(pd, newX = cbind(c(1,0),c(0,0),c(0.579,0.579)),
#     newZ = cbind(c(1,0),c(0,0),c(0.579,0.579)), model = 'ph')
# plotpredictsmcure(predf, model='ph')

# predf=predictsmcure(pd,newX = cbind(c(1,0),c(1,1),c(0.579,0.579)),
#     newZ=cbind(c(1,0),c(1,1),c(0.579,0.579)),model=“ph”)
# plotpredictsmcure(predf,model=“ph”)

par(mfrow = c(1,1))

median(dados2$IDADE)
predf_idade = predictsmcure(pd_idade, newX = c(33,33),
    newZ = c(33,33), model = 'ph')
plotpredictsmcure(predf_idade, model='ph')


predf_SEXOREG = predictsmcure(pd_SEXOREG, newX = c(0,1),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_SEXOREG, model='ph')


predf_TONTURAREG = predictsmcure(pd_TONTURAREG, newX = c(0,1),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_TONTURAREG, model='ph')


predf_SANGRESREG = predictsmcure(pd_SANGRESREG, newX = c(0,1),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_SANGRESREG, model='ph')


predf_MIALGIASREG = predictsmcure(pd_MIALGIASREG, newX = c(0,1),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_MIALGIASREG, model='ph')



predf_ph = predictsmcure(pd_ph, newX = cbind(c(33,33),c(0,0),c(0,1),c(0,1),c(0,1)),
    newZ = cbind(c(33,33),c(0,0),c(0,1),c(0,1),c(0,1)), model = 'ph')
plotpredictsmcure(predf_ph, model='ph')


predf_ph = predictsmcure(pd_ph, newX = cbind(c(33,33),c(1,1),c(0,1),c(0,1),c(0,1)),
    newZ = cbind(c(33,33),c(1,1),c(0,1),c(0,1),c(0,1)), model = 'ph')
plotpredictsmcure(predf_ph, model='ph')




par(mfrow = c(3,2))
plotpredictsmcure(predf_idade, model='ph')
plotpredictsmcure(predf_SEXOREG, model='ph')
plotpredictsmcure(predf_TONTURAREG, model='ph')
plotpredictsmcure(predf_SANGRESREG, model='ph')
plotpredictsmcure(predf_MIALGIASREG, model='ph')


par(mfrow = c(1,1))
plotpredictsmcure(predf_ph, model='ph')


## Kaplan meyer do sex M e F
## Pegar um sub grupo dos meus dados, e testar um kaplan meyer

## Comparar os mundos perfeitos e não perfeitos

## Sexos M e F com sang 0 e tont 0
## Sexos M e F com sang 1 e tont 1


dados3 <- dados[,c('tempo', 'censura','IDADE', 'SEXOREG', 'TONTURAREG', 'SANGRESREG', 'HIPOTENSAOREG', 'MIALGIASREG')]

dadosMasc <- filter(dados3, SEXOREG == 0)
dadosFem <- filter(dados3, SEXOREG == 1)


ekm_dadosMasc <- survfit(Surv(tempo,censura) ~ SEXOREG, data = dadosMasc)
summary(ekm_dadosMasc)
ggsurvplot(ekm_dadosMasc, linetype = 1, xlab = 'Tempo', ylab = 'Probabilidade de sobrevivência')

ekm_dadosFem <- survfit(Surv(tempo,censura) ~ SEXOREG, data = dadosFem)
summary(ekm_dadosFem)
ggsurvplot(ekm_dadosFem, linetype = 1, xlab = 'Tempo', ylab = 'Probabilidade de sobrevivência')


par(mfrow = c(1,1))

par(mfrow = c(2,2))
predf_ph1 = predictsmcure(pd_ph, newX = cbind(c(33,33),c(0,0),c(0,0),c(0,0),c(0,0)),
    newZ = cbind(c(33,33),c(0,0),c(0,0),c(0,0),c(0,0)), model = 'ph')
plotpredictsmcure(predf_ph1, model='ph')

predf_ph2 = predictsmcure(pd_ph, newX = cbind(c(33,33),c(1,1),c(0,0),c(0,0),c(0,0)),
    newZ = cbind(c(33,33),c(1,1),c(0,0),c(0,0),c(0,0)), model = 'ph')
plotpredictsmcure(predf_ph2, model='ph')

predf_ph3 = predictsmcure(pd_ph, newX = cbind(c(33,33),c(0,0),c(1,1),c(1,1),c(1,1)),
    newZ = cbind(c(33,33),c(0,0),c(1,1),c(1,1),c(1,1)), model = 'ph')
plotpredictsmcure(predf_ph3, model='ph')

predf_ph4 = predictsmcure(pd_ph, newX = cbind(c(33,33),c(1,1),c(1,1),c(1,1),c(1,1)),
    newZ = cbind(c(33,33),c(1,1),c(1,1),c(1,1),c(1,1)), model = 'ph')
plotpredictsmcure(predf_ph4, model='ph')

ggsurvplot(ekm_dadosFem, linetype = 1, xlab = 'Tempo', ylab = 'Probabilidade de sobrevivência')





predf_ph4 = predictsmcure(pd_ph, newX = cbind(c(33,33),c(0,1),c(0,1),c(0,1),c(0,1)),
    newZ = cbind(c(33,33),c(1,0),c(1,0),c(1,0),c(1,0)), model = 'ph')
plotpredictsmcure(predf_ph4, model='ph')


table(dados2$SEXOREG)









## Tempo de falha acelerado

pd_aft <- smcure(Surv(tempo,censura)~IDADE,
            cureform = ~IDADE,
            data = dados2, model = 'aft', nboot = 200)


smcure(formula = Surv(Time, Status) ~ TRT, cureform = ~TRT, data = bmt,
   model = 'aft', nboot = 200)


printsmcure(pd_aft)

help(smsurv)




help(bmt)
dados <- data(e1684)
summary(dados)

1 - exp(1.007354+0.427327)/(1+exp(1.007354+0.427327))
