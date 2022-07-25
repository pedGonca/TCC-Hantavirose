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


# Criando a variável de censura
# 0 para censura e 1 para os pacientes com data de óbito registrada
dados$censura = ifelse(dados$CASOCONTROLE == 1,1,0)

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

# Alteradas e padronizadas todas as variáveis, temos:
summary(dados)

kable(table(dados$tempo))## Quanto maior o tempo, menor a quantidade de observações na variável
kable(prop.table(table(dados$tempo))) ##


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
dados$IDADE_CAT[dados$IDADE <= 25] <- '[0;25]'
dados$IDADE_CAT[dados$IDADE > 25 & dados$IDADE <= 50] <- ']25;50]'
dados$IDADE_CAT[dados$IDADE > 25 & dados$IDADE <= 50] <- ']25;50]'
dados$IDADE_CAT[dados$IDADE > 50] <- '[50['


ekm_IDADE_CAT <- survfit(Surv(tempo,censura) ~ IDADE_CAT, data=dados)
summary(ekm_IDADE_CAT)
ggsurvplot(ekm_IDADE_CAT)

# SEXOREG
ekm_SEXOREG <- survfit(Surv(tempo,censura) ~ SEXOREG, data=dados)
summary(ekm_SEXOREG)
ggsurvplot(ekm_SEXOREG)
table(dados$SEXOREG)
mean(dados$tempo[dados$SEXOREG == 1])

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
            SINAISHEMOREG+HEMMAIOR46REG + LEUCCDEREG + AUMENTOUREREG + DERPLEURALREG + INFPULDIFREG + EDEMAPULMREG + 
            ZONALOCINF, data = dados)
summary(fit)

## Testei os modelos retirando as não significativas e não relevantes para o estudo

fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,
            data=dados)
summary(fit) # Tentei rodar com a idade categórica, mas o resultado foi menos satisfatório


### Ajustando o modelos com o pacote smcure
# dados2 contém as variáveis IDADE, SEXOREG, TONTURAREG, SANGRESREG e HIPOTENSAOREG
dados2 <- dados[,c(151,152,21,14,51,62,82)]

dados2$tempo <- as.integer(dados2$tempo)
dados2$censura <- as.integer(dados2$censura)
str(dados2)


dados2 <- dados2 %>% mutate_at(c("SEXOREG", "TONTURAREG", "SANGRESREG", "HIPOTENSAOREG"), as.logical)

head(dados2)
str(dados2)


# Modelo de COX com os dados2
fit <- coxph(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG+HIPOTENSAOREG,
            data=dados2)
summary(fit)


require(smcure)
# Deixar sempre a idade e o sexo e testar as variáveis uma a uma

pd_idade <- smcure(Surv(tempo,censura)~IDADE,
            cureform = ~IDADE,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_idade)

pd_SEXOREG <- smcure(Surv(tempo,censura)~SEXOREG,
            cureform = ~SEXOREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_SEXOREG)

pd_TONTURAREG <- smcure(Surv(tempo,censura)~TONTURAREG,
            cureform = ~TONTURAREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_TONTURAREG)

pd_SANGRESREG <- smcure(Surv(tempo,censura)~SANGRESREG,
            cureform = ~SANGRESREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_SANGRESREG)


pd_ph <- smcure(Surv(tempo,censura)~IDADE+SEXOREG+TONTURAREG+SANGRESREG,
            cureform = ~IDADE+SEXOREG+TONTURAREG+SANGRESREG,
            data = dados2, model = 'ph', nboot = 200)

printsmcure(pd_ph)



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


predf_SEXOREG = predictsmcure(pd_SEXOREG, newX = c(1,0),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_SEXOREG, model='ph')


predf_TONTURAREG = predictsmcure(pd_TONTURAREG, newX = c(0,1),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_TONTURAREG, model='ph')


predf_SANGRESREG = predictsmcure(pd_SANGRESREG, newX = c(0,1),
    newZ = c(0,1), model = 'ph')
plotpredictsmcure(predf_SANGRESREG, model='ph')


predf_ph = predictsmcure(pd_ph, newX = cbind(c(33,33),c(0,0),c(0,1),c(0,1)),
    newZ = cbind(c(33,33),c(0,0),c(0,1),c(0,1)), model = 'ph')
plotpredictsmcure(predf_ph, model='ph')


predf_ph = predictsmcure(pd_ph, newX = cbind(c(33,33),c(1,1),c(0,1),c(0,1)),
    newZ = cbind(c(33,33),c(1,1),c(0,1),c(0,1)), model = 'ph')
plotpredictsmcure(predf_ph, model='ph')



par(mfrow = c(2,2))
plotpredictsmcure(predf_idade, model='ph', type = 'S')
plotpredictsmcure(predf_SEXOREG, model='ph')
plotpredictsmcure(predf_TONTURAREG, model='ph')
plotpredictsmcure(predf_SANGRESREG, model='ph')

par(mfrow = c(1,1))
plotpredictsmcure(predf_ph, model='ph')

# Resultados estranho para idade maior de 40 anos 












## Tempo de falha acelerado

pd_aft <- smcure(Surv(tempo,censura)~IDADE,
            cureform = ~IDADE,
            data = dados3, model = 'aft', nboot = 200)


smcure(formula = Surv(Time, Status) ~ TRT, cureform = ~TRT, data = bmt,
   model = 'aft', nboot = 200)


printsmcure(pd_aft)

help(smsurv)



## Kaplan meyer do sex M e F
## Pegar um sub grupo dos meus dados, e testar um kaplan meyer

## Comparar os mundos perfeitos e não perfeitos

## Sexos M e F com sang 0 e tont 0
## Sexos M e F com sang 1 e tont 1
