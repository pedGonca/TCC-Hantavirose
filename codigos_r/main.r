rm(list = ls())

# Pacotes
library(tidyverse)
library(survival)
library(survminer)
library(ggfortify)
library(knitr)
library(gridExtra)


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

aux=dados$tempo>100
dados$tempo[aux] <- as.numeric(as.Date(dados$DATAENCERR[aux],"%d/%m/%Y")-as.Date(dados$DATA1SINT[aux],"%d/%m/%Y"))

aux <- dados$tempo>100
dados$tempo[aux]<- 80
dados$tempo <- dados$tempo[dados$tempo > 0]

dados[dados$tempo < 0]

dados <- filter(dados, tempo >= 1)


# Criando a variável de censura
# 0 para censura e 1 para os pacientes com data de óbito registrada
dados$censura = ifelse(dados$CASOCONTROLE == 1,1,0)


# Categorizando a idade. Será categorizado de 25 em 25 anos
dados$IDADE_CAT <- ""
dados$IDADE_CAT[dados$IDADE < 20] <- '[0;20]'
dados$IDADE_CAT[dados$IDADE >= 20 & dados$IDADE <= 29] <- '[20;29]'
dados$IDADE_CAT[dados$IDADE >= 30 & dados$IDADE <= 39] <- '[30;39]'
dados$IDADE_CAT[dados$IDADE >= 40 & dados$IDADE <= 49] <- '[40;49]'
dados$IDADE_CAT[dados$IDADE >= 50 & dados$IDADE <= 59] <- '[50;59]'
dados$IDADE_CAT[dados$IDADE >= 60] <- '[60['




### Ajustando o modelos com o pacote smcure
# dados2 contém as variáveis IDADE, SEXOREG, TONTURAREG, CEFALEIAREG, SANGRESREG, DISPNEIAREG, HIPOTENSAOREG, MIALGIASREG, 
# RSRESREG, SINAISHEMOREG, INTERNACAOREG, EDEMAPULMREG, AUMENTOUREREG
dados2 <- dados[,c('tempo', 'censura','IDADE_CAT','IDADE', 'SEXO12','TONTURAREG', 
                    'CEFALEIAREG', 'SANGRESREG', 'DISPNEIAREG','HIPOTENSAOREG',
                    'MIALGIASREG','RSRESR_BACKUP','RSRESREG1','RSRESREG2','RSRESREG3','SINAISHEMOREG','INTERNACAOREG',
                    'DIARREIAREG','RESPMECANREG')]

#dados2$SEXO = ifelse(dados2$SEXO12 == 1,'M','F')

dados2$tempo <- as.integer(dados2$tempo)
dados2$censura <- as.integer(dados2$censura)

str(dados2)
head(dados2)


# Criando um dados3 retirando a obervação com tempo = 80, que atrapalha o Kaplan Meier.
dados3 <- filter(dados2, tempo <= 70)
str(dados3)

## Kaplan Meier

list_survplot1 <- list()
list_survplot2 <- list()
list_survplot3 <- list()
list_survplot4 <- list()
list_survplot5 <- list()
list_survplot6 <- list()
list_survplot7 <- list()

# Idade
ekm <- survfit(Surv(tempo,censura) ~ 1,
            data=dados3, type=c("kaplan-meier")) 
list_survplot1[[1]] <- ggsurvplot(ekm, pval = TRUE,
                ggtheme = theme_minimal()) 

ekm_IDADE_CAT <- survfit(Surv(tempo,censura) ~ IDADE_CAT, data=dados3)
list_survplot1[[2]] <- ggsurvplot(ekm_IDADE_CAT, pval = TRUE,
                legend.labs = c("0 a 20 anos", "20 a 29 anos", "30 a 39 anos",
                "40 a 49 anos", "50 a 59 anos", "Maior ou igual a 60 anos"),
                ggtheme = theme_minimal()) + ggtitle('Idade')

# Idade
ekm_SEXO12 <- survfit(Surv(tempo,censura) ~ SEXO12, data=dados3)
list_survplot2[[1]] <- ggsurvplot(ekm_SEXO12, pval = TRUE,
                legend.labs = c("F", "M"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Sexo')

# TONTURAREG
ekm_TONTURAREG <- survfit(Surv(tempo,censura) ~ TONTURAREG, data=dados3)
list_survplot2[[2]] <-ggsurvplot(ekm_TONTURAREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Tontura')

# CEFALEIAREG
ekm_CEFALEIAREG <- survfit(Surv(tempo,censura) ~ CEFALEIAREG, data=dados3)
list_survplot3[[1]] <- ggsurvplot(ekm_CEFALEIAREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Cefaleia')

# SANGRESREG
ekm_SANGRESREG <- survfit(Surv(tempo,censura) ~ SANGRESREG, data=dados3)
list_survplot3[[2]] <- ggsurvplot(ekm_SANGRESREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Sangramento Respiratório')

# DISPNEIAREG
ekm_DISPNEIAREG <- survfit(Surv(tempo,censura) ~ DISPNEIAREG, data=dados3)
list_survplot4[[1]] <- ggsurvplot(ekm_DISPNEIAREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Dispneia')

# HIPOTENSAOREG
ekm_HIPOTENSAOREG <- survfit(Surv(tempo,censura) ~ HIPOTENSAOREG, data=dados3)
list_survplot4[[2]] <- ggsurvplot(ekm_HIPOTENSAOREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Hipotensão')

# MIALGIASREG
ekm_MIALGIASREG <- survfit(Surv(tempo,censura) ~ MIALGIASREG, data=dados3)
list_survplot5[[1]] <- ggsurvplot(ekm_MIALGIASREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Mialgia')

# RSRESR_BACKUP
ekm_RSRESR_BACKUP <- survfit(Surv(tempo,censura) ~ RSRESR_BACKUP, data=dados3)
list_survplot5[[2]] <- ggsurvplot(ekm_RSRESR_BACKUP, pval = TRUE,
                legend.labs = c("Guarapuava", "Irati", "Outros","União da Vitória"),
                ggtheme = theme_minimal()) + ggtitle('Regional de Saúde')

# SINAISHEMOREG
ekm_SINAISHEMOREG <- survfit(Surv(tempo,censura) ~ SINAISHEMOREG, data=dados3)
list_survplot6[[1]] <- ggsurvplot(ekm_SINAISHEMOREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Sinais Hemorrágicos')

# INTERNACAOREG
ekm_INTERNACAOREG <- survfit(Surv(tempo,censura) ~ INTERNACAOREG, data=dados3)
list_survplot6[[2]] <- ggsurvplot(ekm_INTERNACAOREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Internação')

# DIARREIAREG
ekm_DIARREIAREG <- survfit(Surv(tempo,censura) ~ DIARREIAREG, data=dados3)
list_survplot7[[1]] <- ggsurvplot(ekm_DIARREIAREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Diarreia')

# RESPMECANREG
ekm_RESPMECANREG <- survfit(Surv(tempo,censura) ~ RESPMECANREG, data=dados3)
list_survplot7[[2]] <- ggsurvplot(ekm_RESPMECANREG, pval = TRUE,
                legend.labs = c("N", "S"),
                conf.int = TRUE,
                ggtheme = theme_minimal()) + ggtitle('Respirador Mecânico')




##
require(smcure)
# Modelo de fração de cura uma a uma
pd_idade <- smcure(Surv(tempo,censura)~IDADE,
            cureform = ~IDADE,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_idade)


pd_SEXOREG <- smcure(Surv(tempo,censura) ~ SEXO12,
            cureform = ~ SEXO12,
            data = dados2, model = 'ph', nboot = 150)
printsmcure(pd_SEXOREG) ##


pd_TONTURAREG <- smcure(Surv(tempo,censura)~TONTURAREG,
            cureform = ~TONTURAREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_TONTURAREG)


pd_CEFALEIAREG <- smcure(Surv(tempo,censura)~CEFALEIAREG,
            cureform = ~CEFALEIAREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_CEFALEIAREG)


# converge
pd_SANGRESREG <- smcure(Surv(tempo,censura)~SANGRESREG,
            cureform = ~SANGRESREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_SANGRESREG)


# converge
pd_DISPNEIAREG <- smcure(Surv(tempo,censura)~DISPNEIAREG,
            cureform = ~DISPNEIAREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_DISPNEIAREG)


# converge
pd_HIPOTENSAOREG <- smcure(Surv(tempo,censura)~HIPOTENSAOREG,
            cureform = ~HIPOTENSAOREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_HIPOTENSAOREG)


pd_MIALGIASREG <- smcure(Surv(tempo,censura)~MIALGIASREG,
            cureform = ~MIALGIASREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_MIALGIASREG)


pd_RSRESREG <- smcure(Surv(tempo,censura)~ RSRESREG1 + RSRESREG2 + RSRESREG3,
            cureform = ~ RSRESREG1 + RSRESREG2 + RSRESREG3,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_RSRESREG) ##


pd_SINAISHEMOREG <- smcure(Surv(tempo,censura)~SINAISHEMOREG,
            cureform = ~SINAISHEMOREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_SINAISHEMOREG)


pd_INTERNACAOREG <- smcure(Surv(tempo,censura)~INTERNACAOREG,
            cureform = ~INTERNACAOREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_INTERNACAOREG) ##

pd_DIARREIAREG <- smcure(Surv(tempo,censura)~DIARREIAREG,
            cureform = ~DIARREIAREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_DIARREIAREG)


pd_RESPMECANREG <- smcure(Surv(tempo,censura)~RESPMECANREG,
            cureform = ~RESPMECANREG,
            data = dados2, model = 'ph', nboot = 200)
printsmcure(pd_RESPMECANREG)


###########################
## Modelo com todas as variáveis
pd_final <- smcure(Surv(tempo,censura) ~ IDADE + SEXO12 + TONTURAREG + CEFALEIAREG + SANGRESREG + DISPNEIAREG +
                    HIPOTENSAOREG + MIALGIASREG + RSRESREG1 + RSRESREG2 + RSRESREG3 + SINAISHEMOREG + INTERNACAOREG +
                    DIARREIAREG + RESPMECANREG,
            cureform = ~ IDADE + SEXO12 + TONTURAREG + CEFALEIAREG + SANGRESREG + DISPNEIAREG +
                    HIPOTENSAOREG + MIALGIASREG + RSRESREG1 + RSRESREG2 + RSRESREG3 + SINAISHEMOREG + INTERNACAOREG +
                    DIARREIAREG + RESPMECANREG,
            data = dados2, model = 'ph', nboot = 500)
printsmcure(pd_final)


pd_final <- smcure(Surv(tempo,censura) ~ IDADE + SEXO12 + TONTURAREG + CEFALEIAREG + SANGRESREG + DISPNEIAREG +
                    HIPOTENSAOREG + MIALGIASREG + RSRESREG1 + RSRESREG2 + RSRESREG3 + SINAISHEMOREG + INTERNACAOREG +
                    DIARREIAREG + RESPMECANREG,
            cureform = ~ IDADE + SEXO12 + TONTURAREG + CEFALEIAREG + SANGRESREG + DISPNEIAREG +
                    HIPOTENSAOREG + MIALGIASREG + RSRESREG1 + RSRESREG2 + RSRESREG3 + SINAISHEMOREG + INTERNACAOREG +
                    DIARREIAREG + RESPMECANREG,
            data = dados2, model = 'ph', nboot = 500)
printsmcure(pd_final)




pd_final <- smcure(Surv(tempo,censura) ~ IDADE + SEXO12 + TONTURAREG + SANGRESREG +
                    MIALGIASREG + RSRESREG1 + RSRESREG2 + RSRESREG3 + SINAISHEMOREG + INTERNACAOREG +
                    DIARREIAREG + RESPMECANREG,
            cureform = ~ IDADE + SEXO12 + TONTURAREG + SANGRESREG + DISPNEIAREG +
                    HIPOTENSAOREG + DIARREIAREG + RESPMECANREG,
            data = dados2, model = 'ph', nboot = 1000)
printsmcure(pd_final)



pd_ph <- smcure(Surv(tempo,censura)~SEXO12+SINAISHEMOREG+RESPMECANREG+CEFALEIAREG+HIPOTENSAOREG,
            cureform = ~SEXO12+TONTURAREG+SANGRESREG+SINAISHEMOREG+CEFALEIAREG,
            data = dados3, model = 'ph', nboot = 500)
printsmcure(pd_ph)
###########################


##################
## É ELEEEE
## É TETRAAAAA
pd_ph_final <- smcure(Surv(tempo,censura)~IDADE+SEXO12+TONTURAREG+SANGRESREG+RESPMECANREG,
            cureform = ~IDADE+SEXO12+TONTURAREG+SANGRESREG+RESPMECANREG,
            data = dados2, model = 'ph', nboot = 200)
##################


dados2 <- dados[,c('tempo', 'censura','IDADE_CAT','IDADE', 'SEXO12','TONTURAREG', 
                    'CEFALEIAREG', 'SANGRESREG', 'DISPNEIAREG','HIPOTENSAOREG',
                    'MIALGIASREG','RSRESREG1','RSRESREG2','RSRESREG3','SINAISHEMOREG','INTERNACAOREG',
                    'DIARREIAREG','RESPMECANREG')]



predf_ph1 = predictsmcure(pd_SEXOREG, newX = cbind(c(0,33)),
    newZ = cbind(c(0,33)), model = 'ph')
plotpredictsmcure(predf_ph1, model='ph')
