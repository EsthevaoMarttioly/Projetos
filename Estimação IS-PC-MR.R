#### Pacotes ####

library(readxl)
library(writexl)
library(zoo)
library(tseries)
library(urca)
library(dynlm)
library(mFilter)
library(lmtest)
library(dplyr)
library(stargazer)

setwd("C:/Users/vfxbl/OneDrive/Área de Trabalho/MacroEmerg")


#### Curva IS ####

## Hiato da função de produção, média ponderada entre emprego e utilização da capacidade

hiatopnad = read_excel("hiatopnad.xlsx", sheet = "data")

pnad = ts(hiatopnad$pnad, start = c(1999, 1), end = c(2022, 2), frequency = 4)
uci = ts(hiatopnad$uci, start = c(1999, 1), end = c(2022, 2), frequency = 4)

# Hiato do Emprego e da Capacidade Ociosa

pnad = log(1 - pnad/100)
pnad_hp = hpfilter(pnad, freq = 1600)

uci = log(uci)
uci_hp = hpfilter(uci, freq = 1600)

# Função de produção

fp = 0.6*(pnad_hp$cycle) + 0.4*(uci_hp$cycle)

fp = ts(fp, start = c(1999, 1), end = c(2022, 2), frequency = 4)

par(mfrow = c(1,1))
ts.plot(pnad_hp$cycle, uci_hp$cycle, fp,
        gpars  = list(col = c('black', 'blue', 'red'), lwd = c(2,2)),
        main = "Desemprego, Capacidade Ociosa e Função de Produção")

## Função de produção com interferência

hiatonairu = read_excel("nairu.xlsx", sheet = "data")

naicu = ts(hiatonairu$NAICU, start = c(1999, 1), end = c(2022, 2), frequency = 4)
nairu = ts(hiatonairu$NAIRU, start = c(1999, 1), end = c(2022, 2), frequency = 4)

fpl = 0.6*(pnad - nairu) + 0.4*(uci - log(naicu))

fpl = ts(fpl, start = c(1999, 1), end = c(2022, 2), frequency = 4)

# Ambos os hiatos e correlação

ts.plot(fp, fpl, gpars  = list(col = c('black', 'blue'), lwd = c(2, 2)))

cor(fp, fpl)[1]


## Hiato hp com extensão por projeção do Focus

pibe = read_excel("pibbr1e.xlsx", sheet = "data")

pibe = ts(pibe$PIB_BR_EX, start = c(1999, 1), end = c(2025,4), frequency = 4)

logpibe = log(pibe)

pibe_hp = hpfilter(logpibe, freq = 1600)

pib = window(pibe_hp$cycle, start = c(1999, 1), end = c(2022, 2))
pib = ts(pib, start = c(1999, 1), end = c(2022, 2), frequency = 4)

ts.plot(pib, fpl, fp, gpars  = list(col = c('black', 'blue', 'red'), lwd = c(2, 2)))

cor(fp, pib)[1]

## Swap Real, Índice de Confiança e Resultado Primário - apenas após 2003

please = read_excel("please.xlsx", sheet = "dados")
please = please[please$...1 >= "2003Q1", ]

swapreal = ts(please$LSWAPREL360MAY19, start = c(2003, 1), end = c(2022, 2), frequency = 4)
lconf = ts(please$LCONF19, start = c(2003, 1), end = c(2022, 2), frequency = 4)
primario = ts(please$RESPRIMARIO, start = c(2003, 1), end = c(2022, 2), frequency = 4)

# Dummies para 2003Q4, 2008Q4, 2020Q2, entre 2011Q1 e 2013Q1 e depois de 2015Q1

d0304 = ts(as.numeric(please$...1 == "2003Q4"), start = c(2003, 1),
                  end = c(2022, 2), frequency = 4)

d0804 = ts(as.numeric(please$...1 == "2008Q4"), start = c(2003, 1),
                  end = c(2022, 2), frequency = 4)

d2002 = ts(as.numeric(please$...1 == "2020Q2"), start = c(2003, 1),
                  end = c(2022, 2), frequency = 4)

dtombini = ts(as.numeric(please$...1 <= "2013Q3" & please$...1 >= "2011Q1"),
                     start = c(2003, 1), end = c(2022, 2), frequency = 4)

ddilma = ts(as.numeric(please$...1 >= "2015Q1"),
                   start = c(2003, 1), end = c(2022, 2), frequency = 4)

# Reajustamos as outras variáveis para depois de 2003

pib = window(pib, start = c(2003, 1), end = c(2022, 2), frequency = 4)
fpl = window(fpl, start = c(2003, 1), end = c(2022,2), frequency = 4)
fp = window(fp, start = c(2003, 1), end = c(2022,2), frequency = 4)

pib = ts(pib, start = c(2003, 1), end = c(2022, 2), frequency = 4)
fpl = ts(fpl, start = c(2003, 1), end = c(2022,2), frequency = 4)
fp = ts(fp, start = c(2003, 1), end = c(2022,2), frequency = 4)

# Hiatos adicionais

hiatos = read_excel("dados_hiatos.xlsx", sheet = "dados")
hiatos = hiatos[hiatos$Data >= "2003Q1", ]

hiato_ipea = ts(hiatos$HIATO0822PNAD, start = c(2003, 1), end = c(2022, 2), frequency = 4)
hiato_mkt25 = ts(hiatos$HIATO_MKT25f, start = c(2003, 1), end = c(2022, 2), frequency = 4)
hiato_0822R = ts(hiatos$HIATO_0822R, start = c(2003, 1), end = c(2022, 2), frequency = 4)

ts.plot(hiato_ipea, hiato_mkt25, hiato_0822R,
        gpars  = list(col = c('black', 'blue', 'red'), lwd = c(2, 2)))


# Inflação

phillips = read_excel("phillips.xlsx", sheet = 'dados')
phillips = phillips[phillips$Data > "2003Q1", ]


cambio = ts(phillips$CAMBIO, start = c(2003,1), end = c(2022, 2), frequency = 4)
cambio = log(cambio)
inflacao = ts(phillips$LINF, start = c(2003,1), end = c(2022,2), frequency = 4)
inflivre = ts(phillips$LINFLIVRE, start = c(2003, 1),end = c(2022, 2), frequency = 4)
expect = ts(phillips$LEXPECT, start = c(2003, 1), end = c(2022, 2), frequency = 4)
meta = ts(phillips$LMETA, start = c(2003, 1), end = c(2022, 2), frequency = 4)

ts.plot(inflacao, inflivre, gpars  = list(col = c('red', 'blue'), lwd = c(2, 2)))
legend("topright", legend = c("Inflação", "Inflação Preços Livres"), fill = c("red", "blue"))


#### Curva IS ####

lag = stats::lag

is = dynlm(hiato_0822R ~ lag(swapreal/4, -2) + lag(swapreal/4, -3) + primario + lconf +
             lag(d0804, 1) + lag(hiato_0822R, 1) + d2002 + lag(d2002, 1))

summary(is)


#### Curva de Phillips ####

pc = dynlm(inflivre ~ lag(expect/4 - inflacao, 2) + lag(fp, 1) + lag(d2002, -1))

summary(pc)

plot.ts(pc$residuals)


#### Regra de Taylor ####

mr = dynlm(swapreal/4 ~ lag(swapreal/4, 1) + lag(swapreal/4, 2) +
              lag(expect/4 - meta, 3) + lag(fp, 1))

summary(mr)


#### Sumário ####

par(mfrow = c(1,1))
ts.plot(is$residuals, pc$residuals, mr$residuals,
        gpars  = list(col = c('red', 'black', "blue"), lwd = c(2, 2)))
legend("topleft", legend = c("IS", "PC", "MR"), fill = c("red", "black", "blue"))


par(mfrow = c(2, 3))
acf(is$residuals, lag.max = NULL, main = "Autocorrelacoes IS", ylab = '')
acf(pc$residuals, lag.max = NULL, main = "Autocorrelacoes PC", ylab = '')
acf(mr$residuals, lag.max = NULL, main = "Autocorrelacoes MR", ylab = '')
pacf(is$residuals, lag.max = NULL, main = "Autocorrelacoes Parcial IS", ylab = '')
pacf(pc$residuals, lag.max = NULL, main = "Autocorrelacoes Parcial PC", ylab = '')
pacf(mr$residuals, lag.max = NULL, main = "Autocorrelacoes Parcial MR", ylab = '')
par(mfrow = c(1,1))

stargazer(is, pc, mr)

