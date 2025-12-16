## Autor: Henrique da Cruz ##

#Pacotes
library(tidyverse)
library(readxl)
library(lubridate)
library(seasonal)
library(mFilter)
library(hpfilter)
library(forecast)
library(sidrar)
library(tseries)

#Working directory
setwd("C:/Users/00hcr/Documents/case_sicredi")

#######################################################################################

## Questao 1: ##

#Carregando a serie do PIB trimestral
df <- read_excel("pib_trimestral.xlsx")
View(df)


# Dessazonalizando a serie #

# 1. Processar data
df <- df %>%
  mutate(
    ano = as.numeric(substr(Data, 1, 4)),
    tri = as.numeric(substr(Data, 7, 7))
  )

# 2. Criar série ts trimestral
pib_ts <- ts(df$PIB, start = c(df$ano[1], df$tri[1]), frequency = 4)

# 3. Dessazonalizar com metodo X-13
ajuste <- seas(pib_ts)
pib_dessaz <- final(ajuste)

# 4. Plot
plot(pib_ts, col = "red", main = "PIB trimestral: original vs dessazonalizado")
lines(pib_dessaz, col = "blue", lwd = 2)
legend("topleft", legend = c("Original", "Dessazonalizado"),
       col = c("red", "blue"), lty = 1, bty = "n")


# Estimando o hiato do produto #

#Filtro HP unilateral
pib_dessaz <- as.data.frame(pib_dessaz)
pib_trend <- hp1(pib_dessaz, 1600)

#Hiato do produto
hiato <- ((pib_dessaz - pib_trend)/pib_trend)*100

hiato <- ts(hiato) #time series

#Plot do PIB e da tendencia
plot(ts(pib_trend), col = "red", main = "PIB vs Tendência")
lines(ts(pib_dessaz), col = "blue", lwd = 2)
legend("topleft", legend = c("Tendência", "PIB"),
       col = c("red", "blue"), lty = 1, bty = "n")

View(hiato)

#Plot do hiato
plot(hiato, col = "blue", main = "Hiato do Produto (Trimestral)",
     lwd = 2,
     xlab = "Período", ylab = "Hiato (%)")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Ajustando um modelo para o hiato do produto #
auto.arima(hiato, d = 0)
fit <- auto.arima(hiato, d = 0)

summary(fit) #analisando os coeficientes

#Plot (hiatos vs fitted)
plot(hiato, col = "red", main = "Hiato vs Ajuste")
lines(ts(fit$fitted), col = "blue", lwd = 2)
legend("topleft", legend = c("Hiato", "Ajuste"),
       col = c("red", "blue"), lty = 1, bty = "n")

acf(residuals(fit)) #ruidos sao white noise

# Avaliando a capacidade preditiva do modelo #

# tamanho total da série
T <- length(hiato)

# tamanho mínimo da amostra inicial de treino
T0 <- floor(0.5 * T)

# vetor para armazenar erros de previsão
erros <- ts(rep(NA, T - T0),
            start = time(hiato)[T0 + 1],
            frequency = frequency(hiato))

erros <- tsCV(
  hiato,
  forecastfunction = function(y, h) {
    forecast(Arima(y, order = c(1,0,0)), h = h)
  },
  h = 1
)

#RMSE
RMSE <- sqrt(mean(erros^2, na.rm = TRUE))
print(RMSE)

RMSE / sd(hiato) #RMSE < sd(hiato)

#Fazendo a previsao para 2025 e 2026
fc_gap <- forecast(fit, h = 6)

print(fc_gap)

# 3. Plotar histórico + previsão
plot(fc_gap,
     main = "Previsão do Hiato do Produto (ARIMA)",
     xlab = "Tempo",
     ylab = "Hiato (%)")

# 4. Adicionar linha horizontal no zero
abline(h = 0, col = "gray50", lwd = 2, lty = 2)

#######################################################################################

## Questao 2: ##

library(rbcb)
library(dplyr)
library(rbcb)
library(quantmod)
library(lubridate)
library(urca)
library(ipeadatar)
library(vars)
library(zoo)
library(sandwich)
library(tibble)
library(xtable)

# Baixando as series #

# Taxa de cambio (media mensal)
ptax <- get_series(
  code = 3697,
  start_date = "2000-01-01")

ptax <- ptax %>% rename(cambio = "3697")

#Selic
selic <- get_series(
  code = 4189,
  start_date = "2000-01-01")

selic <- selic[1:(nrow(selic) - 2), ]
selic <- selic %>% rename(selic = "4189")


#Fed Funds rate
getSymbols("FEDFUNDS", src = "FRED")

fedfunds <- FEDFUNDS %>%
  as.data.frame() %>%
  mutate(date = as.Date(rownames(.))) %>%
  rename(fedfunds = FEDFUNDS)

fedfunds <- fedfunds %>%
  filter(year(date) >= 2000) #filtra o ano

fedfunds <- fedfunds[1:(nrow(fedfunds) - 1), ]

#Diferencial de juros
juros <- full_join(selic, fedfunds, by = "date") %>%
  arrange(date) %>%
  mutate(
    dif_juros = selic - fedfunds
  )

#IPCA
inflacao_br <- get_series(
  code = 433,
  start_date = "2000-01-01"
) %>%
  rename(inflacao_br = `433`)

inflacao_br <- inflacao_br[1:(nrow(inflacao_br) - 1), ]

#CPI
getSymbols("CPIAUCSL", src = "FRED")

cpi_us <- CPIAUCSL %>%
  as.data.frame() %>%
  mutate(date = as.Date(rownames(.))) %>%
  rename(cpi = CPIAUCSL)

# Inflação mensal EUA (variação % m/m)
cpi_us <- cpi_us %>%
  mutate(infl_us = 100 * (cpi / lag(cpi) - 1))

#Diferencial de inflacao
inflacao <- left_join(inflacao_br, cpi_us, by = "date") %>%
  arrange(date) %>%
  mutate(
    dif_inflacao = inflacao_br - infl_us
  )

# VIX
getSymbols("VIXCLS", src = "FRED")

vix <- VIXCLS %>%
  as.data.frame() %>%
  mutate(date = as.Date(rownames(.))) %>%
  rename(vix = VIXCLS)

vix <- vix %>%
  filter(year(date) >= 2000) %>% 
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(vix_media = mean(vix, na.rm = TRUE)) %>% #converte para media mensal
  ungroup() %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")))

vix <- vix[1:(nrow(vix) - 2), ]
vix <- vix[c(3,4)]

#Termos de troca
tt_br <- ipeadata("FUNCEX12_TTR12") %>%
  filter(year(date) >= 2000) %>%
  arrange(date)

tt_br <- tt_br[c(2,3)]
tt_br <- tt_br %>%
  rename(tt = value)

#Juntando as séries
cambio_dados <- inner_join(ptax, juros)
cambio_dados <- inner_join(cambio_dados, inflacao)
cambio_dados <- inner_join(cambio_dados, vix)
cambio_dados <- inner_join(cambio_dados, tt_br)

cambio_dados <- cambio_dados[c(2,5,9,10,11)]

View(cambio_dados)

# Estimando o modelo SVAR #

#Teste de cointegracao (Johansen)
jo <- ca.jo(cambio_dados, type = "trace")
print(summary(jo))

#Estimacao do VAR reduzido
VARselect(cambio_dados, lag.max = 12, type = "const") -> sel # selecionar lags por AIC/BIC
print(sel$selection)

var_lvl <- VAR(cambio_dados, p = 2, type = "const")
summary(var_lvl)

# estabilidade (roots)
roots_var <- roots(var_lvl)
cat("Máximo módulo das raízes (deve ser < 1 para estabilidade):", max(Mod(roots_var)), "\n")

#Identificando o SVAR via restricoes contemporaneas (short-run restrictions):

# Implementamos A matrix com 1s na diagonal e NA para elementos livres, 0 para restrições.

Ainit <- matrix(NA, nrow = 5, ncol = 5)
colnames(Ainit) <- rownames(Ainit) <- c("cambio","dif_juros","dif_inflacao","tt","vix_media")
diag(Ainit) <- 1

# vix exógeno: não é afetado contemporaneamente por le,idiff,dinf,tot
Ainit["vix_media", c("cambio","dif_juros","dif_inflacao","tt")] <- 0
Ainit["dif_juros", c("cambio","dif_inflacao","tt","vix_media")] <- 0
Ainit["cambio", "dif_inflacao"] <- 0

print(Ainit)

#Estimar o SVAR
svar_cambio <- SVAR(var_lvl, estmethod = "direct", Amat = Ainit)

#Matriz A estimada
svar_cambio$A

#IRFs
irf_svar <- irf(svar_cambio, impulse = "cambio",response = "cambio", 
                n.ahead = 24, boot = TRUE, runs = 1000)

plot(irf_svar)

#FEVD
fevd <- fevd(svar_cambio, n.ahead = 24)
fevd_cambio <- fevd$cambio
print(fevd_cambio)

# Forecast usando predict() no VAR (reduced-form)
fc <- predict(var_lvl, n.ahead = 8, ci = 0.95)

# Extrair previsão da variável cambio
fc_cambio <- fc$fcst$cambio
print(fc_cambio)

mean(fc_cambio[c(3,4,5)]) #previsao 1T26

#######################################################################################

## Questao 3: ##

library(tidyverse)
library(fredr)
library(lubridate)
library(zoo)
library(dynlm)
library(scales)
library(ggplot2)
library(stargazer)

# Baixando os dados do FRED #

fredr_set_key("6f2a970b294654fc0319c3da94c57ab0") #api_key

# Importar calendario de reuniões do FOMC
fomc_dates <- read.csv(
  "fomc_meetings_1989_2024.csv",
  stringsAsFactors = FALSE
)

fomc_dates$date <- as.Date(fomc_dates$date)

fomc_dates$month <- month(fomc_dates$date)

start_date <- as.Date("1989-01-01")

# Fed funds rate
ffr <- fredr(
  series_id = "FEDFUNDS",
  observation_start = start_date
)

ffr$i <- ffr$value

# Inflacao esperada
infl_exp <- fredr(
  series_id = "T5YIE",
  observation_start = start_date
)

infl_exp$pi_e <- infl_exp$value

#Hiato do produto
gdp <- fredr(
  series_id = "GDPC1",
  observation_start = start_date
)

gdp$gdp <- gdp$value

gdp_pot <- fredr(
  series_id = "GDPPOT",
  observation_start = start_date
)

gdp_pot$gdp_pot <- gdp_pot$value

gap <- merge(gdp, gdp_pot, by = "date")

gap$output_gap <- 100 * (log(gap$gdp) - log(gap$gdp_pot))

#Base final
df <- merge(ffr, infl_exp, by = "date")
df <- merge(df, gap, by = "date")

df$pi_gap <- df$pi_e - 2
df$i_lag  <- lag(df$i)

df <- na.omit(df)

# Estimação da Regra de Taylor com smoothing
taylor_fl <- lm(
  i ~ i_lag + pi_gap + output_gap,
  data = df
)

summary(taylor_fl)

#Recuperando os coeficientes de longo prazo

rho <- coef(taylor_fl)["i_lag"]

phi_pi_lr <- coef(taylor_fl)["pi_gap"] / (1 - rho)
print(phi_pi_lr)

phi_gap_lr <- coef(taylor_fl)["output_gap"] / (1 - rho)
print(phi_gap_lr)

intercept_lr <- coef(taylor_fl)["(Intercept)"] / (1 - rho)
print(intercept_lr)

View(df)

# Exportar para LaTeX
stargazer(
  taylor_fl,
  type = "latex",
  title = "Resultados da Regressão OLS",
  label = "tab:ols",
  dep.var.labels = "Variável dependente",
  covariate.labels = c("X1", "X2", "X3"),
  omit.stat = c("f", "ser"),
  digits = 3
)

#######################################################################################

