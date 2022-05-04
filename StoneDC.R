                            #############################
                            # STONE DATA CHALLENGE 2022 #
                            #############################



################################################
#                                              #
# Etapa 2 - Compreensão e importação dos dados #
#                                              #
################################################                            

                            
### Definindo a pasta de trabalho ###
setwd("C:/Users/Matheus e Rany/Desktop/DC2022/Dados")

                            
### Confirmando se o caminho da pasta de trabalho foi importado corretamente ###
getwd()


### Carregando o dataset ###

clientes <- read.csv("portfolio_clientes.csv")

comunicados <- read.csv("portfolio_comunicados.csv")

geral <- read.csv("portfolio_geral.csv")

tpv <- read.csv("portfolio_tpv.csv")


### Verificando a importação (número de linhas) e existência de valores nulos ###

nrow(na.omit(clientes))

nrow(na.omit(comunicados))

nrow(na.omit(geral))

nrow(na.omit(tpv))


################################################
#                                              #
#   Etapa 3 - Preparação e limpeza dos dados   #
#                                              #
################################################


### Visualização dos datasets importados ###

View(clientes)
# as colunas cidade, subsegmento e segmento vieram com erro de codificação
# transformação para UTF-8
clientes <- read.csv("portfolio_clientes.csv", fileEncoding = "UTF-8")
View(clientes)

View(comunicados)

View(geral)

View(tpv)


### Resumo das variáveis do dataset ###

str(clientes)

str(comunicados)
# os campos dt_ref_portfolio e data_acao foram importados como strings
# precisamos modificar o tipo do campo para data conforme consta no dicionário de dados
comunicados <- transform(
  comunicados,
  dt_ref_portfolio = as.Date(
    comunicados$dt_ref_portfolio, tryFormats = c("%Y-%m-%d")),
  data_acao = as.Date(
    comunicados$dt_ref_portfolio, tryFormats = c("%Y-%m-%d")));

str(geral)
# os campos dt_ref_portfolio, safra(formato ano-mês),
# dt_contrato, dt_desembolso, dt_vencimento, dt_wo foram importados como strings
# precisamos modificar o tipo do campo para data conforme consta no dicionário de dados
# por causa da coluna safra só conter ano e mês, foi realizado um tratamento diferente
# decidi por adotar o dia respectivo de cada data como 01 e para isso vamos usar a biblioteca zoo
library(zoo)
geral <- transform(
  geral,
  dt_ref_portfolio = as.Date(
    geral$dt_ref_portfolio, tryFormats = c("%Y-%m-%d")),
    dt_contrato = as.Date(
    geral$dt_contrato, tryFormats = c("%Y-%m-%d")),
  dt_desembolso = as.Date(
    geral$dt_desembolso, tryFormats = c("%Y-%m-%d")),
  dt_vencimento = as.Date(
    geral$dt_vencimento, tryFormats = c("%Y-%m-%d")),
  dt_wo = as.Date(
    geral$dt_wo, tryFormats = c("%Y-%m-%d")),
  safra = as.Date(as.yearmon(geral$safra)));
str(geral)

str(tpv)
# o campo dt_transacao foi transformado em data
# para o entendimento e similaridade com outros dados do dataset
# o campo dt_transacao foi importado como integer e no dicionário consta
# como um campo string
tpv <- transform(
  tpv,
  dt_transacao = as.Date(as.character(tpv$dt_transacao), format = "%Y%m%d"));
str(tpv)


##########################################
# MEDIDAS DE RESUMO                      #
# TENDÊNCIA CENTRAL, POSIÇÃO, DISPERSÃO  #
##########################################

### Resumo estatístico de uma ou mais variáveis do dataset ###

summary(clientes)

summary(comunicados)

summary(tpv)
# apresenta outliers em qtd_transacoes e vlr_tpv
# vamos entender como se apresentam e tratar
library(outliers)
library(psych)

# verificando o resumo estatístico das variáveis
summary(tpv[c('qtd_transacoes', 'vlr_tpv')])

# Gráficos

par(mfrow=c(1,2))

# histograma
hist(tpv$qtd_transacoes)
hist(tpv$vlr_tpv)

# boxplot
boxplot(tpv$qtd_transacoes, outline = T)
boxplot(tpv$vlr_tpv, outline = T)

# identificando e removendo outliers (BACON)

install.packages("robustX")
library(robustX)

outliers = robustX::mvBACON(tpv[c("qtd_transacoes" ,"vlr_tpv")],
                            alpha = 0.95, init.sel = "random")

tpv_outliers = within(tpv,
                      {outliers = outliers$subset})
table(tpv_outliers$outliers)
tpv_outliers = subset(tpv_outliers, outliers == TRUE)
tpv_outliers$outliers = NULL
remove(outliers)

# variáveis antes e depois do tratamento
summary(tpv[c('qtd_transacoes', 'vlr_tpv')])
summary(tpv_outliers[c('qtd_transacoes', 'vlr_tpv')])

par(mfrow=c(1,2))

# histograma
hist(tpv$qtd_transacoes)
hist(tpv$vlr_tpv)

hist(tpv_outliers$qtd_transacoes)
hist(tpv_outliers$vlr_tpv)


# boxplot
boxplot(tpv$qtd_transacoes, outline = T)
boxplot(tpv$vlr_tpv, outline = T)

boxplot(tpv_outliers$qtd_transacoes, outline = T)
boxplot(tpv_outliers$vlr_tpv, outline = T)

#winsorização
tpv_outliers <- within(tpv_outliers,
              {qtd_transacoes_w1 <- psych::winsor(tpv_outliers$qtd_transacoes, trim = 0.01)})

tpv_outliers <- within(tpv_outliers,
              {qtd_transacoes_w5 <- psych::winsor(tpv_outliers$qtd_transacoes, trim = 0.05)})

tpv_outliers <- within(tpv_outliers,
              {vlr_tpv_w1 <- psych::winsor(tpv_outliers$vlr_tpv, trim = 0.01)})

tpv_outliers <- within(tpv_outliers,
              {vlr_tpv_w5 <- psych::winsor(tpv_outliers$vlr_tpv, trim = 0.05)})

#antes e depois
summary(tpv_outliers[c("qtd_transacoes", "qtd_transacoes_w1", "qtd_transacoes_w5")])
summary(tpv_outliers[c("vlr_tpv", "vlr_tpv_w1", "vlr_tpv_w5")])

par(mfrow=c(1,3))

#qtd_transacoes#
hist(tpv_outliers$qtd_transacoes)
hist(tpv_outliers$qtd_transacoes_w1)
hist(tpv_outliers$qtd_transacoes_w5)

boxplot(tpv_outliers$qtd_transacoes, outline = T)
boxplot(tpv_outliers$qtd_transacoes_w1, outline = T)
boxplot(tpv_outliers$qtd_transacoes_w5, outline = T)

head(sort(tpv_outliers$qtd_transacoes, decreasing = T), n = 10)
head(sort(tpv_outliers$qtd_transacoes_w1, decreasing = T), n = 10)
head(sort(tpv_outliers$qtd_transacoes_w5, decreasing = T), n = 10)

tail(sort(tpv_outliers$qtd_transacoes, decreasing = T), n = 10)
tail(sort(tpv_outliers$qtd_transacoes_w1, decreasing = T), n = 10)
tail(sort(tpv_outliers$qtd_transacoes_w5, decreasing = T), n = 10)

#vlr_tpv#
hist(tpv_outliers$vlr_tpv)
hist(tpv_outliers$vlr_tpv_w1)
hist(tpv_outliers$vlr_tpv_w5)

boxplot(tpv_outliers$vlr_tpv, outline = T)
boxplot(tpv_outliers$vlr_tpv_w1, outline = T)
boxplot(tpv_outliers$vlr_tpv_w5, outline = T)

head(sort(tpv_outliers$vlr_tpv, decreasing = T), n = 10)
head(sort(tpv_outliers$vlr_tpv_w1, decreasing = T), n = 10)
head(sort(tpv_outliers$vlr_tpv_w5, decreasing = T), n = 10)

tail(sort(tpv_outliers$vlr_tpv, decreasing = T), n = 10)
tail(sort(tpv_outliers$vlr_tpv_w1, decreasing = T), n = 10)
tail(sort(tpv_outliers$vlr_tpv_w5, decreasing = T), n = 10)

# Variância
var(tpv_outliers$qtd_transacoes)
var(tpv_outliers$qtd_transacoes_w1)
var(tpv_outliers$qtd_transacoes_w5)

var(tpv_outliers$vlr_tpv)
var(tpv_outliers$vlr_tpv_w1)
var(tpv_outliers$vlr_tpv_w5)

# Desvio Padrão
sd(tpv_outliers$qtd_transacoes)
sd(tpv_outliers$qtd_transacoes_w1)
sd(tpv_outliers$qtd_transacoes_w5)

cv_qtd_transacoes = 100*sd(tpv_outliers$qtd_transacoes)/mean(tpv_outliers$qtd_transacoes)
cv_qtd_transacoes_w1 = 100*sd(tpv_outliers$qtd_transacoes_w1)/mean(tpv_outliers$qtd_transacoes_w1)
cv_qtd_transacoes_w5 = 100*sd(tpv_outliers$qtd_transacoes_w5)/mean(tpv_outliers$qtd_transacoes_w5)

cv_qtd_transacoes
cv_qtd_transacoes_w1
cv_qtd_transacoes_w5


sd(tpv_outliers$vlr_tpv)
sd(tpv_outliers$vlr_tpv_w1)
sd(tpv_outliers$vlr_tpv_w5)

cv_vlr_tpv = 100*sd(tpv_outliers$vlr_tpv)/mean(tpv_outliers$vlr_tpv)
cv_vlr_tpv_w1 = 100*sd(tpv_outliers$vlr_tpv_w1)/mean(tpv_outliers$vlr_tpv_w1)
cv_vlr_tpv_w5 = 100*sd(tpv_outliers$vlr_tpv_w5)/mean(tpv_outliers$vlr_tpv_w5)

cv_vlr_tpv
cv_vlr_tpv_w1
cv_vlr_tpv_w5

remove(cv_vlr_tpv,
       cv_vlr_tpv_w1,
       cv_vlr_tpv_w5,
       cv_qtd_transacoes,
       cv_qtd_transacoes_w1,
       cv_qtd_transacoes_w5)

# w5 aplicado após a remoção dos outliers apresentou o melhor resultado


summary(geral)
# apresenta outliers em vlr_desembolsado, vlr_tarifa, perc_retencao, vlr_pgto_realizado,
# vlr_pgto_esperado, vlr_saldo_devedor, vlr_saldo_devedor_esperado, dsp, dspp
# vamos entender como se apresentam e tratar
# anotações específicas sobre variáveis:
# vlr_tarifa representa 1% do valor_desembolsado, após ajuste, refazer o cálculo da variável
# juros_diario calculo baseado no juros_mes

par(mfrow=c(1,2))

#prazo
summary(geral$prazo)

hist(geral$prazo)
boxplot(geral$prazo)

sd(geral$prazo)
cv_geral_prazo = 100*sd(geral$prazo)/mean(geral$prazo)

#vlr_desembolsado
summary(geral$vlr_desembolsado)

hist(geral$vlr_desembolsado)
boxplot(geral$vlr_desembolsado)

sd(geral$vlr_desembolsado)
cv_vlr_desembolsado = 100*sd(geral$vlr_desembolsado)/mean(geral$vlr_desembolsado)

#juros_mes
summary(geral$juros_mes)

hist(geral$juros_mes)
boxplot(geral$juros_mes)

sd(geral$juros_mes)
cv_juros_mes = 100*sd(geral$juros_mes)/mean(geral$juros_mes)

#perc_retencao
summary(geral$perc_retencao)

hist(geral$perc_retencao)
boxplot(geral$perc_retencao)

sd(geral$perc_retencao)
cv_perc_retencao = 100*sd(geral$perc_retencao)/mean(geral$perc_retencao)

#vlr_pgto_realizado
summary(geral$vlr_pgto_realizado)

hist(geral$vlr_pgto_realizado)
boxplot(geral$vlr_pgto_realizado)

sd(geral$vlr_pgto_realizado)
cv_perc_retencao = 100*sd(geral$vlr_pgto_realizado)/mean(geral$vlr_pgto_realizado)

#vlr_pgto_esperado
summary(geral$vlr_pgto_esperado)

hist(geral$vlr_pgto_esperado)
boxplot(geral$vlr_pgto_esperado)

sd(geral$vlr_pgto_esperado)

#vlr_saldo_devedor
summary(geral$vlr_saldo_devedor)

hist(geral$vlr_saldo_devedor)
boxplot(geral$vlr_saldo_devedor)

sd(geral$vlr_saldo_devedor)

#vlr_saldo_devedor_esperado
summary(geral$vlr_saldo_devedor_esperado)

hist(geral$vlr_saldo_devedor_esperado)
boxplot(geral$vlr_saldo_devedor_esperado)

sd(geral$vlr_saldo_devedor_esperado)

#dsp
summary(geral$dsp)

hist(geral$dsp)
boxplot(geral$dsp)

sd(geral$dsp)

#dspp
summary(geral$dspp)

hist(geral$dspp)
boxplot(geral$dspp)

sd(geral$dspp)



# identificando e removendo outliers 

outliers = robustX::mvBACON(geral[c("vlr_desembolsado", "vlr_tarifa", "perc_retencao",
                                    "vlr_pgto_realizado", "vlr_pgto_esperado",
                                    "vlr_saldo_devedor", "vlr_saldo_devedor_esperado",
                                    "dsp", "dspp")],
                            alpha = 0.95, init.sel = "random")

geral_outliers = within(geral,
                      {outliers = outliers$subset})
table(geral_outliers$outliers)
geral_outliers = subset(geral_outliers, outliers == TRUE)
geral_outliers$outliers = NULL
remove(outliers)

# variáveis antes e depois do tratamento
summary(geral[c("vlr_desembolsado", "vlr_tarifa", "perc_retencao",
                   "vlr_pgto_realizado", "vlr_pgto_esperado",
                   "vlr_saldo_devedor", "vlr_saldo_devedor_esperado",
                   "dsp", "dspp")])
summary(geral_outliers[c("vlr_desembolsado", "vlr_tarifa", "perc_retencao",
                         "vlr_pgto_realizado", "vlr_pgto_esperado",
                         "vlr_saldo_devedor", "vlr_saldo_devedor_esperado",
                         "dsp", "dspp")])


#winsorização

#w1
geral_outliers <- within(geral_outliers,
                       {vlr_desembolsado_w1 <- psych::winsor(geral_outliers$vlr_desembolsado, trim = 0.01)})
geral_outliers <- within(geral_outliers,                       
                       {vlr_tarifa_w1 <- psych::winsor(geral_outliers$vlr_tarifa, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {perc_retencao_w1 <- psych::winsor(geral_outliers$perc_retencao, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {vlr_pgto_realizado_w1 <- psych::winsor(geral_outliers$vlr_pgto_realizado, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {vlr_pgto_esperado_w1 <- psych::winsor(geral_outliers$vlr_pgto_esperado, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {vlr_saldo_devedor_w1 <- psych::winsor(geral_outliers$vlr_saldo_devedor, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {vlr_saldo_devedor_esperado_w1 <- psych::winsor(geral_outliers$vlr_saldo_devedor_esperado, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {dsp_w1 <- psych::winsor(geral_outliers$dsp, trim = 0.01)})
geral_outliers <- within(geral_outliers,
                       {dspp_w1 <- psych::winsor(geral_outliers$dspp, trim = 0.01)})

#w5
geral_outliers <- within(geral_outliers,
                         {vlr_desembolsado_w5 <- psych::winsor(geral_outliers$vlr_desembolsado, trim = 0.05)})
geral_outliers <- within(geral_outliers,                       
                         {vlr_tarifa_w5 <- psych::winsor(geral_outliers$vlr_tarifa, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {perc_retencao_w5 <- psych::winsor(geral_outliers$perc_retencao, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {vlr_pgto_realizado_w5 <- psych::winsor(geral_outliers$vlr_pgto_realizado, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {vlr_pgto_esperado_w5 <- psych::winsor(geral_outliers$vlr_pgto_esperado, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {vlr_saldo_devedor_w5 <- psych::winsor(geral_outliers$vlr_saldo_devedor, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {vlr_saldo_devedor_esperado_w5 <- psych::winsor(geral_outliers$vlr_saldo_devedor_esperado, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {dsp_w5 <- psych::winsor(geral_outliers$dsp, trim = 0.05)})
geral_outliers <- within(geral_outliers,
                         {dspp_w5 <- psych::winsor(geral_outliers$dspp, trim = 0.05)})


#antes e depois
summary(geral_outliers[c("vlr_desembolsado", "vlr_tarifa", "perc_retencao",
                         "vlr_pgto_realizado", "vlr_pgto_esperado",
                         "vlr_saldo_devedor", "vlr_saldo_devedor_esperado",
                         "dsp", "dspp")])
summary(geral_outliers[c("vlr_desembolsado_w1", "vlr_tarifa_w1", "perc_retencao_w1",
                         "vlr_pgto_realizado_w1", "vlr_pgto_esperado_w1",
                         "vlr_saldo_devedor_w1", "vlr_saldo_devedor_esperado_w1",
                         "dsp_w1", "dspp_w1")])
summary(geral_outliers[c("vlr_desembolsado_w5", "vlr_tarifa_w5", "perc_retencao_w5",
                         "vlr_pgto_realizado_w5", "vlr_pgto_esperado_w5",
                         "vlr_saldo_devedor_w5", "vlr_saldo_devedor_esperado_w5",
                         "dsp_w5", "dspp_w5")])

