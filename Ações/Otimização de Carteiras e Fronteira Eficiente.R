library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(plotly)

#Ticks das empresas analisadas
symbols <- c("BBAS3.SA","LREN3.SA","BBDC4.SA","JBSS3.SA","ITUB4.SA","TASA4.SA","PETR4.SA")

#Coletando os preço das ações no período a ser analisado
prices <- tq_get(symbols, 
                     from = "2015-01-01", 
                     to = "2020-09-10",
                 get = "stock.prices")

#Logaritmo dos retornos diários das ações
log_ret <- prices %>%
  group_by(symbol)%>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "ret",
               type = "log")

#Transformando em uma série temporal
log_ret_xts <- log_ret%>%
  spread(symbol, value = ret)%>%
  tk_xts()

#Média de retorno diário das ações
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret,5))

#Matriz de Covariância das ações anualizada
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat, 4))

#Colocando os pesos de cada ativo no portfólio
wts <- runif(n = length(symbols))
wts <- wts/ sum(wts)
print(wts)

#Calculando o retorno anualizado do portfólio
port_returns <- (sum(wts* mean_ret)+1)^252 - 1
print(port_returns)

#Calculando o risco (Desvio Padrão) do portfólio
port_risk <- sqrt(t(wts)%*%(cov_mat %*% wts))
print(port_risk)

#Sharpe do portfólio, utilizando a selic (2%) como a taxa livre de risco
sharpe_ratio <- (port_returns - 0.02)/port_risk 
print(sharpe_ratio)

#Otimizando o portfólio
num_port <- 500000

  #Criando uma matriz para guardar os pesos
all_wts <- matrix(nrow = num_port,
                  ncol = length(symbols))
  #Criando um vetor vazio para guardar os resultados
    #Retorno do Portfólio
port_returns <- vector('numeric', length = num_port)
    #Risco do Portfólio
port_risk <- vector('numeric', length = num_port)
    #Sharpe do Portfólio
sharpe_ratio <- vector('numeric', length = num_port)

#Rodando 5000 Portfólios
for (i in seq_along(port_returns)){
  wts <- runif(length(symbols))
  wts <- wts/sum(wts)
  
  all_wts[i,] <- wts
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  port_returns[i] <- port_ret
  
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  sr <- (port_ret - 0.02)/port_sd
  sharpe_ratio[i] <- sr
}

  #Colocando os valores na tabela
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)
  #Convertendo a matriz para tibble 
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

  #Combinando todos os valores 
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

#Encontrando o valor máximo de Sharpe e mínimos da variância
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

#Mínima variância do portfólio
pvar <- min_var %>%
  gather(BBAS3.SA:TASA4.SA, key = Asset,
         value = Weights)%>%
  mutate(Asset = as.factor(Asset))%>%
  ggplot(aes(x = fct_reorder(Asset, Weights), y = Weights, fill = Asset))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(x = "Ativos", y = "Pesos", title = "Portfólio Com a Menor Variância", colour = "Ativos")+
  scale_y_continuous(labels = scales::percent)
pvar

#Máximo Sharpe do portfólio
msharp <- max_sr %>%
  gather(BBAS3.SA:TASA4.SA, key = Asset,
         value = Weights)%>%
  mutate(Asset = as.factor(Asset))%>%
  ggplot(aes(x = fct_reorder(Asset, Weights), y = Weights, fill = Asset))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(x = "Ativos", y = "Pesos", title = "Portfólio Com o Maior Sharpe", colour = "Ativos")+
  scale_y_continuous(labels = scales::percent)
msharp

#Fronteira Eficiente do Portfólio
effp <- portfolio_values%>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Risco',
       y = 'Retorno',
       title = "Fronteira Eficiente do Portfólio") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.20, y = 0.42, label = "Carteira Ótima") +
  annotate('text', x = 0.18, y = 0.01, label = "Menor Risco") +
  annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01, 
           yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405, 
           yend = 0.365, color = 'red', arrow = arrow(type = "open"))
ggplotly(effp)


