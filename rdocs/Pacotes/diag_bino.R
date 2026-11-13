#------------------------------------------------------------#
# Para rodar este programa  deixe no objeto   fit.model 
# a saída do  ajuste  da  regressão com  erros binomial. 
# Deixe  os dados disponíveis  através do comando 
# attach(...). Depois use o comando source(...) no S-Plus 
# ou R para  executar o programa. A sequência de comandos
# é a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_bino")
#
# A saída  terá  quatro gráficos: de pontos de alacanca,
# de pontos influentes  e  dois de resíduos.  
# Para  identificar os pontos que  mais  se destacam  usar
# o comando  identify(...). Se por exemplo se destacarem 
# 3 pontos no plot(fitted(fit.model),h,...), após esse
# comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
# 
# O mesmo  pode  ser feito para os demais gráficos. 
# Nos gráficos de resíduos foram  colocados os limites
# ylim=c(a-1,b+1), em que a é o menor valor e b o maior
# valor para o resíduo.
#------------------------------------------------------------#
# X <- model.matrix(fit.model)
# n <- nrow(X)
# p <- ncol(X)
# w <- fit.model$weights
# W <- diag(w)
# H <- solve(t(X)%*%W%*%X)
# H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
# h <- diag(H)
# ts <- resid(fit.model,type="pearson")/sqrt(1-h)
# td <- resid(fit.model,type="deviance")/sqrt(1-h)
# di <- (h/(1-h))*(ts^2)
# a <- max(td)
# b <- min(td)
# par(mfrow=c(2,2))
# plot(fitted(fit.model),h,xlab="Valor Ajustado", 
# ylab="Medida h", pch=16)
# identify(fitted(fit.model), h, n=3)
# #
# plot(di,xlab="Índice", ylab="Distância de Cook",pch=16)
# identify(di, n=5)
# #
# plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
# ylim=c(b-1,a+1), pch=16)
# abline(2,0,lty=2)
# abline(-2,0,lty=2)
# identify(td, n=4)
# #
# plot(fitted(fit.model), td,xlab="Valor Ajustado", 
# ylab="Resíduo Componente do Desvio", pch=16)
# identify(fitted(fit.model), td, n=3)
# par(mfrow=c(1,1))
#------------------------------------------------------------#

#------------------------------------------------------------#
# diag_bino_qres.R
# Diagnóstico para modelo binomial com resíduos quantílicos
# Requer: statmod instalado e carregado

# A documentação do pacote statmod é enfática: "Quantile residuals are the only useful 
# residuals for binomial or Poisson data when the response takes on only a small number 
# of distinct values". Em modelos com desvio ou dispersão grande, os resíduos de deviance 
# e Pearson podem ser grosseiramente não normais; os quantílicos são a escolha recomendada 
# nesses casos.
#------------------------------------------------------------#

library(statmod)   # para qresid()

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)

# --- Medidas de alavancagem e influência (mantidas) ---
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)

# Distância de Cook (usando resíduos de Pearson internos)
ts <- resid(fit.model, type = "pearson") / sqrt(1 - h)
di <- (h / (1 - h)) * (ts^2)

# --- Resíduos quantílicos aleatorizados ---
resq <- qresid(fit.model)

a <- max(resq)
b <- min(resq)

# --- Preditor Linear vs. Variável z ---
# Extrai os componentes do modelo binomial
eta <- predict(fit.model)          # preditor linear (η)
w   <- fit.model$weights           # pesos do modelo

# Constrói a variável z (resíduo de trabalho)
z <- eta + residuals(fit.model, type = "pearson") / sqrt(w)


# --- Layout 2x2 ---
par(mfrow = c(2, 2))

# (1) Alavancagem vs. valores ajustados
plot(fitted(fit.model), h,
     xlab = "Valor Ajustado", ylab = "Medida h", pch = 16)
#identify(fitted(fit.model), h, n=3)   # descomente para identificar pontos

# (2) Distância de Cook
plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
identify(di, n = 5)

# (3) Resíduo quantílico vs. índice
plot(resq, xlab = "Índice", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
 identify(resq, n = 4)

# (4) Resíduo quantílico vs. valores ajustados
plot(fitted(fit.model), resq,
     xlab = "Valor Ajustado", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
 identify(fitted(fit.model), resq, n = 3)

par(mfrow = c(1, 1))

# # (5) Gráfico Preditor Linear vs. Variável z
plot(eta, z,
     xlab = "Preditor Linear (η)",
     ylab = "Variável z",
     pch = 16)
lines(smooth.spline(eta, z, df = 2))
identify(eta, z, n = 4)

# (6) QQ-plot dos resíduos quantílicos
# qqnorm(resq, pch = 16, main = "QQ-plot - Res. Quantílico")
# qqline(resq, col = "firebrick", lty = 2)

#------------------------------------------------------------#