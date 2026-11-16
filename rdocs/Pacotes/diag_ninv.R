# #------------------------------------------------------------#
# # Para rodar este programa  deixe no objeto fit.model a saida 
# # do  ajuste  da  regressao com erros n.inversa. Deixe  
# # os dados disponiveis  atraves do comando attach(...). 
# # Depois  use  o comando source(...) no R ou S-Plus para
# # executar o programa. A sequencia de comandos e a seguinte:
# #
# #        > fit.model <- ajuste
# #        > attach(dados)
# #        > source("diag_ninv")
# #
# # A saida tera quatro graficos: de pontos de alacanca, de
# # pontos influentes  e  dois de residuos. Para identificar 
# # os pontos que  mais  se destacam usar o comando 
# # identify(...). Se por exemplo se destacam tres pontos 
# # no plot(fitted(fit.model),h,...),  apos esse comando coloque
# #     
# #        > identify(fitted(fit.model),h,n=3)
# #
# # O mesmo pode ser feito nos demais gr�ficos. Nos graficos de 
# # residuos foram colocados os limites ylim=c(a-1,b+1), em que a
# # e o menor valor e b o maior valor para o residuo. 
# #------------------------------------------------------------#
# X <- model.matrix(fit.model)
# n <- nrow(X)
# p <- ncol(X)
# w <- fit.model$weights
# W <- diag(w)
# H <- solve(t(X)%*%W%*%X)
# H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
# h <- diag(H)
# soma <- resid(fit.model, type="pearson")
# soma <- sum(soma^2)
# fi <- (n-p)/soma
# ts <- resid(fit.model,type="pearson")*sqrt(fi/(1-h))
# td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
# par(mfrow=c(2,2))
# di <- (h/(1-h))*(ts^2)
# a <- max(td)
# b <- min(td)
# #
# plot(fitted(fit.model),h,xlab="Valor Ajustado", ylab="Medida h", pch=16)
# #identify(fitted(fit.model), h, n=3)
# #
# plot(fitted(fit.model), di,xlab="Valor Ajustado", ylab="Distancia de Cook", pch=16)
# #identify(fitted(fit.model),di, n=6)
# #
# plot(fitted(fit.model),td,xlab="Valor Ajustado", ylab="Residuo Componente do Desvio",
# ylim=c(b-1,a+1),pch=16)
# abline(2,0,lty=2)
# abline(-2,0,lty=2)
# #identify(fitted(fit.model),td, n=1)
# eta <- predict(fit.model)
# z <- eta + resid(fit.model, type="pearson")/sqrt(w)
# plot(predict(fit.model),z,xlab="Preditor Linear",
# ylab="Variavel z", pch=16)
# lines(smooth.spline(predict(fit.model), z, df=2))
# par(mfrow=c(1,1))
# #------------------------------------------------------------#

#------------------------------------------------------------#
# diag_ninv_qres.R
# Diagnóstico para modelo Normal Inversa com resíduos quantílicos
# Padronizado conforme diag_bino_qres.R e diag_glm_qres.R
#
# Layout 2x2 + 2 gráficos adicionais:
#   (1) Alavancagem vs. valores ajustados
#   (2) Distância de Cook
#   (3) Resíduo quantílico vs. índice
#   (4) Resíduo quantílico vs. valores ajustados
#   (5) Preditor linear vs. variável z (função de ligação)
#   (6) QQ-plot dos resíduos quantílicos (opcional, comentado)
#
# ATENÇÃO: qresid() requer family$p e family$q.
# A família inverse.gaussian nativa do R NÃO tem essas funções.
# Use statmod::invgauss() no ajuste. Ex:

# library(statmod)
# 
# # Em vez de:
# # fit.model <- glm(y ~ x, family = inverse.gaussian(link = "1/mu^2"), data = dados)
# 
# # Use:
# fit.model <- glm(y ~ x,
#                  family = invgauss(link = "1/mu^2"),   # ou "log", "identity"
#                  data   = dados)
#------------------------------------------------------------#

# library(statmod)   # para qresid() e invgauss()
#
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
#
# # --- Medidas de alavancagem e influência ---
h  <- hatvalues(fit.model)
di <- cooks.distance(fit.model)

# # --- Resíduos quantílicos ---
 resq <- qresid(fit.model)

a <- max(resq)
b <- min(resq)

# # --- Preditor linear e variável z ---
eta <- predict(fit.model)          # preditor linear (η)
w   <- fit.model$weights           # pesos do modelo
z   <- eta + residuals(fit.model, type = "pearson") / sqrt(w)

# # --- Layout 2x2 ---
 par(mfrow = c(2, 2))

# (1) Alavancagem vs. valores ajustados
plot(fitted(fit.model), h,
     xlab = "Valor Ajustado", ylab = "Medida h", pch = 16)
 identify(fitted(fit.model), h)

# (2) Distância de Cook
plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
identify(di, n = 2)

# (3) Resíduo quantílico vs. índice
plot(resq, xlab = "Índice", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
 identify(resq)

# # (4) Resíduo quantílico vs. valores ajustados
plot(fitted(fit.model), resq,
     xlab = "Valor Ajustado", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
 identify(fitted(fit.model), resq)
#
par(mfrow = c(1, 1))

# (5) Gráfico Preditor Linear vs. Variável z
plot(eta, z,
     xlab = "Preditor Linear (η)",
     ylab = "Variável z",
     pch = 16)
lines(smooth.spline(eta, z, df = 2))
 identify(eta, z, n = 4)

# (6) QQ-plot dos resíduos quantílicos (opcional)
# qqnorm(resq, pch = 16, main = "QQ-plot - Res. Quantílico")
# qqline(resq, col = "firebrick", lty = 2)
#------------------------------------------------------------#
