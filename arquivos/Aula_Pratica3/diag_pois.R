# #------------------------------------------------------------#
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
# par(mfrow=c(2,2))
# a <- min(td)
# b <- max(td)
# plot(fitted(fit.model), h,xlab="Valor Ajustado", ylab="Medida h",
# pch=16)
# #identify(fitted(fit.model), h, n=1)
# #
# plot(di,xlab="Índice", ylab="Distância de Cook",
# pch=16)
# #identify(di, n=1)
# #
# plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
# ylim=c(a-1,b+1), pch=16)
# abline(2,0,lty=2)
# abline(-2,0,lty=2)
# #identify(td, n=1)
# #
# w <- fit.model$weights
# eta <- predict(fit.model)
# z <- eta + resid(fit.model, type="pearson")/sqrt(w)
# plot(predict(fit.model),z,xlab="Preditor Linear", 
# ylab="Variavel z", pch=16)
# lines(smooth.spline(predict(fit.model), z, df=2))
# par(mfrow=c(1,1))
#---------------------------------------------------------------#

#------------------------------------------------------------#
# diag_pois_qres.R
# Diagnóstico para modelo Poisson com resíduos quantílicos
#
# Layout 2x3:
#   (1) Alavancagem vs. valores ajustados
#   (2) Distância de Cook
#   (3) Resíduo quantílico vs. índice
#   (4) Resíduo quantílico vs. valores ajustados  ← NOVO
#   (5) Preditor linear vs. variável z
#   (6) QQ-plot dos resíduos quantílicos
#
# Requer: statmod instalado e carregado
#------------------------------------------------------------#

library(statmod)   # para qresid()

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)

# --- Medidas de alavancagem e influência ---
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)

ts <- resid(fit.model, type = "pearson") / sqrt(1 - h)
di <- (h / (1 - h)) * (ts^2)

# --- Resíduos quantílicos aleatorizados ---
resq <- qresid(fit.model)
a <- min(resq)
b <- max(resq)

# --- Preditor linear e variável z ---
eta <- predict(fit.model)
z   <- eta + resid(fit.model, type = "pearson") / sqrt(w)

# --- Layout 2x3 ---
par(mfrow = c(2, 3))

# (1) Alavancagem vs. valores ajustados
plot(fitted(fit.model), h,
     xlab = "Valor Ajustado", ylab = "Medida h", pch = 16)
# identify(fitted(fit.model), h, n = 1)

# (2) Distância de Cook
plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
# identify(di, n = 1)

# (3) Resíduo quantílico vs. índice
plot(resq, xlab = "Índice", ylab = "Resíduo Quantílico",
     ylim = c(a - 1, b + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(resq, n = 1)

# (4) Resíduo quantílico vs. valores ajustados  
plot(fitted(fit.model), resq,
     xlab = "Valor Ajustado", ylab = "Resíduo Quantílico",
     ylim = c(a - 1, b + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(fitted(fit.model), resq, n = 1)

par(mfrow = c(1, 1))

# # (5) Preditor linear vs. variável z
# plot(eta, z,
#      xlab = "Preditor Linear", ylab = "Variável z", pch = 16)
# lines(smooth.spline(eta, z, df = 2))

# # (6) QQ-plot dos resíduos quantílicos
# qqnorm(resq, pch = 16, main = "QQ-plot - Res. Quantílico")
# qqline(resq, col = "firebrick", lty = 2)

#------------------------------------------------------------#