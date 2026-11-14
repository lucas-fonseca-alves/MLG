# #------------------------------------------------------------#
# # diag_glm.R — adaptado para GLM (mantém o estilo do script do Prof. Gilberto)
# #------------------------------------------------------------#
# X <- model.matrix(fit.model)
# n <- nrow(X)
# p <- ncol(X)
# 
# # Medidas de alavancagem (o R calcula corretamente para glm)
# h <- hatvalues(fit.model)
# 
# # Desvio-padrão residual (para gaussian = resíduo padrão do lm)
# s <- summary(fit.model)$sigma
# 
# # Resíduos de deviance (= resíduos brutos para gaussian)
# r <- residuals(fit.model, type = "deviance")
# 
# # Resíduo padronizado internamente
# ts <- r / (s * sqrt(1 - h))
# 
# # Distância de Cook (o R calcula corretamente para glm)
# di <- cooks.distance(fit.model)
# 
# # Resíduo padronizado (Pearson) — substitui o "externamente studentizado"
# tsi <- rstandard(fit.model, type = "pearson")
# 
# a <- max(tsi, na.rm = TRUE)
# b <- min(tsi, na.rm = TRUE)
# 
# par(mfrow = c(2, 2))
# 
# plot(h, xlab = "Índice", ylab = "Medida h", pch = 16, ylim = c(0, 1))
# abline(2 * p / n, 0, lty = 2)
# identify(h, n=6)
# 
# plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
# identify(di, n=6)
# 
# plot(tsi, xlab = "Índice", ylab = "Resíduo Padronizado",
#      ylim = c(b - 1, a + 1), pch = 16)
# abline(2, 0, lty = 2); abline(-2, 0, lty = 2)
# identify(tsi, n=5)
# 
# plot(fitted(fit.model), tsi, xlab = "Valor Ajustado",
#      ylab = "Resíduo Padronizado", ylim = c(b - 1, a + 1), pch = 16)
# abline(2, 0, lty = 2); abline(-2, 0, lty = 2)
# identify(fitted(fit.model),tsi)
# 
# par(mfrow = c(1, 1))
# #------------------------------------------------------------#

#------------------------------------------------------------#
# diag_glm_qres.R
# Diagnóstico para GLM com resíduos quantílicos
# Padronizado conforme diag_bino_qres.R
#
# Layout 2x2 + 2 gráficos adicionais:
#   (1) Alavancagem vs. valores ajustados
#   (2) Distância de Cook
#   (3) Resíduo quantílico vs. índice
#   (4) Resíduo quantílico vs. valores ajustados
#   (5) Preditor linear vs. variável z (função de ligação)
#   (6) QQ-plot dos resíduos quantílicos (opcional, comentado)
#
# Requer: statmod instalado e carregado
#------------------------------------------------------------#

library(statmod)   # para qresid()

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)

# --- Medidas de alavancagem e influência ---
h  <- hatvalues(fit.model)
di <- cooks.distance(fit.model)

# --- Resíduos quantílicos aleatorizados ---
resq <- qresid(fit.model)

a <- max(resq)
b <- min(resq)

# --- Preditor linear e variável z ---
eta <- predict(fit.model)          # preditor linear (η)
w   <- fit.model$weights           # pesos do modelo
z   <- eta + residuals(fit.model, type = "pearson") / sqrt(w)

# --- Layout 2x2 ---
par(mfrow = c(2, 2))

# (1) Alavancagem vs. valores ajustados
plot(fitted(fit.model), h,
     xlab = "Valor Ajustado", ylab = "Medida h", pch = 16)
# identify(fitted(fit.model), h, n = 3)

# (2) Distância de Cook
plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
# identify(di, n = 5)

# (3) Resíduo quantílico vs. índice
plot(resq, xlab = "Índice", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(resq, n = 4)

# (4) Resíduo quantílico vs. valores ajustados
plot(fitted(fit.model), resq,
     xlab = "Valor Ajustado", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(fitted(fit.model), resq, n = 3)

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