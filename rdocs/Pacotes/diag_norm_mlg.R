#------------------------------------------------------------#
# diag_glm.R — adaptado para GLM (mantém o estilo do script do Prof. Gilberto)
#------------------------------------------------------------#
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)

# Medidas de alavancagem (o R calcula corretamente para glm)
h <- hatvalues(fit.model)

# Desvio-padrão residual (para gaussian = resíduo padrão do lm)
s <- summary(fit.model)$sigma

# Resíduos de deviance (= resíduos brutos para gaussian)
r <- residuals(fit.model, type = "deviance")

# Resíduo padronizado internamente
ts <- r / (s * sqrt(1 - h))

# Distância de Cook (o R calcula corretamente para glm)
di <- cooks.distance(fit.model)

# Resíduo padronizado (Pearson) — substitui o "externamente studentizado"
tsi <- rstandard(fit.model, type = "pearson")

a <- max(tsi, na.rm = TRUE)
b <- min(tsi, na.rm = TRUE)

par(mfrow = c(2, 2))

plot(h, xlab = "Índice", ylab = "Medida h", pch = 16, ylim = c(0, 1))
abline(2 * p / n, 0, lty = 2)

plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)

plot(tsi, xlab = "Índice", ylab = "Resíduo Padronizado",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2); abline(-2, 0, lty = 2)

plot(fitted(fit.model), tsi, xlab = "Valor Ajustado",
     ylab = "Resíduo Padronizado", ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2); abline(-2, 0, lty = 2)

par(mfrow = c(1, 1))
#------------------------------------------------------------#