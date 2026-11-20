source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(ants); 

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

attach(ants)


caminho_Problema2 <- "resultados/Problema 2/Formiga - Poisson"

#------------------------------------------------------------------------------#---- 
#               Modelo Poisson - Ligação Log                                   #
#------------------------------------------------------------------------------#----

#*************************** Métodos automatizados ****************************#----
# Modelo nulo (apenas o intercepto)
modelo_nulo <- glm(Srich ~ 1,
                   family = poisson(link = "log"))

stepAIC(fit1_PL,
        direction = "backward")

stepAIC(modelo_nulo,
        scope = list(lower = modelo_nulo,
                     upper = fit1_PL),
        direction = "forward")

stepAIC(fit1_PL,
        scope = list(lower = modelo_nulo,
                     upper = fit1_PL),
        direction = "both")

#Levaram para o modelo completo

#------------------------------------------------------------------------------#----

#****************************** Ajustes ***************************************#
fit1_PL <- glm(Srich ~ Latitude + Elevation + Habitat , 
               family=poisson(link = "log"),
               data = ants)
summary(fit1_PL)

fit2_PL <- glm(Srich ~ Latitude + Habitat , 
               family=poisson(link = "log"),
               data = ants)
summary(fit2_PL)

fit3_PL <- glm(Srich ~ Elevation + Habitat , 
               family=poisson(link = "log"),
               data = ants)
summary(fit3_PL)


##**********************************Diagnóstico**********************************#----

#Envelope ficou ótimo (chocante)
envelope(fit1_PL, rep=100, conf=0.95, type="quantile")
envelope(fit1_PL, rep=100, conf=0.95, type="deviance")
envelope(fit1_PL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit1_PL
source(here::here("rdocs/Pacotes", "diag_pois.R"))
#3  7  8 25 29
#   7 25 29

#*** Mudança Relativa ***# 
summary(fit1_PL)
fit1_PL_w3 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(3));summary(fit1_PL_w3)

summary(fit1_PL)
fit1_PL_w7 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(7));summary(fit1_PL_w7)

summary(fit1_PL)


fit1_PL_w8 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(8));summary(fit1_PL_w8)

summary(fit1_PL)
fit1_PL_w25 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                   family=poisson(link = "log"),
                   data = ants,
                   subset=-c(25));summary(fit1_PL_w25)

summary(fit1_PL)
fit1_PL_w29 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                   family=poisson(link = "log"),
                   data = ants,
                   subset=-c(29));summary(fit1_PL_w29)

summary(fit1_PL)
fit1_PL_w <- glm(Srich ~ Latitude + Elevation + Habitat , 
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(3,7,8,25,29));summary(fit1_PL_w)

#***mudança nas estimativas***#
MR(coef(fit1_PL), coef(fit1_PL_w3)) # -5.84 -6.89 -1.44 -5.14 %  (Ok)
MR(coef(fit1_PL), coef(fit1_PL_w7)) # 3.33 3.71 10.62 5.23 %  (Ok)
MR(coef(fit1_PL), coef(fit1_PL_w8)) # -5.96 -7.49 19.64 -3.9 %  (Perigoso)
MR(coef(fit1_PL), coef(fit1_PL_w25)) # -8.08 -8.73 -1.84 12.55 % (Dar uma olhada)
MR(coef(fit1_PL), coef(fit1_PL_w29)) # 3.06 2.95 8.44 -7.76 (ok)
MR(coef(fit1_PL), coef(fit1_PL_w))   # -14.15 -17.3 37.65 -0.25 % (Perigoso)

vif(fit1_PL)

#Envelope ficou ótimo (chocante)
envelope(fit2_PL, rep=100, conf=0.95, type="quantile")
envelope(fit2_PL, rep=100, conf=0.95, type="deviance")
envelope(fit2_PL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit2_PL
source(here::here("rdocs/Pacotes", "diag_pois.R"))
#1 3  4 25
#1 3 13 25
#1 3 13 14 25 29
#  25 é o ponto que se destaca na variável Z 

fit2_PL_w1 <- glm(Srich ~ Latitude + Habitat, 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(1));summary(fit2_PL_w1)

fit2_PL_w3 <- glm(Srich ~ Latitude + Habitat, 
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(3));summary(fit2_PL_w3)

fit2_PL_w13 <- glm(Srich ~ Latitude + Habitat, 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(13));summary(fit2_PL_w13)

fit2_PL_w14 <- glm(Srich ~ Latitude + Habitat, 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(14));summary(fit2_PL_w14)

fit2_PL_w25 <- glm(Srich ~ Latitude + Habitat, 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(25));summary(fit2_PL_w25)

fit2_PL_w29 <- glm(Srich ~ Latitude + Habitat, 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(29));summary(fit2_PL_w29)

fit2_PL_w <- glm(Srich ~ Latitude + Habitat, 
                  family=poisson(link = "log"),
                  data = ants,
                  subset=-c(1, 3, 13, 14, 25, 29));summary(fit2_PL_w)

#***mudança nas estimativas***# (Lida muito melhor com os pontos 'influentes')
MR(coef(fit2_PL), coef(fit2_PL_w1))  # 5.77 6.55 4.87 %
MR(coef(fit2_PL), coef(fit2_PL_w3))  # -5.93 -6.74 -5.5 % 
MR(coef(fit2_PL), coef(fit2_PL_w13)) # 1.19 1.35 4.82 % 
MR(coef(fit2_PL), coef(fit2_PL_w14)) # 0.3 0.34 3.77 % 
MR(coef(fit2_PL), coef(fit2_PL_w25)) # -7.83 -8.21 12.9 % 
MR(coef(fit2_PL), coef(fit2_PL_w29)) # 2.97 3.06 -5.98 % 
MR(coef(fit2_PL), coef(fit2_PL_w))   # -2.03 -1.94 16.33 %  


#Envelope não fica tão bom quanto os outros.
envelope(fit3_PL, rep=100, conf=0.95, type="quantile")
envelope(fit3_PL, rep=100, conf=0.95, type="deviance")
envelope(fit3_PL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit3_PL
source(here::here("rdocs/Pacotes", "diag_pois.R"))
# 3, 8,25, 29, 42
# 3  4  8 25 42
# 25

fit3_PL_w3 <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(3));summary(fit3_PL_w3)

fit3_PL_w4 <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(4));summary(fit3_PL_w4)

fit3_PL_w8 <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(8));summary(fit3_PL_w8)

fit3_PL_w25 <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(25));summary(fit3_PL_w25)

fit3_PL_w29 <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c( 29));summary(fit3_PL_w29)

fit3_PL_w42 <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(3, 4, 8, 25, 29, 42));summary(fit3_PL_w42)

fit3_PL_w <- glm(Srich ~ Elevation + Habitat ,  
                 family=poisson(link = "log"),
                 data = ants,
                 subset=-c(3, 4, 8, 25, 29, 42));summary(fit3_PL_w)

#***mudança nas estimativas***# (Lida mal com os pontos 'influentes')
MR(coef(fit3_PL), coef(fit3_PL_w3))  # -0.55 -3.57 -6.66 % 
MR(coef(fit3_PL), coef(fit3_PL_w4)) # -1.45 -9.33 -3.97 % 
MR(coef(fit3_PL), coef(fit3_PL_w8)) # 2.84 18.78 -5.21 % 
MR(coef(fit3_PL), coef(fit3_PL_w25)) #-5.3 -3.91 14.04 % 
MR(coef(fit3_PL), coef(fit3_PL_w29)) #  3.19 6.37 -6.62 % 
MR(coef(fit3_PL), coef(fit3_PL_w42)) # 2.99 17.58 -18.2 % 
MR(coef(fit3_PL), coef(fit3_PL_w)) # 2.99 17.58 -18.2 % 

#-----------------------Gerando 10 gráficos de envelope------------------------#----


#*** Fit1_PL ***#
# Abra um dispositivo PDF com tamanho grande (em polegadas)

pdf(file.path(caminho_Problema2, "Fit1_PL-Envelopes_Anexo.pdf"),
    width = 12, height = 15)

# Defina o layout e as margens
par(mfrow = c(5, 2), mar = c(2.5, 2.5, 1.5, 1.5))

# Plote os 10 gráficos
envelope(fit1_PL, rep = 100, conf = 0.95, type = "quantile", main = "")
# ... (outros 9 gráficos)

# Feche o dispositivo para finalizar o arquivo
dev.off()

#*** Fit2_PL ***#

pdf(file.path(caminho_Problema2, "Fit2_PL-Envelopes.pdf"),
    width = 12, height = 15)

# Defina o layout e as margens
par(mfrow = c(5, 2), mar = c(2.5, 2.5, 1.5, 1.5))

# Plote os 10 gráficos
envelope(fit2_PL, rep = 100, conf = 0.95, type = "quantile", main = "")
# ... (outros 9 gráficos)

# Feche o dispositivo para finalizar o arquivo
dev.off()


#*** Fit3_PL ***#

pdf(file.path(caminho_Problema2, "Fit3_PL-Envelopes.pdf"),
    width = 12, height = 15)

# Defina o layout e as margens
par(mfrow = c(5, 2), mar = c(2.5, 2.5, 1.5, 1.5))

# Plote os 10 gráficos
envelope(fit3_PL, rep = 100, conf = 0.95, type = "quantile", main = "")
# ... (outros 9 gráficos)

# Feche o dispositivo para finalizar o arquivo
dev.off()
#------------------------------------------------------------------------------#----

#-----------------Gerando 10 independencia (quantílica)------------------------#----

#*** Fit2_PL ***#
fit.model <- fit2_PL

# --- Resíduos quantílicos aleatorizados ---
pdf(file.path(caminho_Problema2, "Fit2_PL-Independencia(Quantílica).pdf"),
    width = 12, height = 15)

par(mfrow = c(5, 2), mar = c(2.5, 2.5, 1.5, 1.5))


resq <- qresid(fit.model)

a <- max(resq)
b <- min(resq)

# Defina o layout e as margens

# (3) Resíduo quantílico vs. índice
plot(resq, xlab = "Índice", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
#identify(resq)
# 1  3 13 14 25 29 (Valores únicos que aparecem nos 10 gráficos gerados)
dev.off()


#*** Fit3_PL ***#
fit.model <- fit3_PL

# --- Resíduos quantílicos aleatorizados ---
pdf(file.path(caminho_Problema2, "Fit3_PL-Independencia(Quantílica).pdf"),
    width = 12, height = 15)

par(mfrow = c(5, 2), mar = c(2.5, 2.5, 1.5, 1.5))


resq <- qresid(fit.model)

a <- max(resq)
b <- min(resq)

# Defina o layout e as margens

# (3) Resíduo quantílico vs. índice
plot(resq, xlab = "Índice", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
#identify(resq)
#3, 8, 29, 42 #Valores únicos que aparecem nos 10 gráficos
dev.off()

#------------------Gerando 10 gráficos de homocedasticidade------------------#----
#*** Fit2_PL ***#
fit.model <- fit2_PL

# --- Resíduos quantílicos aleatorizados ---
pdf(file.path(caminho_Problema2, "Fit2_PL-Homocedasticidade(Quantílica).pdf"),
    width = 12, height = 15)

par(mfrow = c(5, 2), mar = c(2.5, 2.5, 1.5, 1.5))


# --- Resíduos quantílicos aleatorizados ---
resq <- qresid(fit.model)

a <- max(resq)
b <- min(resq)

# (4) Resíduo quantílico vs. valores ajustados
plot(fitted(fit.model), resq,
     xlab = "Valor Ajustado", ylab = "Resíduo Quantílico",
     ylim = c(b - 1, a + 1), pch = 16)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
identify(fitted(fit.model), resq, n = 5)

dev.off()
