source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(ants); 

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

attach(ants)

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


