source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(ants); 

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

attach(ants)
#------------------------------------------------------------------------------#---- 
#               Modelo Normal - Ligação Log (ótimo, chocante)                  #
#------------------------------------------------------------------------------#----
#*** Ajustes ***#
fit1_NL <- glm(Srich ~ Latitude + Elevation + Habitat , 
               family=gaussian(link = "log"),
               data=ants)
summary(fit1_NL)


#*************************** Métodos automatizados ****************************#----
# Modelo nulo (apenas o intercepto)
modelo_nulo <- glm(Srich ~ 1,
                   family = gaussian(link = "log"))

stepAIC(fit1_NL,
        direction = "backward")

stepAIC(modelo_nulo,
        scope = list(lower = modelo_nulo,
                     upper = fit1_NL),
        direction = "forward")

stepAIC(fit1_NL,
        scope = list(lower = modelo_nulo,
                     upper = fit1_NL),
        direction = "both")

#Levaram para o modelo completo

#------------------------------------------------------------------------------#----

##**********************************Diagnóstico**********************************#----

#Envelope ficou ótimo (chocante)
envelope(fit1_NL, rep=100, conf=0.95, type="quantile")
envelope(fit1_NL, rep=100, conf=0.95, type="deviance")
envelope(fit1_NL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit1_NL
source(here::here("rdocs/Pacotes", "diag_norm_mlg.R"))
#Leve problema de independencia 
#1  2  4  8 20 22
#1  3  7  8 25 29
#   3  7  8 25 29
#   3  7  8 25 29

#*** Mudança Relativa ***# 

summary(fit1_NL)
fit1_NL_w3 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                 family=gaussian(link = "log"),
                     data = ants,
                     subset=-c(3));summary(fit1_NL_w3)
#Reduz um pouco a significância de Latitude

summary(fit1_NL)
fit1_NL_w8 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                  family=gaussian(link = "log"),
                  data = ants,
                  subset=-c(8));summary(fit1_NL_w8)

summary(fit1_NL)
fit1_NL_w25 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                  family=gaussian(link = "log"),
                  data = ants,
                  subset=-c(25));summary(fit1_NL_w25)

summary(fit1_NL)
fit1_NL_w29 <- glm(Srich ~ Latitude + Elevation + Habitat , 
                   family=gaussian(link = "log"),
                   data = ants,
                   subset=-c(29));summary(fit1_NL_w29)

summary(fit1_NL)
fit1_NL_w <- glm(Srich ~ Latitude + Elevation + Habitat , 
                   family=gaussian(link = "log"),
                   data = ants,
                   subset=-c(3,8,25,29));summary(fit1_NL_w)

#***mudança nas estimativas***#
MR(coef(fit1_NL), coef(fit1_NL_w3)) #-8.22 -9.68 1.3 -5.74 % (Ok)
MR(coef(fit1_NL), coef(fit1_NL_w8)) # -5.48 -6.74 20.12 -2.12 % (Perigoso)
MR(coef(fit1_NL), coef(fit1_NL_w25)) #-6.06 -6.25 2.02 14.95 % (Dar uma olhada)
MR(coef(fit1_NL), coef(fit1_NL_w29)) # 2.21 1.91 5.37 -9.84 %  (ok)
MR(coef(fit1_NL), coef(fit1_NL_w))   #-18.88 -22.41 33.3 -3.89 %  (Perigoso)


# PESQUISAR: Ocorre correlação baixa para altitude latitude quando a correlação é pequena 
