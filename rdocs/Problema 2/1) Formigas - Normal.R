source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(ants); 

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

attach(ants)
#------------------------------------------------------------------------------#---- 
#               Modelo Normal - Ligação Log (ótimo, chocante) (Fit2 fica melhor)#
#------------------------------------------------------------------------------#----
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

#*** Ajustes ***#
fit1_NL <- glm(Srich ~ Latitude + Elevation + Habitat , 
               family=gaussian(link = "log"),
               data=ants)
summary(fit1_NL)

fit2_NL <- glm(Srich ~ Latitude + Habitat , 
               family=gaussian(link = "log"),
               data=ants)
summary(fit2_NL)

fit3_NL <- glm(Srich ~ Elevation + Habitat , 
               family=gaussian(link = "log"),
               data=ants)
summary(fit3_NL)



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


#Envelope ficou ótimo (chocante)
envelope(fit2_NL, rep=100, conf=0.95, type="quantile")
envelope(fit2_NL, rep=100, conf=0.95, type="deviance")
envelope(fit2_NL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit2_NL
source(here::here("rdocs/Pacotes", "diag_norm_mlg.R"))
#A ligação fica bem esquisita
# 1  3  4 25
# 1  3 13 25
# 1  3 13 25
# 25 43 44

summary(fit2_NL)
fit2_NL_w1 <- glm(Srich ~ Latitude + Habitat, 
                 family=gaussian(link = "log"),
                 data = ants,
                 subset=-c(1));summary(fit2_NL_w1)

fit2_NL_w3 <- glm(Srich ~ Latitude + Habitat, 
                 family=gaussian(link = "log"),
                 data = ants,
                 subset=-c(3));summary(fit2_NL_w3)

fit2_NL_w25 <- glm(Srich ~ Latitude + Habitat, 
                 family=gaussian(link = "log"),
                 data = ants,
                 subset=-c(25));summary(fit2_NL_w25)

fit2_NL_w <- glm(Srich ~ Latitude + Habitat, 
                 family=gaussian(link = "log"),
                 data = ants,
                 subset=-c(1,3,25));summary(fit2_NL_w)

#***mudança nas estimativas***# (Lidou melhor)
MR(coef(fit2_NL), coef(fit2_NL_w1)) #10.7 12.07 6.54 % 
MR(coef(fit2_NL), coef(fit2_NL_w3)) # -8.6 -9.68 -6.36 %
MR(coef(fit2_NL), coef(fit2_NL_w25)) #-5.77 -5.76 15.28 % 
MR(coef(fit2_NL), coef(fit2_NL_w)) # -4.94 -4.83 15.19 % 



#Envelope ficou ótimo (chocante)
envelope(fit3_NL, rep=100, conf=0.95, type="quantile")
envelope(fit3_NL, rep=100, conf=0.95, type="deviance")
envelope(fit3_NL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit3_NL
source(here::here("rdocs/Pacotes", "diag_norm_mlg.R"))
#Utilizar só elevação parece retirar um pouco da dependência geográfica. 

# 2  3  4  7  8 20 25
# 3  8 25
# 25

fit3_NL_w3 <- glm(Srich ~ Elevation + Habitat , 
                 family=gaussian(link = "log"),
                 data = ants,
                 subset=-c(3));summary(fit3_NL_w3)

fit3_NL_w8 <- glm(Srich ~ Elevation + Habitat , 
                  family=gaussian(link = "log"),
                  data = ants,
                  subset=-c(8));summary(fit3_NL_w8)

fit3_NL_w25 <- glm(Srich ~ Elevation + Habitat , 
                  family=gaussian(link = "log"),
                  data = ants,
                  subset=-c(25));summary(fit3_NL_w25)

fit3_NL_w <- glm(Srich ~ Elevation + Habitat , 
                  family=gaussian(link = "log"),
                  data = ants,
                  subset=-c(3, 8 ,25));summary(fit3_NL_w)


#***mudança nas estimativas***# (Lidou melhor)
MR(coef(fit3_NL), coef(fit3_NL_w3)) #-0.18 -1.45 -6.65 % 
MR(coef(fit3_NL), coef(fit3_NL_w8)) # 1.92 16.54 -2.17 % 
MR(coef(fit3_NL), coef(fit3_NL_w25)) # -5.01 0.3 14.56 % 
MR(coef(fit3_NL), coef(fit3_NL_w)) # -2.98 18.27 5.59 % 

