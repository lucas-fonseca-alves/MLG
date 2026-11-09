source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(ants); attach(ants)

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

#------------------------------------------------------------------------------#---- 
#               Modelo Poisson - Ligação Log                                   #
#------------------------------------------------------------------------------#----
#*** Ajustes ***#
fit1_PL <- glm(Srich ~ Latitude + Elevation + Habitat , 
               family=poisson(link = "log"))
summary(fit1_PL)

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

##**********************************Diagnóstico**********************************#----

#Envelope ficou ótimo (chocante)
envelope(fit1_PL, rep=100, conf=0.95, type="quantile")
envelope(fit1_PL, rep=100, conf=0.95, type="deviance")
envelope(fit1_PL, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit1_PL
source(here::here("rdocs/Pacotes", "envel_pois.R"))
source(here::here("rdocs/Pacotes", "diag_pois.R"))

