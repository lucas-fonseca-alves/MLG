source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(ants); attach(ants)

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

#------------------------------------------------------------------------------#---- 
#               Modelo Normal - Ligação Log (ótimo, chocante)                  #
#------------------------------------------------------------------------------#----
#*** Ajustes ***#
fit1_NL <- glm(Srich ~ Latitude + Elevation + Habitat , 
               family=gaussian(link = "log"))
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
source(here::here("rdocs/Pacotes", "envel_norm.R"))
source(here::here("rdocs/Pacotes", "diag_norm_mlg.R"))




