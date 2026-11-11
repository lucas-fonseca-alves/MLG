source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(nminer); 

nminer <- nminer %>% 
  dplyr::select(-Minerab) %>% 
  dplyr::mutate(Miners = factor(Miners, levels = c(0, 1)),
                Grazed = factor(Grazed, levels = c(0, 1)),
                Shrubs = factor(Shrubs, levels = c(0, 1)))
attach(nminer)
#------------------------------------------------------------------------------#---- 
#               Modelo Binomial - Ligação Logito                               #
#------------------------------------------------------------------------------#----

#*************************** Métodos automatizados ****************************#----
# Modelo nulo (apenas o intercepto)
modelo_nulo <- glm(Miners ~ 1,
                   family = binomial(link = "logit"),
                   data = nminer);summary(modelo_nulo)

modelo_completo_Grazed <- glm(Miners ~ Eucs + Bulokes + Area + Timber + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer);summary(modelo_completo_Grazed)

modelo_completo_Shrubs <- glm(Miners ~ Eucs + Bulokes + Area + Timber + Shrubs,
                              family = binomial(link = "logit"),
                              data = nminer);summary(modelo_completo_Shrubs)

stepAIC(modelo_completo_Shrubs,
        direction = "backward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs + Timber + Shrubs

stepAIC(modelo_nulo,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Shrubs),
        direction = "forward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs

stepAIC(modelo_completo_Shrubs,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Shrubs),
        direction = "both")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs + Timber + Shrubs
#-------------------------------------------------------------------------------#----

#*** Ajustes ***#

fit1_Blogit <- glm(Miners ~ Eucs,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit1_Blogit)

fit2_Blogit <- glm(Miners ~ Eucs + Timber + Grazed,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit2_Blogit)

fit3_Blogit <- glm(Miners ~ Eucs + Timber + Shrubs,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit3_Blogit)

fit4_Blogit <- glm(Miners ~ Eucs + Grazed,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit4_Blogit)

fit5_Blogit <- glm(Miners ~ Eucs + Shrubs,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit5_Blogit)

fit6_Blogit <- glm(Miners ~ Eucs + Area,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit6_Blogit)

fit7_Blogit <- glm(Miners ~ Eucs + Bulokes,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit7_Blogit)

fit8_Blogit <- glm(Miners ~ Eucs + Timber + Grazed,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit8_Blogit)

fit9_Blogit <- glm(Miners ~ Eucs + Timber + Shrubs,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit9_Blogit)


##**********************************Diagnóstico**********************************#----

#Envelope ficou ótimo (chocante)
envelope(fit1_Blogit, rep=100, conf=0.95, type="quantile")
envelope(fit1_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit1_Blogit, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit1_Blogit
source(here::here("rdocs/Pacotes", "envel_bino.R"))
source(here::here("rdocs/Pacotes", "diag_bino.R"))


#Envelope ficou ótimo (chocante)
envelope(fit2_Blogit, rep=100, conf=0.95, type="quantile")
envelope(fit2_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit2_Blogit, rep=100, conf=0.95, type="pearson")

#Numa visualização geral, ficou ok, mas o residuo x índice não tenho certeza se dá pra considerar independente
# e visualente a dist de cookie parece que está acomodando menos pontos (apenas visualmente)
fit.model <- fit2_Blogit
source(here::here("rdocs/Pacotes", "envel_bino.R"))
source(here::here("rdocs/Pacotes", "diag_bino.R"))
