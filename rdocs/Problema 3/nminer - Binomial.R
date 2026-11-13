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
#               Modelo Binomial - Ligação Logito (fit8 ganhou)                 #
#------------------------------------------------------------------------------#----

#*************************** Métodos automatizados ****************************#----
# Modelo nulo (apenas o intercepto)
modelo_nulo <- glm(Miners ~ 1,
                   family = binomial(link = "logit"),
                   data = nminer);summary(modelo_nulo)

modelo_completo_Grazed <- glm(Miners ~ Bulokes + Area + Timber + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer);summary(modelo_completo_Grazed)

modelo_completo_Shrubs <- glm(Miners ~ Bulokes + Area + Timber + Shrubs,
                              family = binomial(link = "logit"),
                              data = nminer);summary(modelo_completo_Shrubs)

stepAIC(modelo_completo_Shrubs,
        direction = "backward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs + Timber + Shrubs
# Miners ~ Bulokes

stepAIC(modelo_nulo,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Shrubs),
        direction = "forward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs
#Miners ~ Bulokes

stepAIC(modelo_completo_Shrubs,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Shrubs),
        direction = "both")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs + Timber + Shrubs
#Miners ~ Bulokes
#-------------------------------------------------------------------------------#----

# OBS: conforme sugestão ad professora, devido as poucas observações e como tudo é assintótico
# podemos pensar em "aceitar" covariáveis com 10% ou valores próximo de significÂncia, pois
# só EUCS em si é um modelo que não acomoda bem todos os pontos
# Verificar também modelos com variáveis não significativas as que geram bons ajustes, para
# verificar se há um diagnóstico melhor. 


#*** Ajustes ***#

fit1_Blogit <- glm(Miners ~ Eucs,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit1_Blogit)


# Modelo com covariáveis maiores que 10% mas que vale a pena verificar o diagnóstico
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


#A 10% todas variáveis são significantes
fit8_Blogit <- glm(Miners ~ Bulokes + Timber + Shrubs,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit8_Blogit)

fit9_Blogit <- glm(Miners ~ Bulokes,
                   family = binomial(link = "logit"),
                   data = nminer);summary(fit9_Blogit)


##**********************************Diagnóstico**********************************#----
#*** Envelope ***#
#GERAR MAIS DE UM ENVELOPE QUANTÍLICO POIS É DADO DISCRETO
#Envelope ficou ótimo (chocante)
envelope(fit1_Blogit, rep=100, conf=0.95, type="quantile")
envelope(fit1_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit1_Blogit, rep=100, conf=0.95, type="pearson")

envelope(fit2_Blogit, rep=100, conf=0.95, type="quantile")
envelope(fit2_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit2_Blogit, rep=100, conf=0.95, type="pearson")

#Nem todos os envelopes quantílicos convergem 
envelope(fit3_Blogit, rep=100, conf=0.95, type="quantile")
#Envelopes ficam terríveis não quantílicos ficam feios
envelope(fit3_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit3_Blogit, rep=100, conf=0.95, type="pearson")

#A maioria dos envelopes quantílicos convergem, 
envelope(fit4_Blogit, rep=100, conf=0.95, type="quantile")
#Envelopes ficam terríveis não quantílicos ficam feios
envelope(fit4_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit4_Blogit, rep=100, conf=0.95, type="pearson")

#Fica Ok, quando gera vários
envelope(fit5_Blogit, rep=100, conf=0.95, type="quantile")
#Envolopes não quantílicos fica bons até
envelope(fit5_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit5_Blogit, rep=100, conf=0.95, type="pearson")

#Envelopes quantílicos convergem 
envelope(fit6_Blogit, rep=100, conf=0.95, type="quantile")
#Envelopes naõ quantílicos passam
envelope(fit6_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit6_Blogit, rep=100, conf=0.95, type="pearson")

#Envelops quantílicos convergem
envelope(fit7_Blogit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit7_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit7_Blogit, rep=100, conf=0.95, type="pearson")

#Não há evidências para rejeitar que a distribuição binomial seja inadequada para nenhum
#dos modelos

#Envelopes quantílicos gerar mais para ter certeza, pq ficou metade metade
envelope(fit8_Blogit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit8_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit8_Blogit, rep=100, conf=0.95, type="pearson")

envelope(fit9_Blogit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit9_Blogit, rep=100, conf=0.95, type="deviance")
envelope(fit9_Blogit, rep=100, conf=0.95, type="pearson")

#*** Gráficos de diagnóstico ***# 
#Numa visualização geral, ficou bom o ajuste. verificar os pontos
fit.model <- fit1_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
#fAZER OS DOIS GRÁFICOS DE BAIXO  COM O RESIDUO QUANTILICO

# Gráfico de alavanva parece que os pontos estão sobrepostos
#Distância de Cook pontos: 18, 28
# RCD x índice: 18, 28
# RCD x Valor Ajustado: 18, 28 (Nesse caso escolhi os pontos extremos mais isolados, 
# mas sem uma justificativa direta, apenas intuição)

#*** Mudança Relativa ***#
fit1_Blogit_w18 <- glm(Miners ~ Eucs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(18));summary(fit1_Blogit_w18)
# Ao retirar o 18, não há mudança de sinal nem de significância, entretanto a estimativa 
# é quase o dobro 
fit1_Blogit_w28 <- glm(Miners ~ Eucs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit1_Blogit_w28)
# Ao retirar o 28, não há mudança de sinal nem de significância,

summary(fit1_Blogit)
fit1_Blogit_w <- glm(Miners ~ Eucs,
                     family = binomial(link = "logit"),
                     data = nminer,
                     subset=-c(18, 28));summary(fit1_Blogit_w)
#Retirar as duas Aumenta Muito a estimativa, e há mudança na significância da variável.
#Muda a interpretação (Problema), mas pelo número da amostra esse seria um possível problema


#***mudança nas estimativas***#
MR(coef(fit1_Blogit), coef(fit1_Blogit_w18)) # 73.01 58.15 % (PROBLEMMÁTICO)
MR(coef(fit1_Blogit), coef(fit1_Blogit_w28)) # 16.87 25.83 % 
MR(coef(fit1_Blogit), coef(fit1_Blogit_w))   # 277.82 270.81 % (PROBLEMÁTICO)
 

fit.model <- fit2_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))

# Gráfico de alavanva parece que os pontos estão sobrepostos (29, 14,7)
#Distância de Cook pontos: 7, 18, 21, 28, 29
# RCD x índice: 18, 28, 29
# RCD x Valor Ajustado: 18, 28, 29 (Nesse caso escolhi os pontos extremos mais isolados, 
# mas sem uma justificativa direta, apenas intuição)

#*** Mudança Relativa ***#
summary(fit2_Blogit)
fit2_Blogit_w18 <- glm(Miners ~ Eucs + Timber + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(18));summary(fit2_Blogit_w18)
# Ao retirar o 18, não há mudança de sinal nem de significância

summary(fit2_Blogit)
fit2_Blogit_w28 <- glm(Miners ~ Eucs + Timber + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit2_Blogit_w28)

summary(fit2_Blogit)
fit2_Blogit_w29 <- glm(Miners ~ Eucs + Timber + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(29));summary(fit2_Blogit_w29)

summary(fit2_Blogit)
fit2_Blogit_w <- glm(Miners ~ Eucs + Timber + Grazed,
                     family = binomial(link = "logit"),
                     data = nminer,
                     subset=-c(18, 28, 29));summary(fit2_Blogit_w)


#***mudança nas estimativas***# (ponto 28 é um problema)
MR(coef(fit2_Blogit), coef(fit2_Blogit_w18)) # 39.45   19.27   -6.14   -2.36 % 
MR(coef(fit2_Blogit), coef(fit2_Blogit_w28)) # 1727.91 3103.64 3999.59 2866.64 % 
MR(coef(fit2_Blogit), coef(fit2_Blogit_w29)) # 54.64   60.7    118.58  113.46 % 
MR(coef(fit2_Blogit), coef(fit2_Blogit_w))   # 2196.41 2619.19 2997.81 2549.89 % 



fit.model <- fit3_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
# 7 19 29
# 7, 18, 21, 28, 29
# 14, 18, 21, 28
# 14, 18, 28

summary(fit3_Blogit)
fit3_Blogit_w18 <- glm(Miners ~ Eucs + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(18));summary(fit3_Blogit_w18)
# Ao retirar o 18, não há mudança de sinal nem de significância

summary(fit3_Blogit)
fit3_Blogit_w28 <- glm(Miners ~ Eucs + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit3_Blogit_w28)

summary(fit3_Blogit)
fit3_Blogit_w14 <- glm(Miners ~ Eucs + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(14));summary(fit3_Blogit_w14)

summary(fit3_Blogit)
fit3_Blogit_w <- glm(Miners ~ Eucs + Timber + Shrubs,
                     family = binomial(link = "logit"),
                     data = nminer,
                     subset=-c(14, 18, 28));summary(fit3_Blogit_w)


#***mudança nas estimativas***# (ponto 28 é um problema)
MR(coef(fit3_Blogit), coef(fit3_Blogit_w18)) # 324.54   62.58 -13.35 7.05 % 
MR(coef(fit3_Blogit), coef(fit3_Blogit_w28)) # 41.01    18.47 8.26 -22.67 %  
MR(coef(fit3_Blogit), coef(fit3_Blogit_w14)) # -1261.32 252.08 764.9 677.21 
MR(coef(fit3_Blogit), coef(fit3_Blogit_w))   # -6172.96 3073.6 5810.5 4812.73 %  


fit.model <- fit4_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
# Parece um arco, o que dá a entender que não tem nenhum ponto que se destaca
# 7, 18, 21, 28, 26
# 16
# 13, 16
# 18, 21, 28

summary(fit4_Blogit)
fit4_Blogit_w7 <- glm(Miners ~ Eucs + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(7));summary(fit4_Blogit_w7)


summary(fit4_Blogit)
fit4_Blogit_w18 <- glm(Miners ~ Eucs + Grazed,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(18));summary(fit4_Blogit_w18)


summary(fit4_Blogit)
fit4_Blogit_w21 <- glm(Miners ~ Eucs + Grazed,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(21));summary(fit4_Blogit_w21)

summary(fit4_Blogit)
fit4_Blogit_w26 <- glm(Miners ~ Eucs + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(26));summary(fit4_Blogit_w26)


summary(fit4_Blogit)
fit4_Blogit_w28 <- glm(Miners ~ Eucs + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit4_Blogit_w28)

summary(fit4_Blogit)
fit4_Blogit_w <- glm(Miners ~ Eucs + Grazed,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(7, 18, 21, 26, 28));summary(fit4_Blogit_w)

MR(coef(fit4_Blogit), coef(fit4_Blogit_w7))  # 20.26 9.62  40 %
MR(coef(fit4_Blogit), coef(fit4_Blogit_w18)) # 58.61 54.54 0.2 % 
MR(coef(fit4_Blogit), coef(fit4_Blogit_w21)) # 21.99 20.54 71.99 % 
MR(coef(fit4_Blogit), coef(fit4_Blogit_w26)) # 0.18  0.17  24.73 % 
MR(coef(fit4_Blogit), coef(fit4_Blogit_w28)) #-0.16  12.66 -25.59 %  
MR(coef(fit4_Blogit), coef(fit4_Blogit_w))   # 1624.48 1586.23 2210.01 %  


fit.model <- fit5_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
# Parece um arco, o que dá a entender que não tem nenhum ponto que se destaca
# 7, 18, 21, 28, 26
# 18, 21, 28
# 7, 18, 26

summary(fit5_Blogit)
fit5_Blogit_w7 <- glm(Miners ~ Eucs + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(7));summary(fit5_Blogit_w7)

summary(fit5_Blogit)
fit5_Blogit_w18 <- glm(Miners ~ Eucs + Shrubs,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(18));summary(fit5_Blogit_w18)

summary(fit5_Blogit)
fit5_Blogit_w21 <- glm(Miners ~ Eucs + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(21));summary(fit5_Blogit_w21)

summary(fit5_Blogit)
fit5_Blogit_w26 <- glm(Miners ~ Eucs + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(26));summary(fit5_Blogit_w26)

summary(fit5_Blogit)
fit5_Blogit_w28 <- glm(Miners ~ Eucs + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit5_Blogit_w28)

fit5_Blogit_w <- glm(Miners ~ Eucs + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(7, 18, 21, 26, 28));summary(fit5_Blogit_w)



MR(coef(fit5_Blogit), coef(fit5_Blogit_w7))   # 10.28 9.83 45 %  
MR(coef(fit5_Blogit), coef(fit5_Blogit_w18))  # 92.03 61.53 9.09 %
MR(coef(fit5_Blogit), coef(fit5_Blogit_w21))  # -2.92 15.28 57.36 % 
MR(coef(fit5_Blogit), coef(fit5_Blogit_w26))  # 14.99 14.32 -32.19 % 
MR(coef(fit5_Blogit), coef(fit5_Blogit_w28))  # -11.58 -1.9 18.63 % 
MR(coef(fit5_Blogit), coef(fit5_Blogit_w))    # 3011.62 3073.51 2515.79 %


fit.model <- fit6_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))

# 18, 21, 28
# 18, 28
# 18, 28

summary(fit6_Blogit)
fit6_Blogit_w18 <- glm(Miners ~ Eucs + Area,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(18));summary(fit6_Blogit_w18)

summary(fit6_Blogit)
fit6_Blogit_w21 <- glm(Miners ~ Eucs + Area,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(21));summary(fit6_Blogit_w21)

summary(fit6_Blogit)
fit6_Blogit_w28 <- glm(Miners ~ Eucs + Area,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit6_Blogit_w28)

summary(fit6_Blogit)
fit6_Blogit_w <- glm(Miners ~ Eucs + Area,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(18, 21, 28));summary(fit6_Blogit_w)


MR(coef(fit6_Blogit), coef(fit6_Blogit_w18))
MR(coef(fit6_Blogit), coef(fit6_Blogit_w21))
MR(coef(fit6_Blogit), coef(fit6_Blogit_w28))     
MR(coef(fit6_Blogit), coef(fit6_Blogit_w))

fit.model <- fit7_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
# 18, 28
# 5, 18, 21, 28
# 5, 18, 28

summary(fit7_Blogit)
fit7_Blogit_w5 <- glm(Miners ~ Eucs + Bulokes,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(5));summary(fit7_Blogit_w5)

summary(fit7_Blogit)
fit7_Blogit_w18 <- glm(Miners ~ Eucs + Bulokes,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(18));summary(fit7_Blogit_w18)

summary(fit7_Blogit)
fit7_Blogit_w21 <- glm(Miners ~ Eucs + Bulokes,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(21));summary(fit7_Blogit_w21)

summary(fit7_Blogit)
fit7_Blogit_w28 <- glm(Miners ~ Eucs + Bulokes,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(28));summary(fit7_Blogit_w28)

summary(fit7_Blogit)
fit7_Blogit_w <- glm(Miners ~ Eucs + Bulokes,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(5, 18, 21, 28));summary(fit7_Blogit_w)

MR(coef(fit7_Blogit), coef(fit7_Blogit_w5))   # -0.16 12.66 -25.59 %  
MR(coef(fit7_Blogit), coef(fit7_Blogit_w18))
MR(coef(fit7_Blogit), coef(fit7_Blogit_w21))
MR(coef(fit7_Blogit), coef(fit7_Blogit_w28))
MR(coef(fit7_Blogit), coef(fit7_Blogit_w))

fit.model <- fit8_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
# 2, 8, 10, 13
# 2, 10, 18, 28
# 18, 20
# 18, 28

summary(fit8_Blogit)
fit8_Blogit_w2 <- glm(Miners ~ Bulokes + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(2));summary(fit8_Blogit_w2)
summary(fit8_Blogit)
fit8_Blogit_w10 <- glm(Miners ~ Bulokes + Timber + Shrubs,
                      family = binomial(link = "logit"),
                      data = nminer,
                      subset=-c(10));summary(fit8_Blogit_w10)

summary(fit8_Blogit)
fit8_Blogit_w18 <- glm(Miners ~ Bulokes + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(18));summary(fit8_Blogit_w18)

fit8_Blogit_w28 <- glm(Miners ~ Bulokes + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(28));summary(fit8_Blogit_w28)

fit8_Blogit_w <- glm(Miners ~ Bulokes + Timber + Shrubs,
                       family = binomial(link = "logit"),
                       data = nminer,
                       subset=-c(2, 10, 28, 28));summary(fit8_Blogit_w)

MR(coef(fit8_Blogit), coef(fit8_Blogit_w2))   #0.31 8.58 -6.22 -14.45 %
MR(coef(fit8_Blogit), coef(fit8_Blogit_w10))  #-6.39 2.4 -10.55 -10.21 % 
MR(coef(fit8_Blogit), coef(fit8_Blogit_w18))  #-5.45 -3.73 -4.88 -6.83 %
MR(coef(fit8_Blogit), coef(fit8_Blogit_w28))  #5.03 8.49 3.42 -10.81 % 
MR(coef(fit8_Blogit), coef(fit8_Blogit_w))    #-0.21 24.06 -14.66 -41.45 %


fit.model <- fit9_Blogit
source(here::here("rdocs/Pacotes", "diag_bino.R"))

#------------------------------------------------------------------------------#---- 
#               Modelo Binomial - Ligação Probito                               #
#------------------------------------------------------------------------------#----

#*************************** Métodos automatizados ****************************#----
# Modelo nulo (apenas o intercepto)
modelo_nulo <- glm(Miners ~ 1,
                   family = binomial(link = "probit"),
                   data = nminer);summary(modelo_nulo)

modelo_completo_Grazed <- glm(Miners ~ Bulokes + Area + Timber + Grazed,
                              family = binomial(link = "probit"),
                              data = nminer);summary(modelo_completo_Grazed)

modelo_completo_Shrubs <- glm(Miners ~ Bulokes + Area + Timber + Shrubs,
                              family = binomial(link = "probit"),
                              data = nminer);summary(modelo_completo_Shrubs)

stepAIC(modelo_completo_Grazed,
        direction = "backward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs + Timber + Shrubs
# Miners ~ Bulokes + Timber + Shrubs
# Miners ~ Bulokes
stepAIC(modelo_nulo,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Grazed),
        direction = "forward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs
#Miners ~ Bulokes

stepAIC(modelo_completo_Grazed,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Grazed),
        direction = "both")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs + Timber + Shrubs
#Miners ~ Bulokes + Timber + Shrubs
#Miners ~Bulokes
#-------------------------------------------------------------------------------#----


fit1_Bprobit <- glm(Miners ~ Bulokes,
                   family = binomial(link = "probit"),
                   data = nminer);summary(fit1_Bprobit)

envelope(fit1_Bprobit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit1_Bprobit, rep=100, conf=0.95, type="deviance")
envelope(fit1_Bprobit, rep=100, conf=0.95, type="pearson")

fit.model <- fit1_Bprobit
source(here::here("rdocs/Pacotes", "diag_bino.R"))



fit2_Bprobit <- glm(Miners ~ Bulokes + Timber + Shrubs,
                    family = binomial(link = "probit"),
                    data = nminer);summary(fit2_Bprobit)

envelope(fit2_Bprobit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit2_Bprobit, rep=100, conf=0.95, type="deviance")
envelope(fit2_Bprobit, rep=100, conf=0.95, type="pearson")

fit.model <- fit2_Bprobit
source(here::here("rdocs/Pacotes", "diag_bino.R"))


fit3_Bprobit <- glm(Miners ~ Eucs,
                    family = binomial(link = "probit"),
                    data = nminer);summary(fit3_Bprobit)

envelope(fit3_Bprobit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit3_Bprobit, rep=100, conf=0.95, type="deviance")
envelope(fit3_Bprobit, rep=100, conf=0.95, type="pearson")

fit.model <- fit3_Bprobit
source(here::here("rdocs/Pacotes", "diag_bino.R"))


#A função de ligação ficou melhor que logit
fit4_Bprobit <- glm(Miners ~ Eucs + Timber + Grazed,
                    family = binomial(link = "probit"),
                    data = nminer);summary(fit4_Bprobit)

envelope(fit4_Bprobit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit4_Bprobit, rep=100, conf=0.95, type="deviance")
envelope(fit4_Bprobit, rep=100, conf=0.95, type="pearson")

fit.model <- fit4_Bprobit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
# 28, 29
# 21, 28 ,29, 31
# 15, 21, 28
# 18, 28

summary(fit4_Bprobit)
fit4_Bprobit_w18 <- glm(Miners ~ Eucs + Timber + Grazed,
                        family = binomial(link = "probit"),
                        data = nminer,
                        subset=-c(18));summary(fit4_Bprobit_w18)

summary(fit4_Bprobit)
fit4_Bprobit_w21 <- glm(Miners ~ Eucs + Timber + Grazed,
                        family = binomial(link = "probit"),
                        data = nminer,
                        subset=-c(21));summary(fit4_Bprobit_w21)

summary(fit4_Bprobit)
fit4_Bprobit_w28 <- glm(Miners ~ Eucs + Timber + Grazed,
                      family = binomial(link = "probit"),
                      data = nminer,
                      subset=-c(28));summary(fit4_Bprobit_w28)

summary(fit4_Bprobit)
fit4_Bprobit_w29 <- glm(Miners ~ Eucs + Timber + Grazed,
                        family = binomial(link = "probit"),
                        data = nminer,
                        subset=-c(29));summary(fit4_Bprobit_w29)

summary(fit4_Bprobit)
fit4_Bprobit_w <- glm(Miners ~ Eucs + Timber + Grazed,
                        family = binomial(link = "probit"),
                        data = nminer,
                        subset=-c(18, 21, 28, 29));summary(fit4_Bprobit_w)

MR(coef(fit4_Bprobit), coef(fit4_Bprobit_w18))  #36.92 17.44 -10.18 -7.53 % 
MR(coef(fit4_Bprobit), coef(fit4_Bprobit_w28))  #806.78 1527.25 2015.42 1405.95 % 
MR(coef(fit4_Bprobit), coef(fit4_Bprobit_w29))  #62.78 70.99 135 127.64 % 
MR(coef(fit4_Bprobit), coef(fit4_Bprobit_w))    #807.42 932.8 861.92 761.02 % 


#------------------------------------------------------------------------------#---- 
#               Modelo Binomial - Ligação Cauchy  (lixo)                       #
#------------------------------------------------------------------------------#----

#*************************** Métodos automatizados ****************************#----
# Modelo nulo (apenas o intercepto)
modelo_nulo <- glm(Miners ~ 1,
                   family = binomial(link = "cauchit"),
                   data = nminer);summary(modelo_nulo)

modelo_completo_Grazed <- glm(Miners ~ Bulokes + Area + Timber + Grazed,
                              family = binomial(link = "cauchit"),
                              data = nminer);summary(modelo_completo_Grazed)

modelo_completo_Shrubs <- glm(Miners ~ Bulokes + Area + Timber + Shrubs,
                              family = binomial(link = "cauchit"),
                              data = nminer);summary(modelo_completo_Shrubs)

stepAIC(modelo_completo_Grazed,
        direction = "backward")
#Miners ~ Eucs + Timber + Grazed
#Miners ~ Eucs
#Miners ~Bulokes
stepAIC(modelo_nulo,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Grazed),
        direction = "forward")
#miners ~ eucs
#Miners ~Bulokes


stepAIC(modelo_completo_Grazed,
        scope = list(lower = modelo_nulo,
                     upper = modelo_completo_Grazed),
        direction = "both")
# Miners ~ Eucs + Timber + Grazed
# Miners ~Bulokes
#-------------------------------------------------------------------------------#----

#*************************** Ajuste + Diagnóstico ****************************#----
fit1_Bcauchit <- glm(Miners ~ Eucs,
                    family = binomial(link = "cauchit"),
                    data = nminer);summary(fit1_Bcauchit)

envelope(fit1_Bcauchit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit1_Bcauchit, rep=100, conf=0.95, type="deviance")
envelope(fit1_Bcauchit, rep=100, conf=0.95, type="pearson")

fit.model <- fit1_Bcauchit
source(here::here("rdocs/Pacotes", "diag_bino.R"))


fit2_Bcauchit <- glm(Miners ~ Eucs + Timber + Grazed,
                    family = binomial(link = "cauchit"),
                    data = nminer);summary(fit2_Bcauchit)

envelope(fit2_Bcauchit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit2_Bcauchit, rep=100, conf=0.95, type="deviance")
envelope(fit2_Bcauchit, rep=100, conf=0.95, type="pearson")

fit.model <- fit2_Bcauchit
source(here::here("rdocs/Pacotes", "diag_bino.R"))



fit3_Bcauchit <- glm(Miners ~ Bulokes,
                     family = binomial(link = "cauchit"),
                     data = nminer);summary(fit3_Bcauchit)

envelope(fit3_Bcauchit, rep=100, conf=0.95, type="quantile")
#Envelopes não quantílicos passa 
envelope(fit3_Bcauchit, rep=100, conf=0.95, type="deviance")
envelope(fit3_Bcauchit, rep=100, conf=0.95, type="pearson")

fit.model <- fit3_Bcauchit
source(here::here("rdocs/Pacotes", "diag_bino.R"))
#-------------------------------------------------------------------------------#----



