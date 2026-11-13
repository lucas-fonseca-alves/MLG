source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime)

lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))

attach(lime)


# Não utilizar DBH e Age juntas, pois, tem alta multicolinearidade
#------------------------------------------------------------------------------#---- 
#                     Modelo Normal - Ligação Log (RUINS)                      #
#------------------------------------------------------------------------------#----
#*** Ajustes ***#
fit1_NL <- glm(Foliage ~ DBH + Origin , 
              family=gaussian(link = "log"))
summary(fit1_NL)

fit2_NL <- glm(Foliage ~ Age + Origin , 
               family=gaussian(link = "log"))
summary(fit2_NL)

fit3_NL <- glm(Foliage ~ DBH, 
               family=gaussian(link = "log"))
summary(fit3_NL)

fit4_NL <- glm(Foliage ~ Age, 
               family=gaussian(link = "log"))
summary(fit4_NL)

fit5_NL <- glm(Foliage ~ Origin, 
               family=gaussian(link = "log"))
summary(fit5_NL)

fit6_NL <- glm(Foliage ~ DBH+Origin +DBH*Origin , 
               family=gaussian(link = "log"))
summary(fit6_NL)

fit7_NL <- glm(Foliage ~ log(DBH) , 
               family=gaussian(link = "log"))
summary(fit7_NL)

fit8_NL <- glm(Foliage ~ log(Age) + Origin, 
               family=gaussian(link = "log"))
summary(fit8_NL)

fit9_NL <- glm(Foliage ~ log(DBH), 
               family=gaussian(link = "log"))
summary(fit9_NL)

fit10_NL <- glm(Foliage ~ log(Age), 
               family=gaussian(link = "log"))
summary(fit10_NL)


#-------------------------------------------------------------------------------#----

#**********************************Diagnóstico**********************************#----

#*** Avaliando a suposicao de distribuicao Normal da resposta com Ligação Log ***#
#Envelope horrível
envelope(fit1_NL, rep=100, conf=0.95, type="quantile")
envelope(fit1_NL, rep=100, conf=0.95, type="deviance")
envelope(fit1_NL, rep=100, conf=0.95, type="pearson")

#Envelope horrível
envelope(fit2_NL, rep=100, conf=0.95, type="quantile")
envelope(fit2_NL, rep=100, conf=0.95, type="deviance")
envelope(fit2_NL, rep=100, conf=0.95, type="pearson")

#Envelope horrível
envelope(fit3_NL, rep=100, conf=0.95, type="quantile")
envelope(fit3_NL, rep=100, conf=0.95, type="deviance")
envelope(fit3_NL, rep=100, conf=0.95, type="pearson")

#Envelope horrível
envelope(fit4_NL, rep=100, conf=0.95, type="quantile")
envelope(fit4_NL, rep=100, conf=0.95, type="deviance")
envelope(fit4_NL, rep=100, conf=0.95, type="pearson")

#Envelope horrível
envelope(fit5_NL, rep=100, conf=0.95, type="quantile")
envelope(fit5_NL, rep=100, conf=0.95, type="deviance")
envelope(fit5_NL, rep=100, conf=0.95, type="pearson")

#Envelope horrível
envelope(fit6_NL, rep=100, conf=0.95, type="quantile")
envelope(fit6_NL, rep=100, conf=0.95, type="deviance")
envelope(fit6_NL, rep=100, conf=0.95, type="pearson")

#Envelope Horrível
envelope(fit7_NL, rep=100, conf=0.95, type="quantile")
envelope(fit7_NL, rep=100, conf=0.95, type="deviance")
envelope(fit7_NL, rep=100, conf=0.95, type="pearson")

#Envelope horrível (2)
envelope(fit8_NL, rep=100, conf=0.95, type="quantile")
envelope(fit8_NL, rep=100, conf=0.95, type="deviance")
envelope(fit8_NL, rep=100, conf=0.95, type="pearson")


#Menos horrível (1)
envelope(fit9_NL, rep=100, conf=0.95, type="quantile")
envelope(fit9_NL, rep=100, conf=0.95, type="deviance")
envelope(fit9_NL, rep=100, conf=0.95, type="pearson")

envelope(fit10_NL, rep=100, conf=0.95, type="quantile")
envelope(fit10_NL, rep=100, conf=0.95, type="deviance")
envelope(fit10_NL, rep=100, conf=0.95, type="pearson")


#Acho que o modelo fit7_NL é o menos pior dentre todos, já que os pontos ficaram mais próximos
#do envolope

#** Diagnostico - Scripts Prof. Gilberto **#
fit.model <- fit1_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

#Modelo 1 foge da suposição de homocedasticidade!
#Modelos não cumprem a suposição de homocedasticidade! 

fit.model <- fit2_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))


fit.model <- fit3_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

fit.model <- fit4_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

#Apenas a variável categórica
fit.model <- fit5_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

fit.model <- fit6_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

fit.model <- fit7_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

#Não tenho certeza da violação de homocedasticidade
fit.model <- fit8_NL
source(here::here("rdocs/Pacotes", "diag_norm_mlg.R"))
# 37  51  92 108 144 145 355 360 365
# 15  37 127 213 216 217 226 227 229 232 377 380 383
# 13  14  15  19 37 65  90 117 127 213 216 217 226 227 228 229 230 232 233 239 308 327 377 380 383
# 13  14  15  19  37  65  90 117 127 213 216 217 226 227 228 229 230 232 233 308 327 377 380 383


fit.model <- fit9_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))

fit.model <- fit10_NL
source(here::here("rdocs/Pacotes", "diag_norm.R"))


