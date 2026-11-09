source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime); attach(lime)

lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))

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
envelope(fit1_NL, rep=100, conf=0.95, type="quantile")
envelope(fit1_NL, rep=100, conf=0.95, type="deviance")
envelope(fit1_NL, rep=100, conf=0.95, type="pearson")

envelope(fit2_NL, rep=100, conf=0.95, type="quantile")
envelope(fit2_NL, rep=100, conf=0.95, type="deviance")
envelope(fit2_NL, rep=100, conf=0.95, type="pearson")

envelope(fit3_NL, rep=100, conf=0.95, type="quantile")
envelope(fit3_NL, rep=100, conf=0.95, type="deviance")
envelope(fit3_NL, rep=100, conf=0.95, type="pearson")

envelope(fit4_NL, rep=100, conf=0.95, type="quantile")
envelope(fit4_NL, rep=100, conf=0.95, type="deviance")
envelope(fit4_NL, rep=100, conf=0.95, type="pearson")

envelope(fit5_NL, rep=100, conf=0.95, type="quantile")
envelope(fit5_NL, rep=100, conf=0.95, type="deviance")
envelope(fit5_NL, rep=100, conf=0.95, type="pearson")

envelope(fit6_NL, rep=100, conf=0.95, type="quantile")
envelope(fit6_NL, rep=100, conf=0.95, type="deviance")
envelope(fit6_NL, rep=100, conf=0.95, type="pearson")

envelope(fit7_NL, rep=100, conf=0.95, type="quantile")
envelope(fit7_NL, rep=100, conf=0.95, type="deviance")
envelope(fit7_NL, rep=100, conf=0.95, type="pearson")

envelope(fit8_NL, rep=100, conf=0.95, type="quantile")
envelope(fit8_NL, rep=100, conf=0.95, type="deviance")
envelope(fit8_NL, rep=100, conf=0.95, type="pearson")

envelope(fit9_NL, rep=100, conf=0.95, type="quantile")
envelope(fit9_NL, rep=100, conf=0.95, type="deviance")
envelope(fit9_NL, rep=100, conf=0.95, type="pearson")

envelope(fit10_NL, rep=100, conf=0.95, type="quantile")
envelope(fit10_NL, rep=100, conf=0.95, type="deviance")
envelope(fit10_NL, rep=100, conf=0.95, type="pearson")


#Acho que o modelo fit7_NL é o menos pior dentre todos, já que os pontos ficaram mais próximos
#do envolope

#** Diagnostico Modelo 7 - Scripts Prof. Gilberto **#
fit.model <- fit4_NL
source(here::here("rdocs/Pacotes", "envel_norm.R"))
source(here::here("rdocs/Pacotes", "diag_norm_mlg.R"))

#Modelo 7 foge da suposição de homocedasticidade!
#Modelos não cumprem a suposição de homocedasticidade! 