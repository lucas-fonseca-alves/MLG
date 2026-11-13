source(here::here("rdocs/Pacotes", "Pacotes.R"))

# Dados
data(lime)

lime <- lime %>%
  mutate(Origin = fct_reorder(Origin, Foliage, .fun = median))

attach(lime)
# Não utilizar DBH e Age juntas, pois, tem alta multicolinearidade
#------------------------------------------------------------------------------#---- 
#                     Modelo Gama - Ligação Log                                #
#------------------------------------------------------------------------------#----
#*** Ajustes ***#
fit1_GL <- glm(Foliage ~ DBH + Origin , 
               family=Gamma(link = "log"))
summary(fit1_GL)

fit2_GL <- glm(Foliage ~ Age + Origin , 
               family=Gamma(link = "log"))
summary(fit2_GL)

fit3_GL <- glm(Foliage ~ DBH, 
               family=Gamma(link = "log"))
summary(fit3_GL)

fit4_GL <- glm(Foliage ~ Age, 
               family=Gamma(link = "log"))
summary(fit4_GL)

fit5_GL <- glm(Foliage ~ log(DBH) + Origin , 
               family=Gamma(link = "log"))
summary(fit5_GL)

fit6_GL <- glm(Foliage ~ log(Age) + Origin , 
               family=Gamma(link = "log"))
summary(fit6_GL)

fit7_GL <- glm(Foliage ~ log(DBH), 
               family=Gamma(link = "log"))
summary(fit7_GL)

fit8_GL <- glm(Foliage ~ log(Age), 
               family=Gamma(link = "log"))
summary(fit8_GL)

#-------------------------------------------------------------------------------#----

#**********************************Diagnóstico**********************************#----

#*** Avaliando a suposicao de distribuicao Gamma da resposta com Ligação Log ***#
envelope(fit1_GL, rep=100, conf=0.95, type="quantile")
envelope(fit1_GL, rep=100, conf=0.95, type="deviance")
envelope(fit1_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit1_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))

envelope(fit2_GL, rep=100, conf=0.95, type="quantile")
envelope(fit2_GL, rep=100, conf=0.95, type="deviance")
envelope(fit2_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit2_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit3_GL, rep=100, conf=0.95, type="quantile")
envelope(fit3_GL, rep=100, conf=0.95, type="deviance")
envelope(fit3_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit3_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit4_GL, rep=100, conf=0.95, type="quantile")
envelope(fit4_GL, rep=100, conf=0.95, type="deviance")
envelope(fit4_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit4_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit5_GL, rep=100, conf=0.95, type="quantile")
envelope(fit5_GL, rep=100, conf=0.95, type="deviance")
envelope(fit5_GL, rep=100, conf=0.95, type="pearson")

#** Diagnostico - Scripts Prof. Gilberto **# #Numa olhada geral, esse ganharia
fit.model <- fit5_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))

envelope(fit6_GL, rep=100, conf=0.95, type="quantile")
envelope(fit6_GL, rep=100, conf=0.95, type="deviance")
envelope(fit6_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit6_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


envelope(fit7_GL, rep=100, conf=0.95, type="quantile")
envelope(fit7_GL, rep=100, conf=0.95, type="deviance")
envelope(fit7_GL, rep=100, conf=0.95, type="pearson")

fit.model <- fit7_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))


fit.model <- fit8_GL
source(here::here("rdocs/Pacotes", "diag_gama.R"))

envelope(fit8_GL, rep=100, conf=0.95, type="quantile")
envelope(fit8_GL, rep=100, conf=0.95, type="deviance")
envelope(fit8_GL, rep=100, conf=0.95, type="pearson")

#------------------------------------------------------------------------------#----