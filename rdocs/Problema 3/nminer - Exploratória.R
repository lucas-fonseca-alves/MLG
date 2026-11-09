source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(nminer); attach(nminer)

nminer <- nminer %>% 
  dplyr::select(-Minerab) %>% 
  dplyr::mutate(Miners = as.factor(Miners),
                Grazed = as.factor(Grazed),
                Shrubs = as.factor(Shrubs))


#------------------------------------------------------------------------------#---- 
#                               Correlação                                     #
#------------------------------------------------------------------------------#----

mat_corr <- cor(nminer[,-8])

ggcorrplot(mat_corr,
           method = "square",       # ou "circle"
           #type   = "lower",        # mostra só o triângulo inferior
           lab    = TRUE,           # mostra os valores
           lab_size = 3,
           colors = c("firebrick", "white", "steelblue"),
           title = "Matriz de correlação",
           ggtheme = theme_minimal())

# EUCS é a variável que tem maior correlação 0.73 com Miners
# Variáveis não parecem ter alta correlação entre si, o maior valor é -0.64 entre shrubs
# e Grazed
# As variáveis área e grazed tem correlação baixa com Miners
# A maioria das variáveis tem correlação negativa com a resposta. 


#------------------------------------------------------------------------------#---- 
#                         Variáveis individuais                                #
#------------------------------------------------------------------------------#----

#*** Miners (Resposta)***#

ggplot(nminer, aes(x = Miners)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(x = "Presença de noisy miners", y = "Frequência") +
  theme_minimal()

#*** Grazed ***#

ggplot(nminer, aes(x = Grazed)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(x = "Se a área foi pastada", y = "Frequência") +
  theme_minimal()

#*** Shrubs ***#

ggplot(nminer, aes(x = Shrubs)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(x = "Se havia arbustos presentes na área", y = "Frequência") +
  theme_minimal()

#*** Eucs ***#
psych::describe(Eucs)

summary(Eucs)

ggplot(nminer ,aes(x = Eucs)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(x = 'Número de árvores de eucalipto', y = "Frequência") +
  theme_minimal()

ggplot(nminer, aes(x = "", y = Eucs)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de árvores de eucalipto") +
  theme_minimal()


#*** Bulokes ***#
psych::describe(Bulokes)

summary(Bulokes)

ggplot(nminer ,aes(x = Bulokes)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(x = 'Número de árvores de Bulokes', y = "Frequência") +
  theme_minimal()

ggplot(nminer, aes(x = "", y = Bulokes)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de árvores de Bulokes") +
  theme_minimal()

#*** Area ***#
psych::describe(Area)

summary(Area)

ggplot(nminer ,aes(x = Area)) +
  geom_histogram(bins = 7, fill = "steelblue", color = "white") +
  labs(x = 'Área do fragmento remanescente contínuo de vegetação', y = "Frequência") +
  theme_minimal()

ggplot(nminer, aes(x = "", y = Area)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Área do fragmento remanescente contínuo de vegetação") +
  theme_minimal()

#*** Timber ***#
psych::describe(Timber)

summary(Timber)

ggplot(nminer ,aes(x = Timber)) +
  geom_histogram(bins = 7, fill = "steelblue", color = "white") +
  labs(x = 'Número de pedaços de madeira caída', y = "Frequência") +
  theme_minimal()

ggplot(nminer, aes(x = "", y = Timber)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de pedaços de madeira caída") +
  theme_minimal()


#------------------------------------------------------------------------------#---- 
#                         Relação entre as Variáveis                           #
#------------------------------------------------------------------------------#----

#*** Miners x Eucs ***#
ggplot(nminer, aes(x = reorder(Miners, Eucs, FUN = median),
                 y = Eucs,
                 fill = reorder(Miners, Eucs, FUN = median))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Número de árvores de eucalipto",
       fill = "Miners") +
  theme_minimal()

describeBy(nminer$Eucs, group = nminer$Miners, mat = TRUE)
tapply(nminer$Eucs, nminer$Miners, summary)

#*** Miners x Bulokes ***# 
ggplot(nminer, aes(x = reorder(Miners, Bulokes, FUN = median),
                   y = Bulokes,
                   fill = reorder(Miners, Bulokes, FUN = median))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Número de árvores de bulokes",
       fill = "Miners") +
  theme_minimal()

describeBy(nminer$Bulokes, group = nminer$Miners, mat = TRUE)
tapply(nminer$Bulokes, nminer$Miners, summary)

#*** Miners x Area ***# 
ggplot(nminer, aes(x = reorder(Miners, Area, FUN = median),
                   y = Area,
                   fill = reorder(Miners, Area, FUN = median))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Área do fragmento remanescente contínuo de vegetação",
       fill = "Miners") +
  theme_minimal()

describeBy(nminer$Area, group = nminer$Miners, mat = TRUE)
tapply(nminer$Area, nminer$Miners, summary)


#*** Miners x Timber ***# 
ggplot(nminer, aes(x = reorder(Miners, Timber, FUN = median),
                   y = Timber,
                   fill = reorder(Miners, Timber, FUN = median))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Número de pedaços de madeira caída",
       fill = "Miners") +
  theme_minimal()

describeBy(nminer$Timber, group = nminer$Miners, mat = TRUE)
tapply(nminer$Timber, nminer$Miners, summary)


#*** Miners x Grazed ***#

ggplot(nminer, aes(x = Grazed, fill = factor(Miners))) +
  geom_bar(position = "fill", color = "white") +
  labs(x = "Se a área foi pastada", y = "Proporção", fill = "Miners") +
  theme_minimal()


#*** Miners x Shrubs ***#

ggplot(nminer, aes(x = Shrubs, fill = factor(Miners))) +
  geom_bar(position = "fill", color = "white") +
  labs(x = "Se havia arbusto presentes na área", y = "Proporção", fill = "Miners") +
  theme_minimal()
