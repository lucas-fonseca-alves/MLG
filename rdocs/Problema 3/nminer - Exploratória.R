source(here::here("rdocs/Pacotes", "Pacotes.R"))

#Dados
data(nminer); 

nminer <- nminer %>% 
  dplyr::select(-Minerab) %>% 
  dplyr::mutate(Miners = factor(Miners, levels = c(0, 1)),
                Grazed = factor(Grazed, levels = c(0, 1)),
                Shrubs = factor(Shrubs, levels = c(0, 1)))

attach(nminer)

caminho_Problema3 <- "resultados/Problema 3/nminer - Exploratória"

#------------------------------------------------------------------------------#---- 
#   Correlação (Só para as variáveis contínuas, verificar multicolinearidade)  #
#------------------------------------------------------------------------------#----

nminer %>%
  dplyr::select(!where(is.factor)) %>% 
  cor() %>% 
  ggcorrplot(
             method = "square",       # ou "circle"
             #type   = "lower",        # mostra só o triângulo inferior
             lab    = TRUE,           # mostra os valores
             lab_size = 3,
             colors = c("firebrick", "white", "steelblue"),
             title = "Matriz de correlação",
             ggtheme = theme_minimal())

ggsave("Correlacao_nminer.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


# EUCS é a variável que tem maior correlação 0.73 com Miners
# Variáveis não parecem ter alta correlação entre si, o maior valor é -0.64 entre shrubs
# e Grazed
# As variáveis área e grazed tem correlação baixa com Miners
# A maioria das variáveis tem correlação negativa com a resposta. 

table(nminer$Grazed, nminer$Shrubs)

# h0: não há associação
# h1: há associação
chisq.test(table(nminer$Grazed, nminer$Shrubs))
#p-value = 0.001389, logo a 5% rejeita h0, então há associação entre as variáveis
#NÃO usar elas juntas 

#------------------------------------------------------------------------------#---- 
#                         Variáveis individuais                                #
#------------------------------------------------------------------------------#----

#*** Miners (Resposta)***#

ggplot(nminer, aes(x = Miners)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(x = "Presença de noisy miners", y = "Frequência") +
  theme_minimal()

ggsave("Frequencia_nminer.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")

#*** Grazed ***#

ggplot(nminer, aes(x = Grazed)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(x = "Se a área foi pastada", y = "Frequência") +
  theme_minimal()

ggsave("Frequencia_Grazed.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Shrubs ***#

ggplot(nminer, aes(x = Shrubs)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(x = "Se havia arbustos presentes na área", y = "Frequência") +
  theme_minimal()

ggsave("Frequencia_Shrubs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Eucs ***#
psych::describe(Eucs)

summary(Eucs)

ggplot(nminer ,aes(x = Eucs)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(x = 'Número de árvores de eucalipto', y = "Frequência") +
  theme_minimal()

ggsave("Histograma_Eucs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = "", y = Eucs)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de árvores de eucalipto") +
  theme_minimal()

ggsave("Boxplot_Eucs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")

#*** Bulokes ***#
psych::describe(Bulokes)

summary(Bulokes)

ggplot(nminer ,aes(x = Bulokes)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(x = 'Número de árvores de Bulokes', y = "Frequência") +
  theme_minimal()

ggsave("Boxplot_Bulokes.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = "", y = Bulokes)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de árvores de Bulokes") +
  theme_minimal()

ggsave("Boxplot_Bulokes.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Area ***#
psych::describe(Area)

summary(Area)

ggplot(nminer ,aes(x = Area)) +
  geom_histogram(bins = 7, fill = "steelblue", color = "white") +
  labs(x = 'Área do fragmento remanescente contínuo de vegetação', y = "Frequência") +
  theme_minimal()
ggsave("Histograma_Area.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = "", y = Area)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Área do fragmento remanescente contínuo de vegetação") +
  theme_minimal()

ggsave("Boxplot_Area.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Timber ***#
psych::describe(Timber)

summary(Timber)

ggplot(nminer ,aes(x = Timber)) +
  geom_histogram(bins = 7, fill = "steelblue", color = "white") +
  labs(x = 'Número de pedaços de madeira caída', y = "Frequência") +
  theme_minimal()

ggsave("Histograma_Timber.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = "", y = Timber)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de pedaços de madeira caída") +
  theme_minimal()

ggsave("Boxplot_Timber.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#------------------------------------------------------------------------------#---- 
#                         Relação entre as Variáveis                           #
#------------------------------------------------------------------------------#----
cores_y <- c("0" = "#F8766D", "1" = "#00BFC4")


#*** Miners x Eucs ***#

D_E_M <- ggplot(nminer, aes(x = Eucs, y = Miners, color = factor(Miners))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de árvores de eucalipto", 
       y = "Miners (0/1)", 
       color = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Dispersao_Miners_Eucs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Miners,
                 y = Eucs,
                 fill = Miners)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Número de árvores de eucalipto",
       fill = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Boxplot_Miners_Eucs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


describeBy(nminer$Eucs, group = nminer$Miners, mat = TRUE)
tapply(nminer$Eucs, nminer$Miners, summary)

#*** Miners x Bulokes ***# 

ggplot(nminer, aes(x = Bulokes, y = Miners, color = factor(Miners))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de árvores de bulokes", 
       y = "Miners (0/1)", 
       color = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Dispersão_Miners_Bulokes.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Miners,
                   y = Bulokes,
                   fill = Miners)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Número de árvores de bulokes",
       fill = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Boxplot_Miners_Bulokes.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


describeBy(nminer$Bulokes, group = nminer$Miners, mat = TRUE)
tapply(nminer$Bulokes, nminer$Miners, summary)

#*** Miners x Area ***# 

ggplot(nminer, aes(x = Area, y = Miners, color = factor(Miners))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Área do fragmento remanescente contínuo de vegetação", 
       y = "Miners (0/1)", 
       color = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Dispersao_Miners_Area.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Miners,
                   y = Area,
                   fill = Miners)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Área do fragmento remanescente contínuo de vegetação",
       fill = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Boxplot_Miners_Area.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


describeBy(nminer$Area, group = nminer$Miners, mat = TRUE)
tapply(nminer$Area, nminer$Miners, summary)


#*** Miners x Timber ***# 

ggplot(nminer, aes(x = Timber, y = Miners, color = factor(Miners))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de pedaços de madeira caída", 
       y = "Miners (0/1)", 
       color = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Dispersao_Miners_Timber.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Miners,
                   y = Timber,
                   fill = Miners)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Miners",
       y = "Número de pedaços de madeira caída",
       fill = "Miners") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave("Boxplot_Miners_Timber.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


describeBy(nminer$Timber, group = nminer$Miners, mat = TRUE)
tapply(nminer$Timber, nminer$Miners, summary)


#*** Miners x Grazed ***#
table(nminer$Grazed, nminer$Miners)

ggplot(nminer, aes(x = Grazed, fill = factor(Miners))) +
  geom_bar(position = "fill", color = "white") +
  labs(x = "Se a área foi pastada", y = "Proporção", fill = "Miners") +
  theme_minimal()

ggsave("Barras_Miners_Grazed.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Miners x Eucs x Grazed***#
ggplot(nminer, aes(x = Eucs, y = Miners, color = factor(Grazed))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de árvores de eucalipto", 
       y = "Miners (0/1)", 
       color = "Área pastada?") +
  theme_minimal()

ggsave("Dispersao_Miners_Eucs_Grazed.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Miners x Bulokes x Grazed***#
ggplot(nminer, aes(x = Bulokes, y = Miners, color = factor(Grazed))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de árvores de bulokes", 
       y = "Miners (0/1)", 
       color = "Área pastada?") +
  theme_minimal()
ggsave("Dispersao_Miners_Bulokes_Grazed.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Miners x Area x Grazed***#
ggplot(nminer, aes(x = Area, y = Miners, color = factor(Grazed))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Área do fragmento remanescente contínuo de vegetação", 
       y = "Miners (0/1)", 
       color = "Área pastada?") +
  theme_minimal()
ggsave("Dispersao_Miners_Area_Grazed.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")

#*** Miners x Timber x Grazed ***#
ggplot(nminer, aes(x = Timber, y = Miners, color = factor(Grazed))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de pedaços de madeira caída", 
       y = "Miners (0/1)", 
       color = "Área pastada?") +
  theme_minimal()
ggsave("Dispersao_Miners_Timber_Grazed.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#*** Miners x Shrubs ***#

table(nminer$Shrubs, nminer$Miners)


ggplot(nminer, aes(x = Shrubs, fill = factor(Miners))) +
  geom_bar(position = "fill", color = "white") +
  labs(x = "Se havia arbusto presentes na área", y = "Proporção", fill = "Miners") +
  theme_minimal()
ggsave("Barras_Miners_Shrubs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


D_E_M_S <- ggplot(nminer, aes(x = Eucs, y = Miners, color = factor(Shrubs))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de árvores de eucalipto", 
       y = "Miners (0/1)", 
       color = "Arbusto presentes na área?") +
  theme_minimal()
ggsave("Dispersao_Miners_Eucs_Shrubs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Bulokes, y = Miners, color = factor(Shrubs))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de árvores de bulokes", 
       y = "Miners (0/1)", 
       color = "Arbusto presentes na área?") +
  theme_minimal()
ggsave("Dispersao_Miners_Bulokes_Shrubs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Area, y = Miners, color = factor(Shrubs))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Área do fragmento remanescente contínuo de vegetação", 
       y = "Miners (0/1)", 
       color = "Arbusto presentes na área?") +
  theme_minimal()
ggsave("Dispersao_Miners_Area_Shrubs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


ggplot(nminer, aes(x = Timber, y = Miners, color = factor(Shrubs))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.6, size = 2) +
  scale_color_manual(values = cores_y) +
  labs(x = "Número de pedaços de madeira caída", 
       y = "Miners (0/1)", 
       color = "Arbusto presentes na área?") +
  theme_minimal()
ggsave("Dispersao_Miners_Timber_Shrubs.pdf",path = caminho_Problema3, width = 158, height = 93, units = "mm")


#------------------------------------------------------------------------------#----
