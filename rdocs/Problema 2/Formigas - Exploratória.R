source(here::here("rdocs/Pacotes", "Pacotes.R"))

data(ants); attach(ants)

ants <- ants %>% 
  dplyr::select(-Site) %>% 
  dplyr::mutate(Habitat = fct_reorder(Habitat, Srich, .fun = median))

caminho_Problema2 <- "resultados/Problema 2/Formiga - Exploratória"


#------------------------------------------------------------------------------#---- 
#                               Correlação                                     #
#------------------------------------------------------------------------------#----

mat_corr <- cor(ants[,-2])

ggcorrplot(mat_corr,
           method = "square",       # ou "circle"
           #type   = "lower",        # mostra só o triângulo inferior
           lab    = TRUE,           # mostra os valores
           lab_size = 3,
           colors = c("firebrick", "white", "steelblue"),
           title = "Matriz de correlação",
           ggtheme = theme_minimal())
ggsave("Correlacao_Formiga.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")


#Variáveis tem correlação média com a resposta, e baixa correlação entre si


#------------------------------------------------------------------------------#---- 
#                         Variáveis individuais                                #
#------------------------------------------------------------------------------#----

#*** Srich (Resposta)***#
psych::describe(Srich)

summary(Srich)

ggplot(ants ,aes(x = Srich)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(x = 'Número de espécies de formigas', y = "Frequência") +
  theme_minimal()
ggsave("Histograma_Srich.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")



ggplot(ants, aes(x = "", y = Srich)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  labs(x = NULL, y = "Número de espécies de formigas") +
  theme_minimal()
ggsave("Boxplot_Srich.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")


#*** Latitude ***#
psych::describe(Latitude)

summary(Latitude)

ggplot(ants, aes(x = Latitude)) +
  geom_histogram(bins = 5, fill = "steelblue", color = "white") +
  labs(x = "Latitude (graus decimais)", y = "Frequência") +
  theme_minimal()
ggsave("Histograma_Latitude.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")


#*** Elevation ***#
psych::describe(Elevation)

summary(Elevation)

ggplot(ants, aes(x = Elevation)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(x = "Altitude (m)", y = "Frequência") +
  theme_minimal()
ggsave("Histograma_Elevate.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")


#------------------------------------------------------------------------------#---- 
#                         Relação entre as Variáveis                           #
#------------------------------------------------------------------------------#----

#*** Srich x Latitude ***#
ggplot(ants, aes(x = Latitude, y = Srich, color = Habitat)) +
  geom_point(size = 2.5) +
  labs(x = "Latitude (em graus decimais) do local",
       y = "Número de espécies de formigas",
       color = "Habitat") +
  theme_minimal()
ggsave("Dispersao_Srich_Latitude.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")


scatterplot((Srich)~Latitude, regLine = T, smooth=F, boxplots=F, pch=16, xlab="Latitude (em graus decimais) do local",
            ylab="Número de espécies de formigas",cex=1.2,cex.lab=1.5, cex.axis=1.5)


#*** Srich x Elevation ***#
ggplot(ants, aes(x = Elevation, y = Srich, color = Habitat)) +
  geom_point(size = 2.5) +
  labs(x = "Altitude (em metros acima do nível do mar)",
       y = "Número de espécies de formigas",
       color = "Habitat") +
  theme_minimal()
ggsave("Dispersao_Srich_Elevation.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")


scatterplot((Srich)~Latitude, regLine = T, smooth=F, boxplots=F, pch=16, xlab="Latitude (em graus decimais) do local",
            ylab="Número de espécies de formigas",cex=1.2,cex.lab=1.5, cex.axis=1.5)


#*** Srich x Habitat ***#
ggplot(ants, aes(x = reorder(Habitat, Srich, FUN = median),
                 y = Srich,
                 fill = reorder(Habitat, Srich, FUN = median))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = "white") +
  labs(x = "Habitat",
       y = "Número de espécies de formigas",
       fill = "Habitat") +
  theme_minimal()
ggsave("Boxplot_Srich_Habitat.pdf",path = caminho_Problema2, width = 158, height = 93, units = "mm")



describeBy(ants$Srich, group = ants$Habitat, mat = TRUE)
tapply(ants$Srich, ants$Habitat, summary)

