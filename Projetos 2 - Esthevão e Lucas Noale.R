library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(reshape2)
library(lmtest)
library(xtable)
library(geobr)
library(sf)
library(ggspatial)
library(ggridges)
library(tibble)

# Primeiro, devemos importar as bases de dados:

pib = read_excel("C:/Users/vfxbl/Downloads/PIB por estado.xlsx", sheet = "PIB")
iniciais = read_excel("C:/Users/vfxbl/Downloads/IDEB.xlsx", sheet = "AnosIniciais")[3:130,]
finais = read_excel("C:/Users/vfxbl/Downloads/IDEB.xlsx", sheet = "AnosFinais")[3:130,]
medio = read_excel("C:/Users/vfxbl/Downloads/IDEB.xlsx", sheet = "EnsinoMedio")[3:98,]

stargazer(pib)
stargazer(iniciais)
stargazer(finais)
stargazer(medio)


anos = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
estados = pib[1:2]

# Depois disso, filtramos os dados de educação para as notas da prova SAEB
# E utilizamos os dados totais, sem separar em escolas públicas e privadas

iniciais = iniciais %>%
  filter(Rede == "Total (3)(4)" | Rede == "Total (4)") %>%
  select(UF, "Nota SAEB - 2005", "Nota SAEB - 2007", 
         "Nota SAEB - 2009", "Nota SAEB - 2011",
         "Nota SAEB - 2013", "Nota SAEB - 2015",
         "Nota SAEB - 2017", "Nota SAEB - 2019")

finais = finais %>%
  filter(Rede == "Total (3)(4)" | Rede == "Total (4)") %>%
  select(UF, "Nota SAEB - 2005", "Nota SAEB - 2007", 
         "Nota SAEB - 2009", "Nota SAEB - 2011",
         "Nota SAEB - 2013", "Nota SAEB - 2015",
         "Nota SAEB - 2017", "Nota SAEB - 2019")

medio = medio %>%
  filter(Rede == "Total (3)(4)" | Rede == "Total (4)") %>%
  select(UF, "Nota SAEB - 2005", "Nota SAEB - 2007", 
         "Nota SAEB - 2009", "Nota SAEB - 2011",
         "Nota SAEB - 2013", "Nota SAEB - 2015",
         "Nota SAEB - 2017", "Nota SAEB - 2019")

pib = pib %>%
  select("Sigla", as.character(anos)) %>%
  rename("UF" = Sigla)


# Transformamos os dados de educação para numérico

iniciais = cbind.data.frame(iniciais$UF, apply(iniciais[2:ncol(iniciais)],
                                               2, as.numeric))
finais = cbind.data.frame(finais$UF, apply(finais[2:ncol(finais)], 2,
                                           as.numeric))
medio = cbind.data.frame(medio$UF, apply(medio[2:ncol(medio)], 2,
                                         as.numeric))

colnames(iniciais) = c("UF", 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
colnames(finais) = c("UF", 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
colnames(medio) = c("UF", 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)

iniciais = iniciais %>% left_join(estados, by = "UF") %>% relocate(Sigla, .after = "UF")
finais = finais %>% left_join(estados, by = "UF") %>% relocate(Sigla, .after = "UF")
medio = medio %>% left_join(estados, by = "UF") %>% relocate(Sigla, .after = "UF")


iniciais = iniciais[-1] %>% rename("UF" = Sigla)
finais = finais[-1] %>% rename("UF" = Sigla)
medio = medio[-1] %>% rename("UF" = Sigla)

# Para usar painel, devemos criar uma nova variável para cada um dos anos


df = melt(iniciais[1:ncol(iniciais)]) %>%
  left_join(melt(finais[1:ncol(finais)]), by = c("UF", "variable")) %>%
  left_join(melt(medio[1:ncol(medio)]), by = c("UF", "variable")) %>%
  left_join(melt(pib), by = c("UF", "variable"))

colnames(df) = c("UF", "year", "iniciais", "finais", "medio", "pib")

# Criamos dummys para cada um dos estados brasileiros

for (i in 1:27) {
  for (j in 1:nrow(df)) {
    df[j,paste(estados[i,2])] = as.integer(df[j,1] == estados[i,2]) } }


# Também buscamos ver o efeito de educação 2 anos antes no PIB 2 anos depois, então:


lagdf = ifelse(df$year == 2019, NA, ifelse(df$year == 2017, 2019,
              ifelse(df$year == 2015, 2017, ifelse(df$year == 2013, 2015,
                ifelse(df$year == 2011, 2013, ifelse(df$year == 2009, 2011,
                  ifelse(df$year == 2007, 2009, 2007)))))))
lagdf = lagdf[!is.na(lagdf)]

# E criamos um data frame com o PIB do ano t e educação do ano t-2

lagdf = cbind.data.frame(lagdf, filter(df, year != 2019)[c(1,3,4,5)],
                         filter(df, year != 2005)[6]) %>%
  relocate(UF, .before = "lagdf")
colnames(lagdf) = c("UF", "year", "iniciais", "finais", "medio", "pib")

for (i in 1:27) {
  for (j in 1:nrow(lagdf)) {
    lagdf[j,paste(estados[i,2])] = as.integer(lagdf[j,1] == estados[i,2]) } }

# Análise descritiva das bases de dados:

  # Boxplots

boxplot(iniciais[2:ncol(iniciais)], col = 4, ylab = "Iniciais", main = "Anos Iniciais")
boxplot(finais[2:(ncol(finais)-1)], col = 4, ylab = "Finais", main = "Anos Finais")
boxplot(medio[2:(ncol(medio)-1)], col = 4, ylab = "Médio", main = "Ensino Médio")
boxplot(pib[,2:ncol(pib)], outline = FALSE, col = 4, main = "PIB sem outliers")

   # Boxplot por anos

ggplot(df,
       aes(x = year, y = iniciais, fill = year)) +
  geom_boxplot(width = 0.5) +
  theme_bw() +
  ggtitle("Nota SAEB nos Anos Iniciais por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  scale_fill_viridis_d()

ggplot(df,
       aes(x = year, y = finais, fill = year)) +
  geom_boxplot(width = 0.5) +
  theme_bw() +
  ggtitle("Nota SAEB nos Anos Finais por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  scale_fill_viridis_d()

ggplot(df,
       aes(x = year, y = medio, fill = year)) +
  geom_boxplot(width = 0.5) +
  theme_bw() +
  ggtitle("Nota SAEB no Ensino Médio por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  scale_fill_viridis_d()

   # Distribuição por anos

ggplot(df,
       aes(y = year, x = iniciais, fill = year)) +
  geom_density_ridges() +
  labs(x = "Anos", y = "Quantidade", fill = "Anos",
    title = "Densidade da Nota SAEB nos Anos Iniciais") +
  scale_fill_viridis_d()

ggplot(df,
       aes(y = year, x = finais, fill = year)) +
  geom_density_ridges() +
  labs(x = "Anos", y = "Quantidade", fill = "Anos",
       title = "Densidade da Nota SAEB nos Anos Finais") +
  scale_fill_viridis_d()

ggplot(df,
       aes(y = year, x = medio, fill = year)) +
  geom_density_ridges() +
  labs(x = "Anos", y = "Quantidade", fill = "Anos",
       title = "Densidade da Nota SAEB no Ensino Médo") +
  scale_fill_viridis_d()

  # Distribuição por estado

ggplot(df,
       aes(x = year, y = iniciais, col = UF, group = 1)) +
  geom_line(size = 0.8) +
  theme_bw() +
  facet_wrap(~ UF) +
  ggtitle("Nota SAEB nos Anos Iniciais por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  theme(axis.text.x = element_blank())

ggplot(df,
       aes(x = year, y = finais, col = UF, group = 1)) +
  geom_line(size = 0.8) +
  theme_bw() +
  facet_wrap(~ UF) +
  ggtitle("Nota SAEB nos Anos Finais por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  theme(axis.text.x = element_blank())

ggplot(df,
       aes(x = year, y = medio, col = UF, group = 1)) +
  geom_line(size = 0.8) +
  theme_bw() +
  facet_wrap(~ UF) +
  ggtitle("Nota SAEB no Ensino Médio por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  theme(axis.text.x = element_blank())

ggplot(df,
       aes(x = year, y = pib, col = UF, group = 1)) +
  geom_line(size = 0.8) +
  theme_bw() +
  facet_wrap(~ UF) +
  ggtitle("Nota SAEB no Ensino Médio por Estado") +
  xlab("Anos") +
  ylab("Nota") +
  theme(axis.text.x = element_blank())


  # Geobr

states = read_state(year = 2019)
states = cbind.data.frame(states$abbrev_state, states$geom)
colnames(states) = c("UF", "geometry")

geobr = df %>%
  left_join(states, by = "UF") %>%
  select(UF, year, iniciais, finais, medio, pib, geometry)


ggplot(as_tibble(st_as_sf(geobr))) +
  aes(fill = iniciais, geometry = geometry) +
  geom_sf(color = NA) +
  labs(x = "Longitude", y = "Latitude", title = "Notas do SAEB para Anos Iniciais") +
  scale_fill_viridis_c(name = "Nota", trans = "reverse") +
  facet_wrap(~ year) +
  theme_minimal()

ggplot(as_tibble(st_as_sf(geobr))) +
  aes(fill = finais, geometry = geometry) +
  geom_sf(color = NA) +
  labs(x = "Longitude", y = "Latitude", title = "Notas do SAEB para Anos Finais") +
  scale_fill_viridis_c(name = "Nota", trans = "reverse") +
  facet_wrap(~ year) +
  theme_minimal()

ggplot(as_tibble(st_as_sf(geobr))) +
  aes(fill = medio, geometry = geometry) +
  geom_sf(color = NA) +
  labs(x = "Longitude", y = "Latitude", title = "Notas do SAEB para Ensino Médio") +
  scale_fill_viridis_c(name = "Nota", trans = "reverse") +
  facet_wrap(~ year) +
  theme_minimal()

ggplot(as_tibble(st_as_sf(geobr))) +
  aes(fill = pib, geometry = geometry) +
  geom_sf(color = NA) +
  labs(x = "Longitude", y = "Latitude", title = "Notas do SAEB para Ensino Médio") +
  scale_fill_viridis_c(name = "Nota", trans = "reverse") +
  facet_wrap(~ year) +
  theme_minimal()


  # Plot das séries de tempo

dftotal = cbind.data.frame(apply(pib[2:length(pib)], 2, sum), 
                           apply(iniciais[2:length(iniciais)], 2, mean), 
                           apply(finais[2:length(finais)], 2, mean), 
                           apply(medio[2:length(medio)], 2, mean), anos)
colnames(dftotal) = c("pib", "iniciais", "finais", "medio", "year")


ggplot(dftotal) +
  geom_line(data = dftotal, aes(x = year, y = iniciais,
                                col = "Anos iniciais"), size = 0.8) +
  geom_line(data = dftotal, aes(x = year, y = finais,
                                col = "Anos finais"), size = 0.8) +
  geom_line(data = dftotal, aes(x = year, y = medio,
                                col = "Ensino Médio"), size = 0.8) +
  scale_color_viridis_d() + 
  labs(x = "Anos", y = "Nota média", title = "Evolução da nota média do SAEB",
       colour = "Educação")


summary(lm(pib ~ iniciais, dftotal))
summary(lm(pib ~ finais, dftotal))
summary(lm(pib ~ medio, dftotal))


# Correlações

correl = matrix(0, nrow = 3, ncol = 8)
for (i in 1:8) {
  for (j in 1:3) {
    correl[j, i] = cor(filter(df, year == anos[i])[j+2],
                       filter(df, year == anos[i])[6]) } }

correl = as.data.frame(cbind(t(correl), anos), anos)
colnames(correl) = c("iniciais", "finais", "medio", "year")


lagcorrel = matrix(0, nrow = 3, ncol = 7)
for (i in 1:7) {
  for (j in 1:3) {
    lagcorrel[j, i] = cor(filter(lagdf, year == anosplus[i])[j+2],
                          filter(lagdf, year == anosplus[i])[6]) } }

lagcorrel = as.data.frame(cbind(t(lagcorrel), anosplus[1:7]), row.names = anosplus[1:7])
colnames(lagcorrel) = c("iniciais", "finais", "medio", "year")


  # Plot de correlações

plot(as.numeric(correl[1,]), type = "l", xlab = "Anos", ylab = "Correlação",
     main = "Correlação entre Educação e PIB", ylim = c(0.25,0.6), col = 6)
lines(as.numeric(correl[2,]), col = 3)
lines(as.numeric(correl[3,]), col = 4)
legend(x = 7, y = 0.6, legend = c("Iniciais", "Finais", "Médio"),
       fill = c(6, 3, 4))

plot(as.numeric(lagcorrel[1,]), type = "l", xlab = "Anos", ylab = "Correlação Defasada",
     main = "Correlação Defasada entre Educação e PIB", ylim = c(0.25,0.6), col = 6)
lines(as.numeric(lagcorrel[2,]), col = 3)
lines(as.numeric(lagcorrel[3,]), col = 4)
legend(x = 5.25, y = 0.6, legend = c("Iniciais", "Finais", "Médio"),
       fill = c(6, 3, 4))

    # Ggplot

ggplot(correl) +
  geom_line(data = correl, aes(x = year, y = iniciais,
                               col = "Anos iniciais"), size = 0.8) +
  geom_line(data = correl, aes(x = year, y = finais,
                               col = "Anos finais"), size = 0.8) +
  geom_line(data = correl, aes(x = year, y = medio,
                               col = "Ensino Médio"), size = 0.8) +
  scale_color_viridis_d() + 
  labs(x = "Anos", y = "Correlação", title = "Correlação entre Educação e PIB",
       colour = "Educação")


ggplot(lagcorrel) +
  geom_line(data = lagcorrel, aes(x = year, y = iniciais,
                                  col = "Anos iniciais"), size = 0.8) +
  geom_line(data = lagcorrel, aes(x = year, y = finais,
                                  col = "Anos finais"), size = 0.8) +
  geom_line(data = lagcorrel, aes(x = year, y = medio,
                                  col = "Ensino Médio"), size = 0.8) +
  scale_color_viridis_d() + 
  labs(x = "Anos", y = "Correlação", title = "Correlação defasada entre Educação e PIB",
       colour = "Educação")

# Regressõoes Lineares


lm1 = lm(pib ~ iniciais + AC + AP + AL + AM + BA + CE + ES + GO + MA + MT + MS + MG +
        PA + PB + PR + PE + PI + RJ + RN + RS + RO + RR + SC + SP + SE + TO + DF,
        data = df)


lm2 = lm(pib ~ finais + AC + AP + AL + AM + BA + CE + ES + GO + MA + MT + MS + MG +
          PA + PB + PR + PE + PI + RJ + RN + RS + RO + RR + SC + SP + SE + TO + DF,
        data = df)


lm3 = lm(pib ~ medio + AC + AP + AL + AM + BA + CE + ES + GO + MA + MT + MS + MG +
          PA + PB + PR + PE + PI + RJ + RN + RS + RO + RR + SC + SP + SE + TO + DF,
        data = df)


  # Regressões com a defasagem

lm4 = lm(pib ~ iniciais + AC + AP + AL + AM + BA + CE + ES + GO + MA + MT + MS + MG +
           PA + PB + PR + PE + PI + RJ + RN + RS + RO + RR + SC + SP + SE + TO + DF,
                data = lagdf)

lm5 = lm(pib ~ finais + AC + AP + AL + AM + BA + CE + ES + GO + MA + MT + MS + MG +
          PA + PB + PR + PE + PI + RJ + RN + RS + RO + RR + SC + SP + SE + TO + DF,
              data = lagdf)

lm6 = lm(pib ~ medio + AC + AP + AL + AM + BA + CE + ES + GO + MA + MT + MS + MG +
          PA + PB + PR + PE + PI + RJ + RN + RS + RO + RR + SC + SP + SE + TO + DF,
             data = lagdf)

  # Regressões normais e com defasagem


lm7 = lm(lagdf$pib ~ filter(df, year != 2005)$iniciais + lagdf$iniciais + lagdf$AC +
    lagdf$AP + lagdf$AL + lagdf$AM + lagdf$BA + lagdf$CE + lagdf$ES + lagdf$GO +
      lagdf$MA + lagdf$MT + lagdf$MS + lagdf$MG + lagdf$PA + lagdf$PB + lagdf$PR +
      lagdf$PE + lagdf$PI + lagdf$RJ + lagdf$RN + lagdf$RS + lagdf$RO + lagdf$RR +
      lagdf$SC + lagdf$SP + lagdf$SE + lagdf$TO +lagdf$DF)

lm8 = lm(lagdf$pib ~ filter(df, year != 2005)$finais + lagdf$finais + lagdf$AC +
       lagdf$AP + lagdf$AL + lagdf$AM + lagdf$BA + lagdf$CE + lagdf$ES + lagdf$GO +
       lagdf$MA + lagdf$MT + lagdf$MS + lagdf$MG + lagdf$PA + lagdf$PB + lagdf$PR +
       lagdf$PE + lagdf$PI + lagdf$RJ + lagdf$RN + lagdf$RS + lagdf$RO + lagdf$RR +
        lagdf$SC + lagdf$SP + lagdf$SE + lagdf$TO +lagdf$DF)

lm9 = lm(lagdf$pib ~ filter(df, year != 2005)$medio + lagdf$medio + lagdf$AC +
        lagdf$AP + lagdf$AL + lagdf$AM + lagdf$BA + lagdf$CE + lagdf$ES + lagdf$GO +
        lagdf$MA + lagdf$MT + lagdf$MS + lagdf$MG + lagdf$PA + lagdf$PB + lagdf$PR +
       lagdf$PE + lagdf$PI + lagdf$RJ + lagdf$RN + lagdf$RS + lagdf$RO + lagdf$RR +
       lagdf$SC + lagdf$SP + lagdf$SE + lagdf$TO +lagdf$DF)

lm9

stargazer(lm1,lm2,lm3, type = "latex", summary = T)
stargazer(lm4,lm5,lm6, type = "latex", summary = T)
stargazer(lm7,lm8,lm9, type = "latex", summary = T)
