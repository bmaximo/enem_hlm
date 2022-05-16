base_exemplo <- enem_escola_df %>%
  filter(CO_ESCOLA %in% c("11000058",
                          "11000260",
                          "11001640",
                          "11003065",
                          "29387736",
                          "52031110"))


# TP_NACIONALIDADE_1 - maybe
# Q001_F - s
# Q001_G - s
# Q002_H - maybe
# Q006_C - maybe
# Q006_F - s
# Q022_A - maybe
# Q022_B - s
# Q024_C - s

ggplotly(
  base_exemplo %>%
    ggplot(aes(x = TP_COR_RACA, y = MEDIA_GERAL, color = as.factor(CO_ESCOLA) )) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Q006_K",
         y = "Desempenho Escolar") +
    theme_bw()
)

enem_escola_df %>% 
  group_by(Q006) %>% 
  summarise(quantidade = n()) %>% 
  mutate(freq = round((quantidade / sum(quantidade)) * 100, 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

summary(enem_escola_df)
#Estudo sobre o desbalanceamento dos dados
enem_predict %>% 
  group_by(QT_SALAS_UTILIZADAS) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#predicao por cidade
enem_predict %>% 
  group_by(CO_MUNICIPIO_RESIDENCIA) %>% 
  summarise("Média Geral" = mean(MEDIA_GERAL), 
            'Mediana' = median(MEDIA_GERAL), 
            "Média Geral Prevista" = mean(hlm2_fitted), 
            'Mediana Prevista' = median(hlm2_fitted)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)


# Nota por Municipio
municipios = c("Porto Velho/RO (26,15%)", "Feira de Santana/BA (22,05%)",
               "Sorocaba/SP (22,90%)", "Londrina/PR (9,48%)", "Aparecida de Goiânia/GO (19,42%)")
mean_mun <- enem_escola_df %>% 
  group_by(CO_MUNICIPIO_RESIDENCIA) %>% 
  summarize(average = mean(MEDIA_GERAL)) %>%
  ungroup()

ggplotly(
  enem_escola_df %>%
  ggplot(aes(x=as.factor(CO_MUNICIPIO_RESIDENCIA), y=MEDIA_GERAL)) +
    geom_boxplot(aes(fill=as.factor(CO_MUNICIPIO_RESIDENCIA)),show.legend = FALSE)+
    geom_point(data = mean_mun, 
               mapping = aes(x = as.factor(CO_MUNICIPIO_RESIDENCIA), y = average),
               size=1.5) +
    geom_line(data = mean_mun, 
              mapping = aes(x = as.factor(CO_MUNICIPIO_RESIDENCIA), y = average, group=1), 
              size=1)+
    labs(y = "Média Geral",
         x = "Municípios") +
    theme(legend.position = "none")+
    scale_x_discrete(labels = municipios) +
    #guides(fill = "none", color = "none", linetype = "none", shape = "none")+
    scale_colour_viridis_d() +
    theme_bw()
)




#notas por sexo
mean_sexo <- enem_escola_df %>% 
  group_by(TP_SEXO) %>% 
  summarize(average = mean(MEDIA_GERAL)) %>%
  ungroup()


enem_escola_df %>%
  ggplot(aes(x=as.factor(TP_SEXO), y=MEDIA_GERAL)) +
  geom_boxplot(aes(fill=as.factor(TP_SEXO)),show.legend = FALSE)+
  geom_point(data = mean_sexo, 
             mapping = aes(x = as.factor(TP_SEXO), y = average),
             size=1.5) +
  geom_line(data = mean_sexo, 
            mapping = aes(x = as.factor(TP_SEXO), y = average, group=1), 
            size=1)+
  labs(y = "Média Geral",
       x = "Sexo") +
  scale_x_discrete(labels = c("Feminino (56,85%)", "Masculino (43,15%)")) +
  scale_colour_viridis_d() +
  theme_bw()


#notas por raça cor
mean_raca <- enem_escola_df %>% 
  group_by(TP_COR_RACA) %>% 
  summarize(average = mean(MEDIA_GERAL)) %>%
  ungroup()

raçaPerCent = c('Não declarado (2,41%)',
         'Branca (35,41%)','Preta (15,16%)',
         'Parda (42,93%)','Amarela (3,64%)',
         'Indígena (0,45%)')

enem_escola_df %>%
  ggplot(aes(x=as.factor(TP_COR_RACA), y=MEDIA_GERAL)) +
  geom_boxplot(aes(fill=as.factor(TP_COR_RACA)),show.legend = FALSE)+
  geom_point(data = mean_raca, 
             mapping = aes(x = as.factor(TP_COR_RACA), y = average),
             size=1.5) +
  geom_line(data = mean_raca, 
            mapping = aes(x = as.factor(TP_COR_RACA), y = average, group=1), 
            size=1)+
  labs(y = "Média Geral",
       x = "Raça/Cor") +
  scale_x_discrete(labels = raçaPerCent) +
  scale_colour_viridis_d() +
  theme_bw()




#notas por tp dependencia escola
dependenciaPerCent = c('Federal (4,18%)',
                'Estadual (78,23%)',
                'Municipal (0,02%)',
                'Privada (17,19%)')

mean_dependencia <- enem_escola_df %>% 
  group_by(TP_DEPENDENCIA) %>% 
  summarize(average = mean(MEDIA_GERAL)) %>%
  ungroup()

ggplotly(
  enem_escola_df %>%
    drop_na(TP_DEPENDENCIA) %>%
    ggplot(aes(x=as.factor(TP_DEPENDENCIA), y=MEDIA_GERAL)) +
    geom_boxplot(aes(fill=as.factor(TP_DEPENDENCIA)),show.legend = FALSE)+
    geom_point(data = mean_dependencia, 
               mapping = aes(x = as.factor(TP_DEPENDENCIA), y = average),
               size=1.5) +
    geom_line(data = mean_dependencia, 
              mapping = aes(x = as.factor(TP_DEPENDENCIA), y = average, group=1), 
              size=1)+
    labs(y = "Média Geral",
         x = "Dependência Administrativa da Escola") +
    scale_x_discrete(labels = dependenciaPerCent) +
    scale_colour_viridis_d() +
    theme_bw()
)


#notas por renda Q006
rendaPerCent = c('Nenhuma renda (2,56%)',
                 'Até R$ 998,00 (17,76%)',
                 'R$ 998,01 à R$ 1.497,00 (25,86%)',
                 'R$ 1.497,01 à R$ 1.996,00 (9,70%)',
                 'R$ 1.996,01 à R$ 2.495,00 (12,30%)',
                 'R$ 2.495,01 à R$ 2.994,00 (5,24%)',
                 'R$ 2.994,01 à R$ 3.992,00 (8,44%)',
                 'R$ 3.992,01 à R$ 4.990,00 (4,64%)',
                 'R$ 4.990,01 à R$ 5.988,00 (3,57%)',
                 'R$ 5.988,01 à R$ 6.986,00 (1,64%)',
                 'R$ 6.986,01 à R$ 7.984,00 (1,57%)',
                 'R$ 7.984,01 à R$ 8.982,00 (1,03%)',
                 'R$ 8.982,01 à R$ 9.980,00 (1,01%)',
                 'R$ 9.980,01 à R$ 11.976,00 (1,28%)',
                 'R$ 11.976,01 à R$ 14.970,00 (1,07%)',
                 'R$ 14.970,01 à R$ 19.960,00 (1,05%)',
                 'Mais de R$ 19.960,00 (1,25%)')

mean_renda <- enem_escola_df %>% 
  group_by(Q006) %>% 
  summarize(average = mean(MEDIA_GERAL)) %>%
  ungroup()

  
enem_escola_df %>%
  ggplot(aes(x=as.factor(Q006), y=MEDIA_GERAL)) +
  geom_boxplot(aes(fill=as.factor(Q006)),show.legend = FALSE)+
  coord_flip() +
  scale_x_log10()+
  geom_point(data = mean_renda, 
             mapping = aes(x = as.factor(Q006), y = average),
             size=1.5) +
  geom_line(data = mean_renda, 
            mapping = aes(x = as.factor(Q006), y = average, group=1), 
            size=1)+
  labs(y = "Média Geral",
       x = "Renda") +
  scale_x_discrete(labels = rendaPerCent) +
  scale_colour_viridis_d() +
  theme_bw()



###########################
# Separados por municipio #
###########################

# raça cor

raça = c('Não declarado',
                'Branca','Preta',
                'Parda','Amarela',
                'Indígena')

enem_escola_df %>%
  ggplot(aes(fill=as.factor(TP_COR_RACA), y=MEDIA_GERAL, x=as.factor(CO_MUNICIPIO_RESIDENCIA))) + 
  geom_bar(position="dodge", stat = "summary", fun = "mean")+
  labs(y = "Média Geral",
       x = "Municípios") +
  scale_x_discrete(labels = municipios) +
  scale_fill_discrete(name = "Raça/Cor", labels = raça) +
  scale_colour_viridis_d() +
  theme_bw()


# sexo
enem_escola_df %>%
  ggplot(aes(fill=as.factor(TP_SEXO), y=MEDIA_GERAL, x=as.factor(CO_MUNICIPIO_RESIDENCIA))) +
  geom_bar(position="dodge", stat = "summary", fun = "mean")+
  labs(y = "Média Geral",
       x = "Municípios") +
  scale_x_discrete(labels = municipios) +
  scale_fill_discrete(name = "Sexo", labels = c("Feminino", "Masculino")) +
  scale_colour_viridis_d() +
  theme_bw()


#dependencia escola
dependencia = c('Federal',
                'Estadual',
                'Municipal',
                'Privada')

enem_escola_df %>%
  drop_na(TP_DEPENDENCIA) %>%
  ggplot(aes(fill=as.factor(TP_DEPENDENCIA), x=as.factor(CO_MUNICIPIO_RESIDENCIA), y=MEDIA_GERAL)) +
  geom_bar(position="dodge", stat = "summary", fun = "mean")+
  labs(y = "Média Geral",
       x = "Municípios",
       fill = "Dependência Administrativa") +
  scale_x_discrete(labels = municipios) +
  scale_fill_discrete(labels = dependencia) +
  scale_colour_viridis_d() +
  theme_bw()



renda = c('Nenhuma renda.',
                 'Até R$ 998,00',
                 'R$ 998,01 à R$ 1.497,00',
                 'R$ 1.497,01 à R$ 1.996,00',
                 'R$ 1.996,01 à R$ 2.495,00',
                 'R$ 2.495,01 à R$ 2.994,00',
                 'R$ 2.994,01 à R$ 3.992,00',
                 'R$ 3.992,01 à R$ 4.990,00',
                 'R$ 4.990,01 à R$ 5.988,00',
                 'R$ 5.988,01 à R$ 6.986,00',
                 'R$ 6.986,01 à R$ 7.984,00',
                 'R$ 7.984,01 à R$ 8.982,00',
                 'R$ 8.982,01 à R$ 9.980,00',
                 'R$ 9.980,01 à R$ 11.976,00',
                 'R$ 11.976,01 à R$ 14.970,00',
                 'R$ 14.970,01 à R$ 19.960,00',
                 'Mais de R$ 19.960,00')
