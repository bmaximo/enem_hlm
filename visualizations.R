base_exemplo <- enem_escola_dummies %>%
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
    ggplot(aes(x = QT_SALAS_UTILIZADAS, y = MEDIA_GERAL, color = as.factor(CO_ESCOLA) )) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Q006_K",
         y = "Desempenho Escolar") +
    theme_bw()
)

enem_escola_df %>% 
  group_by(Q002) %>% 
  summarise(quantidade = n()) %>% 
  mutate(freq = round((quantidade / sum(quantidade)) * 100, 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

summary(enem_escola_df)
#Estudo sobre o desbalanceamento dos dados
enem_escola_df %>% 
  group_by(TP_ENSINO) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

# Nota por Municipio
municipios = c("Porto Velho/RO (26,15%)", "Feira de Santana/BA (22,05%)",
               "Sorocaba/SP (22,90%)", "Londrina/PR (9,48%)", "Aparecida de Goiânia/GO (19,42%)")
ggplotly(
  enem_escola_df %>%
  ggplot(aes(x=as.factor(CO_MUNICIPIO_RESIDENCIA), y=MEDIA_GERAL)) +
    geom_boxplot(aes(fill=as.factor(CO_MUNICIPIO_RESIDENCIA)),show.legend = FALSE)+
    labs(y = "Média Geral",
         x = "Municípios") +
    theme(legend.position = "none")+
    scale_x_discrete(labels = municipios) +
    #guides(fill = "none", color = "none", linetype = "none", shape = "none")+
    scale_colour_viridis_d() +
    theme_bw()
)


#notas por sexo
ggplotly(
  enem_escola_df %>%
    ggplot(aes(x=as.factor(TP_SEXO), y=MEDIA_GERAL)) +
    geom_boxplot(aes(fill=as.factor(TP_SEXO)),show.legend = FALSE)+
    labs(y = "Média Geral",
         x = "Sexo") +
    scale_x_discrete(labels = c("Feminino (56,85%)", "Masculino (43,15%)")) +
    scale_colour_viridis_d() +
    theme_bw()
)

#notas por raça cor
raça = c('Não declarado (2,41%)',
         'Branca (35,41%)','Preta (15,16%)',
         'Parda (42,93%)','Amarela (3,64%)',
         'Indígena (0,45%)')
ggplotly(
  enem_escola_df %>%
    ggplot(aes(x=as.factor(TP_COR_RACA), y=MEDIA_GERAL)) +
    geom_boxplot(aes(fill=as.factor(TP_COR_RACA)),show.legend = FALSE)+
    labs(y = "Média Geral",
         x = "Raça/Cor") +
    scale_x_discrete(labels = raça) +
    scale_colour_viridis_d() +
    theme_bw()
)


#notas por tp dependencia escola
dependencia = c('Federal (4,18%)',
                'Estadual (78,23%)',
                'Municipal (0,02%)',
                'Privada (17,19%)')
ggplotly(
  enem_escola_df %>%
    drop_na(TP_DEPENDENCIA) %>%
    ggplot(aes(x=as.factor(TP_DEPENDENCIA), y=MEDIA_GERAL)) +
    geom_boxplot(aes(fill=as.factor(TP_DEPENDENCIA)),show.legend = FALSE)+
    labs(y = "Média Geral",
         x = "Dependência Administrativa da Escola") +
    scale_x_discrete(labels = dependencia) +
    scale_colour_viridis_d() +
    theme_bw()
)


