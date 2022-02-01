ggplotly(
  enem_escola_dummies %>%
    ggplot(aes(x = NU_IDADE, y = MEDIA_GERAL, color = as.factor(CO_ESCOLA) )) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "idade",
         y = "Desempenho Escolar") +
    theme_bw()
)

enem_escola_df %>% 
  group_by(TP_NACIONALIDADE) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

summary(enem_escola_df)
#Estudo sobre o desbalanceamento dos dados
enem_escola_df %>% 
  group_by(CO_ESCOLA) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)