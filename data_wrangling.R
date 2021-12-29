load(file = "data/enemDF.RData")
load(file = "data/escolaDF.RData")

# Incluir média de notas



enemDF <- enemDF %>% mutate(MEDIA_GERAL = (NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5)
head(enemDF, n = 5)
save(enemDF, file = "data/enemDF.RData")

enem_escola_df <- enemDF %>% filter(
    (TP_ST_CONCLUSAO == 2 | TP_ST_CONCLUSAO == 3) & 
    !is.na(CO_ESCOLA)
)

enem_escola_df <- left_join(enem_escola_df, escolaDF,
                            by = "CO_ESCOLA")

head(enem_escola_df, n = 5)

# Save a new RData
save(enem_escola_df, file = "data/enem_escola_df.RData")

#Estudo sobre o desbalanceamento dos dados
enem_escola_df %>% 
  group_by(CO_ESCOLA) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)
