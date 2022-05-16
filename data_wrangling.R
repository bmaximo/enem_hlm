load(file = "data/enemDF.RData")
load(file = "data/escolaDF.RData")

enemDF <- select(enemDF, c(enem_vars))
escolaDF <- select(escolas, c(escola_vars))
escolaDF <- escolaDF %>% rename(CO_ESCOLA=CO_ENTIDADE)

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


str(enem_escola_dummies)
names(enem_escola_dummies)

# DUMMIES
enem_escola_dummies <- dummy_cols(.data = enem_escola_df,
                                       select_columns = c(
                                         "TP_SEXO",
                                         "TP_COR_RACA", 
                                         "TP_NACIONALIDADE",
                                         "TP_DEPENDENCIA",
                                         "TP_ENSINO",
                                         "TP_LOCALIZACAO_ESC",
                                         "Q001",
                                         "Q002",
                                         "Q006",
                                         "Q025"
                                         ),
                                       remove_first_dummy = TRUE,
                                       remove_selected_columns = TRUE)

enem_escola_dummies <- dummy_cols(.data = enem_escola_dummies,
                                  select_columns = c("Q022", "Q024"),
                                  remove_most_frequent_dummy = TRUE,
                                  remove_selected_columns = TRUE)

save(enem_escola_dummies, file = "data/enem_escola_dummies.RData")

enem_predict = enem_escola_dummies %>% filter(
    !is.na(IN_BIBLIOTECA) && !is.na(QT_SALAS_UTILIZADAS)
)

enem_predict = enem_predict %>% filter(
  !is.na(QT_SALAS_UTILIZADAS)
)

enem_predict = enem_predict %>% filter(
  !is.na(TP_ENSINO_2)
)

Atest = enem_predict %>% filter(
  is.na(TP_LOCALIZACAO_ESC_2)
)
