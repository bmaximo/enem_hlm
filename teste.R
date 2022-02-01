modelo_intercepttest <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_COR_RACA_1
                          + TP_COR_RACA_2
                          + NU_IDADE
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
                          + Q006_B
                          + Q006_C
                          + Q006_D
                          + Q006_E
                          + Q006_F
                          + Q006_G
                          + Q006_H
                          + Q006_I
                          + Q006_J
                          + Q006_K
                          + Q006_L
                          + Q006_M
                          + Q006_N
                          + Q006_O
                          + Q006_P
                          + Q006_Q
                          + Q022_A
                          + Q022_B
                          + Q025_B
                          + Q024_A
                          + Q024_C
                          + Q024_D
                          + Q024_E
                          + TP_LOCALIZACAO_ESC_2
                          + TP_DEPENDENCIA_2,
                          random = ~ TP_SEXO_M 
                          + TP_COR_RACA_1
                          + TP_COR_RACA_2
                          + NU_IDADE
                          | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
summary(modelo_intercepttest)
stderr_nlme(modelo_intercepttest)

modelo_intercept_inclination = lme(fixed = MEDIA_GERAL ~ TP_ENSINO_2,
    random = ~ TP_ENSINO_2 | CO_ESCOLA,
    data = enem_escola_dummies,
    method = "REML",
    na.action = na.omit,
    control =list(msMaxIter = 1000, msMaxEval = 1000))
summary(modelo_intercept_inclination)
