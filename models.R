load(file = "data/enem_escola_df.RData")

# Modelo nulo hlm
modelo_nulo_hlm2 <- lme(fixed = MEDIA_GERAL ~ 1, 
                        random = ~ 1 | CO_ESCOLA,
                        data = enem_escola_df,
                        method = "REML")
#Parâmetros do modelo
summary(modelo_nulo_hlm2)

#Verificando standard errors
stderr_nlme(modelo_nulo_hlm2)

# Comparação com OLS
modelo_ols_nulo <- lm(formula = MEDIA_GERAL ~ 1, 
                      data = enem_escola_df)

#Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

export_summs(modelo_nulo_hlm2, modelo_ols_nulo, scale = F, digits = 4, to.file = "pdf")

###########################
# Modelos com interceptos #
###########################

# variavel TP_SEXO - Enem
modelo_intercept1 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M,
                             random = ~ 1 | CO_ESCOLA,
                             data = enem_escola_dummies,
                             method = "REML")

summary(modelo_intercept1)
stderr_nlme(modelo_intercept1)
save(modelo_intercept1, file = "dataModels/modelo_intercept1.RData")

# variavel TP_COR_RACA
modelo_intercept2 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + TP_COR_RACA_3
                         + TP_COR_RACA_4
                         + TP_COR_RACA_5,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -71542.01
summary(modelo_intercept2)
stderr_nlme(modelo_intercept2)

lrtest(modelo_intercept1, modelo_intercept2)
save(modelo_intercept2, file = "dataModels/modelo_intercept2.RData")

# variavel NU_IDADE
modelo_intercept3 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -71368.04
summary(modelo_intercept3)
stderr_nlme(modelo_intercept3)
lrtest(modelo_intercept2, modelo_intercept3)

# variavel TP_NACIONALIDADE
modelo_intercept4 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -71351.71
summary(modelo_intercept4)
stderr_nlme(modelo_intercept4)
lrtest(modelo_intercept3, modelo_intercept4)

# variavel TP_DEPENDENCIA
modelo_intercept5 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1
                         + TP_DEPENDENCIA_2
                         + TP_DEPENDENCIA_3,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70885.18
summary(modelo_intercept5)
stderr_nlme(modelo_intercept5)
lrtest(modelo_intercept4, modelo_intercept5)


# variavel TP_ENSINO - NÃO PASSOU
modelo_intercept6 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1
                         + TP_ENSINO_2
                         + TP_DEPENDENCIA_2
                         + TP_DEPENDENCIA_3,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))

modelo_intercept6 <- lme(fixed = MEDIA_GERAL ~ TP_ENSINO_2 
                         + TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65489.44
# apenas com o tp_ensino = -66154.25
summary(modelo_intercept6)
stderr_nlme(modelo_intercept6)
lrtest(modelo_intercept5, modelo_intercept6)


# variavel TP_LOCALIZACAO_ESC
modelo_intercept7 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1
                         + TP_LOCALIZACAO_ESC_2
                         + TP_DEPENDENCIA_2,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70873.37
summary(modelo_intercept7)
stderr_nlme(modelo_intercept7)
lrtest(modelo_intercept6, modelo_intercept7)


# variavel IN_SEM_RECURSO - NÃO PASSOU
modelo_intercept8 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1
                         + IN_SEM_RECURSO
                         + TP_LOCALIZACAO_ESC_2
                         + TP_DEPENDENCIA_2,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70874.39
summary(modelo_intercept8)
stderr_nlme(modelo_intercept8)
lrtest(modelo_intercept6, modelo_intercept7)


# variavel Q001 - RESOLVI IGNORAR
modelo_intercept9 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1
                         + Q001_D
                         + Q001_E
                         + Q001_F
                         + Q001_G
                         + TP_LOCALIZACAO_ESC_2
                         + TP_DEPENDENCIA_2,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70809.69
summary(modelo_intercept9)
stderr_nlme(modelo_intercept9)


# variavel Q002 - RESOLVI IGNORAR
modelo_intercept10 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                         + TP_COR_RACA_1
                         + TP_COR_RACA_2
                         + NU_IDADE
                         + TP_NACIONALIDADE_1
                         + Q001_D
                         + Q001_E
                         + Q001_F
                         + Q001_G
                         + Q002_H
                         + TP_LOCALIZACAO_ESC_2
                         + TP_DEPENDENCIA_2,
                         random = ~ 1 | CO_ESCOLA,
                         data = enem_escola_dummies,
                         method = "REML",
                         na.action = na.omit,
                         control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70797.46
summary(modelo_intercept10)
stderr_nlme(modelo_intercept10)


# variavel Q006
modelo_intercept11 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
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
                          + TP_LOCALIZACAO_ESC_2
                          + TP_DEPENDENCIA_2,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70652.85
summary(modelo_intercept11)
stderr_nlme(modelo_intercept11)


# variavel Q022
modelo_intercept12 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
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
                          + TP_LOCALIZACAO_ESC_2
                          + TP_DEPENDENCIA_2,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70630.08
summary(modelo_intercept12)
stderr_nlme(modelo_intercept12)

# variavel Q025
modelo_intercept13 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_COR_RACA_1
                          + TP_COR_RACA_2
                          + NU_IDADE
                          + TP_NACIONALIDADE_1
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
                          + TP_LOCALIZACAO_ESC_2
                          + TP_DEPENDENCIA_2,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70616.56
summary(modelo_intercept13)
stderr_nlme(modelo_intercept13)

# variavel Q024
modelo_intercept14 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
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
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -70558.28
summary(modelo_intercept14)
stderr_nlme(modelo_intercept14)
#enem_intercepts <- modelo_intercept14
#save(enem_intercepts, file = "dataModels/enem_intercepts.RData")

#modelo melhorado com a tp_ensino
modelo_intercept15 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65386.06
summary(modelo_intercept15)
stderr_nlme(modelo_intercept15)

# IN_AGUA_POTAVEL - NÃO PASSA
modelo_intercept16 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_AGUA_POTAVEL,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65185.22
summary(modelo_intercept16)
stderr_nlme(modelo_intercept16)


#IN_ESGOTO_INEXISTENTE - NÃO PASSA
modelo_intercept17 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_ESGOTO_INEXISTENTE,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65183.83
summary(modelo_intercept17)
stderr_nlme(modelo_intercept17)



# IN_BIBLIOTECA
modelo_intercept18 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65181.97
summary(modelo_intercept18)
stderr_nlme(modelo_intercept18)


#IN_LABORATORIO_CIENCIAS - NÃO PASSA
modelo_intercept18 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_LABORATORIO_CIENCIAS,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65179.36
summary(modelo_intercept18)
stderr_nlme(modelo_intercept18)


#IN_LABORATORIO_INFORMATICA -  NÃO PASSA
modelo_intercept19 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_LABORATORIO_INFORMATICA,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65179.03
summary(modelo_intercept19)
stderr_nlme(modelo_intercept19)



#IN_QUADRA_ESPORTES - NÃO PASSA
modelo_intercept20 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_QUADRA_ESPORTES,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65178.5
summary(modelo_intercept20)
stderr_nlme(modelo_intercept20)



#IN_REFEITORIO - NÃO PASSA
modelo_intercept21 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_REFEITORIO,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65178.72
summary(modelo_intercept21)
stderr_nlme(modelo_intercept21)


#IN_ACESSIBILIDADE_INEXISTENTE - NÃO PASSA
modelo_intercept22 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_ACESSIBILIDADE_INEXISTENTE,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65179.06
summary(modelo_intercept22)
stderr_nlme(modelo_intercept22)



#IN_COMPUTADOR
modelo_intercept23 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65174.51
summary(modelo_intercept23)
stderr_nlme(modelo_intercept23)


#IN_DESKTOP_ALUNO - NÃO PASSA
modelo_intercept24 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + IN_DESKTOP_ALUNO,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65174.51
summary(modelo_intercept24)
stderr_nlme(modelo_intercept24)


#IN_COMP_PORTATIL_ALUNO - NÃO PASSA
modelo_intercept25 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + IN_COMP_PORTATIL_ALUNO,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65172.38
summary(modelo_intercept25)
stderr_nlme(modelo_intercept25)


#IN_TABLET_ALUNO - NÃO PASSA
modelo_intercept26 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + IN_TABLET_ALUNO,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65171.85
summary(modelo_intercept26)
stderr_nlme(modelo_intercept26)


#IN_INTERNET -  NÃO PASSA
modelo_intercept27 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + IN_INTERNET,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65169.87
summary(modelo_intercept27)
stderr_nlme(modelo_intercept27)


#IN_ALIMENTACAO - NÃO PASSA
modelo_intercept28 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + IN_ALIMENTACAO,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65171.38
summary(modelo_intercept28)
stderr_nlme(modelo_intercept28)


#QT_SALAS_UTILIZADAS
modelo_intercept29 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + QT_SALAS_UTILIZADAS,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65168.42
summary(modelo_intercept29)
stderr_nlme(modelo_intercept29)


#QT_SALAS_UTILIZA_CLIMATIZADAS - NÃO PASSA
modelo_intercept30 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + QT_SALAS_UTILIZADAS
                          + QT_SALAS_UTILIZA_CLIMATIZADAS,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65169.22
summary(modelo_intercept30)
stderr_nlme(modelo_intercept30)


# QT_SALAS_UTILIZADAS_ACESSIVEIS - NÃO PASSA
modelo_intercept31 <- lme(fixed = MEDIA_GERAL ~ TP_SEXO_M
                          + TP_ENSINO_2
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_H
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
                          + TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA
                          + IN_COMPUTADOR
                          + QT_SALAS_UTILIZADAS
                          + QT_SALAS_UTILIZADAS_ACESSIVEIS,
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65168.19
summary(modelo_intercept31)
stderr_nlme(modelo_intercept31)



# Modelo com todos os interceptos escolhidos
modelo_intercept32 <- lme(fixed = MEDIA_GERAL 
                          ~ TP_SEXO_M
                          + TP_COR_RACA_1
                          + TP_NACIONALIDADE_1
                          + Q001_E
                          + Q001_F
                          + Q001_G
                          + Q002_E
                          + Q002_F
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
                          + TP_ENSINO_2 #escola
                          + TP_COR_RACA_1:TP_ENSINO_2
                          + TP_LOCALIZACAO_ESC_2 #escola
                          + TP_DEPENDENCIA_2 #escola
                          + TP_COR_RACA_1:TP_DEPENDENCIA_2
                          + IN_BIBLIOTECA #escola
                          + IN_COMPUTADOR #escola
                          + TP_COR_RACA_1:IN_COMPUTADOR
                          + QT_SALAS_UTILIZADAS, #escola
                          random = ~ 1 | CO_ESCOLA,
                          data = enem_escola_dummies,
                          method = "REML",
                          na.action = na.omit,
                          control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65146.56
summary(modelo_intercept32)
stderr_nlme(modelo_intercept32)
save(modelo_intercept32, file = "dataModels/modelo_intercept32.RData")


#####################################################################
# INCLINATIONS
####################################################################

#TP_COR_RACA_1
modelo_inclination1 <- lme(fixed = MEDIA_GERAL 
                           ~ TP_SEXO_M
                           + TP_COR_RACA_1
                           + TP_NACIONALIDADE_1
                           + Q001_E
                           + Q001_F
                           + Q001_G
                           + Q002_E
                           + Q002_F
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
                           + TP_ENSINO_2 #escola
                           + TP_COR_RACA_1:TP_ENSINO_2
                           + TP_LOCALIZACAO_ESC_2 #escola
                           + TP_DEPENDENCIA_2 #escola
                           + TP_COR_RACA_1:TP_DEPENDENCIA_2
                           + IN_BIBLIOTECA #escola
                           + IN_COMPUTADOR #escola
                           + TP_COR_RACA_1:IN_COMPUTADOR
                           + QT_SALAS_UTILIZADAS, #escola
                           random = ~ TP_COR_RACA_1 + Q001_F + Q001_G + Q006_F + Q022_B | CO_ESCOLA,
                           data = enem_escola_dummies,
                           method = "REML",
                           na.action = na.omit,
                           control =list(msMaxIter = 1000, msMaxEval = 1000))
# LogLik = -65146.56
summary(modelo_inclination1)
stderr_nlme(modelo_inclination1)

save(modelo_inclination1, file = "dataModels/modelo_inclination1.RData")
modelo_inclination2 <- modelo_inclination1
save(modelo_inclination2, file = "dataModels/modelo_inclination2.RData")

# Q001_F -> -65146.28
# Q001_F + Q001_G -> -65145.63
# Q001_F + Q001_G + Q006_F -> -65144.22
# modelo_inclination1 = Q001_F + Q001_G + Q006_F + Q022_B -> -65139.94
# modelo_inclination2 = TP_COR_RACA_1 + Q001_F + Q001_G + Q006_F + Q022_B -> -65136.74
# modelo_inclination3 = TP_COR_RACA_1 + Q001_F + Q001_G + Q006_F + Q022_B + Q024_C -> -65133.31


modelo_inclination3 <- lme(fixed = MEDIA_GERAL 
                           ~ TP_SEXO_M
                           + TP_COR_RACA_1
                           + TP_NACIONALIDADE_1
                           + Q001_E
                           + Q001_F
                           + Q001_G
                           + Q002_E
                           + Q002_F
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
                           + TP_ENSINO_2 #escola
                           + TP_COR_RACA_1:TP_ENSINO_2
                           + TP_LOCALIZACAO_ESC_2 #escola
                           + TP_DEPENDENCIA_2 #escola
                           + TP_COR_RACA_1:TP_DEPENDENCIA_2
                           + IN_BIBLIOTECA #escola
                           + IN_COMPUTADOR #escola
                           + TP_COR_RACA_1:IN_COMPUTADOR
                           + QT_SALAS_UTILIZADAS, #escola
                           random = ~ TP_COR_RACA_1 + Q001_F + Q001_G + Q006_F + Q022_B + Q024_C | CO_ESCOLA,
                           data = enem_escola_dummies,
                           method = "REML",
                           na.action = na.omit,
                           control =list(msMaxIter = 1000, msMaxEval = 1000))

save(modelo_inclination3, file = "dataModels/modelo_inclination3.RData")
summary(modelo_inclination3)


modelo_final <- lme(fixed = MEDIA_GERAL 
                           ~ TP_SEXO_M
                           + TP_COR_RACA_1
                           + TP_NACIONALIDADE_1
                           + Q001_E
                           + Q001_F
                           + Q001_G
                           + Q002_E
                           + Q002_F
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
                           + TP_ENSINO_2 #escola
                           + TP_COR_RACA_1:TP_ENSINO_2
                           + TP_LOCALIZACAO_ESC_2 #escola
                           + TP_DEPENDENCIA_2 #escola
                           + TP_COR_RACA_1:TP_DEPENDENCIA_2
                           + IN_BIBLIOTECA #escola
                           + IN_COMPUTADOR #escola
                           + TP_COR_RACA_1:IN_COMPUTADOR
                           + QT_SALAS_UTILIZADAS, #escola
                           random = ~ TP_COR_RACA_1 | CO_ESCOLA,
                           data = enem_escola_dummies,
                           method = "REML",
                           na.action = na.omit,
                           control =list(msMaxIter = 1000, msMaxEval = 1000))
summary(modelo_final)
stderr_nlme(modelo_final)
save(modelo_final, file = "dataModels/modelo_final.RData")
# logLik = -65144.52

enem_predict$hlm2_fitted <- predict(modelo_final, enem_predict)
