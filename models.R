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
