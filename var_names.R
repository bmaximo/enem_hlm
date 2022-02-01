enem_vars <- c(
  "NU_INSCRICAO",
  "CO_MUNICIPIO_RESIDENCIA",
  "SG_UF_RESIDENCIA",
  "NU_IDADE", #incluido (passa mas não deixa passar o TP_ENSINO, que no caso melhora o loglik)
  "TP_SEXO", #incluido
  "TP_COR_RACA", #incluido
  "TP_NACIONALIDADE", #incluido
  "TP_ST_CONCLUSAO",
  "TP_ANO_CONCLUIU",  #ignorado
  "TP_ESCOLA", #ignorado
  "TP_ENSINO", #incluido (passa e melhora o loglik se tirar a idade)
  "IN_TREINEIRO", #ignorado - todos não são treineiros
  "CO_ESCOLA",
  "CO_MUNICIPIO_ESC",
  "SG_UF_ESC",
  "TP_DEPENDENCIA_ADM_ESC", #ignorado
  "TP_LOCALIZACAO_ESC", #incluido
  "TP_SIT_FUNC_ESC", #ignorado
  "IN_SEM_RECURSO", #incluido (não passou no p-value)
  "NU_NOTA_CN",
  "NU_NOTA_CH",
  "NU_NOTA_LC",
  "NU_NOTA_MT",
  "NU_NOTA_REDACAO",
  "Q001", #incluido
  "Q002", #incluido
  "Q003", #ignorado
  "Q004", #ignorado
  "Q005", #ignorado
  "Q006", #incluido
  "Q022", #incluido
  "Q024", #incluido
  "Q025" #incluido
)

escola_vars <- c(
  "CO_ENTIDADE",
  "TP_DEPENDENCIA", #incluido
  "TP_LOCALIZACAO", #ignorado
  "IN_AGUA_POTAVEL", #incluido - não passa
  "IN_ENERGIA_INEXISTENTE", #ignorado - todos são zero
  "IN_ESGOTO_INEXISTENTE", #incluido - não passa
  "IN_TRATAMENTO_LIXO_INEXISTENTE", # ignorado
  "IN_AREA_VERDE", #ignorado
  "IN_BIBLIOTECA", #incluido
  "IN_LABORATORIO_CIENCIAS", #incluido - não passa
  "IN_LABORATORIO_INFORMATICA", #incluido - não passa
  "IN_QUADRA_ESPORTES", #incluido - não passa
  "IN_REFEITORIO", #incluido - não passa
  "IN_ACESSIBILIDADE_INEXISTENTE", #incluido - não passa
  "IN_COMPUTADOR", #INCLUIDO
  "IN_DESKTOP_ALUNO", #incluido - não passa
  "IN_COMP_PORTATIL_ALUNO", #incluido - não passa
  "IN_TABLET_ALUNO", #incluido - não passa
  "IN_INTERNET", #incluido - não passa
  "IN_ACESSO_INTERNET_COMPUTADOR", # ignorado
  "IN_ALIMENTACAO", #incluido - não passa
  "QT_SALAS_UTILIZADAS", #incluido
  "QT_SALAS_UTILIZA_CLIMATIZADAS", #incluido - não passa
  "QT_SALAS_UTILIZADAS_ACESSIVEIS", #incluido - não passa
)
