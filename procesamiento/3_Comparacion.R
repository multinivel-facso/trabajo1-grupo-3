###############################################################################
# Comparación: OLS individual  |  OLS agregado  |  Multinivel
# Base procesada: casen_educ.rds   (32 comunas núcleo urbano RM)
###############################################################################

## 1) Paquetes ---------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, lme4, texreg)

## 2) Leer la base limpia ----------------------------------------------------
casen_educ <- readRDS("output/casen_educ.rds")   # <-- ajusta si está en otra carpeta

## 3) MODELO OLS INDIVIDUAL --------------------------------------------------
reg_ind <- lm(
  esc ~ female + nse_numerico + part_social_num +
    prop_empleo + n_escuelas,
  data = casen_educ
)

## 4) CREAR BASE AGREGADA A NIVEL COMUNA -------------------------------------
agg_educ <- casen_educ %>% 
  group_by(comuna) %>% 
  summarise(
    esc             = mean(esc, na.rm = TRUE),
    female          = mean(female, na.rm = TRUE),
    nse_numerico    = mean(nse_numerico, na.rm = TRUE),
    part_social_num = mean(part_social_num, na.rm = TRUE),
    prop_empleo     = first(prop_empleo),   # ya es constante por comuna
    n_escuelas      = first(n_escuelas),    # idem
    .groups = "drop"
  )

## 5) MODELO OLS AGREGADO -----------------------------------------------------
reg_agg <- lm(
  esc ~ female + nse_numerico + part_social_num +
    prop_empleo + n_escuelas,
  data = agg_educ
)

## 6) (Re)-cargar tu modelo multinivel ---------------------------------------
# results_3 fue estimado antes; si no está en memoria, descomenta:
# results_3 <- lmer(
#   esc ~ female + nse_numerico + part_social_num +
#         prop_empleo + n_escuelas + (1 | comuna),
#   data = casen_educ
# )

## 7) COMPARAR COEFICIENTES Y E.E. EN PANTALLA -------------------------------
screenreg(
  list(reg_ind, reg_agg, results_3),
  custom.model.names = c("OLS individual", "OLS agregado", "Multinivel"),
  custom.coef.names  = c("(Intercepto)",
                         "Mujer (1)",
                         "NSE (1–7)",
                         "Hogar carente (1)",
                         "Prop. empleo",
                         "N° escuelas"),
  digits = 3
)

## 8) EXPORTAR TABLA HTML PARA EL INFORME ------------------------------------
htmlreg(
  list(reg_ind, reg_agg, results_3),
  custom.model.names = c("Individual","Agregado","Multinivel"),
  custom.coef.names  = c("(Intercepto)",
                         "Mujer (1)",
                         "NSE (1–7)",
                         "Hogar carente (1)",
                         "Prop. empleo",
                         "N° escuelas"),
  custom.gof.names = c("R²","R² ajust.","N",
                       "AIC","BIC","LogLik",
                       "N grupos (comuna)",
                       "Var:Int. comuna","Var:Residual"),
  custom.note = "%stars. Errores estándar en paréntesis",
  caption = "Comparación de modelos Individual, Agregado y Multinivel",
  caption.above = TRUE,
  file = "tabla_comparacion_modelos.html",
  doctype = FALSE
)
