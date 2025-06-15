# ******************************************************************************  
#          Universidad de Chile · Modelos Multinivel 2025 · Casos influyentes  
#                  Análisis nivel-2 (comunas) – Modelo m4 (pend. NSE)  
# ******************************************************************************  
library(pacman)
pacman::p_load(lme4,
               reghelper,
               haven,
               stargazer,
               ggplot2, # gráficos
               texreg, # tablas de regresion (screenreg)
               dplyr,# manipulacion de datos
               foreign,
               lattice,
               sjPlot,
               ggeffects, 
               here, 
               summarytools
)
pacman::p_load(influence.ME, dplyr, sjPlot)
#----------------Establecer directorio-----------------------------------------

setwd("Colocar directorio")           # ajústalo a tu ruta
getwd()


#Cargar base de datos trabaja anteriormente (cansen_educ)

casen_educ_cen <- readRDS("output/casen_educ_cen.rds")


# Modelo 4: pendiente aleatoria para NSE (ya estimado) -------------------------
m4 <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    prop_empleo_gmc + n_escuelas_gmc +          # efectos fijos
    (1 + nse_cmc | comuna),                     # intercepto + pendiente NSE
  data = casen_educ
)
anova(m4, m3)   # test de devianza (m3 = fijos L1+L2)


# 1. Objeto de influencia por comuna -------------------------------------------
infl_m4 <- influence(m4, group = "comuna")   # m4: modelo con pendiente NSE

# 2. Cook’s distance y punto de corte (4/k) ------------------------------------
cook_vals <- cooks.distance(infl_m4, sort = TRUE)

k       <- length(unique(casen_educ$comuna))
cutoff  <- 4 / k                               # ≈ 0.125 si k = 32

influentes <- names(cook_vals)[cook_vals > cutoff]
print(influentes)

# 3. Gráfico de Cook -----------------------------------------------------------
plot(
  infl_m4, which = "cook",
  cutoff  = cutoff, sort = TRUE,
  xlab = "Cook's distance", ylab = "Comuna",
  main = "Distancia de Cook – Influencia por comuna (modelo m4)"
)

# 4. Estimar modelo SIN las comunas influyentes --------------------------------
casen_trim <- filter(casen_educ, !comuna %in% influentes)

m4_trim <- lmer(
  esc ~ 1 + female + nse_cmc + part_social_num +
    prop_empleo_gmc + n_escuelas_gmc +
    (1 + nse_cmc | comuna),
  data = casen_trim,
  REML = FALSE        # para comparabilidad plena
)

# 5. Tabla comparativa ---------------------------------------------------------
sjPlot::tab_model(
  m4, m4_trim,
  dv.labels = c("Modelo original", "Sin comunas influyentes"),
  show.ci   = FALSE,
  p.style   = "stars",
  digits    = 3
)

# 6. Cambio relativo de coeficientes (útil para decidir reporte) ---------------
library(broom.mixed)

orig  <- tidy(m4,  effects = "fixed")  %>% select(term, estimate) %>% rename(est_orig  = estimate)
trim  <- tidy(m4_trim, effects = "fixed") %>% select(term, estimate) %>% rename(est_trim = estimate)

left_join(orig, trim, by = "term") %>%
  mutate(delta_pct = 100 * (est_trim - est_orig) / abs(est_orig)) %>%
  print(n = Inf)
