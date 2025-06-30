# ******************************************************************************  
#          Universidad de Chile · Modelos Multinivel 2025 · Casos influyentes  
#                  Análisis nivel-2 (comunas) – Modelo m4 (pend. NSE)  
# ******************************************************************************  

# ---------------------------------------------------------------
# 0. Cargar paquetes y base de datos ----------------------------
# ---------------------------------------------------------------
library(lme4)          # modelos multinivel
library(influence.ME)  # Cook's distance por grupo
library(dplyr)         # manipulación de datos
library(forcats)       # fct_drop
library(sjPlot)        # tab_model
library(broom.mixed)   # tidy() para lmer

casen_educ_cen <- readRDS("output/casen_educ_cen.rds") %>%
  filter(!is.na(comuna)) %>%                # asegura no haber NAs
  mutate(comuna = as.factor(comuna))        # comuna como factor

stopifnot(nlevels(casen_educ_cen$comuna) == 32)  # debe haber 32 comunas

# ────────────────────────────────────────────────────────────────────────
# 1. Modelo original (m4) ‒ con nuevas variables contextuales
# ────────────────────────────────────────────────────────────────────────
m4 <- lmer(
  esc ~ female + nse_cmc + part_social_num +
    prop_empleo_gmc + prop_pob_esc_gmc + idh_gmc +
    (1 + nse_cmc | comuna),
  data = casen_educ_cen
)

# ────────────────────────────────────────────────────────────────────────
# 2. Cook’s Distance por comuna
# ────────────────────────────────────────────────────────────────────────
infl_m4   <- influence(m4, group = "comuna")
cook_vals <- cooks.distance(infl_m4, sort = FALSE)

# añade nombres si faltan
if (is.null(names(cook_vals)) || length(names(cook_vals)) == 0) {
  names(cook_vals) <- levels(casen_educ_cen$comuna)
}

k      <- nlevels(casen_educ_cen$comuna)   # 32
cutoff <- 4 / k                            # ≈ 0.125
influentes <- names(cook_vals)[!is.na(cook_vals) & cook_vals > cutoff]

cat("Comunas influyentes (Cook >", round(cutoff, 3), "):\n")
print(influentes)

# ────────────────────────────────────────────────────────────────────────
# 3. Gráfico de Cook
# ────────────────────────────────────────────────────────────────────────
plot(
  infl_m4, which = "cook",
  cutoff  = cutoff, sort = TRUE,
  xlab    = "Cook's distance", ylab = "Comuna",
  main    = "Distancia de Cook – Influencia por comuna (modelo m4)"
)

# ────────────────────────────────────────────────────────────────────────
# 4. Modelo recortado (m4_trim) sin comunas influyentes
# ────────────────────────────────────────────────────────────────────────
casen_trim <- casen_educ_cen %>%
  filter(!comuna %in% influentes) %>% 
  mutate(comuna = fct_drop(comuna))

m4_trim <- lmer(
  esc ~ female + nse_cmc + part_social_num +
    prop_empleo_gmc + prop_pob_esc_gmc + idh_gmc +
    (1 + nse_cmc | comuna),
  data  = casen_trim,
  REML  = FALSE
)

cat("\nNúmero de comunas en m4_trim:\n")
print(summary(m4_trim)$ngrps)

# ────────────────────────────────────────────────────────────────────────
# 5. Comparación de efectos fijos (tabla)
# ────────────────────────────────────────────────────────────────────────
tab_model(
  m4, m4_trim,
  dv.labels = c("Modelo original", "Sin comunas influyentes"),
  show.ci   = FALSE, p.style = "stars", digits = 3
)

# ────────────────────────────────────────────────────────────────────────
# 6. Cambio porcentual de coeficientes
# ────────────────────────────────────────────────────────────────────────
orig <- broom.mixed::tidy(m4,      effects = "fixed") %>% 
  select(term, estimate) %>% rename(est_orig = estimate)

trim <- broom.mixed::tidy(m4_trim, effects = "fixed") %>% 
  select(term, estimate) %>% rename(est_trim = estimate)

left_join(orig, trim, by = "term") %>%
  mutate(delta_pct = 100 * (est_trim - est_orig) / abs(est_orig)) %>%
  arrange(desc(abs(delta_pct))) %>% 
  print(n = Inf)



