# ==============================================================================
# PROYECTO DE TESIS: REDES, ESTRATIFICACIÓN Y SALUD EN CHILE
# ARCHIVO 2
# ==============================================================================

# --- 1. CARGA DE PAQUETES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sjPlot, sjmisc, car, knitr, gtsummary, gt)

# --- 2. CARGA DE DATOS ---
url_datos <- "https://github.com/FFFRANC0X/Proyecto-tesis-redes-estratificaci-n-y-salud/raw/main/elsoc_2022_lite.RData"
load(url(url_datos))
datos_brutos <- datos_para_github
codigos_na <- c(-999, -888, -777, -666, -99)

# --- 3. PREPARACIÓN DE DATOS CON ÍNDICE Z ---
datos_base <- datos_brutos %>%
  mutate(across(everything(), ~ if_else(. %in% codigos_na, NA, .))) %>%
  filter(!is.na(r13_nredes))

medianas_por_tramo <- datos_base %>%
  group_by(m14) %>%
  summarise(mediana_calculada = median(as.numeric(m29), na.rm = TRUE)) %>% drop_na()

datos_finales <- datos_base %>%
  left_join(medianas_por_tramo, by = "m14") %>%
  mutate(
    # NO sé si acá hago algo mal, pero después explota
    # INVERTIR VICIOS: Si 5 es "Todos los días" (Malo), hacemos (6 - valor) para que 5 sea "Nunca" (Sano).
    s04_clean = as.numeric(s04),      # Actividad Física (Mayor es mejor)
    s08_clean = 6 - as.numeric(s08),  # Cigarro (Invertido: Mayor es más sano/menos fuma)
    s10_clean = 6 - as.numeric(s10),  # Alcohol (Invertido: Mayor es más sano/menos toma)

    # Estandarizamos a Z (media 0, sd 1)
    z_fisica  = as.numeric(scale(s04_clean)),
    z_cigarro = as.numeric(scale(s08_clean)),
    z_alcohol = as.numeric(scale(s10_clean))
  ) %>%
  rowwise() %>%
  mutate(
    # Índice Promedio de Zs (Estilos de Vida)
    indice_habitos_z = mean(c(z_fisica, z_cigarro, z_alcohol), na.rm = TRUE),

    # Variables Originales
    salud_subjetiva = as.numeric(s03),
    ego_sin_titulo = if_else(as.numeric(m01) < 8, 1, 0),
    n_contactos_altos = sum(c_across(starts_with("r13_educ_")) %in% c(4, 5), na.rm = TRUE),
    tiene_palanca = factor(if_else(ego_sin_titulo == 1 & n_contactos_altos > 0, "Con Palanca", "Sin Palanca"),
                           levels = c("Sin Palanca", "Con Palanca")),
    apego_barrio = as.numeric(t02_01),
    edad = as.numeric(m0_edad),
    sexo = factor(m0_sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    educacion = as.numeric(m01),
    ingreso_final = coalesce(as.numeric(m29), mediana_calculada),
    ln_ingreso = log(ingreso_final + 1),
    comuna_cluster = as.factor(m02),
    zona = factor(if_else(region_cod == 13, "Gran Santiago", "Regiones"), levels = c("Regiones", "Gran Santiago"))
  ) %>%
  ungroup() %>%
  # Seleccionamos incluyendo el Índice Z
  select(salud_subjetiva, tiene_palanca, n_contactos_altos, apego_barrio,
         edad, sexo, educacion, ln_ingreso, comuna_cluster, zona, indice_habitos_z) %>%
  drop_na()

# --- 4. MODELAMIENTO DE ROBUSTEZ ---
# Modelo 1: Sin Hábitos
m_geo <- lm(salud_subjetiva ~ edad + sexo + educacion + ln_ingreso + apego_barrio + tiene_palanca + zona, data = datos_finales)


#Acá explota pq basicamnete todos los datos de comportamiento saludable son na.
#Este es mi problema:
datos_base %>%
  summarise(
    faltantes_deporte = sum(is.na(s04)),
    faltantes_cigarro = sum(is.na(s08)),
    faltantes_alcohol = sum(is.na(s10))
  )
