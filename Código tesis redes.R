# ==============================================================================
# PROYECTO DE TESIS: REDES, ESTRATIFICACIÓN Y SALUD EN CHILE
# Autor: Franco
# Fecha: Diciembre 2025
# Datos: ELSOC 2022 (Ola 6)
# ==============================================================================

# --- 1. CARGA DE PAQUETES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sjPlot, sjmisc, car, knitr, gtsummary, gt)

# ==============================================================================
# --- 2. CARGA DE DATOS (REPRODUCIBLE) ---
# ==============================================================================
# Nota para el Profesor:
# Se carga una versión reducida de la base (solo Ola 2022) alojada en GitHub

url_datos <- "https://github.com/FFFRANC0X/Proyecto-tesis-redes-estratificaci-n-y-salud/raw/main/elsoc_2022_lite.RData"

# Cargar datos desde la nube
load(url(url_datos))

# Renombramos el objeto cargado para que calce con el resto del script
# (Asumiendo que guardaste el objeto como 'datos_para_github')
datos_brutos <- datos_para_github

# Definir códigos de No Respuesta
codigos_na <- c(-999, -888, -777, -666, -99)

# ==============================================================================
# --- 3. PREPARACIÓN DE DATOS ---
# ==============================================================================

# A) Filtrado Inicial y Limpieza

datos_base <- datos_brutos %>%
  mutate(across(everything(), ~ if_else(. %in% codigos_na, NA, .))) %>%
  filter(!is.na(r13_nredes))

# B) Imputación de Ingresos (Medianas)
medianas_por_tramo <- datos_base %>%
  group_by(m14) %>%
  summarise(mediana_calculada = median(as.numeric(m29), na.rm = TRUE)) %>%
  drop_na()

# C) Construcción de Variables Finales
datos_finales <- datos_base %>%
  left_join(medianas_por_tramo, by = "m14") %>%
  rowwise() %>%
  mutate(
    # --- VARIABLE DEPENDIENTE ---
    salud_subjetiva = as.numeric(s03), # 1=Mala a 5=Excelente

    # --- VARIABLE CLAVE: PALANCA (BRIDGING) ---
    ego_sin_titulo = if_else(as.numeric(m01) < 8, 1, 0),
    # Guardamos esta variable para la tabla descriptiva
    n_contactos_altos = sum(c_across(starts_with("r13_educ_")) %in% c(4, 5), na.rm = TRUE),

    tiene_palanca = factor(if_else(ego_sin_titulo == 1 & n_contactos_altos > 0,
                                   "Con Palanca",
                                   "Sin Palanca"),
                           levels = c("Sin Palanca", "Con Palanca")),

    # --- BARRIO ---
    apego_barrio = as.numeric(t02_01),

    # --- CONTROLES ---
    edad = as.numeric(m0_edad),
    sexo = factor(m0_sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    educacion = as.numeric(m01),

    # Ingreso
    ingreso_final = coalesce(as.numeric(m29), mediana_calculada),
    ln_ingreso = log(ingreso_final + 1),

    comuna_cluster = as.factor(m02),

    # --- ZONA GEOGRÁFICA ---
    zona = case_when(
      region_cod == 13 ~ "Gran Santiago",
      TRUE ~ "Regiones"
    ),
    zona = factor(zona, levels = c("Regiones", "Gran Santiago"))
  ) %>%
  ungroup() %>%

  # --- SELECCIÓN FINAL ---
  # Incluimos TODO lo necesario para modelos Y descriptivos
  select(salud_subjetiva, tiene_palanca, n_contactos_altos, apego_barrio,
         edad, sexo, educacion, ln_ingreso, comuna_cluster, zona) %>%
  drop_na()


# -----------------------
# 4. ANÁLISIS DESCRIPTIVO (TABLA 1)

# Preparamos etiquetas para que la tabla se vea bonita (paper style)
tabla_explicativa <- datos_finales %>%
  mutate(
    salud_subjetiva = factor(salud_subjetiva, levels = 1:5,
                             labels = c("1 (Mala)", "2", "3", "4", "5 (Excelente)")),
    n_contactos_altos = factor(n_contactos_altos),
    apego_barrio = factor(apego_barrio, levels = 1:5,
                          labels = c("1 (Totalmente en desacuerdo)",
                                     "2 (En desacuerdo)",
                                     "3 (Ni de acuerdo ni desacuerdo)",
                                     "4 (De acuerdo)",
                                     "5 (Totalmente de acuerdo)"))
  )

# Generamos la tabla
t1_explicativa <- tabla_explicativa %>%
  select(salud_subjetiva, tiene_palanca, n_contactos_altos, educacion, apego_barrio) %>%
  tbl_summary(
    label = list(
      salud_subjetiva ~ "Salud Subjetiva",
      tiene_palanca ~ "¿Tiene Palanca? (Movilidad)",
      n_contactos_altos ~ "Nº de Contactos Educ. Alta",
      educacion ~ "Nivel Educativo (Años promedio)",
      apego_barrio ~ "Apego al Barrio"
    ),
    statistic = list(
      educacion ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels() %>%
  add_n()

t1_explicativa
#t1_explicativa %>% as_gt() %>% gtsave(filename = "Tabla_Para_Docs.html")


# ------------------
# 5. MODELAMIENTO ESTADÍSTICO (REGRESIONES)

# Modelo 1: Base
m_base <- lm(salud_subjetiva ~ edad + sexo + educacion + ln_ingreso +
               apego_barrio + tiene_palanca,
             data = datos_finales)

# Modelo 2: Final (Con Zona)
m_geo <- lm(salud_subjetiva ~ edad + sexo + educacion + ln_ingreso +
              apego_barrio + tiene_palanca + zona,
            data = datos_finales)

# Tabla Comparativa de Modelos
tab_model(
  m_base, m_geo,
  show.se = TRUE, show.ci = FALSE, show.std = TRUE, p.style = "stars",
  vcov.fun = "CL", vcov.args = list(cluster = datos_finales$comuna_cluster),
  dv.labels = c("Modelo Base", "Modelo Final"),
  pred.labels = c("(Intercepto)", "Edad", "Mujer", "Nivel Educativo", "Log(Ingreso)",
                  "Apego al Barrio", "Tiene palanca (Movilidad)", "Zona: Gran Santiago"),
  title = "Tabla 2: Determinantes de la Salud Subjetiva en Chile"
)


# ==============================================================================
# 6. GRÁFICOS (FOREST PLOT Y PREDICCIÓN)
# ==============================================================================

# Forest Plot, el Impacto Estandarizado
plot_model(m_geo,
           type = "std",
           show.values = TRUE, show.p = TRUE,
           value.offset = 0.4, value.size = 4, dot.size = 3.5, line.size = 1.2,
           vline.color = "grey50", colors = "Set1",
           title = "Figura 1: Impacto Estandarizado en Salud",
           axis.labels = c("Zona: G. Santiago", "Tiene Palanca (Movilidad)",
                           "Apego al Barrio", "Log(Ingreso)", "Nivel Educativo",
                           "Mujer", "Edad")
) + theme_sjplot(base_size = 12) + labs(y = "Beta Estandarizado", x = "")

# Predicción Marginal (Efecto Visual de la Palanca)
plot_model(m_geo,
           type = "pred",
           terms = "tiene_palanca",
           title = "Figura 2: Efecto Predicho de la Movilidad Social",
           colors = "Set1"
) +
  theme_sjplot(base_size = 12) +
  labs(y = "Puntaje Salud Subjetiva (1-5)", x = "Capital Social de Movilidad")



