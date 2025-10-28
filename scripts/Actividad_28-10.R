#Generar tabla de zonas censales con 3 variables sociadas (pueden ser las 2 del trabajo 1 +
#ingreso per capita]).
# Aplicar kmeans (normalizacion, metodo del codo, algoritmo de agrupamineto)
#Asignar clusters a zonas, interpretar cluster.

# =============================================================================
# 1) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(DBI)
library(RPostgres)
library(dplyr)
library(factoextra)
library(sf)
library(cluster)
library(ggplot2)

# =============================================================================
# 2) CONEXIÓN A BASE DE DATOS
# =============================================================================
con <- dbConnect(
  Postgres(),
  dbname   = "censo_v_2017",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# =============================================================================
# 3) CONSULTA SQL: GENERAR TABLA DE ZONAS CENSALES CON 3 VARIABLES ASOCIADAS
#    (IVMP, EDUCACIÓN SUPERIOR Y INGRESO PER CÁPITA)
# =============================================================================

sql_indicadores <- "
WITH agg AS (
  SELECT
    z.geocodigo::double precision AS geocodigo,
    c.nom_comuna,

    -- IVMP: % viviendas con materialidad precaria (muros OR techo OR piso)
    COALESCE(ROUND(
      COUNT(*) FILTER (
        WHERE v.p02 = 1 AND (
          v.p03a = 6 OR          -- muros: materiales precarios
          v.p03b IN (6,7) OR     -- techo: precarios o sin cubierta sólida
          v.p03c IN (4,5)        -- piso: capa de cemento sobre tierra / tierra
        )
      ) * 100.0 /
      NULLIF(COUNT(*) FILTER (WHERE v.p02 = 1), 0), 2), 0) AS ivmp,

    -- % personas mayores de 18 años con P15 >= 7 (media completa o superior)
    COALESCE(ROUND(
      COUNT(*) FILTER (WHERE p.p09 > 18 AND p.p15 >= 7) * 100.0 /
      NULLIF(COUNT(*) FILTER (WHERE p.p09 > 18), 0), 2), 0) AS edu_alta

  FROM public.personas   AS p
  JOIN public.hogares    AS h  ON p.hogar_ref_id    = h.hogar_ref_id
  JOIN public.viviendas  AS v  ON h.vivienda_ref_id = v.vivienda_ref_id
  JOIN public.zonas      AS z  ON v.zonaloc_ref_id  = z.zonaloc_ref_id
  JOIN public.comunas    AS c  ON z.codigo_comuna   = c.codigo_comuna
  JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id
  WHERE pr.nom_provincia = 'SAN ANTONIO'
  GROUP BY z.geocodigo, c.nom_comuna
)
SELECT
  geocodigo,
  nom_comuna,
  ivmp,
  edu_alta
FROM agg
ORDER BY ivmp DESC;
"

df_indicadores <- dbGetQuery(con, sql_indicadores)
# =============================================================================
# 4) CARGAR GEOMETRÍA DE ZONAS CENSALES (PROVINCIA SAN ANTONIO)
# =============================================================================
sql_geometria <- "
SELECT
  geocodigo::double precision AS geocodigo,
  geom
FROM dpa.zonas_censales_v
WHERE nom_provin = 'SAN ANTONIO';
"
sf_zonas <- st_read(con, query = sql_geometria, quiet = TRUE)

# =============================================================================
# 4) NORMALIZACIÓN DE VARIABLES
# =============================================================================
datos_cluster <- sf_zonas %>%
  select(ivmp, pct_edu_sup, ingreso_pc)

datos_scaled <- scale(datos_cluster)

# =============================================================================
# 5) MÉTODO DEL CODO PARA DETERMINAR K ÓPTIMO
# =============================================================================
fviz_nbclust(datos_scaled, kmeans, method = "wss") +
  labs(title = "Método del codo — Determinación de K óptimo",
       x = "Número de Clusters (K)",
       y = "Suma de cuadrados intra-cluster (WSS)")

# Observa el gráfico y elige el punto donde la curva se estabiliza (ejemplo K=3)
k_opt <- 3

# =============================================================================
# 6) APLICAR K-MEANS
# =============================================================================
set.seed(123)
km_res <- kmeans(datos_scaled, centers = k_opt, nstart = 25)

# Asignamos el cluster a cada zona censal
sf_zonas$cluster <- as.factor(km_res$cluster)

# =============================================================================
# 7) INTERPRETACIÓN Y RESUMEN DE CLUSTERS
# =============================================================================
cluster_summary <- df_zonas %>%
  group_by(cluster) %>%
  summarise(
    n_zonas = n(),
    IVMP_prom = round(mean(ivmp, na.rm = TRUE), 2),
    EduSup_prom = round(mean(pct_edu_sup, na.rm = TRUE), 2),
    Ingreso_prom = round(mean(ingreso_pc, na.rm = TRUE), 0)
  ) %>%
  arrange(cluster)

print(cluster_summary)