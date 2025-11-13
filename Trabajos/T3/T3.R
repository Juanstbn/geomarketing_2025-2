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
# 2) Entrada
# =============================================================================
zonas_gs_ingreso = st_read("../output/zonas_gs_ingreso.geojson")
zonas_gs_diabetes = st_read("../output/zonas_diabetes.geojson")

# =============================================================================
# 3) CONEXIÓN A BASE DE DATOS
# =============================================================================
con <- dbConnect(
  Postgres(),
  dbname   = "censo_rm_clases",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# =============================================================================
# 4) CONSULTA SQL: 
# =============================================================================
sql_indicadores = "

SELECT
  z.geocodigo AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de adultos 
--  ROUND(
--    COUNT(*) FILTER (WHERE p.p09 >= 18) * 100
--    / NULLIF(COUNT(*) FILTER (WHERE p.p09 IS NOT NULL), 0),
--  2) AS ptje_adultos,

  -- Porcentaje de mujeres
  ROUND(
    COUNT(*) FILTER (WHERE p.p08 = 2) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p08 IS NOT NULL), 0),
  2) AS ptje_mujeres,
  
    -- Porcentaje de hombres
  ROUND(
    COUNT(*) FILTER (WHERE p.p08 = 1) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p08 IS NOT NULL), 0),
  2) AS ptje_hombres,
  
  -- Porcentaje de personas con escolaridad mayor a 18 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 18) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_18
  
 -- Ingreso promedio por hogar
  ROUND(AVG(h.ingreso_hogar), 0) AS ingreso_promedio
  
FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_adultos DESC;

"
## =========================================================
## 5) Unir base espacial con indicadores
## =========================================================
# Ejecutar consulta y importar resultados a data.frame en R
df_indicadores = dbGetQuery(con, sql_indicadores)

# Unir ambos dataframes
zonas_gs_indicadores = merge(
  zonas_gs_diabetes,
  df_indicadores[, c('geocodigo', 'ptje_adultos', 'ptje_mujeres','ptje_hombres','ptje_esc_mayor_18','ingreso_promedio')],
  by = "geocodigo",
  all.x = TRUE,
  all.y = FALSE
  
)

zonas_gs_indicadores = na.omit(zonas_gs_indicadores)

## =========================================================
## 6) Seleccionar variables para K-Means
## =========================================================
## Seleccionar variables y escalarlas

vars_clusters = zonas_gs_indicadores[, c('ptje_adultos', 'ptje_mujeres','ptje_hombres','ptje_esc_mayor_18','ingreso_promedio')]

# Se elimina la geometría
vars_clusters$geometry = NULL

# vars_clusters = na.omit(vars_clusters)

# Se escalan las variables
vars_scaled = scale(vars_clusters)

## =========================================================
## 7) K-Means
## =========================================================
# Método del codo para elegir K 
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del codo", x = "Número de clusters (k)", y = "WSS")

## Ejecutar kmeans
set.seed(123)
km = kmeans(vars_scaled, centers = 3, nstart = 25)


zonas_gs_indicadores$cluster = as.factor(km$cluster)

## =========================================================
## 8) Visualización territorial
## =========================================================
