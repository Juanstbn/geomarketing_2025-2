## Librerías

library(factoextra)
library(ggfortify)
library(plotly)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(GGally)

## Entradas 
zonas_gs_ingreso = st_read("../output/zonas_gs_ingreso.geojson")

## Conexión a BD

# Definir parámetros de conexión
db_host     = "localhost"       # servidor de BD
db_port     = 5432                # puerto de escucha
db_name     = "censo_rm_clases"   # nombre de la base
db_user     = "postgres"        # usuario de conexión
db_password = "postgres"        # clave de usuario

# Establecer conexión usando RPostgres
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

## Consulta SQL

sql_indicadores = "

SELECT
  z.geocodigo AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad mayor a 16 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_16
  
  -- Porcentaje de adultos mayores
--  ROUND(
--    COUNT(*) FILTER (WHERE p.p09 >= 65) * 100
--    / NULLIF(COUNT(*) FILTER (WHERE p.p09 IS NOT NULL), 0),
--  2) AS ptje_adulto_mayor
  
FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_16 DESC;

"

# Ejecutar consulta y importar resultados a data.frame en R
df_indicadores = dbGetQuery(con, sql_indicadores)


# Unir ambos dataframes
zonas_gs_indicadores = merge(
  zonas_gs_ingreso,
  df_indicadores[, c('geocodigo', 'ptje_migrantes', 'ptje_esc_mayor_16')],
  by = "geocodigo",
  all.x = TRUE,
  all.y = FALSE
  
)

zonas_gs_indicadores = na.omit(zonas_gs_indicadores)


## Seleccionar variables y escalarlas

vars_clusters = zonas_gs_indicadores[, c('ptje_migrantes', 'ptje_esc_mayor_16', 'mediana_ingreso')]

# Se elimina la geometría
vars_clusters$geometry = NULL

# vars_clusters = na.omit(vars_clusters)

# Se escalan las variables
vars_scaled = scale(vars_clusters)


# Método del codo para elegir K 
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del codo", x = "Número de clusters (k)", y = "WSS")

## Ejecutar kmeans
set.seed(123)
km = kmeans(vars_scaled, centers = 3, nstart = 25)


zonas_gs_indicadores$cluster = as.factor(km$cluster)



# CLuster 1: tasa migración baja, baja escolaridad, ingreso bajo ->
# Cluster 2: tasa migración media-baja, alta escolaridad, alto ingreso -> 
# CLuster 3: tasa migración alta, escolaridad "dispersa", ingreso medio-bajo ->  

# Escolaridad v/s Migración
ggplot(zonas_gs_indicadores, aes(x = ptje_esc_mayor_16, y = ptje_migrantes, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Escolaridad v/s Migrantes",
       x = "% Población con >= 16 años de Escolaridad",
       y = "% Población Migrante") +
  theme_minimal()


ggplot(zonas_gs_indicadores, aes(x = ptje_esc_mayor_16, y = mediana_ingreso, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Escolaridad v/s Ingreso",
       x = "% Población con >= 16 años de Escolaridad",
       y = "mediana ingreso") +
  theme_minimal()


# Evaluar utilizar ggpairs

cluster_3 = zonas_gs_indicadores[zonas_gs_indicadores$cluster == 3,]
cluster_1 = zonas_gs_indicadores[zonas_gs_indicadores$cluster == 1,]


# Ver promedios
mean(cluster_3$mediana_ingreso)



# Visualización en mapa


# Se obtiene geometría comunal para Santiago
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"


sf_comunas_santiago = st_read(con, query = sql_comunas)


## bbox del área urbana
bbox = st_bbox(zonas_gs_indicadores)



# Crear mapa de clusters
mapa_clusters = ggplot() +
  geom_sf(data = zonas_gs_indicadores, aes(fill = cluster), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas_santiago), aes(label = nom_comuna), size = 2, fontface = "bold") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title = "Mapa de Clusters de Zonas Censales",
    subtitle = "Provincia de Santiago, Región Metropolitana"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mapa_clusters)
